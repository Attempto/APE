% This file is part of the Attempto Parsing Engine (APE).
% Copyright 2008-2012, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
%
% The Attempto Parsing Engine (APE) is free software: you can redistribute it and/or modify it
% under the terms of the GNU Lesser General Public License as published by the Free Software
% Foundation, either version 3 of the License, or (at your option) any later version.
%
% The Attempto Parsing Engine (APE) is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
% PURPOSE. See the GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with the Attempto
% Parsing Engine (APE). If not, see http://www.gnu.org/licenses/.

:- module(drs_to_coreace, [
		drs_to_coreace/2,
		bigdrs_to_coreace/2
	]).

/** <module> DRS to Core ACE verbalizer

Translates an Attempto DRS into the Core ACE fragment of Attempto Controlled English (ACE).

@author Kaarel Kaljurand
@version 2012-07-15

The setup: we have DRS-boxes and discourse referents, such that

 * Each referent lives-in exactly 1 box.
 * For each box at least 1 referent lives-in it.
 * Each referent has a unique ID and maps-to a set of conditions.
 * Each box has a unique ID and has a name (if, then, '', or, `it is false that', etc.)

The task: verbalize each discourse referent, i.e. the conditions
of each discourse referent. And do it in the context of

 1. the box,
 2. whether the referent has been verbalized already,
 3. whether some other referent from the same box has been verbalized already.

The order is important. It is set by referents_to_orderedreferents/2.

For the developer: to see the internals, use:

==
?- drs_to_coreace:ref2conds(Referent, Conditions, BoxId, SentenceId).
==

@tbd URGENT: Use "Is it true that ... ?" for Yes/No questions
@tbd Reorder conditions in OR-contructs so that the implications always come last
    Every dog barks and it is false that a cat sleeps or there is a man. (3711)
    There is a man or it is false a cat sleeps and if there is a dog ...
@tbd Every pet is a dog or is a cat ,and is not a cow. # Support for mixing 'and' and 'or' is buggy.
@tbd A customer enters every card or enters every code. # Serious scoping problem.
@tbd Every book is something that an author that a publisher that John hates knows writes. (can be improved)
@tbd What does every man like?
@tbd What does no man like?
@tbd There are less than 3 men and less than 3 women.
@tbd Less than 3 men and John wait.
@tbd /for-each-of plural constructions/
@tbd Make `less than' and `at most' work in general
@tbd Maybe use reflexive pronouns also when definite NPs are available: himself, herself, itself.

*/


:- use_module(morphgen, [
		clear_vars/0,
		add_var/1,
		remove_singletons/2,
		listlist_listatom/2,
		surface_noun/4,
		surface_verb/3,
		surface_property/3,
		surface_property/6,
		surface_adverb/3,
		surface_quotedstring/2,
		get_di_marker/4
	]).

:- use_module(drs_utils, [
		get_toplevel_object_referents/2
	]).

:- use_module(drs_to_drslist, [
		drs_to_drslist/2
	]).

/*
:- debug(verbose).
:- debug(that).
:- debug(toplevel).
:- debug(npcoord).
*/

% Operators used in the DRS
:- op(400, fy, -).
:- op(400, fy, ~).
:- op(500, xfx, =>).
:- op(500, xfx, v).
:- op(500, xfx, '--').


:- dynamic is_rep/1, ref2conds/4.


% @tbd rename
bigdrs_to_coreace(Drs, AceList) :-
	drs_to_drslist(Drs, DrsList),
	drslist_to_coreace(DrsList, AceList).


drslist_to_coreace([], []).

drslist_to_coreace([Drs | DrsList], [Ace | AceList]) :-
	drs_to_coreace(Drs, Ace),
	drslist_to_coreace(DrsList, AceList).


%% drs_to_coreace(+Drs:drs, -AceSentenceList:list) is det.
%
%==
% Empty DRS -> []
% Unsupported DRS -> []
% Supported DRS -> [sentence1, sentence2, ..., sentenceN], where sentence is an atom
%==
%
% @param Drs is an Attempto DRS
% @param AceSentenceList is a verbalization of the input DRS in Core ACE (a list of Core ACE sentences)
%
drs_to_coreace(drs([], []), []) :- !.

drs_to_coreace(DRS, AceSentenceList) :-
	retractall(is_rep(_)),
	clear_vars,
	% BUG: why do we need the next line?
	assert(is_rep(nil)),

	% Make a copy to preserve the variables in the caller.
	copy_term(DRS, DRSCopy),

	DRSCopy = drs(Dom, ConditionList),

	get_toplevel_object_referents(ConditionList, toplevel(ToplevelReferentList, UnsortedSubjectList, UnsortedObjectList, NamedList)),
	numbervars(DRSCopy, 0, _),

	drs_to_coreace_nvar_wrapper(drs(Dom, ConditionList), [ToplevelReferentList, UnsortedSubjectList, UnsortedObjectList, NamedList], Result),
	!,
	post_process(Result, AceSentenceList),
	!.

drs_to_coreace(_Drs, []).
% TODO: Throw an exception
%drs_to_coreace(Drs, _) :-
%	throw(error('Not implemented', context(drs_to_coreace/2, Drs))).


%% drs_to_coreace_nvar_wrapper(Drs, List, Result)
%
%
drs_to_coreace_nvar_wrapper(Drs, List, Result) :-
	retractall(ref2conds(_, _, _, _)),
	reset_counter(box_counter),
	make_boxid(BoxId),
	catch(drs_to_coreace_nvar(Drs, BoxId, List, []--_, Result), _, fail).


%% post_process//0
%
% Apply some post-processing on the generated tokens.
%
post_process -->
	remove_singletons,
	find_sentencelist,
	listlist_listatom.


%% drs_to_coreace_nvar(+Drs:drs, +BoxId:term, +Toplevel:list, +RefInRefOut:term, -AceList:list) is det.
%
% @param Drs is an Attempto DRS with variables nvard
% @param BoxId is a DRS box identifier
% @param Toplevel is a list of 4 lists containing referents of various types
% @param RefInRefOut list of referents in--out
% @param AceText is a verbalization of the DRS in Core ACE
%
drs_to_coreace_nvar(Drs, BoxId, [ToplevelReferentList, UnsortedSubjectList, UnsortedObjectList, NamedList], RI--RO, Result1) :-

	debug(toplevel, "toplevel referents:
	ToplevelReferentList: ~W
	UnsortedSubjectList: ~W
	UnsortedObjectList: ~W
	NamedList: ~W~n",
	[ToplevelReferentList, [numbervars(true)], UnsortedSubjectList, [numbervars(true)], UnsortedObjectList, [numbervars(true)], NamedList, [numbervars(true)]]),

	% We assert the DRS conditions.
	% The DRS itself is not used anymore below.
	make_ref2conds(Drs, BoxId),

	% The next line means, that we use the `there is' construction
	% only if needed. If there is a verb that calls a noun
	% then the noun will not be verbalized by `there is'.
	findall(Referent, use_there_is(Referent), ThereIsReferents),

	debug(verbose, "ThereIsReferents: ~W~n", [ThereIsReferents, [numbervars(true)]]),

	% Remove syntactic objects because we do not want to use 'there is' with objects.
	% E.g. There is a cat. John hates the cat. --> John hates a cat.
	subtract(ToplevelReferentList, UnsortedObjectList, List1),

	% Add syntactic subjects because we want to use 'there is' with subjects.
	% E.g. A cat sees John. --> There is a cat X1. The cat X1 sees John.
	append(List1, UnsortedSubjectList, List2),

	% Remove named-objects to avoid e.g. "There is John.", regardless
	% of whether `John' is later used as a subject or as an object.
	subtract(List2, NamedList, UnnamedList2),

	% Remove duplicates
	list_to_set(UnnamedList2, InitialReferents),

	debug(verbose, "InitialReferents: ~w~n", [InitialReferents]),

	append(InitialReferents, ThereIsReferents, List3),
	list_to_set(List3, Refs),
	referents_to_orderedreferents(Refs, OrderedRefs),

	debug(verbose, "input to call_referents_to_acetexts/2: ~W~n", [OrderedRefs, [numbervars(true)]]),

	call_referents_to_acetexts(OrderedRefs, RI--RO, Result1).


%% referents_to_orderedreferents(+Refs:list, -OrderedRefs:list) is det.
%
% Sorts the list of referents so that the resulting verbalization
% would be most natural. There are three levels of keys:
%
% 1. The referents are sorted by their sentence ID (ascending);
% 2. The referents are sorted by their box counter (depth first);
% 3. The referents are sorted by the subject argument of their
% corresponding predicate (if the referent has a corresponding predicate).
%
% @tbd The 3rd point needs rethinking.
%
% @param Refs is a list of discourse referents
% @param OrderedRefs is the list ordered
%
referents_to_orderedreferents(Refs, OrderedRefs) :-
	findall((SentenceId, BoxCounter, N)-Referent, (
		member(Referent, Refs),
		ref2conds(Referent, Conditions, id(BoxCounter, _, _), SentenceId),
		get_orderer(Conditions, N)
	), List1),
	debug(verbose, "Keys          : ~W~n", [List1, [numbervars(true)]]),
	keysort(List1, List2),
	debug(verbose, "Keys (sorted) : ~W~n", [List2, [numbervars(true)]]),
	findall(Referent, member(_Key-Referent, List2), OrderedRefs).


%% get_orderer(+Conditions:list, -N:integer) is det.
%
% Returns an integer that is a sum of subject, object and direct object
% recency. The idea is to order the (predicate) conditions that are
% in the same box and have the same ID, so that they reflect the order
% of respective verbs in the original sentence. We exploit the fact that
% variable creation recency reflects the order of the original words.
% Note also that object-conditions get a smaller ranking than predicate conditions.
% This means that a 'there is'-object is placed before an object with a verb, e.g.
%==
% It is false that a dog barks and that there is a man.
%==
% is paraphrased as:
%==
% It is false that there is a man and that a dog barks.
%==
%
% @tbd This predicate is a hack. The DRS verbalizer should not rely on the
% variable creation times, and it should not try to reflect any "original order".
% Instead it should find the most natural way to order the predicates, e.g. by
% trying to chain them (as does the NP APE verbalizer).
%
% @param Conditions is a list of DRS condition
% @param N is a ranking number that determines how the referent is positioned in the verbalization
%
get_orderer(Conditions, N) :-
	memberchk(predicate(_, _, '$VAR'(N)), Conditions),
	!.

get_orderer(Conditions, N) :-
	memberchk(predicate(_, _, '$VAR'(N1), '$VAR'(N2)), Conditions),
	!,
	N is N1 + N2.

get_orderer(Conditions, N) :-
	memberchk(predicate(_, _, '$VAR'(N1), '$VAR'(N2), '$VAR'(N3)), Conditions),
	!,
	N is N1 + N2 + N3.

get_orderer(Conditions, N) :-
	memberchk(object('$VAR'(N1), _, _, _, _, _), Conditions),
	N is (-1 * (1 / (N1 + 1))),
	!.

get_orderer(_Conditions, 0).


%% find_sentencelist(+TokenList:list, -SentenceList:list) is det.
%
% @param TokenList is a list of ACE tokens
% @param SentenceList is a list of sentences (where a sentence is a list of ACE tokens)
%
find_sentencelist([], []).

find_sentencelist(TokenList, [Sentence | SentenceList]) :-
	find_sentence(TokenList, Sentence, RestTokenList),
	find_sentencelist(RestTokenList, SentenceList).


%% find_sentence(+TokenList:list, -Sentence:list, -RestTokenList:list) is det.
%
% @param TokenList is a list of ACE tokens
% @param Sentence is a list of sentences (where a sentence is a list of ACE tokens)
% @param RestTokenList is a list of ACE tokens
%
find_sentence(['.' | RestTokenList], [], RestTokenList) :- !.

find_sentence([Token | RestTokenList], [Token | RestSentence], RestTokenList2) :-
	find_sentence(RestTokenList, RestSentence, RestTokenList2).


%% use_there_is(+Referent:nvar) is det.
%
% Succeeds if the referent is a toplevel referent.
%
% Referent is toplevel if:
%
% 1. Its condition list is not empty. (BUG: redundant?)
% 2. It is not called by any other Referent in the same box.
% 3. It is called by another Referent in the same box, BUT
% it is toplevel, and it is (first) called by an embedded box.
%
% The 3rd clause was added to correctly paraphrase:
% There is a man who does not sleep. John sees the man.
%
% @param Referent is a discourse referent
%
use_there_is(Referent) :-
	ref2conds(Referent, [Condition | ConditionList], BoxId, _SentenceId),
	\+ is_called(Referent, [Condition | ConditionList], BoxId, _).

% Handles the case: There is a man who does not sleep. John sees the man.
use_there_is(Referent) :-
	toplevel_id(ToplevelId),
	% Referent is top-level
	ref2conds(Referent, [Condition | ConditionList], ToplevelId, _SId1),
	% Referent is called by a top-level referent.
	is_called(Referent, [Condition | ConditionList], ToplevelId, SId2),
	% Referent is called from an embedded box.
	is_called(Referent, [Condition | ConditionList], BoxId, SId3),
	\+ toplevel_id(BoxId),
	% The embedded box is introduced before the other caller (in terms of the sentence Id).
	SId3 < SId2.


%% toplevel_id(?ToplevelId)
%
% Defines the form of the ID of the topmost DRS
%
toplevel_id(id(0, top, [])).


%% is_called(+Referent:nvar, +ReferentConditions:list, +BoxId:term, -SentenceId:integer) is det.
%
% Succeeds if Referent corresponds to a proper name (because proper names are always called).
% Succeeds if Referent is used as a non-first argument (i.e. "called") in any condition in the given box.
%
% @param Referent is a discourse referent
% @param ReferentConditions is a list of conditions whose first argument this referent is.
% @param BoxId is a DRS box identifier
%
is_called(Referent, _, BoxId, SentenceId) :-
	ref2conds(Referent2, ConditionList, BoxId, SentenceId),
	Referent \= Referent2,
	member(Condition, ConditionList),
	Condition \= formula(_, _, _),
	arg(Num, Condition, Referent),
	Num > 1.


%% call_referents_to_acetext(+RefList:list, +RefInRefOut:term, -Result:list) is det.
%
% @param RefList is a list of top-level DRS referents
% @param RefInRefOut list of referents in--out
% @param Result is an ACE text (list of tokens)
%
call_referents_to_acetexts(RefList, RI--RO, Result) :-
	(
		referents_to_acetexts(RefList, RI--RO, ACETexts)
	->
		flatten(ACETexts, Result)
	;
		Result = []
	).


%% referents_to_acetexts(+RefList:list, +RefInRefOut:term, -AceText:list) is det.
%
% @param RefList is a list of top-level DRS referents
% @param RefInRefOut list of referents in--out
% @param AceText is an ACE text (list of tokens)
%
referents_to_acetexts([], RI--RI, '.').

referents_to_acetexts([Referent | ReferentsTail], RI--RO, [Text | TextTail]) :-
	debug(verbose, "used referents: ~W~n", [RI, [numbervars(true)]]),
	referent_text(Referent, top, RI--RTmp, Text, _),
	debug(verbose, "top referent: ~W; text: ~W; used referents: ~W~n", [Referent, [numbervars(true)], Text, [numbervars(true)], RTmp, [numbervars(true)]]),
	!,
	referents_to_acetexts(ReferentsTail, RTmp--RO, TextTail).


%% referent_text(+Referent:nvar, -Text:list, -Features:list) is det.
%% referent_text(+Referent:nvar, +TopDeep:atom, -Text:list, -Features:list) is det.
%
% @param Referent is a discourse referent or data item
% @param TopDeep is in {top, deep}
% @param Text is an ACE text that corresponds to the discourse referent
% @param Features is in {f(sg, subj), f(sg, obj), f(pl, subj), f(pl, obj)}, for number and case sharing
%
referent_text(Referent, RI--RO, Text, Features) :-
	referent_text(Referent, deep, RI--RO, Text, Features).


referent_text(named(Name), _, RI--RI, NameText, f(SgPl, _)) :-
	surface_noun(pn, Name, SgPl, NameText),
	!.

% Recursively call drs_to_coreace_nvar on the embedded DRS.
referent_text(Referent, deep, RI--RO, [that | SentenceListCoord], _) :-
	ref2conds(Referent, [Referent:drs(Dom, CondList)], _BoxId, _SentenceId),
	debug(that, "THAT: drs: ~W~n", [drs(Dom, CondList), [numbervars(true)]]),
	retractall(ref2conds(_, _, _, _)),
	reset_counter(box_counter),
	make_boxid(NewBoxId),
	drs_to_coreace_nvar(drs(Dom, CondList), NewBoxId, [[], [], [], []], RI--RO, Result),
	debug(that, "THAT: result: ~q~n", [Result]),
	find_sentencelist(Result, [_Ignore_BUG | SentenceList]),
	debug(that, "THAT: sentence(s): ~W~n", [SentenceList, [numbervars(true)]]),
	sentencelist_sentencelistcoord(SentenceList, SentenceListCoord).

referent_text(formula(Expr1, Eq, Expr2), top, RI--RO, [BoxMarker, Expr1Text, Eq, Expr2Text], _) :-
	!,
	expr_exprpp(Expr1, RI--RTmp, Expr1Text),
	expr_exprpp(Expr2, RTmp--RO, Expr2Text),
	ref2conds(formula(Expr1, Eq, Expr2), _, BoxId, _SentenceId),
	get_box_prefix(BoxId, BoxMarker).

% Support for query pronouns: 'who' and 'what'.
% Note: 'how' and 'which' are not covered by this rule.
% Note: the question mark is added later.
% Note: we only support top-level query-conditions.
% Note: this rule generates "there is who?" and never "there are who?"
referent_text(Referent, TopDeep, RI--[Referent | RI], [ThereIs, QueryWord], _Features) :-
	toplevel_id(ToplevelId),
	ref2conds(Referent, [Query], ToplevelId, _SentenceId),
	query(Referent, Query, _, QueryWord),
	get_box_prefix(ToplevelId, BoxPrefix),
	get_thereis(TopDeep, sg, BoxPrefix, ThereIs),
	\+ memberchk(Referent, RI).

referent_text(Referent, TopDeep, RI--RO, Text, Features) :-
	get_type(Referent, Type, BoxId),
	get_box_prefix(BoxId, BoxPrefix),
	debug(that, 'referent_text: ~W ~W ~w ~w~n', [Referent, [numbervars(true)], Type, [numbervars(true)], BoxId, BoxPrefix]),
	!,
	conds_text(Type, BoxPrefix, TopDeep, RI--RO, Text, Features),
	debug(verbose, "referent_text_out: ~W; used referents: ~W~n", [Text, [numbervars(true)], RO, [numbervars(true)]]).

referent_text(Expression, deep, RI--RO, ExpressionText, f(sg, _)) :-
	expr_exprpp(Expression, RI--RO, ExpressionText).


%% get_type(+Referent:nvar, -Type:term, -BoxId:term) is det.
%
% Type is a structured representation of the conditions, e.g. we group
% all properties and modifiers together into a list and represent as
% one argument.
%
% @param Referent is a discourse referent
% @param Type is a structured form of Conditions
% @param BoxId is an identifier of the DRS box
%
get_type(Referent, Type, BoxId) :-
	ref2conds(Referent, Conditions, BoxId, _SentenceId),
	conditions_type(Conditions, Type).


%% conditions_type(+Conditions:list, -Type:term) is det.
%
% @param Conditions is a list of DRS conditions that correspond to a referent
% @param Type is a structured form of those conditions
%
% There are three types of referents:
%
% 1. Those that have an object-condition
% 2. Those that have a predicate-condition
% 3. Those that have no conditions (i.e. the that-subordination referent)

conditions_type(Conditions, noun(Mains, parts(Parts), owners(Owners), adjectives(Adjectives))) :-
	member(Condition, Conditions),
	functor(Condition, object, _),
	!,
	conditions_nountype(Conditions, Mains, Parts, Owners, Adjectives).

conditions_type(Conditions, verb(Predicates, adverbs(Adverbs), pps(PPs))) :-
	member(Condition, Conditions),
	is_predicate(Condition),
	!,
	conditions_verbtype(Conditions, Predicates, Adverbs, PPs).

conditions_type(Conditions, Conditions).


%% conditions_nountype(+Conditions:list, -Mains:list, -Parts:list, -Owners:list, -Adjectives:list) is det.
%
% @param Conditions
% @param Mains
% @param Parts
% @param Owners
% @param Adjectives
%
conditions_nountype([], [], [], [], []).

conditions_nountype([H | T], Mains, [Part | Parts], Owners, Adjectives) :-
	H = has_part(_, Part),
	!,
	conditions_nountype(T, Mains, Parts, Owners, Adjectives).

conditions_nountype([H | T], Mains, Parts, [H | Owners], Adjectives) :-
	functor(H, relation, _),
	!,
	conditions_nountype(T, Mains, Parts, Owners, Adjectives).

conditions_nountype([H | T], Mains, Parts, Owners, [H | Adjectives]) :-
	functor(H, Functor, _),
	(
		Functor = property
	;
		Functor = query
	),
	!,
	conditions_nountype(T, Mains, Parts, Owners, Adjectives).

% BUG: Here we convert those atoms that are in fact numbers (e.g. '5') into actual numbers (e.g. 5).
% APE does not generate such "atom numbers", but other DRS generators might.
conditions_nountype(
		[object(Referent, Value, Quantisation, Unit, Operator, Count) | T],
		[object(Referent, Value, Quantisation, Unit, Operator, NCount) | Mains], Parts, Owners, Adjectives
	) :-
	atom(Count), Count \= na,
	!,
	atom_number(Count, NCount),
	conditions_nountype(T, Mains, Parts, Owners, Adjectives).

% Fall back: other conditions go to Mains.
conditions_nountype([H | T], [H | Mains], Parts, Owners, Adjectives) :-
	conditions_nountype(T, Mains, Parts, Owners, Adjectives).


%% conditions_verbtype(+Conditions:list, -Predicates:list, -Adverbs:list, -PPs:list) is det.
%
% @param Conditions
% @param Predicates
% @param Adverbs
% @param PPs
%
conditions_verbtype([], [], [], []).

conditions_verbtype([H | T], Predicates, [H | Adverbs], PPs) :-
	functor(H, Functor, _),
	(
		Functor = modifier_adv
	;
		Functor = query
	),
	!,
	conditions_verbtype(T, Predicates, Adverbs, PPs).

conditions_verbtype([H | T], Predicates, Adverbs, [H | PPs]) :-
	functor(H, Functor, _),
	(
		Functor = modifier_pp
	),
	!,
	conditions_verbtype(T, Predicates, Adverbs, PPs).

% Fall back: other conditions go to Predicates.
conditions_verbtype([H | T], [H | Predicates], Adverbs, PPs) :-
	conditions_verbtype(T, Predicates, Adverbs, PPs).


%% counter(+Name:atom, -N:integer) is det.
%
% Generate integers.
% If no integer has been generated yet then start with 1.
%
% @param Name is the name of the counter
% @param N is an integer
%
% @tbd This is a general predicate, move somewhere else.
%
counter(Name, N) :-
	catch(
		(nb_getval(Name, N0), succ(N0, N), nb_setval(Name, N)),
		_,
		(N = 1, nb_setval(Name, N))
	).


%% reset_counter(+Name:atom) is det.
%
% Reset the named counter to 0.
%
% @tbd This is a general predicate, move somewhere else.
%
reset_counter(Name) :-
	nb_setval(Name, 0).


%% make_ref2conds(+Drs:drs, +BoxId:term) is det.
%
% We assert all discourse referents from DRS into ref2conds/4 to speed up lookup later.
%
% @param Drs is an Attempto DRS
%
make_ref2conds(drs(Dom, Conds), BoxId) :-
	assert_referents(Dom, Conds, BoxId),
	process_conds(Conds, BoxId).


%% assert_referents(+Referents:list, +ConditionList:list, +BoxId:term) is det.
%
% Finds all the conditions in the ConditionList that have the same Referent
% as the first argument. Asserts the predicate =|ref2conds/4|= containing
% information about the shared Referent, the conditions that share the Referent,
% and the DRS-box ID.
% Finally processes separately the remaining conditions: in case the remaining
% conditions have the form formula/3 then they are asserted as they are.
% In case the remaining conditions are complex conditions then they are just ignored.
% Otherwise (e.g. in case of relation/3) a failure is triggered.
%
% flatten/3 is used to remove the DRS embedded lists
%
% @param Referents are discourse referents of the DRS box
% @param ConditionList are conditions of the DRS box
% @param BoxId is a unique ID of the box
%
% @bug Maybe complex conditions should not reach this rule.
%
assert_referents([], Conditions, BoxId) :-
	assert_rest(Conditions, BoxId).

assert_referents([Referent | ReferentsTail], Conditions, BoxId) :-
	flatten(Conditions, FlatConditions),
	mypartition(Referent, FlatConditions, Included, Excluded, Id),
	assert(ref2conds(Referent, Included, BoxId, Id)),
	assert_referents(ReferentsTail, Excluded, BoxId).


first_argument_is_referent(Referent, Condition) :-
	Condition \= formula(_, _, _),
	arg(1, Condition, Referent).


%% mypartition(+Referent, +Conditions, ?Included, ?Excluded, ?Id) is det.
%
%
mypartition(_Referent, [], [], [], _).

mypartition(Referent, [Condition-STId | Conditions], [Condition | Included], Excluded, SId) :-
	first_argument_is_referent(Referent, Condition),
	!,
	get_sentence_id(STId, SId),
	mypartition(Referent, Conditions, Included, Excluded, SId).

mypartition(Referent, [ConditionWithId | Conditions], Included, [ConditionWithId | Excluded], SId) :-
	mypartition(Referent, Conditions, Included, Excluded, SId).


%% assert_rest(+CondList:list, +BoxId:term) is det.
%
%
assert_rest([], _).

assert_rest([formula(Expr1, Eq, Expr2)-STId | CondList], BoxId) :-
	get_sentence_id(STId, SId),
	assert(ref2conds(formula(Expr1, Eq, Expr2), [formula(Expr1, Eq, Expr2)], BoxId, SId)),
	assert_rest(CondList, BoxId).

assert_rest([relation(_, _, _)-_ | _CondList], _BoxId) :-
	!,
	fail.

assert_rest([has_part(_, _)-_ | _CondList], _BoxId) :-
	!,
	fail.

assert_rest([_Cond | CondList], BoxId) :-
	assert_rest(CondList, BoxId).


%% process_conds(+Conditions:list, +BoxId:term) is det.
%
% Traverses the DRS to assert all the referents together with their
% conditions. We also generate an ID for each embedded DRS (i.e. a box).
%
% The conditions are first sorted to make sure that less complex conditions
% (e.g. negations) are verbalized before more complex ones (e.g. implications).
%
% @param Conditions are conditions of the DRS box
% @param BoxId is a unique ID of the box
%
process_conds(Cs, BoxId) :-
	sort(Cs, CsSorted),
	process_conds_loop(CsSorted, BoxId).


process_conds_loop([], _).

process_conds_loop([C | Cs], BoxId) :-
	walk_cond(C, BoxId),
	process_conds_loop(Cs, BoxId).


%% walk_cond(+Condition:term, +BoxId:term) is det.
%
% @param Condition is a DRS (complex) condition
% @param BoxId is a unique ID of the box
%
walk_cond(_-_, _). % ignoring simple conditions

walk_cond([C | Cs], _BoxId) :-
	include(is_complex_or_predicate, [C | Cs], Predicates),
	length(Predicates, Len),
	Len > 1,
	!,
	throw(error('Predicate group is too complex', context(walk/2, Predicates))).

walk_cond([C | Cs], BoxId) :-
	!,
	process_conds([C | Cs], BoxId).

walk_cond(question(drs(QDom, QConds)), BoxId) :-
	!,
	memberchk(query(_, _)-_, QConds),
	assert_referents(QDom, QConds, BoxId),
	process_conds(QConds, BoxId).

walk_cond(Label:drs(Dom, Conds), BoxId) :-
	!,
	assert(ref2conds(Label, [Label:drs(Dom, Conds)], BoxId, 'BUG')).

% BUG: handling of relation/3 alone in the then-box
% IN:  A dog of every man who waits barks.
% OUT: A dog that every man who waits OF-RELATION barks.
% BUG: UniqueRef should actually be a numbervard variable
walk_cond(drs(Dom1, Conds) => drs([], [relation(Owned, of, Owner)-STId]), ParentBoxId) :-
	!,
	make_boxid(if, ParentBoxId, BoxId1),
	assert_referents(Dom1, Conds, BoxId1),
	process_conds(Conds, BoxId1),
	make_boxid(then, ParentBoxId, BoxId2),
	counter(ref, UniqueRef),
	assert_referents([UniqueRef], [predicate(UniqueRef, 'OF-RELATION', Owner, Owned)-STId], BoxId2).

walk_cond(drs(Dom1, Conds1) => drs(Dom2, Conds2), ParentBoxId) :-
	make_boxid(if, ParentBoxId, BoxId1),
	assert_referents(Dom1, Conds1, BoxId1),
	process_conds(Conds1, BoxId1),
	make_boxid(then, ParentBoxId, BoxId2),
	assert_referents(Dom2, Conds2, BoxId2),
	process_conds(Conds2, BoxId2).

% Every disjunction has two DRSs. If the first DRS
% is complex (e.g. implication, negation, ...) then the conditions
% of the second DRS cannot share discourse referents with it.
% This property allows us to safely change the order the DRSs providing
% a more readable paraphrase and/or avoiding scope problems,
% e.g. naively paraphrasing
%   {Every man eats} or John drinks.
% as
%   If there is a man X1 then {the man X1 eats or John drinks}.
%
% @tbd We could relax the requirement that a complex condition
% must contain a single condition that embeds a DRS. Instead we should
% simply require that the OR boxes do not share variables that are
% not declared on a higher level. Unfortunately at this stage we have
% already numbervared the variables which makes this check difficult to implement.
% So we currently only check that the variables-list is empty in the 1st DRS
% but non-empty in the 2nd.
walk_cond(drs([], Conds1) v Drs2, BoxId) :-
	Drs2 = drs([_|_], _),
	!,
	walk_cond(Drs2 v drs([], Conds1), BoxId).

walk_cond(drs(Dom1, Conds1) v drs(Dom2, Conds2), ParentBoxId) :-
	make_boxid(or1, ParentBoxId, BoxId1),
	assert_referents(Dom1, Conds1, BoxId1),
	process_conds(Conds1, BoxId1),
	make_boxid(or2, ParentBoxId, BoxId2),
	assert_referents(Dom2, Conds2, BoxId2),
	process_conds(Conds2, BoxId2).

walk_cond(Cond, ParentBoxId) :-
	parse_unary(Cond, Type, Dom, Conds),
	!,
	make_boxid(Type, ParentBoxId, BoxId),
	assert_referents(Dom, Conds, BoxId),
	process_conds(Conds, BoxId).


% @tbd document
make_boxid(ToplevelId) :-
	toplevel_id(ToplevelId).
make_boxid(Type, ParentBoxId, id(Id, Type, ParentBoxId)) :-
	counter(box_counter, Id).


%% get_box_prefix(BoxId:term, BoxPrefix:list) is det.
%
% In case any Referent from a given box has already been
% verbalized then we conjoin the verbalization of the current Referent.
%
% In case no Referent from a given box has been verbalized,
% then the current Referent is the first one and we
% initiate the box. Then we check the outer box to come
% up for the name of the box (and do this recursively).
%
% @param BoxId is ...
% @param BoxPrefix is ...

% Not the first Referent in the top-box to be verbalized.
get_box_prefix(ToplevelId, ['.']) :-
	toplevel_id(ToplevelId),
	is_rep(0),
	!.

% First Referent in the top-box to be verbalized.
get_box_prefix(ToplevelId, []) :-
	toplevel_id(ToplevelId),
	assert(is_rep(0)),
	!.

% Not the first Referent in the OR1-box to be verbalized.
% Given that the OR1 V OR2 is in the NOT-box.
% Ex?: It is false that there is a cat and that there is a mouse or that there is a dog.
get_box_prefix(id(Id, or1, id(_, Marker, _)), [and, that]) :-
	is_unary_modifier(Marker),
	is_rep(Id),
	!.

% Not the first Referent in the NOT-box to be verbalized.
get_box_prefix(id(Id, Marker, _), [and, that]) :-
	is_unary_modifier(Marker),
	is_rep(Id),
	!.

% BUG: ???
% Not the first Referent in the any box to be verbalized.
get_box_prefix(id(Id, _, _), [and]) :-
	is_rep(Id),
	!.

% The first Referent in the THEN-box to be verbalized.
get_box_prefix(id(Id, then, _), [then]) :-
	assert(is_rep(Id)),
	!.

% The first Referent in the OR2-box to be verbalized.
% Given that the OR1 V OR2 is in the NOT-box.
% BUG: do we have to worry about any higher-level NOT-boxes
% or is checking just for the parent NOT-box OK?
% Ex: It is false that there is a cat or that there is a dog.
get_box_prefix(id(Id, or2, id(_, Marker, _)), [or, that]) :-
	is_unary_modifier(Marker),
	assert(is_rep(Id)),
	!.

% The first Referent in the OR1-box.
% The parent is the NOT-box.
% The parent has already a Referent.
% @tbd Examples
get_box_prefix(id(Id, or1, id(ParentId, Marker, _)), [',', and, that]) :-
	is_unary_modifier(Marker),
	is_rep(ParentId),
	assert(is_rep(Id)),
	!.

% The first Referent in the OR1-box.
% The parent is not the NOT-box and is not the top-box.
% The parent has already a Referent.
get_box_prefix(id(Id, or1, id(ParentId, _, _)), [',', and]) :-
	ParentId \= 0,
	is_rep(ParentId),
	assert(is_rep(Id)),
	!.

% The first Referent in the OR2-box to be verbalized.
% Given that the parent is not a NOT-box.
get_box_prefix(id(Id, or2, _), [or]) :-
	assert(is_rep(Id)),
	!.

% The general case. The first Referent to be verbalized in either:
% IF-box, OR1-box, NOT-box.
get_box_prefix(id(Id, BoxMarker, OuterBoxId), [OuterBoxPrefix | BoxPrefix]) :-
	assert(is_rep(Id)),
	get_box_name(BoxMarker, BoxPrefix),
	get_box_prefix(OuterBoxId, OuterBoxPrefix).


%% is_unary_modifier(+Type:atom) is det.
%
% @param Type is in {not, naf, can, must, should}

is_unary_modifier(not).
is_unary_modifier(naf).
is_unary_modifier(can).
is_unary_modifier(must).
is_unary_modifier(should).
is_unary_modifier(may).


%% get_box_name(+BoxMarker:atom, -BoxPrefix:list) is det.
%
% @param BoxMarker is ...
% @param BoxPrefix is a list of ACE tokens associated with the box
%
% This is the conjunction with a preceding box.
%
get_box_name(if, [if]).
%%get_box_name(then, [then]).
get_box_name(or1, []).
%%get_box_name(or2, [or]).
get_box_name(not, [it, is, false, that]).
get_box_name(naf, [it, is, not, provable, that]).
get_box_name(can, [it, is, possible, that]).
get_box_name(must, [it, is, necessary, that]).
get_box_name(should, [it, is, recommended, that]).
get_box_name(may, [it, is, admissible, that]).


%% conds_text(+Conds:term, +BoxPrefix:atom, +TopDeep:atom, -Text:list, -Features:list) is det.
%
% Note: some discourse referents don't have surface representation.
% We don't call such referents at all.
%
% Note: some objects can have an owner, but in this case max 1 owner.
% ProperName objects cannot have an owner.
%
% @param Conds is a complex term with all the conditions grouped
% @param BoxPrefix is a prefix of tokens to be inserted in front of the verbalization (e.g. `and if it is false that')
% @param TopDeep is in {top, deep}
% @param Text is an ACE text that corresponds to the discourse referent
% @param Features is in {f(sg, subj), f(sg, obj), f(pl, subj), f(pl, obj)}, for number and case sharing


% Old somebody and something are referred to by a variable only.
conds_text(noun([object(A, somebody, countable, na, eq, 1)], parts([]), owners([]), adjectives([])), _, deep, RI--RI, [A], f(sg, _)) :-
	memberchk(A, RI),
	add_var(A),
	!.

conds_text(noun([object(A, something, dom, na, na, na)], parts([]), owners([]), adjectives([])), _, deep, RI--RI, [A], f(sg, _)) :-
	memberchk(A, RI),
	add_var(A),
	!.


% New somebody and something (with 'there is')
conds_text(noun([object(A, somebody, countable, na, eq, 1)], parts([]), owners([]), adjectives([])),
BoxPrefix, top, RI--[A | RI], [BoxPrefix, there, is, somebody, A], f(sg, _)) :-
	\+ memberchk(A, RI),
	add_var(A),
	!.

conds_text(noun([object(A, something, dom, na, na, na)], parts([]), owners([]), adjectives([])),
BoxPrefix, top, RI--[A | RI], [BoxPrefix, there, is, something, A], f(sg, _)) :-
	\+ memberchk(A, RI),
	add_var(A),
	!.

% New somebody and something.
conds_text(noun([object(A, somebody, countable, na, eq, 1)], parts([]), owners([]), adjectives([])), _, deep, RI--[A | RI], [somebody, A], f(sg, _)) :-
	\+ memberchk(A, RI),
	add_var(A),
	!.

conds_text(noun([object(A, something, dom, na, na, na)], parts([]), owners([]), adjectives([])), _, deep, RI--[A | RI], [something, A], f(sg, _)) :-
	\+ memberchk(A, RI),
	add_var(A),
	!.


% NP conjunction. This is the only case where parts/1 has a non-empty list as an argument
% Old plural object, refer to it by 'they' (and 'them'). E.g.
% John and Mary wait. They talk.
% John and Mary wait. Bill sees them.
% * John and Mary wait. John and Mary talk. (this is not supported by refres)
%
% BUG: `them' is not supported
conds_text(noun([object(A, na, countable, _, _, _)], parts([_ | _]), _Owner, _Adjectives), _BoxPrefix, deep, RI--RI, [they], f(pl, subj)) :-
	memberchk(A, RI),
	!.

conds_text(noun([object(A, na, countable, _, _, _)], parts([_ | _]), _Owner, _Adjectives), _BoxPrefix, deep, RI--RI, [them], f(pl, obj)) :-
	memberchk(A, RI),
	!.

% NP conjunction.
% Note that we require that parts/1 has a non-empty list as an argument and that the number of
% arguments matches the number specified in the object-condition (QNum).
% In the DRSs generated by APE, this is not always the case. Consider DRSs where the
% has_part/2 condition is embedded into negation, such as the ones generated from:
% There are less than 3 men and less than 3 women.
% Less than 3 men and John wait.
conds_text(noun([object(A, na, countable, _, eq, QNum)], parts(ListOfObjects), _Owner, _Adjectives), BoxPrefix, TopDeep, RI--RO, [Prefix, ListOfObjectsText], f(pl, _)) :-
	!,
	debug(npcoord, 'npcoord: QNum: ~w; ListOfObjects: ~w~n', [QNum, ListOfObjects]),
	length(ListOfObjects, QNum),
	surface_det(A, RI, pl, TopDeep, BoxPrefix, countable, dummy, dummy, Prefix),
	objects_to_text(ListOfObjects, [A | RI]--RO, ListOfObjectsText),
	debug(verbose, 'top:~w prefix:~w result:~w~n', [TopDeep, BoxPrefix, Prefix]).

% Plural `which'
% Example: which men wait?
% Example: there are which men of John?
% `how many' (+ plural noun)
% Example: how many men wait?
% Example: there are how many men of John?
conds_text(noun([object(A, Agent, countable, na, geq, 2)], parts([]), Owner, adjectives([Query])),
BoxPrefix, TopDeep, RI--RO, [ThereIs, QueryWord, AgentText, OwnerText], f(pl, _)) :-
	query(A, Query, pl, QueryWord),
	!,
	\+ memberchk(A, RI),
	toplevel_id(ToplevelId),
	ref2conds(A, _, ToplevelId, _SentenceId),
	get_thereis(TopDeep, pl, BoxPrefix, ThereIs),
	surface_noun(cn, Agent, pl, AgentText),
	verbalize_owners(A, RI--RO, Owner, OwnerText).

% BUG: temporary handling of "less than" and "at most"
conds_text(noun([object(_, _, countable, na, na, na)], parts([]), _, adjectives(_)), _, _, _, _, _) :-
	!,
	fail.

conds_text(noun([object(A, Agent, countable, na, Comp, Number)], parts([]), Owner, adjectives(Adjectives)),
	BoxPrefix, TopDeep, RI--RO, [SurfaceDeterminer, AdjectivesText, AgentText, A, OwnerText], f(pl, _)) :-
	(Number = 0 ; Number > 1),
	surface_det(A, RI, pl, TopDeep, BoxPrefix, countable, Comp, Number, SurfaceDeterminer),
	surface_noun(cn, Agent, pl, AgentText),
	conds_to_andlist(adjective_to_text, Adjectives, AdjectivesText),
	verbalize_owners(A, RI--RO, Owner, OwnerText),
	add_var(A).


% Singular and mass nouns
%
% 1. Indefinite pronouns (somebody, something) can have owners but not properties.
%
% Everything of John is red.
% If there is something B of John then B is red.
%
% Note that this wouldn't work with quoted strings since they cannot have attached variables:
%
% Everything "Go!" of John is red. -> If there is something "Go!" of John then ??? is red.
%
% (Fortunately we don't support quoted strings in apposition anymore.)
%
% 2. Regular nouns
% 3. BUG: measurement nouns

% If there is somebody of John then ...
conds_text(noun([object(A, Value, _, _, _, _)], parts([]), Owner, _Adjectives), BoxPrefix, top,
	RI--RO, [BoxPrefix, there, is, Value, A, OwnerText], f(sg, _)) :-
	(Value = somebody ; Value = something),
	verbalize_owners(A, RI--RO, Owner, OwnerText),
	add_var(A).


% somebody of John waits ...
conds_text(noun([object(A, Value, _, _, _, _)], parts([]), Owner, _Adjectives), _BoxPrefix, deep,
	RI--RO, [Name, A, OwnerText], f(sg, _)) :-
	(Value = somebody ; Value = something),
	(
		memberchk(A, RI)
	->
		Name = []
	;
		Name = Value
	),
	verbalize_owners(A, RI--RO, Owner, OwnerText),
	add_var(A).


% Singular `which'
% (Note that `which' is not possible with mass nouns, qeneralized quantifiers,
% and indefinite pronouns.)
% Which man waits?
% There is which man of John?
conds_text(noun([object(A, Agent, countable, na, eq, 1)], parts([]), Owner, adjectives([Query])),
BoxPrefix, TopDeep, RI--RO, [ThereIs, QueryWord, AgentText, OwnerText], f(sg, _)) :-
	query(A, Query, sg, QueryWord),
	!,
	\+ memberchk(A, RI),
	toplevel_id(ToplevelId),
	ref2conds(A, _, ToplevelId, _SentenceId),
	get_thereis(TopDeep, sg, BoxPrefix, ThereIs),
	surface_noun(cn, Agent, sg, AgentText),
	verbalize_owners(A, RI--RO, Owner, OwnerText).


% 'how much' (+ mass noun)
% Example: John eats how much food?
conds_text(noun([object(A, Agent, mass, na, na, na)], parts([]), Owner, adjectives([Query])),
BoxPrefix, TopDeep, RI--RO, [ThereIs, QueryWord, AgentText, OwnerText], f(sg, _)) :-
	query(A, Query, mass, QueryWord),
	!,
	\+ memberchk(A, RI),
	toplevel_id(ToplevelId),
	ref2conds(A, _, ToplevelId, _SentenceId),
	get_thereis(TopDeep, sg, BoxPrefix, ThereIs),
	surface_noun(cn, Agent, mass, AgentText),
	verbalize_owners(A, RI--RO, Owner, OwnerText).


% Countable: a man
conds_text(noun([object(A, Agent, countable, na, eq, 1)], parts([]), Owner, adjectives(Adjectives)),
	BoxPrefix, TopDeep, RI--RO, [SurfaceDeterminer, AdjectivesText, AgentText, A, OwnerText], f(sg, _)) :-
	surface_noun(cn, Agent, sg, AgentText),
	conds_to_andlist(adjective_to_text, Adjectives, AdjectivesText),
	surface_det(A, RI, sg, TopDeep, BoxPrefix, countable, _, _, SurfaceDeterminer),
	verbalize_owners(A, RI--RO, Owner, OwnerText),
	add_var(A).


% Countable: At least 1 man
conds_text(noun([object(A, Value, countable, na, Eq, 1)], parts([]), Owner, adjectives(Adjectives)),
	BoxPrefix, TopDeep, RI--RO, [SurfaceDeterminer, AdjectivesText, AgentText, A, OwnerText], f(sg, _)) :-
	Eq \= eq,
	surface_noun(cn, Value, sg, AgentText),
	conds_to_andlist(adjective_to_text, Adjectives, AdjectivesText),
	surface_det(A, RI, sg, TopDeep, BoxPrefix, countable, Eq, 1, SurfaceDeterminer),
	verbalize_owners(A, RI--RO, Owner, OwnerText),
	add_var(A).


% Mass: some rice
conds_text(noun([object(A, Agent, mass, na, na, na)], parts([]), Owner, adjectives(Adjectives)),
BoxPrefix, TopDeep, RI--RO, [SurfaceDeterminer, AdjectivesText, AgentText, A, OwnerText], f(sg, _)) :-
	surface_noun(cn, Agent, mass, AgentText),
	conds_to_andlist(adjective_to_text, Adjectives, AdjectivesText),
	surface_det(A, RI, sg, TopDeep, BoxPrefix, mass, _, _, SurfaceDeterminer),
	verbalize_owners(A, RI--RO, Owner, OwnerText),
	add_var(A).

% Measurement constructs, e.g.
% More than 3 kg of white rice rots.
% 2 kg of water boils. The water is warm.
% 2 kg of apples are ripe. The apples rot.
conds_text(noun([object(A, Agent, Quant, Unit, QType, QNum)], parts([]), Owner, adjectives(Adjectives)),
BoxPrefix, TopDeep, RI--RO, [ThereIs, Specifier, AdjectivesText, AgentText, A, OwnerText], f(SgPl, _)) :-
	(Unit \= na),
	(
		Quant = countable
	->
		SgPl = pl
	;
		SgPl = sg
	),
	(
		memberchk(A, RI)
	->
		ThereIs = [],
		surface_determiner(old, _, _, Specifier),
		RO = RI,
		OwnerText = []
	;
		get_thereis(TopDeep, SgPl, BoxPrefix, ThereIs),
		surface_determiner(new, QType, QNum, Q),
		Specifier = [Q, Unit, of],
		verbalize_owners(A, RI--RO, Owner, OwnerText)
	),
	conds_to_andlist(adjective_to_text, Adjectives, AdjectivesText),
	surface_noun(cn, Agent, SgPl, AgentText),
	add_var(A).


% Intransitive verbs with (adverb | pp)* attachment.
conds_text(verb([predicate(_, Predicate, Argument)], adverbs(Adverbs), pps(PPs)), Box, _,
	RI--RO, [Box, ArgumentText, PredicateText, AdverbsText, PPsText], _) :-
	referent_text(Argument, RI--RTmp, ArgumentText, f(SgPl, subj)),
	surface_verb(SgPl, Predicate, PredicateText),
	conds_to_andlist(adverb_to_text, Adverbs, AdverbsText),
	pps_to_text(PPs, RTmp--RO, PPsText).

% Transitive verbs with (adverb | pp)* attachment.
% BUG: Comment is currently not returned.
conds_text(verb([predicate(_, Predicate, Argument1, Argument2)], adverbs(Adverbs), pps(PPs)), Box, _,
	RI--RO, [Box, Argument1Text, PredicateText, Argument2Text, AdverbsText, PPsText], _) :-
	debug(verbose, "transitive verb: referents in: ~W~n", [RI, [numbervars(true)]]),
	referent_text(Argument1, RI--RTmp1, Argument1Text, f(SgPl, subj)),
	surface_verb(SgPl, Predicate, PredicateText),
	referent_text(Argument2, RTmp1--RTmp2, Argument2Text, f(_, obj)),
	conds_to_andlist(adverb_to_text, Adverbs, AdverbsText),
	pps_to_text(PPs, RTmp2--RO, PPsText),
	make_comment(PredicateText, PPsText, _Comment).

% Ditransitive verbs with (adverb | pp)* attachment.
% Note: the order of calling referent_text/4 is important as assert/1 is performed by this call.
% Test it with "A man gives a dog the dog.", in the paraphrase the order of indef-NP and def-NP must be correct.
%
conds_text(verb([predicate(_, Lemma, Argument1, Argument2, Argument3)], adverbs(Adverbs), pps(PPs)), Box, _,
	RI--RO, DiText, _) :-
	referent_text(Argument1, RI--RTmp1, Argument1Text, f(SgPl, subj)),
	get_di_marker(SgPl, Lemma, SurfaceForm, DiMarker),
	di_text(DiMarker, RTmp1--RTmp2, Box, Argument1Text, SurfaceForm, Argument2, Argument3, AdverbsText, PPsText, DiText),
	conds_to_andlist(adverb_to_text, Adverbs, AdverbsText),
	pps_to_text(PPs, RTmp2--RO, PPsText).


% Properties as copula arguments.
% Must have at least 1 element.
conds_text([FirstProperty | RestProperties], _, _, RI--RO, [PropertiesText], _) :-
	properties_to_text([FirstProperty | RestProperties], RI--RO, PropertiesText).


%% di_text
%
% Two versions of the surface form of ditransitive constructs:
% (1) dative shift, (2) with prepositional marker.
%
di_text('', RI--RO, Box, Argument1Text, PredicateText, Argument2, Argument3, AdverbsText, PPsText, [Box, Argument1Text, PredicateText, Argument3Text, Argument2Text, AdverbsText, PPsText]) :-
	!,
	referent_text(Argument3, RI--RTmp, Argument3Text, f(_, obj)),
	referent_text(Argument2, RTmp--RO, Argument2Text, f(_, obj)).

di_text(Marker, RI--RO, Box, Argument1Text, PredicateText, Argument2, Argument3, AdverbsText, PPsText, [Box, Argument1Text, PredicateText, Argument2Text, Marker, Argument3Text, AdverbsText, PPsText]) :-
	referent_text(Argument2, RI--RTmp, Argument2Text, f(_, obj)),
	referent_text(Argument3, RTmp--RO, Argument3Text, f(_, obj)).



%% verbalize_owners(+Referent:nvar, +Owners:term, -TokenList:list) is det.
%
% @param Referent is a discourse referent
% @param Owners is a term =|owners([relation(_, of, OwnerReferent)])|=.
% Note that is always contains just one relation/3 condition.
% @param TokenList is a list of resulting ACE tokens
%
% Note that we verbalize the of-constructions only in case the
% NP is not anaphoric, i.e. we never output "the dog X of the man Y"
% but simply "the dog X".
%
verbalize_owners(Ref, RI--RI, owners([]), []) :-
	memberchk(Ref, RI),
	!.

verbalize_owners(Ref, RI--[Ref | RI], owners([]), []) :-
	!.

verbalize_owners(Ref, RI--RI, owners([_]), []) :-
	memberchk(Ref, RI),
	!.

verbalize_owners(Ref, RI--RO, owners([relation(_, of, OwnerReferent)]), [of, OwnerText]) :-
	referent_text(OwnerReferent, [Ref | RI]--RO, OwnerText, _Features).


%% conds_to_andlist(:Goal, +Conds:list, -AndList:list) is det.
%
% Maps a list of DRS conditions to an ACE conjunction, e.g.
%  - where and when and how
%  - rich and famous
%
% @param Goal maps a condition to ACE text (a word or two)
% @param Conds is a list of DRS conditions
% @param AndList is list of ACE words conjoined by 'and'
%
conds_to_andlist(_, [], []).

conds_to_andlist(Cond_To_Text, [Cond], [Text]) :-
	!,
	call(Cond_To_Text, Cond, Text).

conds_to_andlist(Cond_To_Text, [Cond | Conds], [Text, 'and' | Texts]) :-
	call(Cond_To_Text, Cond, Text),
	conds_to_andlist(Cond_To_Text, Conds, Texts).


%% adjective_to_text(+AdjCond:term, -AdjToken:term) is det.
%
% @param AdjCond is a DRS condition that corresponds to an ACE adjective
% @param AdjToken is the corresponding token (or possibly a list of tokens)
%
adjective_to_text(property(_, Property, Comparison), PropertyText) :-
	surface_property(Property, Comparison, PropertyText).


%% adverb_to_text(+AdverbCond:term, -AdverbToken:term) is det.
%
% @param AdverbCond is a DRS condition that corresponds to an ACE adverb or how/where/when query word
% @param AdverbToken is the corresponding token (or possibly a list of tokens)
%
% @tbd
% Note that we currently only support top-level query-conditions ('how', 'where', 'when'),
% because we cannot correctly handle e.g. "John how believes that Mary sleeps?"
%
adverb_to_text(modifier_adv(_, Adverb, Comparison), AdverbText) :-
	!,
	surface_adverb(Adverb, Comparison, AdverbText).

adverb_to_text(Query, QueryWord) :-
	query(Referent, Query, _, QueryWord),
	toplevel_id(ToplevelId),
	ref2conds(Referent, _, ToplevelId, _SentenceId).


%% pps_to_text(+Modifiers:list, -TokenList:list) is det.
%
% @param Modifiers is a list of modifier_pp- or query-conditions
% @param TokenList is a list of ACE tokens
%
pps_to_text([], RI--RI, []).

pps_to_text([modifier_pp(_, Prep, Modifier) | PPs], RI--RO, [Prep, ModifierText | PPsText]) :-
	referent_text(Modifier, RI--RTmp, ModifierText, _Features),
	pps_to_text(PPs, RTmp--RO, PPsText).


%% properties_to_text(+Properties:list, -TokenList:list) is det.
%
% @param Properties is a list of property-conditions
% @param TokenList is a list of ACE tokens
%
properties_to_text([], RI--RI, []).

properties_to_text([property(_, Property, Comparison)], RI--RI, PropertyText) :-
	!,
	surface_property(Property, Comparison, PropertyText).

properties_to_text([property(_, Property, Comparison, Argument)], RI--RO, [PropertyText, ArgumentText]) :-
	!,
	surface_property(Property, Comparison, PropertyText),
	referent_text(Argument, RI--RO, ArgumentText, _Features).

properties_to_text([property(_Ref1, Adjective, Ref2, Comparison, ComparisonTarget, Ref3)], RI--RO, PropertyText) :-
	!,
	referent_text(Ref2, RI--RTmp, Ref2Text, _),
	referent_text(Ref3, RTmp--RO, Ref3Text, _),
	surface_property(Adjective, Comparison, ComparisonTarget, Ref2Text, Ref3Text, PropertyText).

properties_to_text([property(_, Property, Comparison) | Properties], RI--RO, [PropertyText, 'and' | PropertiesText]) :-
	!,
	surface_property(Property, Comparison, PropertyText),
	properties_to_text(Properties, RI--RO, PropertiesText).


%% objects_to_text(+ListOfObjects:list, -ListOfObjectsVerbalized:list) is det.
%
% @param ListOfObjects is a list of variables and/or data items
% @param ListOfObjectsVerbalized is a list of ACE tokens
%
% Handling of plural objects which are built from several NPs.
%
objects_to_text([Agent], RI--RO, [AgentText]) :-
	referent_text(Agent, RI--RO, AgentText, _Features).

objects_to_text([Agent | Agents], RI--RO, [AgentText, 'and' | AgentsText]) :-
	referent_text(Agent, RI--RTmp, AgentText, _Features),
	objects_to_text(Agents, RTmp--RO, AgentsText).


%% surface_det(+Referent:term, +SgPl:atom, +TopDeep:atom, +BoxPrefix:list, +NounType:atom, +Comp:atom, +Number:atom, -Result:list) is det.
%
% @param Referent is a discourse referent
% @param SgPl is in {sg, pl}
% @param TopDeep {top, deep, BUG}
% @param BoxPrefix BUG
% @param NounType {mass, countable}
% @param Comp is in {eq, geq, leq, greater, less}
% @param Number is an integer or 'na' or 'dummy'
% @param Result is a list of ACE tokens (can also be an atom)
%
% BUG: "top" means always "new"?

surface_det(Referent, RI, SgPl, TopDeep, BoxPrefix, NounType, Comp, Number, Result) :-
	(
		memberchk(Referent, RI)
	->
		OldNew = old
	;
		OldNew = new
	),
	surface_det_x(SgPl, TopDeep, BoxPrefix, NounType, OldNew, Comp, Number, Result).


surface_det_x(sg, top, BoxPrefix, countable, new, eq, 1, [BoxPrefix, there, is, a]).

surface_det_x(sg, top, BoxPrefix, mass, new, na, na, [BoxPrefix, there, is, some]).

surface_det_x(sg, deep, _, countable, new, eq, 1, [a]) :- !.
surface_det_x(sg, deep, _, mass, new, na, na, [some]).
surface_det_x(sg, deep, _, mass, old, _, _, [the]).


% There are John and Mary.
surface_det_x(pl, top, BoxPrefix, _, _, dummy, dummy, [BoxPrefix, there, are]) :- !.

% John and Mary wait.
surface_det_x(pl, deep, _, _, _, dummy, dummy, []) :- !.


surface_det_x(sg, top, BoxPrefix, countable, Det, Comp, Number, [BoxPrefix, there, is, SurfaceDeterminer]) :-
	surface_determiner(Det, Comp, Number, SurfaceDeterminer).

surface_det_x(sg, deep, _, countable, Det, Comp, Number, SurfaceDeterminer) :-
	surface_determiner(Det, Comp, Number, SurfaceDeterminer).


surface_det_x(pl, top, BoxPrefix, _, Det, Comp, Number, [BoxPrefix, there, are, SurfaceDeterminer]) :-
	surface_determiner(Det, Comp, Number, SurfaceDeterminer).

surface_det_x(pl, deep, _, _, Det, Comp, Number, SurfaceDeterminer) :-
	surface_determiner(Det, Comp, Number, SurfaceDeterminer).


%% surface_determiner(+OldNew:atom, +Operator:atom, +Count:atomic, -SurfaceDeterminer:list) is det.
%
% @param OldNew
% @param Operator is in {eq, geq, leq, greater, less, exactly, na}
% @param Count is a positive integer or 'na'
% @param SurfaceDeterminer is a list of tokens corresponding the an ACE determiner
%
surface_determiner(old, _, _, the).

surface_determiner(new, eq, Number, [Number]).

surface_determiner(new, geq, Number, [at, least, Number]).

surface_determiner(new, leq, Number, [at, most, Number]).

surface_determiner(new, greater, Number, [more, than, Number]).

surface_determiner(new, less, Number, [less, than, Number]).

surface_determiner(new, exactly, Number, [exactly, Number]).

surface_determiner(new, na, na, []).


%% get_thereis(+TopDeep:atom, +SgPl:atom, +BoxPrefix:list, -ThereIs:list) is det.
%
% @param TopDeep is in {top, deep}
% @param SgPl is in {sg, pl}
% @param BoxPrefix
% @param ThereIs is a list of tokens containing the BoxPrefix and 'there is/are' is needed
%
get_thereis(top, sg, BoxPrefix, [BoxPrefix, there, is]).
get_thereis(top, pl, BoxPrefix, [BoxPrefix, there, are]).
get_thereis(deep, _, _, []).


%% make_comment(+VerbText:list, +PpText:list, -Comment:list) is det.
%
% On the basis of the PP construct a comment for novices.
%
% @tbd Experimental
%
make_comment(_, [], []).
make_comment(PredicateText, [H | Tail], ['/*', 'Did you know that in ACE prepositional phrases always modify the verb? I.e.', PredicateText, [H | Tail], '*/']).


%% expr_exprpp(+Expression:term, -ExpressionText:list) is det.
%
% @param Expression
% @param ExpressionText
%
expr_exprpp(string(String), RI--RI, StringText) :-
	!,
	atomic(String), % the content of string/1 is either an atom or a number
	surface_quotedstring(String, StringText).

expr_exprpp(int(Number), RI--RI, NumberText) :-
	!,
	integer(Number),
	with_output_to(atom(NumberText), format("~w", [Number])).

expr_exprpp(real(Number), RI--RI, NumberText) :-
	!,
	float(Number),
	with_output_to(atom(NumberText), format("~w", [Number])).

expr_exprpp(int(Number, Unit), RI--RI, [NumberText, Unit]) :-
	!,
	integer(Number),
	with_output_to(atom(NumberText), format("~w", [Number])).

expr_exprpp(real(Number, Unit), RI--RI, [NumberText, Unit]) :-
	!,
	float(Number),
	with_output_to(atom(NumberText), format("~w", [Number])).

expr_exprpp(named(Name), RI--RI, NameText) :-
	surface_noun(pn, Name, _, NameText),
	!.

% Note: it's ok to call findall where RI--RI because sets cannot introduce new referents
expr_exprpp(set(Set), RI--RI, ['{', CommaSetText, '}']) :-
	!,
	findall(ElText, (member(El, Set), expr_exprpp(El, RI--RI, ElText)), SetText),
	list_commalist(SetText, CommaSetText).

% Note: it's ok to call findall where RI--RI because lists cannot introduce new referents
expr_exprpp(list(List), RI--RI, ['[', CommaListText, ']']) :-
	!,
	findall(ElText, (member(El, List), expr_exprpp(El, RI--RI, ElText)), ListText),
	list_commalist(ListText, CommaListText).

expr_exprpp(expr(Op, Expr1, Expr2), RI--RO, ['(', Expr1Text, Op, Expr2Text, ')']) :-
	!,
	expr_exprpp(Expr1, RI--RTmp, Expr1Text),
	expr_exprpp(Expr2, RTmp--RO, Expr2Text).

expr_exprpp(Referent, RI--RO, Variable) :-
	get_type(Referent, Type, BoxId),
	get_box_prefix(BoxId, BoxPrefix),
	conds_text(Type, BoxPrefix, deep, RI--RO, Text, _),
	flatten(Text, FlatText),
	last(FlatText, Variable),
	debug(verbose, "NP in expr: ~w ~w~n", [Text, Variable]).


%% list_commalist(?List:list, ?CommaList:list) is nondet.
%
% @param List is a list of elements
% @param CommaList is a list where the elements are separated by explicit commas
%
list_commalist([], []).
list_commalist([X], [X]).
list_commalist([X1, X2 | Tail], [X1, ',' | Text]) :-
	list_commalist([X2 | Tail], Text).


%% sentencelist_sentencelistcoord(+SentenceList:list, -SentenceListCoord:list) is det.
%
% Note that sentencelist cannot be empty.
%
sentencelist_sentencelistcoord([Sentence], [Sentence]).
sentencelist_sentencelistcoord([Sentence1, Sentence2 | SentenceList], [Sentence1, [and, that] | SentenceListCoord]) :-
	sentencelist_sentencelistcoord([Sentence2 | SentenceList], SentenceListCoord).


%% get_sentence_id(SId/_TId, SId)
%
% Returns the sentence ID. Supports two
% formats:
%
% * SentenceId/TokenId
% * SentenceId
%
get_sentence_id(SId/_TId, SId) :- !.
get_sentence_id(SId, SId).


%% is_complex_or_predicate(+Condition:term)
%
%
is_complex_or_predicate([_ | _]) :-
	!.
is_complex_or_predicate(Condition) :-
	is_predicate(Condition),
	!.
is_complex_or_predicate(Condition) :-
	embeds_drs(Condition).


%% is_predicate(+Condition:term)
%
% @param Condition DRS condition with or without the sentence ID
%
is_predicate(Condition-_) :-
	!,
	functor(Condition, predicate, _).

is_predicate(Condition) :-
	functor(Condition, predicate, _).


%% embeds_drs(+Condition:term)
%
% Succeeds if the condition embeds a DRS, e.g.
% negation embeds a single DRS, implication embeds two DRSs.
% Simple conditions (e.g. predicate) and list-conditions (that are
% derived e.g. from "at most n") do not embed DRSs.
%
% @tbd this predicate is quite general, move it somewhere else
%
embeds_drs(_Label:drs(_, _)).
embeds_drs(=>(_, _)).
embeds_drs(v(_, _)).
embeds_drs(-(_)).
embeds_drs(~(_)).
embeds_drs(can(_)).
embeds_drs(must(_)).
embeds_drs(should(_)).
embeds_drs(may(_)).


% is_complex_drs(+Drs:drs)
%
% @bug Currently not used
%
% DRS is complex iff
% at least one of its conditions embeds a DRS.
%
/*
is_complex_drs(drs(_, Cs)) :-
	member(C, Cs),
	embeds_drs(C),
	!.
*/

is_complex_drs(drs(_, [Cond])) :-
	embeds_drs(Cond).


%% parse_unary(+Condition:term, -Type:atom, -Dom:list, -Conds:list)
%
parse_unary(-(drs(Dom, Conds)), not, Dom, Conds).
parse_unary(~(drs(Dom, Conds)), naf, Dom, Conds).
parse_unary(can(drs(Dom, Conds)), can, Dom, Conds).
parse_unary(must(drs(Dom, Conds)), must, Dom, Conds).
parse_unary(should(drs(Dom, Conds)), should, Dom, Conds).
parse_unary(may(drs(Dom, Conds)), may, Dom, Conds).


%% query(+Ref, +QueryTerm:term, +SgPl:atom, -QueryPhrase:atom) is det.
%
%
query(X, query(X, QLemma), SgPl, qp(QPhrase)) :-
	query_(QLemma, SgPl, QPhrase),
	!.

query_(which, sg, [which]).
query_(which, pl, [which]).
query_(howm, pl, [how, many]).
query_(howm, mass, [how, much]).
query_(Word, _, [Word]).
