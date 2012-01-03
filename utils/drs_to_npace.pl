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


:- module(drs_to_npace, [
		drs_to_npace/2
	]).

/** <module> Attempto DRS to ACE NP translator

In general we handle only those DRSs which contain only implications, but
there is some support also for objects and predicates in the toplevel.

@author Kaarel Kaljurand
@version 2011-11-09

TODO:

* Support 'should' and 'may' (also in negation etc.)

* DONE: Support for disjunction. Interaction between AND/OR. E.g. Every man works, and eats or sleeps.

* We could use a reflexive pronoun if DeepSubj = DeepObj
problem: himself/herself vs itself, no general reflexive pronoun exists (e.g. 'self').

* Takes very long and fails:
FIXED: Every man sees a dog that sees a cat and that sees a rat and sees a mouse that sees an ant.

* If John waits then he sleeps. (not possible with 'every': Every John ...)
* If there are 2 men then they wait. (not possible with 'every')
* If there is a carnivore then everybody that it eats is an animal. (embedded if-then)
* If a man waits then a dog barks. (no argument-sharing)
* Every animal X hates an animal Y that eats X. (strange paraphrase but it is correct)
* Anaphoric references and looping and new objects via definite NPs. (find an example!!!)
* One idea was to fail whenever there are more than one solutions.
Unfortunately this is almost always the case.

Some notes relocated from the PhD draft:

* Can some of the scope problems that Core ACE has be solved?
* of-constructions and 'whose' and everybody's?
* Loops: are there cases where we have to use a definite NP which contains a relative clause.
Note: we can use variables.


Usage:

* ?- tnp("Every man who likes a dog which sees a rat hates a cat.").
* ?- tnp('Every man who likes a dog hates a cat and sees a rat.').
* ?- tnp('Every man who likes a dog is hated by a cat.').
* ?- tnp("Every man who can see a dog is hated by a cat.").
*/


:- use_module(drs_to_sdrs).

:- use_module(implication_turn, [
		implication_turn/2
	]).

:- use_module(morphgen, [
		clear_vars/0,
		add_var/1,
		remove_singletons/2,
		listlist_listatom/2,
		surface_noun/4,
		surface_verb/3,
		surface_neg_verb/3
	]).


% Operators used in the DRS.
:- op(400, fx, -).
:- op(400, fx, ~).
:- op(500, xfx, =>).
:- op(500, xfx, v).


/*
:- debug(verbose).
:- debug(toplevel).
:- debug(turn).
:- debug(cond).
*/


:- dynamic ref_to_noun/5.


%% drs_to_npace(+Drs:drs, -AceSentenceList:list) is det.
%
% @param Drs is an Attempto DRS
% @param AceSentenceList is a list of NP ACE sentences (atoms) corresponding to the DRS
%
drs_to_npace(Drs, AceSentenceList) :-
	drs_acetext_np_x(Drs, AceList),
	remove_singletons(AceList, AceListPruned),
	listlist_listatom(AceListPruned, AceSentenceList),
	!.

drs_to_npace(_Drs, []).
% TODO: Throw an exception
%drs_to_npace(Drs, _) :-
%	throw(error('Not implemented', context(drs_to_npace/2, Drs))).


%% drs_acetext_np(+Drs:drs, -AceText:list) is det.
%
% Translates a DRS into NP ACE.
%
% 1. Simplify the DRS.
% 2. Assert all nouns and propernames, return referents to toplevel nouns.
% 3. Verbalize the toplevel predicates (John owns a dog.)
% 4. Verbalize the toplevel nouns (There is a man.)
% 5. Preserve only implications.
% 6. Verbalize the implications.
%
% @param Drs is an Attempto DRS
% @param AceText NP ACE text (Prolog list of lists) corresponding to the DRS
%
% BUG: toplevel conditions (e.g. negation, relation/3, etc) which are not supported are ignored,
% but it doesn't cause a failure like in the previous versions.

drs_acetext_np_x(Drs, AceText) :-
	copy_term(Drs, DrsCopy),
	drs_to_sdrs(DrsCopy, DrsSimpleWithNamed),

	% BUG: temporary: remove named-object-conditions
	exclude(is_named, DrsSimpleWithNamed, DrsSimple),

	numbervars(DrsSimple, 0, _),
	retractall(ref_to_noun(_, _, _, _, _)),
	clear_vars,
	retractall(morphgen:name_namespace(_, _)),

	get_toplevel_referents(DrsSimple, toplevel(ToplevelReferents, UnsortedSubjectList, UnsortedObjectList, NamedList)),
	subtract(ToplevelReferents, NamedList, UnnamedReferentList),
	subtract(UnnamedReferentList, UnsortedObjectList, List1),
	append(UnsortedSubjectList, List1, List2),
	list_to_set(List2, SubjectList),
	subtract(SubjectList, NamedList, UnnamedSubjectListWithNamed),
	% BUG: temporary: remove named-object-conditions
	exclude(is_named_ref, UnnamedSubjectListWithNamed, UnnamedSubjectList),
	debug(toplevel, 'Toplevel referents: all: ~w; subjects: ~w; unnamed subjects: ~w~n', [ToplevelReferents, SubjectList, UnnamedSubjectList]),

	individuals_ace(UnnamedSubjectList, AceSubjectList),

	debug(toplevel, 'Individuals (nouns): ~w~n', [AceSubjectList]),

	predicates_ace(DrsSimple, UnnamedSubjectList, RefsOut, AceTextListProperties),

	debug(toplevel, 'Properties: ~w~n', [AceTextListProperties]),

	subtract(UnnamedReferentList, RefsOut, Remaining2WithNamed),
	exclude(is_named_ref, Remaining2WithNamed, Remaining2),

	individuals_ace(Remaining2, AceTextListIndividuals),

	include(is_implication, DrsSimple, Implications),

	% BUG: we don't need the cut here, instead we should make sure that
	% there is no backtracting in the previous statements
	%!,

	drs_ace(Implications, UnnamedReferentList, AceTextListClasses),

	append(AceTextListProperties, AceTextListIndividuals, Tmp),
	append(AceSubjectList, Tmp, Tmp1),
	append(Tmp1, AceTextListClasses, AceText),
	debug(toplevel, 'All: ~w~n', [AceText]).


% BUG: temporary: remove named-object-conditions
is_named(object(named(Name), Name, named, _, _, _)-_).
is_named_ref(named(_)).


%% is_implication(+Condition)
%
% True if condition is an implication.
%
% @param Condition DRS condition
%
is_implication(_ => _).


%% acefragment_acesentence(+Fragment:list, -Sentence) is det.
%
%
acefragment_acesentence(Fragment, FragmentFlat) :-
	flatten(Fragment, FragmentFlat).


%% individuals_ace(+Referents:list, -AceText:list) is det.
%
% Generates a there-is sentence for each referent.

individuals_ace([], []).

individuals_ace([Ref | Refs], [AceSentence | AceTexts]) :-
	get_noun([], yes, no, Ref, Num, SurfaceNoun),
	num_sgpl_copula(Num, _, Copula),
	acefragment_acesentence(['there', Copula, SurfaceNoun], AceSentence),
	individuals_ace(Refs, AceTexts).


%% predicates_ace(+Predicates:list, +RefsIn:list, -RefsOut:list, -AceText:list) is det.
%
% Here we scan all the predicates and also negations and disjunctions
% in the toplevel DRS. We ignore object/6 and the implication.
% All other DRS-conditions (relation/3 etc)
% cause a failure (as NP ACE does not currently support them).
%
predicates_ace([], Refs, Refs, []).

% BUG: this implementation is buggy, we rather fall back to Core ACE.
/*
predicates_ace(
	[-Cond | RestPredicates],
	RefsIn,
	RefsOut,
	[AceSentence | RestAceTexts]
	) :-
	!,
	get_main_subject(Cond, SubjRef),
	get_noun(RefsIn, yes, no, SubjRef, _SubjDom, SurfaceSubj),
	cond_acetext(-Cond, [yes, _Conj, [SubjRef | RefsIn]-RefsTmp, then, Ref, yes:no, Ref:_], AceText),
	acefragment_acesentence([SurfaceSubj, AceText], AceSentence),
	predicates_ace(RestPredicates, RefsTmp, RefsOut, RestAceTexts).
*/

% BUG: this implementation is buggy, we rather fall back to Core ACE.
/*
predicates_ace(
	[Cond1 v Cond2 | RestPredicates],
	RefsIn,
	RefsOut,
	[AceSentence | RestAceTexts]
	) :-
	!,
	get_main_subject(Cond1, SubjRef),
	get_noun(RefsIn, yes, no, SubjRef, _SubjDom, SurfaceSubj),
	cond_acetext(Cond1 v Cond2, [yes, _Conj, [SubjRef | RefsIn]-RefsTmp, then, Ref, yes:no, Ref:_], AceText),
	acefragment_acesentence([SurfaceSubj, AceText], AceSentence),
	predicates_ace(RestPredicates, RefsTmp, RefsOut, RestAceTexts).
*/

predicates_ace(
	[predicate(_, Verb, Subj, Obj)-_ | RestPredicates],
	RefsIn,
	RefsOut,
	[AceSentence | RestAceTexts]
	) :-
	!,
	get_noun(RefsIn, yes, no, Subj, SubjNum, SurfaceSubj),
	get_noun([Subj | RefsIn], yes, no, Obj, _ObjDom, SurfaceObj),
	num_sgpl_copula(SubjNum, SgPl, _Copula),
	surface_verb(SgPl, Verb, SurfaceVerb),
	acefragment_acesentence([SurfaceSubj, SurfaceVerb, SurfaceObj], AceSentence),
	predicates_ace(RestPredicates, [Subj, Obj | RefsIn], RefsOut, RestAceTexts).

predicates_ace(
	[predicate(_, Verb, Subj)-_ | RestPredicates],
	RefsIn,
	RefsOut,
	[AceSentence | RestAceTexts]
	) :-
	!,
	get_noun(RefsIn, yes, no, Subj, SubjNum, SurfaceSubj),
	num_sgpl_copula(SubjNum, SgPl, _Copula),
	surface_verb(SgPl, Verb, SurfaceVerb),
	acefragment_acesentence([SurfaceSubj, SurfaceVerb], AceSentence),
	predicates_ace(RestPredicates, [Subj | RefsIn], RefsOut, RestAceTexts).

predicates_ace([object(_, _, _, na, _, _)-_ | RestPredicates], RefsIn, RefsOut, RestAceTexts) :-
	!,
	predicates_ace(RestPredicates, RefsIn, RefsOut, RestAceTexts).

predicates_ace([[has_part(_, _)-_] => _ | _], _, _, _) :-
	!,
	fail.

% BUG: we should verbalize it now, not later
predicates_ace([_ => _ | RestPredicates], RefsIn, RefsOut, RestAceTexts) :-
	predicates_ace(RestPredicates, RefsIn, RefsOut, RestAceTexts).


%% drs_ace(+Drs:drs, +TopLevelObjects:list, -AceTextList:list) is det.
%
% Just a simplifying wrapper.

drs_ace(Drs, ToplevelObjects, AceTextList) :-
	conds_acetext(Drs, [yes, _, ToplevelObjects-_, _, _, _, _], AceTextList).


%% conds_acetext(+YesNo:atom, +Conj:atom, +Refs:functor, +Box:atom, +MainSubj:atom, +InOut:functor, +Conds:list, -AceTexts:list) is det.
%
% @param Conds list of DRS conditions.
% @param Features A list of features (see =|cond_acetext/3|=).
% @param AceTexts ACE text corresponding to the list of DRS conditions.
%
conds_acetext([], [_, _, Refs-Refs, _, _, F:F, In:In], []).

conds_acetext([Cond | Conds], [YesNo, Conj, RefsIn-RefsOut, Box, MainSubj, FIn:FOut, In:Out], [AceText | AceTexts]) :-
	cond_acetext(Cond, [YesNo, Conj, RefsIn-RefsTmp, Box, MainSubj, FIn:FTmp, In:Tmp], AceText),
	get_coordinator(Cond, Conj, NewConj),
	conds_acetext(Conds, [YesNo, NewConj, RefsTmp-RefsOut, Box, MainSubj, FTmp:FOut, Tmp:Out], AceTexts).



%% cond_acetext(+Cond:term, +Features:list, -AceSentence:list) is det.
%
% @param Cond is a DRS condition.
% @param Features is a list of features.
% @param AceSentence is an ACE sentence corresponding to the DRS condition.
%
% Features:
%
% * YesNo {yes, no}, indicates whether the condition is not negated.
% * Conj {and, ,and}, indicates the conjunction to be used.
% * Refs RefsIn-RefsOut carries around the referents encountered so far.
% * Box {if, then}, indicates whether the condition is in the IF-box or THEN-box.
% * MainSubj discourse referent of the main subject.
% * FInOut Fin:FOut carries around {yes, no} that indicates whether the condition is the first one in the box.
% * InOut In:Out carries around the current subject.
%
% BUG: kaarel: 060805: here we reject stuff, but we should reject even more,
% e.g. everything that is not in if-then. To do this we need levels' support
% like in implication_turn.pl.
%
% BUG: after handling the if-then, we can "cut it out" and verbalize the rest
% with Drace. That should work, since if-then is not reachable for the
% rest of the DRS.

cond_acetext(Condition-_, _Features, _AceText) :-
	functor(Condition, F, Args),
	(
		F = modifier_adv
	;
		F = modifier_pp
	;
		F = has_part
	;
		F = property
	;
		F = relation
	;
		F = query
	;
		F = predicate, Args = 6
	),
	!,
	fail.

cond_acetext(_:_, _, _) :- !, fail.

cond_acetext(~_, _, _) :- !, fail.

% If there is a dog then there is a cat.
% This should fail, since we can't handle it with NPs.
% BUG: But this rule should be made more general to cover e.g. negation and conjunction of object/8.
cond_acetext(_ => [object(_, _, _, _, _, _)-_], _, _) :-
	!,
	fail.

% Here we handle all kinds of implication forms.
% They all produce full sentences.
cond_acetext(A => B, Features, AceSentence) :-
	!,
	implication_turn_acetext(A => B, Features, AceSentence).


% v (disjunction)
cond_acetext(Conds1 v Conds2, [C, Coord, RefsIn-RefsIn, Box, MainSubj, FIn:no, Subj:Subj], [AceText1, or, AceText2]) :-
	!,
	make_comma_and_if_needed(Coord, NewCoord),
	conds_acetext(Conds1, [C, NewCoord, RefsIn-RefsTmp, Box, MainSubj, FIn:no, Subj:_], AceText1),
	conds_acetext(Conds2, [C, [], RefsTmp-_, Box, MainSubj, no:no, Subj:_], AceText2).


% F = -, can, must, should, may (not, can, must, should, may)
% BUG: why not In:In, i.e. why do we expose the embedded subject to the upper level?
% In case of In:In there was one regression. Study it further!
cond_acetext(Cond, [C, Conj, RefsIn-RefsOut, Box, MainSubj, FIn:no, In:Out], [AceText]) :-
	functor(Cond, F, 1),
	arg(1, Cond, Conds),
	modify_first_verb(F, Conds, Subj, NewConds),
	check_conds(Conds, Subj),
	!,
	conds_acetext(NewConds, [C, Conj, RefsIn-RefsOut, Box, MainSubj, FIn:no, In:Out], AceText).


% predicate-condition will be verbalized.
cond_acetext(Predicate, [Neg, Conj, RefsIn-RefsOut, Box, MainSubj, FIn:FOut, A:B], [Subj, Binder, Verb, Obj]) :-
	predicate_acefragment(Predicate, [Conj, Box, MainSubj, FIn:FOut, A:B], [DeepSubj, Binder, DeepVerb, DeepObj]),
	get_noun([], Neg, FIn, DeepSubj, _, Subj),
	get_noun(RefsIn, yes, no, DeepObj, _, Obj),
	ref_to_noun_wrapper(B, _, _, _, Number),
	num_sgpl_copula(Number, SgPl, _),
	get_verb(DeepVerb, SgPl, Verb),
	add_refs(RefsIn, DeepObj, RefsOut).


% Fallback (catches only object/8)
cond_acetext(object(_, _, _, _, _, _)-_, [_C, _Conj, Refs-Refs, _Box, _MSubj, F:F, Subj:Subj], [[], [], [], []]).


% BUG: clean this up, the "1" is not correct,
% in case the named-object is in plural.
ref_to_noun_wrapper(named(_), _, _, _, 1) :- !.

ref_to_noun_wrapper(B, _, _, _, Number) :-
	ref_to_noun(B, _, _, _, Number).



%% implication_turn_acetext(+Implication:term, +Features:list, -AceText:list) is det.
%
% Turns the implication box and verbalizes the result.

implication_turn_acetext(A => B, Features, AceSentence) :-
	implication_turn(A => B, ATurned => BTurned),
	implication_acetext(ATurned => BTurned, Features, AceFragment),
	!,
	acefragment_acesentence(AceFragment, AceSentence).

implication_turn_acetext(_, _, []).



%% implication_acetext(+Cond:functor, +Features:list, -AceText:list) is det.
%
% @param Cond A DRS condition.
% @param Features A list of features (see =|cond_acetext/3|=).
% @param AceText ACE text corresponding to the list of DRS conditions.

% Every man does not wait.
% Every man waits.
implication_acetext([object(Ref, _Lemma, Quant, _, Eq, Number)-_] => Conds2, [NegIn, _, RefsIn-RefsOut, _, _, _, _], [AceText1, AceText2]) :-
	!,
	countable_or_mass_or_dom(Quant),
	(
		Eq = eq,
		Number = 1
	;
		Eq = na,
		Number = na
	),
	(
		Conds2 = [-NotConds]
	->
		NegOut = no,
		Conds2Out = NotConds
	;
		NegOut = yes,
		Conds2Out = Conds2
	),
	get_noun([], NegOut, yes, Ref, _, AceText1),
	%get_noun_x(Ref, new, NegOut, yes, Quant, _Lemma, Eq, Number, AceText1),
	%add_var(Ref),
	conds_acetext(Conds2Out, [NegIn, and, [Ref | RefsIn]-RefsOut, then, Ref, yes:no, Ref:_], AceText2).


% Every man who sleeps does not wait.
% Every man who sleeps waits.
implication_acetext(Conds1 => Conds2, [NegIn, _, RefsIn-RefsOut, _, _, _, _], [AceText1, AceText2]) :-
	select(object(Ref, _, Quant, _, Eq, Number)-_, Conds1, Conds1Out),
	countable_or_mass_or_dom(Quant),
	(
		Eq = eq,
		Number = 1
	;
		Eq = na,
		Number = na
	),
	(
		Conds2 = [-NotConds]
	->
		NegOut = no,
		Conds2Out = NotConds
	;
		NegOut = NegIn,
		Conds2Out = Conds2
	),
	conds_acetext(Conds1Out, [NegOut, and, [Ref | RefsIn]-RefsTmp, if, Ref, yes:no, Ref:_], AceText1),
	conds_acetext(Conds2Out, [NegIn, and, RefsTmp-RefsOut, then, Ref, yes:no, Ref:_], AceText2).


%% predicate_acefragment(+Cond:term, +Features:list, -AceFragment:list) is det.
%
% @param Cond A DRS condition.
% @param Features A list of features.
% @param AceFragment An ACE fragment (part of the sentence) corresponding to the DRS condition.
%	The fragment has the form of [Subject, Coordinator, Verb, Object]
%
% Features:
%
% * Conj {and, ,and}, indicates the conjunction to be used (either the strong or the weak one).
% * Box {if, then}, indicates whether the condition is in the IF-box or THEN-box.
% * MainSubj discourse referent of the main subject.
% * FirstIn:FirstOut {yes, no} Is it the first element to be verbalized in the IF-box or THEN-box.
% * In:Out carries around the discourse referent of the current subject.
%
% Some examples of suported positive and negative sentences:
%
% * OK: Every man sees a cat and sees a dog.
% * NOT OK: Every man sees a cat and that sees a dog.
% * OK: Every man sees a cat that sees a mouse and that sees a dog.

% In THEN-box only.
% Every man [HATES A RAT] ...
% Every man that sees a dog [HATES A RAT] ...
% Every man that sees a dog and that hears a cat that eats a mouse [HATES A RAT] ...
predicate_acefragment(predicate(_, Verb, Subj, Obj)-_, [_, then, Subj, yes:no, Subj:Subj], [[], [], Verb, Obj]) :- !.


% In THEN-box only.
% Every man likes a cat [AND DRINKS SOME BEER].
% Every man likes a cat [OR DRINKS SOME BEER].
predicate_acefragment(predicate(_, Verb, Subj, Obj)-_, [Conj, then, Subj, no:no, _:Subj], [[], Conj, Verb, Obj]) :- !.


% In the IF-box only.
% Every [MAN THAT SEES A DOG] ...
predicate_acefragment(predicate(_, Verb, Subj, Obj)-_, [_, if, Subj, yes:no, Subj:Subj], [Subj, 'that', Verb, Obj]) :- !.


% Every man that sees a dog and that hears a cat [THAT EATS A MOUSE] ...
predicate_acefragment(predicate(_, Verb, NewSubj, Obj)-_, [_, _, _, no:no, Subj:NewSubj], [[], 'that', Verb, Obj]) :- Subj \= NewSubj, !.


% Every man that sees a dog [AND THAT HEARS A CAT] ...
% Every man that sees a dog [OR THAT HEARS A CAT] ...
predicate_acefragment(predicate(_, Verb, Subj, Obj)-_, [Conj, _, _, no:no, Subj:Subj], [[], [Conj, that], Verb, Obj]) :- !.




% Intransitive verb support

% In THEN-box only.
% Every man [WAITS] ...
predicate_acefragment(predicate(_, Verb, Subj)-_, [_, then, Subj, yes:no, Subj:Subj], [[], [], Verb, []]) :- !.

% In THEN-box only.
% Every man waits [AND eats] ...
% Every man waits [OR eats] ...
predicate_acefragment(predicate(_, Verb, Subj)-_, [Conj, then, Subj, no:no, _:Subj], [[], Conj, Verb, []]) :- !.

% In the IF-box only.
% Every man [THAT WAITS] eats ...
predicate_acefragment(predicate(_, Verb, Subj)-_, [_, if, Subj, yes:no, Subj:Subj], [Subj, 'that', Verb, []]) :- !.

predicate_acefragment(predicate(_, Verb, NewSubj)-_, [_, _, _, no:no, Subj:NewSubj], [[], 'that', Verb, []]) :- Subj \= NewSubj, !.

predicate_acefragment(predicate(_, Verb, Subj)-_, [Conj, _, _, no:no, Subj:Subj], [[], [Conj, that], Verb, []]).



%% get_verb(+DeepVerb:functor, +SgPl:atom, -SurfaceVerb:list) is det.
%
% @param DeepVerb Examples: see, not(see), i(see), not(i(see)), ...
% @param SgPl {sg, pl} Shows the number of the subject of the verb.
% @param SurfaceVerb Examples: [sees], [does, not, see], [is, seen, by], [is, not, seen, by]
%
% @bug FIXED: Needs plural support (i.e. 'do not ...', 'are not')
% @bug not(not(be)) not supported: Every man is not no dog. (?)
% @bug not(not(Verb)) not supported: Every man does not see no dog.
% @bug implement/reject can(not(i(be)), ...

get_verb([], _, []) :-
	!.

% BUG: special case if the verb is copula
% this should be handled for other cases as well (can, cannot, ...)
get_verb(not(i(be)), SgPl, IsAreNot) :-
	surface_neg_verb(SgPl, be, IsAreNot),
	!.

get_verb(not(i(Verb)), SgPl, [IsAreNot, VerbEd, by]) :-
	surface_neg_verb(SgPl, be, IsAreNot),
	surface_verb(part, Verb, VerbEd),
	!.

get_verb(can(i(be)), _, [can, be]) :-
	!.

get_verb(can(i(Verb)), _, [can, be, VerbEd, by]) :-
	surface_verb(part, Verb, VerbEd),
	!.

get_verb(must(i(be)), _, [must, be]) :-
	!.

get_verb(must(i(Verb)), _, [must, be, VerbEd, by]) :-
	surface_verb(part, Verb, VerbEd),
	!.

get_verb(should(i(be)), _, [should, be]) :-
	!.

get_verb(should(i(Verb)), _, [should, be, VerbEd, by]) :-
	surface_verb(part, Verb, VerbEd),
	!.

get_verb(may(i(be)), _, [may, be]) :-
	!.

get_verb(may(i(Verb)), _, [may, be, VerbEd, by]) :-
	surface_verb(part, Verb, VerbEd),
	!.

/*
get_verb(not(not(be)), _, [is, bug_not]) :-
	!.

get_verb(not(not(Verb)), _, [does, bug_not, Verb]) :-
	!.
*/


/*
get_verb(not(be), SgPl, IsAreNot) :-
	surface_neg_verb(SgPl, be, IsAreNot),
	!.
*/

get_verb(not(Verb), SgPl, NegVerb) :-
	surface_neg_verb(SgPl, Verb, NegVerb),
	!.

get_verb(can(not(i(Verb))), _, [cannot, be, VerbEd, by]) :-
	surface_verb(part, Verb, VerbEd),
	!.

get_verb(can(not(Verb)), _, [cannot, Verb]) :-
	!.

get_verb(can(Verb), _, [can, Verb]) :-
	!.

get_verb(must(not(i(Verb))), SgPl, [NegHave, to, be, VerbEd, by]) :-
	surface_neg_verb(SgPl, have, NegHave),
	surface_verb(part, Verb, VerbEd),
	!.

get_verb(must(not(Verb)), SgPl, [NegHave, to, Verb]) :-
	surface_neg_verb(SgPl, have, NegHave),
	!.

get_verb(must(Verb), _, [must, Verb]) :- !.
get_verb(should(Verb), _, [should, Verb]) :- !.
get_verb(can(Verb), _, [can, Verb]) :- !.

% BUG: special case if the verb is copula
% this should be handled for other cases as well (can, cannot, ...)
get_verb(i(be), sg, is) :-
	!.

get_verb(i(Verb), sg, [is, VerbEd, by]) :-
	surface_verb(part, Verb, VerbEd),
	!.

get_verb(i(Verb), pl, [are, VerbEd, by]) :-
	surface_verb(part, Verb, VerbEd),
	!.

get_verb(Verb, SgPl, SurfaceVerb) :-
	surface_verb(SgPl, Verb, SurfaceVerb).



%% get_noun(+Refs:list, +Positive:atom, +Pos:atom, +Ref:term, -Number:atomic, -NP:list) is det.
%
% @param Refs is a list of discourse referents (numbervared)
% @param Positive is in {yes, no} depending on whether the NP is positive or under negation
% @param Pos is in {yes, no} depending on whether the word is in the beginning of the sentence or not (BUG?)
% @param Ref is a discourse referent (numbervared)
% @param Number is a positive integer or 'na'
% @param NP is an ACE noun phrase (specifically: a determiner and a noun)

% BUG: is this needed? yes, removing it causes some regression
% BUG: sg is wrong anyway
get_noun(_, _, _, [], 1, []) :- !.

% BUG: The Num-detection does not seem to work.
get_noun(_Refs, _Positive, _Pos, named(Name), Num, NameText) :-
	surface_noun(pn, Name, SgPl, NameText),
	(SgPl = sg -> Num = 1 ; Num = 2),
	!.

get_noun(Refs, Positive, Pos, Ref, Number, NP) :-
	ref_to_noun(Ref, Quantisation, Noun, Operator, Number),
	get_det(Ref, Refs, OldNew),
	add_var(Ref),
	get_noun_x(Ref, OldNew, Positive, Pos, Quantisation, Noun, Operator, Number, NP),
	!.


%% get_noun_x(+Ref:numbervar, +OldNew:atom, +Positive:atom, +Pos:atom, +Quantisation:atom, +Noun:atom, +Operator:atom, +Number:atomic, -NP:list) is det.
%
% @param Ref is a discourse referent (numbervared)
% @param OldNew is in {old, new}
% @param Positive is in {yes, no} depending on whether the NP is positive or under negation
% @param Pos is in {yes, no} depending on whether the word is in the beginning of the sentence or not (BUG?)
% @param Quantisation is in {dom, countable, mass, named}
% @param Noun is a lemma of a noun, proper name, or indefinite pronoun
% @param Operator is in {na, eq, leq, geq, greater, less, exactly}
% @param Number is a positive integer or 'na'
% @param NP is an ACE noun phrase (specifically: a determiner and a noun)
%
% BUG: Note that referents (variables) are not added to plural nouns. Think about it.
% BUG: we should cut here more
%
get_noun_x(Ref, _, yes, yes, countable, somebody, eq, 1, ['everybody', Ref]).
get_noun_x(Ref, _, yes, yes, dom, something, na, na, ['everything', Ref]).
get_noun_x(Ref, _, no, yes, countable, somebody, eq, 1, ['nobody', Ref]).
get_noun_x(Ref, _, no, yes, dom, something, na, na, ['nothing', Ref]).
get_noun_x(Ref, new, yes, _, countable, somebody, eq, 1, [somebody, Ref]).
get_noun_x(Ref, new, yes, _, dom, something, na, na, [something, Ref]).
get_noun_x(Ref, old, yes, _, countable, somebody, eq, 1, [Ref]).
get_noun_x(Ref, old, yes, _, dom, something, na, na, [Ref]).
get_noun_x(Ref, _, yes, yes, countable, Noun, eq, 1, ['every', NounSg, Ref]) :- surface_noun(cn, Noun, sg, NounSg).
%
get_noun_x(Ref, _, yes, yes, mass, Noun, na, na, ['all', NounSg, Ref]) :- surface_noun(cn, Noun, mass, NounSg).
get_noun_x(Ref, _, no, yes, countable, Noun, eq, 1, ['no', NounSg, Ref]) :- surface_noun(cn, Noun, sg, NounSg).
%
get_noun_x(Ref, _, no, yes, mass, Noun, na, na, ['no', NounSg, Ref]) :- surface_noun(cn, Noun, mass, NounSg). % BUG: ambiguity
get_noun_x(Ref, new, yes, _, countable, Noun, eq, 1, [a, NounSg, Ref]) :- surface_noun(cn, Noun, sg, NounSg).
get_noun_x(Ref, new, yes, _, mass, Noun, na, na, [some, NounSg, Ref]) :- surface_noun(cn, Noun, mass, NounSg).
get_noun_x(Ref, old, yes, _, countable, Noun, _, 1, [the, NounSg, Ref]) :- surface_noun(cn, Noun, sg, NounSg).
% BUG: number was 1. why?
get_noun_x(Ref, old, yes, _, mass, Noun, na, na, [the, NounSg, Ref]) :- surface_noun(cn, Noun, mass, NounSg).

% BUG: was group
get_noun_x(_, old, yes, _, countable, Noun, _, _, [the, NounPl]) :- surface_noun(cn, Noun, pl, NounPl).

% Num = {0, 1, ...}
get_noun_x(_, new, yes, _, countable, Noun, eq, Num, [Num, Form]) :- num_num(Num, SgPl), surface_noun(cn, Noun, SgPl, Form).
get_noun_x(_, new, yes, _, countable, Noun, less, Num, [less, than, Num, Form]) :- num_num(Num, SgPl), surface_noun(cn, Noun, SgPl, Form).
get_noun_x(_, new, yes, _, countable, Noun, greater, Num, [more, than, Num, Form]) :- num_num(Num, SgPl), surface_noun(cn, Noun, SgPl, Form).
get_noun_x(_, new, yes, _, countable, Noun, leq, Num, [at, most, Num, Form]) :- num_num(Num, SgPl), surface_noun(cn, Noun, SgPl, Form).
get_noun_x(_, new, yes, _, countable, Noun, geq, Num, [at, least, Num, Form]) :- num_num(Num, SgPl), surface_noun(cn, Noun, SgPl, Form).
get_noun_x(_, new, yes, _, countable, Noun, exactly, Num, [exactly, Num, Form]) :- num_num(Num, SgPl), surface_noun(cn, Noun, SgPl, Form).


%% num_num(+Num:integer, -SgPl:atom) is det.
%
% Examples: 0 cars, 1 car, 4 cars
%
% @param Num is in {0, 1, ...}
% @param SgPl is in {sg, pl}
%
num_num(0, pl) :- !.
num_num(1, sg) :- !.
num_num(N, pl) :- N > 1.


%% get_det(+Element:term, +List:list, -OldNew:atom) is det.
%
%
get_det(El, List, old) :-
	member(El, List),
	!.

get_det(_, _, new).


%% get_toplevel_referents(+Conds:list, -Toplevel:term) is det.
%
% Asserts all common nouns and propernames into ref_to_noun/5.
% Returns a list of toplevel referents for objects.
%
get_toplevel_referents(Conds, toplevel(ReferentList, SubjectList, ObjectList, NamedList)) :-
	store_objects(Conds, toplevel(-([], ReferentList), -([], SubjectList), -([], ObjectList), -([], NamedList))).


%% store_object(+Conds:list, -Toplevel:term) is det.
%
%
store_objects([], toplevel(R-R, S-S, O-O, N-N)).

store_objects([Cond | Conds], toplevel(R1-R2, S1-S2, O1-O2, N1-N2)) :-
	do_cond(Cond, toplevel(R1-RT, S1-ST, O1-OT, N1-NT)),
	store_objects(Conds, toplevel(RT-R2, ST-S2, OT-O2, NT-N2)).


%% do_cond(-Cond:term, -Toplevel:term) is det.
%
%
do_cond(A => B, toplevel(R-R, S-S, O-O, N-N)) :-
	!,
	store_objects(A, _),
	store_objects(B, _).

do_cond(A v B, toplevel(R-R, S-S, O-O, N-N)) :-
	!,
	store_objects(A, _),
	store_objects(B, _).

do_cond(-A, toplevel(R-R, S-S, O-O, N-N)) :-
	!,
	store_objects(A, _).

do_cond(can(A), toplevel(R-R, S-S, O-O, N-N)) :-
	!,
	store_objects(A, _).

do_cond(must(A), toplevel(R-R, S-S, O-O, N-N)) :-
	!,
	store_objects(A, _).

do_cond(should(A), toplevel(R-R, S-S, O-O, N-N)) :-
	!,
	store_objects(A, _).

do_cond(may(A), toplevel(R-R, S-S, O-O, N-N)) :-
	!,
	store_objects(A, _).

do_cond(object(Ref, Noun, Type, _, Eq, na)-_, toplevel(R-[Ref | R], S-S, O-O, N-N)) :-
	!,
	assert(ref_to_noun(Ref, Type, Noun, Eq, na)).

do_cond(object(Ref, Noun, Type, _, Eq, Number)-_, toplevel(R-[Ref | R], S-S, O-O, N-N)) :-
	number(Number),
	!,
	assert(ref_to_noun(Ref, Type, Noun, Eq, Number)).

do_cond(object(Ref, Noun, Type, _, Eq, RawNumber)-_, toplevel(R-[Ref | R], S-S, O-O, N-N)) :-
	atom(RawNumber),
	!,
	atom_number(RawNumber, Number),
	assert(ref_to_noun(Ref, Type, Noun, Eq, Number)).

do_cond(predicate(_, _, Ref)-_, toplevel(R-R, S-[Ref | S], O-O, N-N)) :- !.

do_cond(predicate(_, _, Ref, O1)-_, toplevel(R-R, S-[Ref | S], O-[O1 | O], N-N)) :- !.

do_cond(predicate(_, _, Ref, O1, O2)-_, toplevel(R-R, S-[Ref | S], O-[O1, O2 | O], N-N)) :- !.

do_cond(_, toplevel(R-R, S-S, O-O, N-N)).



%% check_conds(+Conds:list, +Subject:atom) is det.
%
% We can't verbalize coordinations (both AND and OR) under immediate not/must/can,
% therefore we reject such DRSs.
%
% * OR-coordinations are easy to detect and reject.
% * AND-coordinations are harder to detect. We need to check whether there are
% two or more predicates which have the subject which agrees with the upper level subject.
%
% BUG: should we add: Conds \= [object(_, _, _, _, _, _)-_]
%
check_conds([_ v _], _) :- !, fail.

check_conds(Conds, Subject) :-
	findall(Cond, (
		member(Cond-_, Conds),
		functor(Cond, predicate, _),
		arg(4, Cond, Subject)
	), [_, _ | _]),
	!,
	fail.

check_conds(_, _).



%% modify_first_verb(+Functor:atom, +Conds:list, -Subj:nvar, -NewConds:list) is det.
%
% We go deep into the DRS and look for the first predicate. We modify it
% by placing it into a functor, e.g. see -> can(see).
%
modify_first_verb(Functor, Conds, Subject, NewConds) :-
	rep(Functor, Conds, Subject, NewConds).


%% rep(+Functor:atom, +Conds:list, -Subject:nvar, -NewConds:list) is det.
%
%
rep(Functor, Conds, Subject, [ModifiedPredicate | Rest]) :-
	select(Predicate, Conds, Rest),
	pred(Functor, Predicate, Subject, ModifiedPredicate),
	!.

rep(Functor, [-Conds], Subject, [-NewConds]) :-
	rep(Functor, Conds, Subject, NewConds).

rep(Functor, [can(Conds)], Subject, [can(NewConds)]) :-
	rep(Functor, Conds, Subject, NewConds).

rep(Functor, [must(Conds)], Subject, [must(NewConds)]) :-
	rep(Functor, Conds, Subject, NewConds).

rep(Functor, [should(Conds)], Subject, [should(NewConds)]) :-
	rep(Functor, Conds, Subject, NewConds).

rep(Functor, [may(Conds)], Subject, [may(NewConds)]) :-
	rep(Functor, Conds, Subject, NewConds).

pred(Functor, predicate(Ref, Verb, Subject, Object)-Id, Subject, predicate(Ref, ModifiedVerb, Subject, Object)-Id) :-
	verb_modifiedverb(Functor, Verb, ModifiedVerb).

pred(Functor, predicate(Ref, Verb, Subject)-Id, Subject, predicate(Ref, ModifiedVerb, Subject)-Id) :-
	verb_modifiedverb(Functor, Verb, ModifiedVerb).


%% verb_modifiedverb(+Functor:atom, +Verb:atom, -FunctorVerb:term) is semidet.
%
%
verb_modifiedverb('-', Verb, not(Verb)).
verb_modifiedverb(can, Verb, can(Verb)).
verb_modifiedverb(must, Verb, must(Verb)).
verb_modifiedverb(should, Verb, should(Verb)).
verb_modifiedverb(may, Verb, may(Verb)).


%% get_main_subject(+Conds:list, -Subject:atom) is nondet.
%
% Extracts the subject of the condition.
%
get_main_subject(Conds, Subject) :-
	member(predicate(_, _, Subject, _)-_, Conds).

get_main_subject(Conds, Subject) :-
	member(predicate(_, _, Subject)-_, Conds).


%% get_coordinator(+Condition:functor, +OldCoord:atom, -NewCoord:atom) is det.
%
% We find a new coordinator on the basis of the condition that we have
% just verbalized. If this condition was a predicate then the new coordinator
% will be 'and'. If the condition was a disjunction then we need a ',and'.
% Otherwise the old coordinator will be used (which is usually 'and').
% This solution is hackish and doesn't always work, e.g. sometimes we should
% look ahead to be able to decide whether to use 'and' or ',and'.
%
% We added this hackish solution to support:
%
% Every man works or eats and sleeps.
% Every man works or eats ,and sleeps or drinks.

get_coordinator(Cond-_, _, and) :-
	functor(Cond, predicate, _),
	!.

get_coordinator(_ v _, _, ', and') :-
	!.

get_coordinator(_, Old, Old).



%% add_refs(+List:list, +Ref:functor, -List:list) is det.
%
% Updates a list...

add_refs(List, [], List) :- !.
add_refs(List, Ref, [Ref | List]).


%% make_comma_and_if_needed(+Coord:atom, -Coord:list) is det.
%
% BUG: a quick hack...

make_comma_and_if_needed(', and', [',', and]) :- !.
make_comma_and_if_needed(and, [',', and]) :- !.
make_comma_and_if_needed(_, []).


%% num_sgpl_copula(+Number:atomic, -SgPl:atom, -IsAre:atom) is det.
%
%
num_sgpl_copula(1, sg, is) :- !.
num_sgpl_copula(na, sg, is) :- !.
num_sgpl_copula(_, pl, are).


%% countable_or_mass_or_dom(+Quant:atom) is det.
%
%
countable_or_mass_or_dom(countable).
countable_or_mass_or_dom(mass).
countable_or_mass_or_dom(dom).
