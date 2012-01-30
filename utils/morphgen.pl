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


:- module(morphgen, [
		clear_vars/0,
		add_var/1,
		remove_singletons/2,
		acesentencelist_pp/2,
		listlist_listatom/2,
		format_dr/2,
		surface_noun/4,
		surface_verb/3,
		surface_neg_verb/3,
		surface_adverb/3,
		surface_property/3,
		surface_property/6,
		surface_quotedstring/2,
		get_di_marker/4
	]).

/** <module> Morphological synthesis and text generation

@author Kaarel Kaljurand
@version 2010-04-20

*/

:- use_module('../lexicon/lexicon_interface', []).
:- use_module('../lexicon/chars', [
		is_capitalized/1
	]).

:- use_module(ace_niceace, [
		ace_niceace/2,
		word_article/2,
		atom_capitalize/2
	]).


:- dynamic var_once/1.
:- dynamic var_twice/1.
:- dynamic var/2.


%% format_dr(+Dr:nvar, -NiceDr:atom) is det.
%
% @param Dr numbervared variable
% @param NiceDr pretty-printed numbervared variable
%
format_dr(Dr, NiceDr) :-
	format(atom(NiceDr), '~W', [Dr, [numbervars(true)]]).


%% surface_verb(+SgPlPart:atom, +Lemma:atom, -WordForm:atom) is det.
%
% Examples:
%
% * surface_verb(sg, know, knows)
% * surface_verb(pl, know, know)
% * surface_verb(part, know, known)
%
% BUGs:
%
% * reorder the arguments SgPl and Lemma
%
% * rewrite to support plural participles, i.e. we need 2 features:
%	number = {sg, pl}, participleness = {yes, no}
%
% * maybe this would be faster than concat_atom/3:
% format(atom(WordForm), "is ~w by", [WordFormTmp])
%
% * this is a quick hack to make ACE Core verbalization work with i-verbs.
%
% @param SgPlPart {sg, pl, part}, is the number of the verb or its participleness
% @param Lemma lemma of the verb
% @param WordForm is the synthesized form of the verb
%
surface_verb(_SgPl, i(Lemma), WordForm) :-
	!,
	surface_verb_x(part, Lemma, WordFormTmp),
	concat_atom([is, WordFormTmp, by], ' ', WordForm).

surface_verb(SgPl, Lemma, WordForm) :-
	surface_verb_x(SgPl, Lemma, WordForm).


%% surface_verb_x(+SgPlPart:atom, +Lemma:atom, -WordForm:atom) is det.
%
% @param SgPlPart {sg, pl, part}, is the number of the verb or its participleness
% @param Lemma lemma of the verb
% @param WordForm is the synthesized form of the verb
%
surface_verb_x(sg, Verb, SurfaceVerb) :-
	verb_sg(Verb, SurfaceVerb).

surface_verb_x(pl, Verb, SurfaceVerb) :-
	verb_pl(Verb, SurfaceVerb).


surface_verb_x(part, Verb, VerbEd) :-
	lexicon_interface:tv_pp(VerbEd, Verb),
	!.

surface_verb_x(part, Verb, VerbEd) :-
	lexicon_interface:dv_pp(VerbEd, Verb, _),
	!.

surface_verb_x(part, Verb, [v, ':', Verb]).


%% surface_neg_verb(+SgPl:atom, +Lemma:atom, -WordForm:list) is det.
%
% BUG: We should also support participles here.
%
% @param SgPl {sg, pl}, is the number of the negated verb
% @param Lemma lemma of the negated verb
% @param WordForm is the synthesized form of the verb
%
surface_neg_verb(sg, be, [is, not]) :- !.
surface_neg_verb(pl, be, [are, not]) :- !.
surface_neg_verb(sg, Verb, [does, not, WordForm]) :- !, surface_verb(pl, Verb, WordForm).
surface_neg_verb(pl, Verb, [do, not, WordForm]) :- !, surface_verb(pl, Verb, WordForm).


%% surface_adverb(+Adverb:atom, +Comparison:atom, -SurfaceText:list) is det.
%
% Note that comparative forms like 'faster', are lemmatized as 'fast'.
%
% @param Adverb is a lemma form of an adverb, as found in the DRS.
% @param Comparision is one of {pos, comp, sup}
% @param SurfaceText is a combination of the adverb and the comparision type.
%
surface_adverb(Adverb, pos, [Adverb]).

surface_adverb(Adverb, comp, [CompAdverb]) :-
	lexicon_interface:adv_comp(CompAdverb, Adverb),
	!.

surface_adverb(Adverb, comp, [more, Adverb]) :-
	lexicon_interface:adv(Adverb, _),
	!.

surface_adverb(Adverb, comp, [more, a, ':', Adverb]).

surface_adverb(Adverb, sup, [SupAdverb]) :-
	lexicon_interface:adv_sup(SupAdverb, Adverb),
	!.

surface_adverb(Adverb, sup, [most, Adverb]) :-
	lexicon_interface:adv(Adverb, _),
	!.

surface_adverb(Adverb, sup, [most, a, ':', Adverb]).


%% surface_property(+Adjective:atom, +Comparison:atom, -SurfaceText:list) is det.
%
% Note that comparative forms like 'better', are lemmatized as 'good'.
%
% @param Adjective is a lemma form of an adjective, as found in the DRS.
% @param Comparision is one of {pos, pos_as, comp, sup, comp_than}
% @param SurfaceText is a combination of the adjective and the comparision type.
%
surface_property(Adjective, pos, [Adjective]) :-
	lexicon_interface:adj_itr(Adjective, _),
	!.

surface_property(Adjective, pos, [Adjective]) :-
	lexicon_interface:adj_tr(Adjective, _, _),
	!.

surface_property(Adjective, pos, [a, ':', Adjective]).


surface_property(Adjective, pos_as, [as, Adjective, as]) :-
	lexicon_interface:adj_itr(Adjective, _),
	!.

surface_property(Adjective, pos_as, [as, a, ':', Adjective, as]).


surface_property(Adjective, comp, MoreAdjective) :- pos_comp(Adjective, MoreAdjective, _).

surface_property(Adjective, sup, MostAdjective) :- pos_sup(Adjective, MostAdjective).

surface_property(Adjective, comp_than, [MoreAdjective, than]) :- pos_comp(Adjective, MoreAdjective, _).


%% surface_property(+Adjective:atom, +Comparison:atom, +ComparisonTarget:atom, +Ref2Text:list, +Ref3Text:list,-SurfaceText:list) is det.
%
% Note that comparative forms like 'fonder-of', are lemmatized as 'fond-of'.
%
% Examples:
%
% * [John is] as fond-of Mary as Bill
% * [John is] as fond-of Mary as of Bill
% * [John is] more fond-of Mary than Bill
% * [John is] more fond-of Mary than of Bill
%
% @param Adjective is a lemma form of an adjective, as found in the DRS.
% @param Comparision is one of {pos_as, comp_than}
% @param ComparisionTarget is one of {subj, obj}
% @param Ref2Text is the surface text of the 2nd NP argument of the property/6
% @param Ref3Text is the surface text of the 3rd NP argument of the property/6
% @param SurfaceText is a combination of the adjective, comparision type,
% comparison target, and the surface texts of the arguments
%
surface_property(Adjective, pos_as, subj, Ref2Text, Ref3Text, [as, Adjective, Ref2Text, as, Ref3Text]).

surface_property(Adjective, pos_as, obj, Ref2Text, Ref3Text, [as, Adjective, Ref2Text, as, Preposition, Ref3Text]) :-
	pos_comp(Adjective, _, Preposition).

surface_property(Adjective, comp_than, subj, Ref2Text, Ref3Text, [MoreAdjective, Ref2Text, than, Ref3Text]) :-
	pos_comp(Adjective, MoreAdjective, _).

surface_property(Adjective, comp_than, obj, Ref2Text, Ref3Text, [MoreAdjective, Ref2Text, than, Preposition, Ref3Text]) :-
	pos_comp(Adjective, MoreAdjective, Preposition).


%% pos_comp(+Adjective:atom, -ComparativeAdjective:list, -Preposition:list) is det.
%
% Note that as with plural nouns, Clex does not tell us whether the surface form of an adjective (e.g. 'far')
% is to be preferred over another surface form with the same lemma.
% E.g. 'far' can be mapped to either 'further' or 'farther'.
% 
% @param Adjective is an ACE adjective (e.g. 'good')
% @param ComparativeAdjective is an ACE comparative adjective (e.g. 'better', 'more tall', 'fonder-of')
% @param Preposition is the preposition of a transitive adjective (e.g. 'of' in case of 'fonder-of')
%
pos_comp(Positive, [Comparative], []) :-
	lexicon_interface:adj_itr_comp(Comparative, Positive),
	!.

pos_comp(Positive, [Comparative], [Preposition]) :-
	lexicon_interface:adj_tr_comp(Comparative, Positive, Preposition),
	!.

pos_comp(Positive, [more, Positive], []) :-
	lexicon_interface:adj_itr(Positive, _),
	!.

pos_comp(Positive, [more, Positive], [Preposition]) :-
	lexicon_interface:adj_tr(Positive, _, Preposition),
	!.

% If everything else fails, then we add a prefix.
pos_comp(Positive, [more, a, ':', Positive], []).


%% pos_sup(+Adjective:atom, -SuperlativeAdjective:list) is det.
%
% @param Adjective is an ACE adjective (e.g. 'good')
% @param SuperlativeAdjective is an ACE superlative adjective (e.g. 'best', 'most tall')
%
pos_sup(Positive, [Superlative]) :-
	lexicon_interface:adj_itr_sup(Superlative, Positive),
	!.

pos_sup(Positive, [Superlative]) :-
	lexicon_interface:adj_tr_sup(Superlative, Positive, _),
	!.

pos_sup(Positive, [most, Positive]) :-
	lexicon_interface:adj_itr(Positive, _),
	!.

pos_sup(Positive, [most, Positive]) :-
	lexicon_interface:adj_tr(Positive, _, _),
	!.

% If everything else fails, then we add a prefix.
pos_sup(Positive, [most, a, ':', Positive]).


%% surface_quotedstring(+DrsString:atomic, -QuotedString:atom) is det.
%
% Adds quotes around the atom within the DRS string(.),
% escaping also double quotes and backslashes.
% The result is an ACE quoted string.
%
% @param DrsString e.g. 'a\\b"c'
% @param QuotedString e.g. '"a\\\\b\"c"'
%
surface_quotedstring(DrsString, QuotedString) :-
	atom_codes(DrsString, Codes1),
	quote_and_escape(Codes1, Codes2),
	atom_codes(QuotedString, Codes2).


%% quote_and_escape(+CodesIn:list, -CodesOut:list) is det.
%
% Escapes quote (34) and backslash (92) characters in the input list of codes,
% borders the resulting list with quotes (34).
%
quote_and_escape(Cs, [34 | QCs]) :-
	quote_and_escape_(Cs, QCs).

quote_and_escape_([], [34]).

quote_and_escape_([92 | Cs], [92, 92 | QCs]) :-
	!,
	quote_and_escape_(Cs, QCs).

quote_and_escape_([34 | Cs], [92, 34 | QCs]) :-
	!,
	quote_and_escape_(Cs, QCs).

quote_and_escape_([C | Cs], [C | QCs]) :-
	quote_and_escape_(Cs, QCs).


%% surface_noun(+Type:atom, +Lemma:atom, ?Num:atom, -Form:term)
%
% Examples:
%
% * surface_noun(cn, man, sg, man)
% * surface_noun(cn, somebody, sg, somebody)
% * surface_noun(cn, man, pl, men)
% * surface_noun(cn, sand, mass, sand)
%
% Note: we do not add a prefix to unknown proper names unless they start with
% a lowercase character.
%
% Note that in the presence of aliases, the mapping of lemmas to surface forms
% is not deterministic. Consider e.g. the mapping of surface forms to lemmas:
%
% * abaci -> abacus
% * abacuses -> abacus
%
% As Clex does not include information about the main word vs its alias, we
% return whatever comes first.
%
% @param Type is one of {cn, pn}, i.e. common noun or proper name
% @param Lemma is the lemma of the noun as found in the DRS
% @param Num is one of {sg, mass, pl}
% @param Form is the surface form of the noun (possibly a list of tokens)
%
% @bug: possibly slow, because uses an unindexed argument
%
surface_noun(cn, Lemma, sg, Form) :-
	lexicon_interface:noun_sg(Form, Lemma, _),
	!.

surface_noun(cn, Lemma, mass, Form) :-
	lexicon_interface:noun_mass(Form, Lemma, _),
	!.

surface_noun(cn, Lemma, pl, Form) :-
	lexicon_interface:noun_pl(Form, Lemma, _),
	!.

% If the common noun lemma does not exist in the lexicon
% then output it with the n-prefix, regardless of
% its number.
surface_noun(cn, Lemma, _Num, [n, ':', Lemma]).


surface_noun(pn, Lemma, sg, Form) :-
	lexicon_interface:pn_sg(Form, Lemma, _),
	!.

surface_noun(pn, Lemma, sg, [the, Form]) :-
	lexicon_interface:pndef_sg(Form, Lemma, _),
	!.

surface_noun(pn, Lemma, pl, Form) :-
	lexicon_interface:pn_pl(Form, Lemma, _),
	!.

surface_noun(pn, Lemma, pl, [the, Form]) :-
	lexicon_interface:pndef_pl(Form, Lemma, _),
	!.

surface_noun(pn, Lemma, sg, [p, ':', Lemma]) :-
	\+ is_capitalized(Lemma),
	!.

surface_noun(pn, Lemma, pl, [p, ':', Lemma]) :-
	\+ is_capitalized(Lemma),
	!.

surface_noun(pn, Lemma, sg, Lemma).

surface_noun(pn, Lemma, pl, Lemma).



%% verb_pl(+Lemma:atom, -Pl:atom) is det.
%
% @param Lemma is the lemma of the verb as found in the DRS
% @param Pl is the plural form of the verb
%
verb_pl(be, are) :- !. % BUG: is this needed?

verb_pl(Lemma, Pl) :-
	lexicon_interface:iv_infpl(Pl, Lemma),
	!.

verb_pl(Lemma, Pl) :-
	lexicon_interface:tv_infpl(Pl, Lemma),
	!.

verb_pl(Lemma, Pl) :-
	lexicon_interface:dv_infpl(Pl, Lemma, _),
	!.

verb_pl(Lemma, [v, ':', Lemma]).


%% verb_sg(+Lemma:atom, -Sg:atom) is det.
%
% @param Lemma is the lemma of the verb as found in the DRS
% @param Sg is the singular form of the verb
%
verb_sg(be, is) :- !. % BUG: is this needed?

verb_sg(Lemma, Sg) :-
	lexicon_interface:iv_finsg(Sg, Lemma),
	!.

verb_sg(Lemma, Sg) :-
	lexicon_interface:tv_finsg(Sg, Lemma),
	!.

verb_sg(Lemma, Sg) :-
	lexicon_interface:dv_finsg(Sg, Lemma, _),
	!.

verb_sg(Lemma, [v, ':', Lemma]).


%% get_di_marker(+SgPl:atom, +Lemma:atom, -SurfaceForm:term, -DiMarker:atom)
%
% Looks up the prepositional marker of a ditransitive verb
% by its lemma and its number.
% (Note that the ACE lexicon allows the
% same lemma to have a different marker in
% singular and in plural context. This is not
% the case in English though.)
%
get_di_marker(sg, Lemma, Form, DiMarker) :-
	lexicon_interface:dv_finsg(Form, Lemma, DiMarker),
	DiMarker \= '',
	!.

get_di_marker(sg, Lemma, Form, '') :-
	lexicon_interface:dv_finsg(Form, Lemma, ''),
	!.

get_di_marker(pl, Lemma, Form, DiMarker) :-
	lexicon_interface:dv_infpl(Form, Lemma, DiMarker),
	DiMarker \= '',
	!.

get_di_marker(pl, Lemma, Form, '') :-
	lexicon_interface:dv_infpl(Form, Lemma, ''),
	!.

get_di_marker(_, Lemma, [v, ':', Lemma], '').


%% acesentencelist_pp(+AceList:list, -AceText:atom) is det.
%
% Expects the input list to be a list of atoms (ACE sentences) or a list
% of lists of atoms (lists of ACE sentences).
% Produces an ACE text (atom).
%
% @param AceList is a list of (lists of) ACE sentences (atoms)
% @param AceText is an ACE text (atom)
%
acesentencelist_pp([], '').

acesentencelist_pp([X | Xs], Ace) :-
	is_list(X), % peek, if is paragraph
	!,
	maplist(acesentencelist_pp_x, [X | Xs], AceList),
	concat_atom(AceList, '\n\n', Ace).

acesentencelist_pp([X | Xs], Ace) :-
	atom(X), % peek, if is sentence
	!,
	acesentencelist_pp_x([X | Xs], Ace).


acesentencelist_pp_x([], '# ...') :- !.

acesentencelist_pp_x(AceSentenceList, AceText) :-
	concat_atom(AceSentenceList, '\n', AceText).


%% listlist_listatom(+ListOfList:list, -ListOfAtom:list) is det.
%
% @param ListOfList is a list (ACE sentences) of lists (ACE tokens)
% @param ListOfAtom is a list of atoms (ACE sentences)
%
% @deprecated try to use ace_niceace:tokens_to_sentences/2 instead
%
% used only by Core ACE, NP ACE verbalizers, clean it up
%
listlist_listatom([], []).

listlist_listatom([TokenList | Tail], [Atom | RestT]) :-
	(
		TokenList = []
	->
		Atom = 'ERROR'
	;
		sentence_type(TokenList, NewTokenList),
		ace_niceace(NewTokenList, [FirstToken | RestTokenList]),
		atom_capitalize(FirstToken, FirstTokenCapitalized),
		concat_atom([FirstTokenCapitalized | RestTokenList], ' ', Atom)
	),
	listlist_listatom(Tail, RestT).


%% sentence_type(+TokenList:list, -NewTokenList:list) is det.
%% sentence_type(+TokenList:list, -Type:atom, -NewTokenList:list) is det.
%
% Assigns a type to the given sentence. Type is one of '.' and '?'.
%
% The qp/1 term contains a list of tokens that represent the
% query word (e.g. [how, many]).
%
sentence_type(TokenList, NewTokenList) :-
	sentence_type(TokenList, _, NewTokenList).


sentence_type([], '.', ['.']) :- !.

sentence_type([], '?', ['?']).

sentence_type([qp(QPhrase) | T], _, Tokens) :-
	!,
	sentence_type(T, '?', T2),
	append(QPhrase, T2, Tokens).

sentence_type([H | T], Type, [H | T2]) :-
	sentence_type(T, Type, T2).


%% clear_vars is det.
%
% Retracts dynamic predicates: var/2, var_once/1, var_twice/1.
%
clear_vars :-
	retractall(var(_, _)),
	retractall(var_once(_)),
	retractall(var_twice(_)).


%% add_var(+Var:nvar) is det.
%
% Asserts the (numbervared) variable so that we know if it has been
% used once or more times.
%
% @param Var is a numbervared discourse referent
%
add_var(Var) :-
	(
		var_twice(Var)
	->
		true
	;
		(
			var_once(Var)
		->
			retract(var_once(Var)),
			assert(var_twice(Var))
		;
			assert(var_once(Var))
		)
	).


%% remove_singletons(+ListIn:list, -ListOut:list) is det.
%% remove_singletons_x(+VarCount:integer, +ListIn:list, -ListOut:list) is det.
%
%
remove_singletons(In, Out) :-
	remove_singletons_x(1, _, In, Out).

remove_singletons_x(Count, Count, [], []).

remove_singletons_x(CountIn, CountOut, [List | T], [RList | RT]) :-
	remove_singletons_x(CountIn, CountTmp, List, RList),
	remove_singletons_x(CountTmp, CountOut, T, RT).

remove_singletons_x(CountIn, CountOut, ['$VAR'(N) | T], RT) :-
	var_once('$VAR'(N)),
	!,
	remove_singletons_x(CountIn, CountOut, T, RT).

remove_singletons_x(CountIn, CountOut, ['$VAR'(N) | T], [NiceRef | RT]) :-
	var(N, VarNumber),
	!,
	format(atom(NiceRef), 'X~w', [VarNumber]),
	remove_singletons_x(CountIn, CountOut, T, RT).

remove_singletons_x(CountIn, CountOut, ['$VAR'(N) | T], [NiceRef | RT]) :-
	!,
	assert(var(N, CountIn)),
	format(atom(NiceRef), 'X~w', [CountIn]),
	NewCount is CountIn + 1,
	remove_singletons_x(NewCount, CountOut, T, RT).

remove_singletons_x(CountIn, CountOut, [H | T], [H | RT]) :-
	remove_singletons_x(CountIn, CountOut, T, RT).
