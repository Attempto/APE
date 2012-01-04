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


:- module(ace_niceace, [
		tokens_to_sentences/2,
		atom_capitalize/2,
		pronoun_split/2,
		pronoun_split/3,
		ace_niceace/2,
		word_article/2
	]).


:- use_module('../lexicon/chars', [
		is_sentence_end_symbol/1
	]).

/** <module> ACE beautifier

@author Kaarel Kaljurand
@version 2012-01-04

This code does the following:

* a -> an (if appropriate)
* connect every comma, period, and question mark to the preceding word

*/


%% tokens_to_sentences(+ListOfList:list, -ListOfAtom:list) is det.
%
% @param ListOfList is a list (ACE sentences) of lists (ACE tokens)
% @param ListOfAtom is a list of atoms (ACE sentences)
%
% @bug get rid of 'ERROR' (maybe fail in this case?)
%
% called from: get_ape_results
%
tokens_to_sentences([], []).

tokens_to_sentences([TokenList | Tail], [Atom | RestT]) :-
	(
		TokenList = []
	->
		Atom = 'ERROR'
    ;
		ace_niceace(TokenList, [FirstToken | RestTokenList]),
		atom_capitalize(FirstToken, FirstTokenCapitalized),
		concat_atom([FirstTokenCapitalized | RestTokenList], ' ', Atom)
	),
	tokens_to_sentences(Tail, RestT).


%% pronoun_split(+Token:atom, -TokenPair:term) is semidet.
%% pronoun_split(-Token:atom, +TokenPair:term) is nondet.
%
% @param Token is an ACE token
% @param TokenPair is a list of 2 tokens, the 2nd of which is one of {-thing, -body, -one}
%
pronoun_split(Token, TokenPair) :-
	pronoun_split(Token, _, TokenPair).

pronoun_split(everything, lower, (every, '-thing')).
pronoun_split('Everything', upper, (every, '-thing')).
pronoun_split(nothing, lower, (no, '-thing')).
pronoun_split('Nothing', upper, (no, '-thing')).
pronoun_split(something, lower, (a, '-thing')).
pronoun_split('Something', upper, (a, '-thing')).

pronoun_split(everybody, lower, (every, '-body')).
pronoun_split('Everybody', upper, (every, '-body')).
pronoun_split(nobody, lower, (no, '-body')).
pronoun_split('Nobody', upper, (no, '-body')).
pronoun_split(somebody, lower, (a, '-body')).
pronoun_split('Somebody', upper, (a, '-body')).

pronoun_split(everyone, lower, (every, '-one')).
pronoun_split('Everyone', upper, (every, '-one')).
pronoun_split(noone, lower, (no, '-one')).
pronoun_split('Noone', upper, (no, '-one')).
pronoun_split(someone, lower, (a, '-one')).
pronoun_split('Someone', upper, (a, '-one')).


%% atom_capitalize(+Atom:atom, -CapitalizedAtom:atom) is det.
%
% Simple predicate to capitalize those ACE words which can occur
% in the beginning of the sentence.
%
% TODO: every preposition can also start a sentence
%
atom_capitalize(a, 'A') :- !.
atom_capitalize(the, 'The') :- !.
atom_capitalize(somebody, 'Somebody') :- !.
atom_capitalize(something, 'Something') :- !.
atom_capitalize(at, 'At') :- !.
atom_capitalize(less, 'Less') :- !.
atom_capitalize(more, 'More') :- !.
atom_capitalize(exactly, 'Exactly') :- !.
atom_capitalize(some, 'Some') :- !.
atom_capitalize(an, 'An') :- !.

atom_capitalize(there, 'There') :- !.
atom_capitalize(if, 'If') :- !.
atom_capitalize(it, 'It') :- !.
atom_capitalize(is, 'Is') :- !.
atom_capitalize(are, 'Are') :- !.
atom_capitalize(do, 'Do') :- !.
atom_capitalize(does, 'Does') :- !.
atom_capitalize(for, 'For') :- !.
atom_capitalize(not, 'Not') :- !.

atom_capitalize(each, 'Each') :- !.
atom_capitalize(every, 'Every') :- !.
atom_capitalize(everything, 'Everything') :- !.
atom_capitalize(everybody, 'Everybody') :- !.
atom_capitalize(no, 'No') :- !.
atom_capitalize(nothing, 'Nothing') :- !.
atom_capitalize(nobody, 'Nobody') :- !.
atom_capitalize(all, 'All') :- !.

atom_capitalize(who, 'Who') :- !.
atom_capitalize(whose, 'Whose') :- !.
atom_capitalize(what, 'What') :- !.
atom_capitalize(which, 'Which') :- !.
atom_capitalize(where, 'Where') :- !.
atom_capitalize(when, 'When') :- !.
atom_capitalize(how, 'How') :- !.

atom_capitalize(can, 'Can') :- !.
atom_capitalize(must, 'Must') :- !.
atom_capitalize(should, 'Should') :- !.
atom_capitalize(may, 'May') :- !.

atom_capitalize(he, 'He') :- !.
atom_capitalize(his, 'His') :- !.
atom_capitalize(she, 'She') :- !.
atom_capitalize(her, 'Her') :- !.
atom_capitalize(they, 'They') :- !.
atom_capitalize(their, 'Their') :- !.
atom_capitalize(its, 'Its') :- !.

atom_capitalize(Token, Token).


%% ace_niceace(+TokenListIn:list, -TokenListOut:list) is det.
%
% @param TokenListIn is a list of ACE tokens
% @param TokenListOut is a list of ACE tokens
%
% @tbd Some of these transformations (e.g. a -> an) should
% be optional.

ace_niceace([], []) :-
	!.

ace_niceace(In, Out) :-
	ace_merge(In, Prefix, Rest),
	simple_append(Prefix, RestOut, Out),
	ace_niceace(Rest, RestOut).


%% ace_merge(+TokenList:list, -Prefix:list, -NewTokenList:list) is nondet.
%
% @param TokenList is a list of ACE tokens
% @param Prefix is a list of ACE tokens
% @param NewTokenList is a list of ACE tokens
%
ace_merge([Tok1, Tok2 | Rest], [Tok1Tok2], Rest) :-
	pronoun_split(Tok1Tok2, (Tok1, Tok2)),
	!.

ace_merge([^ | Rest], [], Rest) :-
	!.

ace_merge([a, Prefix, ':', Token | Rest], [Article], [Prefix, ':', Token | Rest]) :-
    member(Prefix, [n, a, unknowncat]),
	!,
	word_article(Token, Article).

ace_merge([a, Token | Rest], [Article], [Token | Rest]) :-
	Token \= ':',
	!,
	word_article(Token, Article).

ace_merge([Token, SentenceEndSym | Rest], [TokenPeriod], Rest) :-
	is_sentence_end_symbol(SentenceEndSym),
	!,
	concat_atom([Token, SentenceEndSym], TokenPeriod).

ace_merge([Token, ',' | Rest], [TokenComma], Rest) :-
	!,
	concat_atom([Token, ','], TokenComma).

ace_merge([Prefix, ':', Token | Rest], [PrefixToken], Rest) :-
	member(Prefix, [n, v, p, a]),
	!,
	concat_atom([Prefix, ':', Token], PrefixToken).

ace_merge([unknowncat, ':', Token | Rest], [Token], Rest) :-
	!.

ace_merge([Token | Rest], [Token], Rest).


%% simple_append(?List1:list, ?List2:list, ?List3:list) is nondet.
%
% @param List1 is an empty list or a list of one element
% @param List2 is a list
% @param List3 is a list
%
% This is a special case of append/2
%
simple_append([], List, List).
simple_append([X], List, [X | List]).


%% word_article(+Word:atom, -Article:atom) is det.
%
% This code decides on the article (of the noun phrase)
% on the basis of a word (either adjective or noun).
%
% See also: <http://en.wikipedia.org/wiki/A_and_an>
%
% @param Word is an ACE token
% @param Article is an ACE indefinite article, one of {a, an}
%
word_article(Word, an) :-
	downcase_atom(Word, DowncaseWord),
	atom_chars(DowncaseWord, WordChars),
	good_an_letters(WordChars),
	\+ bad_an_letters(WordChars),
	!.

word_article(_, a).


%% good_an_letters(?LetterList:list) is nondet.
%
% @param LetterList is a list of letters that a word following 'an' can consist of
%
good_an_letters([a | _]).
good_an_letters([e | _]).
good_an_letters([i | _]).
good_an_letters([o | _]).
good_an_letters([u | _]).
good_an_letters([h, o, n, o, r, a, b, l, e | _]).
good_an_letters([h, e, i, r | _]).
good_an_letters([h, o, u, r | _]).

good_an_letters([f]).
good_an_letters([h]).
good_an_letters([l]).
good_an_letters([m]).
good_an_letters([n]).
good_an_letters([r]).
good_an_letters([s]).
good_an_letters([x]).

good_an_letters([f, '-' | _]).
good_an_letters([h, '-' | _]).
good_an_letters([l, '-' | _]).
good_an_letters([m, '-' | _]).
good_an_letters([n, '-' | _]).
good_an_letters([r, '-' | _]).
good_an_letters([s, '-' | _]).
good_an_letters([x, '-' | _]).


%% bad_an_letters(?LetterList:list) is nondet.
%
% @param LetterList is a list of letters that a word following 'an' cannot consist of
%
bad_an_letters([u]).
bad_an_letters([u, '-' | _]).
bad_an_letters([u, r, i | _]).
bad_an_letters([u, t, i | _]).
bad_an_letters([u, n, i | _]).
bad_an_letters([u, s, a | _]).
bad_an_letters([u, s, e | _]).
%bad_an_letters([u, k, '-' | _]).
bad_an_letters([u, k | _]).
bad_an_letters([o, n, e | _]).
