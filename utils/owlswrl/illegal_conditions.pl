% This file is part of the Attempto Parsing Engine (APE).
% Copyright 2008-2010, Kaarel Kaljurand <kaljurand@gmail.com>.
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

:- module(illegal_conditions, [
		illegal_conditions/1,
		illegal_condition/1
	]).

/** <module> DRS checker for DRS-to-OWL/SWRL

Setting some error messages for conditions which ACE->OWL/SWRL cannot handle.

@author Kaarel Kaljurand
@version 2010-12-14

TODO

* add more detailed messages in case of property/[4,5,7]

* paraphrase the DRS to deliver a nicer error message.
DRS->ACE needs to support this though (i.e. the situation when the
the DRS contains referents which are defined on the upper level).

* Should error messages be given on the ACE level or on the DRS level?
For the end user the ACE level is better, but on the other hand the input
to ACE->OWL is the DRS (i.e. the module is actually doing DRS->OWL).

*/

:- use_module(ape('logger/error_logger'), [
		add_error_message/4
	]).

:- use_module(ape('utils/drs_ops'), [
		unary_drs_operator/1,
		binary_drs_operator/1
	]).


% Operators used in the DRS.
:- op(400, fx, -).
:- op(500, xfx, v).
:- op(500, xfx, =>).


%% illegal_conditions(+Conditions:list) is det.
%
%
illegal_conditions([]).

illegal_conditions([Cond | CondList]) :-
	illegal_condition(Cond),
	!,
	illegal_conditions(CondList).


%% illegal_condition(BoxId, Cond, NewCond) is det.
%
%

illegal_condition(relation(_, of, _)-SId/TId) :-
	add_error_message(owl, SId-TId, of, 'Possessive constructions not supported (in this particular case).').

illegal_condition(modifier_adv(_, Adverb, _)-SId/TId) :-
	add_error_message(owl, SId-TId, Adverb, 'Adverbs not supported.').

illegal_condition(modifier_pp(_, Preposition, _)-SId/TId) :-
	add_error_message(owl, SId-TId, Preposition, 'Prepositional phrases not supported.').

% property/4 (comp adjective)
illegal_condition(property(_, Adjective, comp)-SId/TId) :-
	add_error_message(owl, SId-TId, Adjective, 'Comparative adjective not supported.').

% property/4 (sup adjective)
illegal_condition(property(_, Adjective, sup)-SId/TId) :-
	add_error_message(owl, SId-TId, Adjective, 'Superlative adjective not supported.').

% property/5 (comp_than adjective)
illegal_condition(property(_, Adjective, _, _)-SId/TId) :-
	add_error_message(owl, SId-TId, Adjective, 'Adjective not supported.').

% property/7 (comp_than + subj/obj adjective)
illegal_condition(property(_, Adjective, _, _, _, _)-SId/TId) :-
	add_error_message(owl, SId-TId, Adjective, 'Adjective not supported.').

illegal_condition(query(_, QueryWord)-SId/TId) :-
	add_error_message(owl, SId-TId, QueryWord, 'Query not supported.').

illegal_condition(predicate(_, Verb, _)-SId/TId) :-
	add_error_message(owl, SId-TId, Verb, 'Intransitive verbs not supported.').

illegal_condition(predicate(_, Verb, _, _, _)-SId/TId) :-
	add_error_message(owl, SId-TId, Verb, 'Ditransitive verbs not supported.').

illegal_condition(must(Drs)) :-
	conds_sid(Drs, SId),
	add_error_message(owl, SId, 'must/1', 'Necessity not supported.').

illegal_condition(can(Drs)) :-
	conds_sid(Drs, SId),
	add_error_message(owl, SId, 'can/1', 'Possibility not supported.').

illegal_condition(_:Drs) :-
	conds_sid(Drs, SId),
	add_error_message(owl, SId, ':/2', 'Sentence subordination not supported.').

illegal_condition('~'(Drs)) :-
	conds_sid(Drs, SId),
	add_error_message(owl, SId, '~/1', 'Negation-as-failure not supported.').

illegal_condition(should(Drs)) :-
	conds_sid(Drs, SId),
	add_error_message(owl, SId, 'should/1', 'Recommendation not supported.').

illegal_condition(may(Drs)) :-
	conds_sid(Drs, SId),
	add_error_message(owl, SId, 'may/1', 'Admissibility not supported.').

% Note: do not do anything, NP conjunction ('na' as 2nd argument) is handled by the object-rule
illegal_condition(has_part(_, _)-_).

illegal_condition(predicate(_, Value, _, _)-SId/TId) :-
	add_error_message(owl, SId-TId, Value, 'Subject or object of this verb makes an illegal reference.').

illegal_condition(object(_, na, _, na, _, _)-SId/TId) :-
	!,
	add_error_message(owl, SId-TId, and, 'Noun phrase conjunctions not supported.').

illegal_condition(object(_, Value, _, na, _, _)-SId/TId) :-
	Value \= na,
	!,
	add_error_message(owl, SId-TId, Value, 'A reference to this noun either does not exist or is illegal.').

illegal_condition(object(_, _, _, Unit, _, _)-SId/TId) :-
	Unit \= na,
	add_error_message(owl, SId-TId, Unit, 'Measurement nouns are not supported.').


%% conds_sid(+Conditions:list, -SentenceId:integer) is det.
%
% Finds the shared sentence ID of the conditions.
%
conds_sid([], _).

conds_sid([_-SId/_TId | _], SId) :- !.

conds_sid([C | Cs], SId) :-
	cond_sid(C, SId),
	conds_sid(Cs, SId).


cond_sid(Cond, SId) :-
	functor(Cond, F, 1),
	unary_drs_operator(F),
	!,
	arg(1, Cond, Drs),
	conds_id(Drs, SId).

cond_id(_Label:Drs, SId) :-
	!,
	conds_id(Drs, SId).

cond_id(Drs1 v Drs2, SId) :-
	!,
	conds_id(Drs1, SId),
	conds_id(Drs2, SId).

cond_id(Drs1 => Drs2, SId) :-
	!,
	conds_id(Drs1, SId),
	conds_id(Drs2, SId).
