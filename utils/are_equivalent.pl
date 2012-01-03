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


:- module(are_equivalent, [
		check_equivalence/2,
		are_equivalent/2,
		are_equivalent/3
	]).


/** <module> Equivalence checking of two Attempto DRSs

@author Kaarel Kaljurand
@version 2011-07-21

BUG: Think if this code is correct and complete.

OBSOLETE COMMENT: Note that the DRACE-tester also uses the notion of equivalence defined here,
but for DRACE it is sometimes too strict,
e.g. DRACE NP considers reversing the arguments of `be' equivalence-preserving,
to be able to perform the transformation:

==
John is every man. --> Every man is John.
==

But given the definition of equivalence here, they are not.
Similarly:

==
If there is a man then it is necessary that a human is the man. --> Every man must be a human.
==
*/


:- use_module(drs_to_ascii).
:- use_module(is_wellformed).
:- use_module(drs_ops).

:- dynamic conds_unify/2.


%% check_equivalence(+Drs1:term, +Drs2:term) is det.
%
% @param Drs1 is an Attempto DRS
% @param Drs2 is an Attempto DRS
%
check_equivalence(Drs1, Drs2) :-
	are_equivalent(Drs1, Drs2),
	write('OK: the DRSs are equivalent.'), nl,
	!.

check_equivalence(Drs1, Drs2) :-
	write('ERROR: the DRSs are NOT equivalent.'), nl,
	display_drs(Drs1),
	display_drs(Drs2).


%% are_equivalent(+Drs1:term, +Drs2:term) is det.
%% are_equivalent(+Drs1:term, +Drs2:term, +Options:list) is det.
%% are_equivalent_x(+Drs1:term, +Drs2:term) is det.
%
% @param Drs1 is an Attempto DRS
% @param Drs2 is an Attempto DRS
% @param Options is a list of options
%
% The DRSs are equivalent iff they are well-formed and
% every condition list of Drs1 is a subset of some condition list
% of Drs2, and the other way around.
%
% Valid options are:
%
% * ignore_sid(Bool)
%   If =|true|= then the sentence IDs are ignored during comparison,
%   otherwise (default) the sentence IDs are taken into consideration.
%
are_equivalent(Drs1, Drs2) :-
	are_equivalent(Drs1, Drs2, []).

are_equivalent(Drs1, Drs2, []) :-
	are_equivalent(Drs1, Drs2, [ignore_sid(false)]).

are_equivalent(Drs1, Drs2, [ignore_sid(false)]) :-
	retractall(conds_unify(_, _)),
	assert(conds_unify(C-Id, C-Id)),
	assert(conds_unify(C-SId/_, C-SId)),
	assert(conds_unify(C-SId, C-SId/_)),
	!,
	are_equivalent_x(Drs1, Drs2).

are_equivalent(Drs1, Drs2, [ignore_sid(true)]) :-
	retractall(conds_unify(_, _)),
	assert(conds_unify(C-_, C-_)),
	!,
	are_equivalent_x(Drs1, Drs2).

are_equivalent_x(Drs1, Drs2) :-
	is_wellformed(Drs1),
	is_wellformed(Drs2),
	is_subdrs_of(Drs1, Drs2),
	is_subdrs_of(Drs2, Drs1).


%% is_subdrs_of(+Drs1:term, +Drs2:term) is det.
%% is_subdrs_of_x(+Drs1:term, +Drs2:term) is det.
%
% @param Drs1 is an Attempto DRS
% @param Drs2 is an Attempto DRS
%
% Both DRSs are first copied to protect the original variables.
% The 2nd DRS is then numbervared. It is then checked if all the
% conditions of the 1st DRS occur (i.e. can be unified with some condition)
% in the 2nd DRS.
%
is_subdrs_of(drs(_, Conds1), drs(_, Conds2)) :-
	copy_term(Conds1, Conds1Copy),
	copy_term(Conds2, Conds2Copy),
	numbervars(Conds2Copy, 0, _),
	is_subdrs_of_x(drs(_, Conds1Copy), drs(_, Conds2Copy)).

is_subdrs_of_x(drs(_, Conds1), drs(_, Conds2)) :-
	is_subcl_of(Conds1, Conds2).


%% is_subcl_of(+ConditionList1:list, +ConditionList2:list) is det.
%
% @param ConditionList1 is a list of DRS conditions
% @param ConditionList2 is a list of DRS conditions
%
% eq:is_subcl_of([a(X)-1, b(Y)-1, c(X,Y)-1], [a(y)-1, b(x)-1, c(y,x)-1]).
% NOT eq:is_subcl_of([a(X)-1, b(Y)-1, c(X,Y)-1], [a(y)-1, b(x)-1, c(x,x)-1]).
%
is_subcl_of([], _).

is_subcl_of([Condition | ConditionList1], ConditionList2) :-
	member_of_conds(Condition, ConditionList2),
	is_subcl_of(ConditionList1, ConditionList2).


%% member_of_conds(+Condition:term, +ConditionList:list) is det.
%
% @param Condition is a DRS condition
% @param ConditionList is a list of DRS conditions
%
% BUG: it is probably better to use select/3 in order not
% to match the same condition more than once.
%
member_of_conds([H1|T1], [[H2|T2] | _]) :-
	is_subdrs_of_x(drs([],[H1|T1]), drs([],[H2|T2])).

member_of_conds(Drs1, [Drs2 | _]) :-
	Drs1 =.. [Op, SubDrs1],
	Drs2 =.. [Op, SubDrs2],
	unary_drs_operator(Op),
	is_subdrs_of_x(SubDrs1, SubDrs2).

% Experimental: allow (binary) disjunctions to match if they match
% after reordering of the elements, e.g. the DRS of "John drinks or eats."
% is equivalent to the DRS of "John eats or drinks."
member_of_conds(v(SubDrs1a, SubDrs1b), [v(SubDrs2a, SubDrs2b) | _]) :-
	is_subdrs_of_x(SubDrs1a, SubDrs2b), is_subdrs_of_x(SubDrs1b, SubDrs2a).

member_of_conds(Drs1, [Drs2 | _]) :-
	Drs1 =.. [Op, SubDrs1a, SubDrs1b],
	Drs2 =.. [Op, SubDrs2a, SubDrs2b],
	binary_drs_operator(Op),
	is_subdrs_of_x(SubDrs1a, SubDrs2a),
	is_subdrs_of_x(SubDrs1b, SubDrs2b).

member_of_conds(Label:Drs1, [Label:Drs2 | _]) :-
	is_subdrs_of_x(Drs1, Drs2).

member_of_conds(Cond1, [Cond2 | _]) :-
	conds_unify(Cond1, Cond2).

member_of_conds(Cond, [_ | CondsTail]) :-
	member_of_conds(Cond, CondsTail).
