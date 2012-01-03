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


:- module(drs_reverse, [drs_reverse/2]).

:- use_module(drs_ops).

/** <module> Reverse a DRS

@author Kaarel Kaljurand
@author Tobias Kuhn
@version 2008-05-13

*/


%% drs_reverse(+Drs:term, -DrsReversed:term) is det.
%
% Reverses all domains (lists of referents) and condition lists
% of the input DRS.
%
% @param Drs is an Attempto DRS
% @param DrsReversed is the DRS with its domain and conditions reversed
%
drs_reverse(drs([], []), drs([], [])) :- !.

drs_reverse(drs(Dom, Conds), drs(DomR, CondsR)) :-
	reverse(Dom, DomR),
	reverse(Conds, Conds1),
	conds_reverse(Conds1, CondsR).


%% conds_reverse(+Conditions:list, -ConditionsReversed:list) is det.
%
% Reverses each condition in a list of conditions.
%
% @param Conditions is a list of conditions
% @param ConditionsReversed is a list of conditions reversed
%
conds_reverse([], []).

conds_reverse([First | Conds], [FirstR | CondsR]) :-
	condition_reverse(First, FirstR),
	conds_reverse(Conds, CondsR).


%% condition_reverse(+Condition:term, -ConditionReversed:term) is det.
%
% If the input is a complex condition then reverses its component DRSs.
% If the input is an atomic condition then does nothing.
%
% @param Condition is a DRS condition
% @param ConditionReversed is the condition reversed
%
condition_reverse(Drs, DrsR) :-
	Drs =.. [Op, SubDrs],
	unary_drs_operator(Op),
	drs_reverse(SubDrs, SubDrsR),
	!,
	DrsR =.. [Op, SubDrsR].

condition_reverse(Drs, DrsR) :-
	Drs =.. [Op, SubDrs1, SubDrs2],
	binary_drs_operator(Op),
	drs_reverse(SubDrs1, SubDrs1R),
	drs_reverse(SubDrs2, SubDrs2R),
	!,
	DrsR =.. [Op, SubDrs1R, SubDrs2R].

condition_reverse([H|T], R) :-
	reverse([H|T], R1),
	conds_reverse(R1, R),
	!.

condition_reverse(Label:Drs, Label:DrsR) :-
	drs_reverse(Drs, DrsR),
	!.

condition_reverse(Cond, Cond).
