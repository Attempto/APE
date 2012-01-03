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


:- module(drs_to_sdrs, [drs_to_sdrs/2]).

:- use_module(drs_ops).

/** <module> DRS simplifier

Converts the DRS into a simplified form where every
DRS-box is represented as a list of conditions, i.e.

==
drs(Dom, Conds) --> Conds
==

where Conds is a list of conditions (with sentence IDs).

@author Kaarel Kaljurand
@author Tobias Kuhn
@version 2009-05-13

*/


%% drs_to_sdrs(+Drs:drs, -SDrs:sdrs) is det.
%
% Removes the domain-argument from all the (non-atomic) DRS conditions,
% including the embedded ones.
%
% @param Drs is an Attempto DRS
% @param SDrs is an Attempto DRS with the domain-arguments removed

drs_to_sdrs(drs(_, Conds), SConds) :-
	conds_sconds(Conds, SConds).


conds_sconds([], []).

conds_sconds([Cond | Tail], [SCond | STail]) :-
	simplify(Cond, SCond),
	!,
	conds_sconds(Tail, STail).


simplify(DRS, SDRS) :-
	DRS =.. [Op, drs(_, Conds)],
	unary_drs_operator(Op),
	conds_sconds(Conds, SConds),
	SDRS =.. [Op, SConds].

simplify(DRS, SDRS) :-
	DRS =.. [Op, drs(_, Conds1), drs(_, Conds2)],
	binary_drs_operator(Op),
	conds_sconds(Conds1, SConds1),
	conds_sconds(Conds2, SConds2),
	SDRS =.. [Op, SConds1, SConds2].

simplify(Conds, (SConds)) :-
	is_list(Conds),
	conds_sconds(Conds, SConds).

simplify(Label:drs(_, Conds), Label:SConds) :-
	conds_sconds(Conds, SConds).

simplify(Cond, Cond).
