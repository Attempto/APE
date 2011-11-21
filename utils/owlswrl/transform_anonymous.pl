% This file is part of the Attempto Parsing Engine (APE).
% Copyright 2008, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
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


:- module(transform_anonymous, [
		transform_anonymous/2   % +TermIn, -TermOut
	]).

/** <module> Transform Anonymous Individuals

@author Tobias Kuhn
@version 2007-10-29
*/


%% transform_anonymous(+TermIn, -TermOut)

transform_anonymous(TermIn, TermOut) :-
	transform_anonymous(TermIn, TermOut, [], _).


transform_anonymous(Var, Var, Map, Map) :-
	var(Var),
	!.

transform_anonymous(nodeID('$VAR'(LocalID)), nodeID(GlobalID), MapIn, MapOut) :-
	!,
	( member((LocalID, GID), MapIn) ->
		GlobalID = GID,
		MapOut = MapIn
	;
		GlobalID is random(1000000000000000000),
		MapOut = [(LocalID, GlobalID)|MapIn]
	).

transform_anonymous([], [], Map, Map) :-
	!.

transform_anonymous([H1|T1], [H2|T2], MapIn, MapOut) :-
	!,
	transform_anonymous(H1, H2, MapIn, MapTemp),
	transform_anonymous(T1, T2, MapTemp, MapOut).

transform_anonymous(Term, Term, Map, Map) :-
	Term =.. [Term],
	!.

transform_anonymous(Term1, Term2, MapIn, MapOut) :-
	!,
	Term1 =.. List1,
	transform_anonymous(List1, List2, MapIn, MapOut),
	Term2 =.. List2.
