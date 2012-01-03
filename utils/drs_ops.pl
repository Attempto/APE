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


:- module(drs_ops, [
		drs_operator/1,
		unary_drs_operator/1,
		binary_drs_operator/1
	]).

/** <module> DRS Operators

@author Tobias Kuhn
*/


%% drs_operator(?DRSOperator)

drs_operator(DRSOperator) :-
	(
		unary_drs_operator(DRSOperator)
	;
		binary_drs_operator(DRSOperator)
	).


%% unary_drs_operator(?DRSOperator)

unary_drs_operator(-).
unary_drs_operator(~).
unary_drs_operator(can).
unary_drs_operator(must).
unary_drs_operator(should).
unary_drs_operator(may).
unary_drs_operator(question).
unary_drs_operator(command).


%% binary_drs_operator(?DRSOperator)

binary_drs_operator(=>).
binary_drs_operator(v).

