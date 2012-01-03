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


:- module(drs_to_ace, [drs_to_ace/2]).

:- use_module(drs_to_drslist, [
		drs_to_drslist/2
	]).

:- use_module(drs_to_coreace).

:- use_module(drs_to_npace).

/** <module> DRS to ACE verbalizer

Translate an Attempto DRS into Attempto Controlled English (ACE).
The result is either in Core ACE or in NP ACE.

@author Kaarel Kaljurand
@version 2009-06-03
*/


%% drs_to_ace(+Drs:drs, -Ace:list) is det.
%
% Splits the given DRS into a list of (smaller) DRSs,
% and verbalizes each split in either Core ACE or NP ACE.
%
% @param Drs is an Attempto DRS (untyped)
% @param Ace is a list of ACE sentences
%
drs_to_ace(Drs, Ace) :-
	drs_to_drslist(Drs, DrsList),
	drslist_to_ace(DrsList, Ace).


%% drslist_to_ace
%
%
drslist_to_ace([], []).

drslist_to_ace([Drs | DrsList], [Ace | AceList]) :-
	drs_to_ace_x(Drs, Ace),
	drslist_to_ace(DrsList, AceList).


% BUG: As Drace NP might take very long (due to a bug?) we add
% a timeout to fail if it takes longer than, say, 0.5 seconds.
drs_to_ace_x(Drs, Ace) :-
	catch(
		call_with_time_limit(
			0.5,
			drs_to_npace:drs_to_npace(Drs, Ace)
		),
		time_limit_exceeded,
		fail
	),
	Ace \= [],
	\+ member('ERROR', Ace),
	!.

drs_to_ace_x(Drs, Ace) :-
	drs_to_coreace:drs_to_coreace(Drs, Ace).
