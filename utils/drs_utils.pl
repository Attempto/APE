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


:- module(drs_utils, [
		get_toplevel_object_referents/2
	]).

/** <module> Attempto DRS utils

@author Kaarel Kaljurand
@version 2009-04-08

*/

%% get_toplevel_object_referents(+ConditionList:list, -Toplevel:term) is det.
%
% Returns a term toplevel(ReferentList, SubjectList, ObjectList, NamedList) where
%
% * ReferentList is a list of top-level object-referents
% * SubjectList is a list of top-level object-referents that are used as subjects
% * ObjectList is a list of top-level object-referents that are used as objects
% * NamedList is a list of top-level object-referents that correspond to proper names (Type = named)
%
% @param ConditionList is a list of DRS conditions
% @param Toplevel is a term containing top-level DRS discourse referents
%
get_toplevel_object_referents(ConditionList, toplevel(ReferentList, SubjectList, ObjectList, NamedList)) :-
	get_toplevel_object_referents_x(
		ConditionList,
		toplevel(
			-([], ReferentList),
			-([], SubjectList),
			-([], ObjectList),
			-([], NamedList)
		)
	).


%% get_toplevel_object_referents_x(+ConditionList:list, -Toplevel:term) is det.
%
% @param ConditionList is a list of DRS conditions
% @param Toplevel is a term containing top-level DRS discourse referents
%
get_toplevel_object_referents_x([], toplevel(R-R, S-S, O-O, N-N)).

get_toplevel_object_referents_x([Condition | ConditionList], toplevel(R1-R2, S1-S2, O1-O2, N1-N2)) :-
	condition_referents(Condition, toplevel(R1-RT, S1-ST, O1-OT, N1-NT)),
	get_toplevel_object_referents_x(ConditionList, toplevel(RT-R2, ST-S2, OT-O2, NT-N2)).


%% condition_referents(+Condition:term) is det.
%
% Note: the referents of plural objects, mass-objects, query-objects, and objects in the relation-condition
% are considered as named-objects, i.e. they will not be verbalized by "there is/are".
% BUG: This is a hack, and it is DRACE specific.
%
% @param Condition is a DRS condition
%

% BUG: ???
condition_referents(object(Ref, Noun, Type, _, _Eq, na)-_, toplevel(R-[Ref | R], S-S, O-O, N-N)) :-
	Noun \= na,
	Type \= named,
	Type \= mass,
	!.

% Plural objects (i.e. NP conjunction). BUG: we consider them named-objects for the time being.
condition_referents(object(Ref, na, _, _, _, _)-_, toplevel(R-[Ref | R], S-S, O-O, N1-[Ref | N1])) :- !.

% Mass objects. BUG: we consider them objects as named-objects for the time being.
condition_referents(object(Ref, _, mass, _, _, _)-_, toplevel(R-[Ref | R], S-S, O-O, N1-[Ref | N1])) :- !.

% Parts of plural objects. BUG: we consider them named-objects for the time being.
condition_referents(has_part(_, Ref)-_, toplevel(R-R, S-S, O-O, N1-[Ref | N1])) :- !.

% Relation object. BUG: we consider them named-objects for the time being.
condition_referents(relation(Ref1, of, Ref2)-_, toplevel(R-R, S-S, O-O, N-[Ref1, Ref2 | N])) :- !.

% Query objects. BUG: we consider them named-objects for the time being.
condition_referents(query(Ref, _)-_, toplevel(R-[Ref | R], S-S, O-O, N1-[Ref | N1])) :- !.

/* BUG: is the var/nonvar stuff really necessary? */

condition_referents(predicate(_, _, Ref)-_, toplevel(R-R, S-[Ref | S], O-O, N-N)) :-
	var(Ref),
	!.

condition_referents(predicate(_, _, Ref, O1)-_, toplevel(R-R, S-[Ref | S], O-[O1 | O], N-N)) :-
	var(Ref), var(O1),
	!.

condition_referents(predicate(_, _, Ref, O1)-_, toplevel(R-R, S-[Ref | S], O-O, N-N)) :-
	var(Ref), nonvar(O1),
	!.

condition_referents(predicate(_, _, Ref, O1)-_, toplevel(R-R, S-S, O-[O1 | O], N-N)) :-
	nonvar(Ref), var(O1),
	!.

condition_referents(predicate(_, _, Ref, O1, O2)-_, toplevel(R-R, S-[Ref | S], O-[O1, O2 | O], N-N)) :-
	var(Ref), var(O1), var(O2),
	!.

condition_referents(predicate(_, _, Ref, O1, O2)-_, toplevel(R-R, S-S, O-[O1, O2 | O], N-N)) :-
	nonvar(Ref), var(O1), var(O2),
	!.

condition_referents(predicate(_, _, Ref, O1, O2)-_, toplevel(R-R, S-[Ref | S], O-[O2 | O], N-N)) :-
	var(Ref), nonvar(O1), var(O2),
	!.

condition_referents(predicate(_, _, Ref, O1, O2)-_, toplevel(R-R, S-[Ref | S], O-[O1 | O], N-N)) :-
	var(Ref), var(O1), nonvar(O2),
	!.

condition_referents(predicate(_, _, Ref, O1, O2)-_, toplevel(R-R, S-S, O-[O2 | O], N-N)) :-
	nonvar(Ref), nonvar(O1), var(O2),
	!.

condition_referents(predicate(_, _, Ref, O1, O2)-_, toplevel(R-R, S-S, O-[O1 | O], N-N)) :-
	nonvar(Ref), var(O1), nonvar(O2),
	!.

condition_referents(predicate(_, _, Ref, O1, O2)-_, toplevel(R-R, S-[Ref | S], O-O, N-N)) :-
	var(Ref), nonvar(O1), nonvar(O2),
	!.

condition_referents(_, toplevel(R-R, S-S, O-O, N-N)).
