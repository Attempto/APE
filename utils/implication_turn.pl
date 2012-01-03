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


/** <module> Turning the DRS

Supporting module for the NPACE verbalizer.
Switches the arguments of some transitive predicates found
in implications.

@author Kaarel Kaljurand
@version 2008-03-16

TODO:

* optimize, e.g. select/3 of object/8 should be done just once,
only predicate/5 (and predicate/4 and DRSs too) needs backtracking
*/

:- module(implication_turn, [
		implication_turn/2
	]).


% Operators used in the DRS.
:- op(400, fx, -).
:- op(400, fx, ~).
:- op(500, xfx, =>).
:- op(550, xfx, v).

%:- debug(verbose).


%% implication_turn(+Implication:term, -ImplicationTurned:term) is undet.
%
% @param Implication is DRS condition in the form =|Left => Right|=
% @param ImplicationTurned is DRS condition in the form =|Left => Right|=
%
implication_turn(Left => Right, ImplicationTurned) :-
	get_pred(0, _, Left => Right, _, ImplicationTurned).


%% drs_turn(+Level:functor, +Subject:functor, +CondsIn:list, -CondsTurned:list) is undet.
%
%

drs_turn(_, S:S, [], _, []).

drs_turn(Level, In:Out, Conds, Subj, [C | NewNewConds]) :-
	debug(verbose, 'turning DRS: ~w~n', [Conds]),
	select_predicate(Level, In:Tmp, Subj, Conds, C, NewConds),
	drs_turn(Level, Tmp:Out, NewConds, Subj, NewNewConds).


%% select_predicate(Level, In:Out, Conds, C, RConds)
%
%
select_predicate(Level, In:Out, Subj, Conds, C, RConds) :-
	select(Cond, Conds, RConds),
	debug(verbose, 'selected condition: ~w~n', [Cond]),
	debug(verbose, 'remaining conditions: ~w~n', [RConds]),
	get_pred(Level, In:Out, Cond, Subj, C).


%% get_pred(Level, SubjectIn:SubjectOut, Cond, NewCond)
%
%
get_pred(0, _, Drs1 => Drs2, _, [object(S, A, B, C, D, E)-Id | Drs1R] => Drs2R) :-
	!,
	select(object(S, A, B, C, D, E)-Id, Drs1, Drs1Out),
	drs_turn(l(0), S:_, Drs1Out, S, Drs1R),
	drs_turn(l(0), S:_, Drs2, S, Drs2R),
	debug(verbose, 'shared subj: ~w~n', [S]).

get_pred(0, _, Drs1 v Drs2, _, Drs1R v Drs2R) :-
	!,
	drs_turn(l(0), S:_, Drs1, _, Drs1R),
	drs_turn(l(0), S:_, Drs2, _, Drs2R).

get_pred(0, _, -Drs, _, -DrsR) :-
	!,
	drs_turn(l(0), _, Drs, _, DrsR).

get_pred(0, _, ~Drs, _, ~DrsR) :-
	!,
	drs_turn(l(0), _, Drs, _, DrsR).

get_pred(0, _, can(Drs), _, can(DrsR)) :-
	!,
	drs_turn(l(0), _, Drs, _, DrsR).

get_pred(0, _, must(Drs), _, must(DrsR)) :-
	!,
	drs_turn(l(0), _, Drs, _, DrsR).

get_pred(0, _, should(Drs), _, should(DrsR)) :-
	!,
	drs_turn(l(0), _, Drs, _, DrsR).

get_pred(0, _, may(Drs), _, may(DrsR)) :-
	!,
	drs_turn(l(0), _, Drs, _, DrsR).

get_pred(0, _, question(Drs), _, question(DrsR)) :-
	!,
	drs_turn(l(0), _, Drs, _, DrsR).

get_pred(0, _, command(Drs), _, command(DrsR)) :-
	!,
	drs_turn(l(0), _, Drs, _, DrsR).

% Simple toplevel conditions.
get_pred(0, _, Condition-Id, _, Condition-Id).



get_pred(l(_), _:_, _ => _, _, _=> _) :-
	!,
	fail.

get_pred(l(Level), S:S, Drs1 v Drs2, Subject, Drs1R v Drs2R) :-
	!,
	drs_turn(l(l(Level)), S:_, Drs1, Subject, Drs1R),
	drs_turn(l(l(Level)), S:_, Drs2, Subject, Drs2R).

get_pred(l(Level), S:S, -Drs, Subject, -DrsR) :-
	!,
	drs_turn(l(l(Level)), S:_, Drs, Subject, DrsR).

get_pred(l(Level), S:S, ~Drs, Subject, ~DrsR) :-
	!,
	drs_turn(l(l(Level)), S:_, Drs, Subject, DrsR).

get_pred(l(Level), S:S, can(Drs), Subject, can(DrsR)) :-
	!,
	drs_turn(l(l(Level)), S:_, Drs, Subject, DrsR).

get_pred(l(Level), S:S, must(Drs), Subject, must(DrsR)) :-
	!,
	drs_turn(l(l(Level)), S:_, Drs, Subject, DrsR).

get_pred(l(Level), S:S, should(Drs), Subject, should(DrsR)) :-
	!,
	drs_turn(l(l(Level)), S:_, Drs, Subject, DrsR).

get_pred(l(Level), S:S, may(Drs), Subject, may(DrsR)) :-
	!,
	drs_turn(l(l(Level)), S:_, Drs, Subject, DrsR).

get_pred(l(Level), S:S, question(Drs), Subject, question(DrsR)) :-
	!,
	drs_turn(l(l(Level)), S:_, Drs, Subject, DrsR).

get_pred(l(Level), S:S, command(Drs), Subject, command(DrsR)) :-
	!,
	drs_turn(l(l(Level)), S:_, Drs, Subject, DrsR).



% Handling transitive verbs.
get_pred(l(_), S:S, predicate(Ref, Verb, S, O)-Id, _, predicate(Ref, Verb, S, O)-Id) :-
	debug(verbose, 'turning1: ~w :: ~w :: ~w~n', [S, Verb, S-O]).

get_pred(l(_), S:S, predicate(Ref, Verb, O, S)-Id, _, predicate(Ref, i(Verb), S, O)-Id) :-
	Verb \= be,
	debug(verbose, 'turning2: ~w :: ~w~n', [S, Verb, O-S]).

get_pred(l(_), S:O, predicate(Ref, Verb, S, O)-Id, _, predicate(Ref, Verb, S, O)-Id) :-
	debug(verbose, 'turning3: ~w :: ~w~n', [O, Verb, S-O]).

get_pred(l(_), S:O, predicate(Ref, Verb, O, S)-Id, _, predicate(Ref, i(Verb), S, O)-Id) :-
	Verb \= be,
	debug(verbose, 'turning4: ~w :: ~w~n', [O, Verb, O-S]).


% Handling intransitive verbs.
get_pred(l(_), S:S, predicate(Ref, Verb, S)-Id, _, predicate(Ref, Verb, S)-Id) :-
	debug(verbose, 'intr subj: ~w~n', [Verb-S]).



% Handling transitive verbs.
get_pred(l(_), _:S, predicate(Ref, Verb, S, O)-Id, S, predicate(Ref, Verb, S, O)-Id) :-
	debug(verbose, 'turning1: ~w :: ~w :: ~w~n', [S, Verb, S-O]).

get_pred(l(_), _:S, predicate(Ref, Verb, O, S)-Id, S, predicate(Ref, i(Verb), S, O)-Id) :-
	Verb \= be,
	debug(verbose, 'turning2: ~w :: ~w~n', [S, Verb, O-S]).

get_pred(l(_), _:O, predicate(Ref, Verb, S, O)-Id, S, predicate(Ref, Verb, S, O)-Id) :-
	debug(verbose, 'turning3: ~w :: ~w~n', [O, Verb, S-O]).

get_pred(l(_), _:O, predicate(Ref, Verb, O, S)-Id, S, predicate(Ref, i(Verb), S, O)-Id) :-
	Verb \= be,
	debug(verbose, 'turning4: ~w :: ~w~n', [O, Verb, O-S]).


% Handling intransitive verbs.
get_pred(l(_), _:S, predicate(Ref, Verb, S)-Id, S, predicate(Ref, Verb, S)-Id) :-
	debug(verbose, 'intr subj: ~w~n', [Verb-S]).




% Simple deep conditions.
get_pred(l(_), S:S, Cond-Id, _, Cond-Id) :-
	Cond \= predicate(_, _, _),
	Cond \= predicate(_, _, _, _).
