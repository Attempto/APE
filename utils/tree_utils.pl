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


:- module(tree_utils, [
		unsplit_pronouns_in_tree/2,  % +TreeIn, -TreeOut
		remove_gaps_in_tree/2,       % +TreeIn, -TreeOut
		unify_coords_in_tree/2       % +TreeIn, -TreeOut
	]).

/** <module> Tree Utils

@author Tobias Kuhn
*/

:- use_module(ace_niceace, [
		pronoun_split/2
	]).


%% unsplit_pronouns_in_tree(+TreeIn, -TreeOut)

unsplit_pronouns_in_tree([], []).

unsplit_pronouns_in_tree([np, [det, T1], [nbar, [n, T2]|RestIn]], [np, [pn, T]|RestOut]) :-
    pronoun_split(T, (T1, T2)),
    !,
    unsplit_pronouns_in_tree(RestIn, RestOut).

unsplit_pronouns_in_tree([HeadIn|TailIn], [HeadOut|TailOut]) :-
    !,
    unsplit_pronouns_in_tree(HeadIn, HeadOut),
    unsplit_pronouns_in_tree(TailIn, TailOut).

unsplit_pronouns_in_tree(Term, Term).


%% remove_gaps_in_tree(+TreeIn, -TreeOut)

remove_gaps_in_tree([], []).

remove_gaps_in_tree([HeadIn|TailIn], TailOut) :-
    empty_branch(HeadIn),
    !,
    remove_gaps_in_tree(TailIn, TailOut).

remove_gaps_in_tree([HeadIn|TailIn], [HeadOut|TailOut]) :-
    !,
    remove_gaps_in_tree(HeadIn, HeadOut),
    remove_gaps_in_tree(TailIn, TailOut).

remove_gaps_in_tree(Term, Term).


%% empty_branch(+Branch)

empty_branch([]).

empty_branch([_|Tail]) :-
    empty_branches(Tail).


%% empty_branches(+BranchList)

empty_branches([]).

empty_branches([Head|Tail]) :-
    empty_branch(Head),
    empty_branches(Tail).


%% unify_coords_in_tree(TreeIn, -TreeOut)

unify_coords_in_tree(Leaf, Leaf) :-
    atomic(Leaf).

unify_coords_in_tree(TreeIn, TreeOut) :-
    TreeIn = [Node, VP1, Coord, [Node|VP2Rest]],
    member(Node, [np_coord, vp_coord, s_coord, ap_coord]),
    !,
    unify_coords_in_tree(VP1, VP1T),
    unify_coords_in_tree([Node|VP2Rest], [Node|VP2RestT]),
    TreeOut = [Node, VP1T, Coord|VP2RestT].

unify_coords_in_tree([Node|ChildrenIn], [Node|ChildrenOut]) :-
    atom(Node),
    !,
    unify_coords_in_tree(ChildrenIn, ChildrenOut).

unify_coords_in_tree([FirstIn|RestIn], [FirstOut|RestOut]) :-
    unify_coords_in_tree(FirstIn, FirstOut),
    unify_coords_in_tree(RestIn, RestOut).
