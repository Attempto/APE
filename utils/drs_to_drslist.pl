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

:- module(drs_to_drslist, [
		drs_to_drslist/2
	]).


:- use_module(drs_ops, [
		drs_operator/1
	]).

/** <module> DRS to DRS lists converter

This module provides predicates to split a given DRS into
a list of DRSs, such that if the elements of the resulting list
are merged (by concatenating the domains and the condition lists
of its DRSs),
then the resulting DRS is structurally equivalent to the original.
The DRSs in the resulting list do not share discourse referents.

@author Kaarel Kaljurand
@version 2009-06-03

*/


%% drs_to_drslist(+Drs:drs, -DrsList:list) is det.
%
% @param Drs is an Attempto DRS
% @param DrsList is a list of Attempto DRSs
%

drs_to_drslist(drs(_, CondList), DrsListSorted) :-
	termlist_to_termlistlist(CondList, CondListList),
	maplist(condlist_to_drs, CondListList, DrsList),
	sort_drslist(DrsList, DrsListSorted).


condlist_to_drs(CondList, drs(Dom, CondList)) :-
	get_toplevel_conds(CondList, ToplevelCondList),
	term_variables(ToplevelCondList, Dom).


%% get_toplevel_conds(+CondsIn:list, -CondsOut:list)
%
% Extracts from the given list of DRS conditions only
% those conditions that contribute discourse referents
% to the toplevel domain. Such conditions are simple
% toplevel conditions, but also simple conditions in the lists.
% For example, in the DRS of the sentence
%
% Less than 3 dogs hate less than 3 cats.
%
% all discourse referents are toplevel.
%
get_toplevel_conds([], []).

get_toplevel_conds([Cond-Id | Conds], [Cond-Id | SelectedConds]) :-
	!,
	get_toplevel_conds(Conds, SelectedConds).

get_toplevel_conds([[H | T] | Conds], SelectedConds) :-
	!,
	get_toplevel_conds([H | T], SelectedConds1),
	get_toplevel_conds(Conds, SelectedConds2),
	append(SelectedConds1, SelectedConds2, SelectedConds).

get_toplevel_conds([_ComplexCond | Conds], SelectedConds) :-
	get_toplevel_conds(Conds, SelectedConds).


termlist_to_termlistlist(TermList, TermListList) :-
	termlist_to_termlistlist(TermList, [], TermListList).

termlist_to_termlistlist([], Out, Out).

termlist_to_termlistlist([Term | TermList], TermListList, Out) :-
	(
		term_and_termlist_share_variables(Term, TermListList, Included, Excluded)
	->
		termlist_to_termlistlist(TermList, [[Term | Included] | Excluded], Out)
	;
		termlist_to_termlistlist(TermList, [[Term] | TermListList], Out)
	).


term_and_termlist_share_variables(Term, TermListList, IncludedFlat, Excluded) :-
	partition(terms_share_variables(Term), TermListList, Included, Excluded),
	append(Included, IncludedFlat).

terms_share_variables(Term1, Term2) :-
	term_variables(Term1, List1),
	term_variables(Term2, List2),
	exists_intersection(List1, List2).

exists_intersection(List1, List2) :-
	member(El1, List1),
	member(El2, List2),
	El1 == El2,
	!.


%% sort_drslist(+DrsList:list, -DrsListSorted:list) is det.
%
% Sorts the list of DRSs. Sorting is done by keysort/2, and
% the keys are assigned by assign_key/2. See the definition
% of assign_key/2 to find out how the soring order is determined.
%
%
sort_drslist([], []) :- !.

sort_drslist([Drs], [Drs]) :- !.

sort_drslist(DrsList, DrsListSorted) :-
	maplist(assign_key, DrsList, DrsListWithKeys),
	keysort(DrsListWithKeys, DrsListWithKeysSorted),
	maplist(remove_key, DrsListWithKeysSorted, DrsListSorted).


%% remove_key(+DrsWithKey:term, -Drs:term)
%
remove_key(_-Drs, Drs).


%% assign_key(+Drs:term, -DrsWithKey:term)
%
% The key of a DRS is the smallest (in the standard order of terms)
% sentence ID and token ID combination (SId/TId) of a DRS condition.
% The SId/TId of a complex condition is determined recursively.
% Note that we do not check the 2nd DRS of complex binary conditions
% (e.g. the then-part of an implication), as there the keys are known
% to be larger.
%
assign_key(drs(Dom, Conds), Key-drs(Dom, Conds)) :-
	assign_key_to_conds(Conds, Key).


assign_key_to_conds(Conds, Key) :-
	maplist(get_key, Conds, Keys),
	sort(Keys, [Key | _]).


get_key(_-Key, Key) :- !.

get_key(Cond, Key) :-
	functor(Cond, Op, _),
	drs_operator(Op),
	arg(1, Cond, Drs),
	assign_key(Drs, Key-_).

get_key(_Label:Drs, Key) :-
	assign_key(Drs, Key-_).

get_key([H | T], Key) :-
	assign_key_to_conds([H | T], Key).
