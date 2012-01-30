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


:- module(is_wellformed, [
		is_wellformed/1
	]).

/** <module> Wellformedness checking of the Attempto DRS

@author Kaarel Kaljurand
@version 2010-11-22

*/

:- use_module(drs_ops, [
		unary_drs_operator/1,
		binary_drs_operator/1
	]).


%%%:- debug(verbose).

%% is_wellformed(+Drs:term) is det.
%
% Succeeds if Drs is wellformed. Wellformedness means:
%
% 1. No discourse referent is declared twice.
% 2. Every used discourse referent is declared in an accessible DRS.
% 3. Every declared discourse referent is used in the same DRS.
%
% @param Drs is an Attempto DRS
%
is_wellformed(DRS) :-
	copy_term(DRS, drs(ReferentList, ConditionList)),

	is_domain(ReferentList),
	debug(verbose, 'The top-level domain has arguments of correct type.~n', []),

	is_conditionlist(ConditionList),
	debug(verbose, 'Every condition has arguments of correct type.~n', []),

	conditionlist_allreferents(ConditionList, EmbeddedReferents),
	append(ReferentList, EmbeddedReferents, AllReferents),
	has_no_duplicates(AllReferents),
	debug(verbose, 'Every discourse referent is globally unique.~n', []),

	dr_decl(ReferentList, ConditionList),
	debug(verbose, 'Every discourse referent is declared.~n', []),

	is_dom_used(ReferentList, ConditionList),
	are_dr_used(ConditionList),
	debug(verbose, 'Every discourse referent is used in the same box.~n', []).


%% is_domain(+Referents:list) is det.
%
% @param Referents is a list of discourse referents (domain of the DRS)
%
is_domain([]).

is_domain([Referent | ReferentList]) :-
	var(Referent),
	nonvar(ReferentList),
	is_domain(ReferentList).


%% is_conditionlist(+Conditions:list) is det.
%
% Note: empty lists are not allowed.
%
% @param Conditions is a list of DRS conditions
%
is_conditionlist([Condition]) :-
	nonvar(Condition),
	is_condition(Condition).

is_conditionlist([Condition | ConditionList]) :-
	nonvar(Condition),
	is_condition(Condition),
	nonvar(ConditionList),
	is_conditionlist(ConditionList).


%% is_drs(+DRS:term) is det.
%
% @param DRS is an Attempto DRS
%
is_drs(drs(Dom, ConditionList)) :- 
	is_domain(Dom),
	is_conditionlist(ConditionList).


%% is_condition(+Condition:term) is det.
%
% Succeeds if the condition has an allowed form and arguments of an allowed type.
%
% @param Condition is an Attempto DRS condition
%
is_condition(DRS) :-
	DRS =.. [Op, SubDRS],
	unary_drs_operator(Op),
	is_drs(SubDRS).

is_condition(DRS) :-
	DRS =.. [Op, SubDRS1, SubDRS2],
	binary_drs_operator(Op),
	is_drs(SubDRS1),
	is_drs(SubDRS2).

is_condition(Conds) :-
	is_conditionlist(Conds).

is_condition(Label:DRS) :-
	var(Label),
	is_drs(DRS).

is_condition(query(R, Lemma)-Id) :-
	id(Id),
	var(R),
	nonvar(Lemma),
	member(Lemma, [who, what, which, how, where, when, howm]).

is_condition(relation(R1, of, R2)-Id) :-
	id(Id),
	var(R1),
	is_argument(R2).

is_condition(modifier_adv(R, Lemma, Degree)-Id) :-
	id(Id),
	var(R),
	atom(Degree),
	member(Degree, [pos, comp, sup]),
	nonvar(Lemma).

is_condition(modifier_pp(R1, Lemma, R2)-Id) :-
	id(Id),
	var(R1),
	nonvar(Lemma),
	is_argument(R2).

is_condition(property(R, Lemma, Comp)-Id) :-
	id(Id),
	var(R),
	atom(Comp),
	member(Comp, [pos, comp, sup]),
	nonvar(Lemma).

is_condition(property(R1, Lemma, Comp, R2)-Id) :-
	id(Id),
	var(R1),
	is_argument(R2),
	atom(Comp),
	member(Comp, [pos, pos_as, comp, comp_than, sup]),
	nonvar(Lemma).

is_condition(property(R1, Lemma, R2, Comp, SubjObj, R3)-Id) :-
	id(Id),
	var(R1),
	is_argument(R2),
	is_argument(R3),
	atom(Comp),
	member(Comp, [pos_as, comp_than]),
	atom(SubjObj),
	member(SubjObj, [subj, obj]),
	nonvar(Lemma).

is_condition(predicate(R1, Lemma, R2)-Id) :-
	id(Id),
	var(R1),
	is_argument(R2),
	nonvar(Lemma).

is_condition(predicate(R1, Lemma, R2, R3)-Id) :-
	id(Id),
	var(R1),
	is_argument(R2),
	is_argument(R3),
	nonvar(Lemma).

is_condition(predicate(R1, Lemma, R2, R3, R4)-Id) :-
	id(Id),
	var(R1),
	is_argument(R2),
	is_argument(R3),
	is_argument(R4),
	nonvar(Lemma).

is_condition(object(R, something, dom, na, na, na)-Id) :-
	id(Id),
	var(R).

is_condition(object(R, somebody, countable, na, eq, 1)-Id) :-
	id(Id),
	var(R).

is_condition(object(R, Lemma, mass, na, na, na)-Id) :-
	id(Id),
	var(R),
	nonvar(Lemma).

is_condition(object(R, Lemma, mass, na, na, na)-Id) :-
	id(Id),
	var(R),
	nonvar(Lemma).

is_condition(object(R, na, countable, na, eq, Number)-Id) :-
	id(Id),
	var(R),
	integer(Number).

is_condition(object(R, Lemma, Quantisation, Unit, Eq, Number)-Id) :-
	id(Id),
	var(R),
	nonvar(Lemma),
	atom(Quantisation), member(Quantisation, [countable, mass]),
	atom(Unit),
	atom(Eq), member(Eq, [na, eq, geq, greater, leq, less, exactly]),
	(
		integer(Number)
	;
		number(Number),
		\+ integer(Number),
		Unit \= na
	;
		Number = na
	).

is_condition(formula(R1, Symbol, R2)-Id) :-
	id(Id),
	is_argument(R1),
	is_argument(R2),
	atom(Symbol),
	member(Symbol, ['<', '>', '=', '\\=', '>=', '=<']).

is_condition(has_part(R1, R2)-Id) :-
	id(Id),
	var(R1),
	is_argument(R2).


%% is_argument(+Argument:term) is det.
%
% Checks if the argument of
% predicate, relation, property, has_part, etc. is a
% discourse referent, number, string, list, set, or an expression.
%
% @param Argument is DRS-condition argument
%
is_argument(Argument) :-
	var(Argument),
	!.

is_argument(named(Lemma)) :-
	nonvar(Lemma).

is_argument(int(Argument)) :-
	integer(Argument).

is_argument(real(Argument)) :-
	float(Argument).

is_argument(int(Argument, Unit)) :-
	integer(Argument),
	atom(Unit).

is_argument(real(Argument, Unit)) :-
	float(Argument),
	atom(Unit).

is_argument(string(Argument)) :-
	atomic(Argument).

is_argument(list(List)) :-
	is_argument_list(List).

is_argument(set(Set)) :-
	is_argument_list(Set).

is_argument(expr(Operator, Arg1, Arg2)) :-
	is_argument(Arg1),
	is_argument(Arg2),
	atom(Operator),
	member(Operator, ['+', '-', '*', '/', '&']).


%% is_argument_list(+Arguments:list) is det.
%
% @param Arguments is a list of DRS-condition arguments
%
is_argument_list([]).

is_argument_list([Head | Tail]) :-
	is_argument(Head),
	is_argument_list(Tail).


%% conditionlist_allreferents(+ConditionList:list, -AllReferents:list) is det.
%
% Extract declared discourse referents from conditions.
%
conditionlist_allreferents([], []).

conditionlist_allreferents([Condition | ConditionList], AllReferents) :-
	condition_domain(Condition, Domain),
	conditionlist_allreferents(ConditionList, Referents),
	flatten([Domain, Referents], AllReferents).


%% condition_domain(+Condition:term, -AllReferents:list) is det.
%
% Extract declared discourse referents from conditions.
%
condition_domain([FirstCond|Conds], AllReferents) :-
	conditionlist_allreferents([FirstCond|Conds], AllReferents).

condition_domain(DRS, AllReferents) :-
	DRS =.. [Op, drs(Domain, ConditionList)],
	unary_drs_operator(Op),
	conditionlist_allreferents(ConditionList, Referents),
	flatten([Domain, Referents], AllReferents).

condition_domain(DRS, AllReferents) :-
	DRS =.. [Op, drs(Domain1, ConditionList1), drs(Domain2, ConditionList2)],
	binary_drs_operator(Op),
	conditionlist_allreferents(ConditionList1, Referents1),
	conditionlist_allreferents(ConditionList2, Referents2),
	flatten([Domain1, Domain2, Referents1, Referents2], AllReferents).

condition_domain(_:drs(Domain, ConditionList), AllReferents) :-
	conditionlist_allreferents(ConditionList, Referents),
	flatten([Domain, Referents], AllReferents).

condition_domain(_-_, []).


%% has_no_duplicates(+ListofVariables:list) is det.
%
%
has_no_duplicates([]).

has_no_duplicates([D | DomsTail]) :-
	\+ is_member(D, DomsTail),
	has_no_duplicates(DomsTail).


% Check 2. Every used discourse referent is declared.
dr_decl(_ReferentList, []).

dr_decl(ReferentList, [Condition | ConditionList]) :-
	are_referents_declared(ReferentList, Condition),
	dr_decl(ReferentList, ConditionList).


are_referents_declared(ReferentList, [FirstCond|Conds]) :-
	dr_decl(ReferentList, [FirstCond|Conds]).

are_referents_declared(ReferentList, DRS) :-
	DRS =.. [Op, drs(Domain, ConditionList)],
	unary_drs_operator(Op),
	append(ReferentList, Domain, AllReferents),
	dr_decl(AllReferents, ConditionList).

are_referents_declared(ReferentList, DRS) :-
	DRS =.. [Op, drs(Domain1, ConditionList1), drs(Domain2, ConditionList2)],
	binary_drs_operator(Op),
	append(ReferentList, Domain1, IfReferents),
	append(IfReferents, Domain2, ThenReferents),
	dr_decl(IfReferents, ConditionList1),
	dr_decl(ThenReferents, ConditionList2).

are_referents_declared(ReferentList, _:drs(Domain, ConditionList)) :-
	append(ReferentList, Domain, AllReferents),
	dr_decl(AllReferents, ConditionList).

are_referents_declared(ReferentList, Predicate-_) :-
	Predicate =.. [_ | Arguments],
	term_variables(Arguments, VariableArguments),
	are_members(VariableArguments, ReferentList).


% Check 3. Every declared discourse referent is used (in the same DRS).
are_dr_used([]).

are_dr_used([Condition | ConditionList]) :-
	is_dr_used(Condition),
	are_dr_used(ConditionList).


is_dr_used([FirstCond|Conds]) :-
	are_dr_used([FirstCond|Conds]).

is_dr_used(DRS) :-
	DRS =.. [Op, drs(Dom, Conds)],
	unary_drs_operator(Op),
	is_dom_used(Dom, Conds),
	are_dr_used(Conds).

is_dr_used(DRS) :-
	DRS =.. [Op, drs(Dom1, Conds1), drs(Dom2, Conds2)],
	binary_drs_operator(Op),
	is_dom_used(Dom1, Conds1),
	is_dom_used(Dom2, Conds2),
	are_dr_used(Conds1),
	are_dr_used(Conds2).

is_dr_used(_:drs(Dom, Conds)) :-
	is_dom_used(Dom, Conds),
	are_dr_used(Conds).

is_dr_used(_-_).


%% is_dom_used(+Dom, +Conds) is det.
%
% Checks if all discourse referents are used in the same DRS
% (i.e. in atomic conditions).
%
is_dom_used([], _).

is_dom_used([X | Dom], Conds) :-
	is_in_simple_conds(X, Conds),
	is_dom_used(Dom, Conds).


is_in_simple_conds(X, [Predicate-_ | _]) :-
	Predicate =.. [_ | Arguments],
	term_variables(Arguments, VariableArguments),
	is_member(X, VariableArguments),
	!.

is_in_simple_conds(X, [[FirstCond|Conds] | _]) :-
	is_in_simple_conds(X, [FirstCond|Conds]),
	!.

is_in_simple_conds(X, [_ | Conds]) :-
	is_in_simple_conds(X, Conds).

%%
%
% are_members/2 for variables
%
are_members([], _).

are_members([H | L1], L2) :-
	is_member(H, L2),
	are_members(L1, L2).


%% is_member(+Referent:var, +ReferentList:list) is det.
%
% member/2 for variables and lists of variables.
%
% @param Referent is a discourse referent
% @param ReferentList is a list of discourse referents
%
is_member(X, [A | _]) :-
	X == A,
	!.

is_member(X, [_ | List]) :-
	is_member(X, List).


%% id(+ID)
%
% Succeeds it is a valid ID.

id(ID) :-
	integer(ID).

id(SID/TID) :-
	integer(SID),
	integer(TID).

id(SID/TID) :-
	integer(SID),
	TID == ''.
