% This file is part of the Attempto Parsing Engine (APE).
% Copyright 2008-2010, Kaarel Kaljurand <kaljurand@gmail.com>.
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


:- module(implication_to_swrl, [
		implication_to_swrl/3
	]).

:- use_module(drs_to_owlswrl_core, [
		is_toplevel/3,
		get_entity/3,
		condlist_and/4,
		is_object_with_generalized_quantifier/1,
		has_dom_for_member/3,
		dataitem_datavalue_datatypeuri/3
	]).


/** <module> DRS implication to SWRL-rule translator

Translate an Attempto DRS implication into Semantic Web Rule Language (SWRL) rule.

We use the SWRL rule syntax specified in
<http://www.webont.org/owled/2009/papers/owled2009_submission_16.pdf>

TODO: add a constraint that the variables in the head
is a subset of the variables in the body.

@author Kaarel Kaljurand
@version 2010-12-14
*/


%% implication_to_swrl(+Condition:term, +RefList:list, -Implies:term) is semidet.
%
% @param Condition is a DRS implication condition
% @param RefList is a list of DRS toplevel referents
% @param Implies is a SWRL 'DLSafeRule'-rule
%
implication_to_swrl(
	'=>'(CondList1, CondList2),
	RefList,
	'DLSafeRule'('Body'(AtomList1), 'Head'(AtomList2))
) :-
	condlist_atomlist(CondList1, RefList, AtomList1),
	condlist_atomlist(CondList2, RefList, AtomList2).


%% condlist_atomlist(+ConditionList:list, +RefList:list, -AtomList:list) is det.
%
% Note that most conditions can produce several SWRL atoms because
% an argument of the condition can be a complex expression which
% will have to be unravelled into multiple SWRL builtins.
%
% @param ConditionList is a list of DRS conditions
% @param RefList is a list of DRS toplevel referents
% @param AtomList is list of SWRL atoms
%
condlist_atomlist([], _, []).

% Attempt to roll up the whole condition list into a class expression
% E.g.: If X loves Y and Y loves X then X is a man.
% We need an additional contraint that it exposes all the variables
% that are also present in the body.
%condlist_atomlist(CondList, RefList, ['ClassAtom'(Class, OutX)]) :-
%	condlist_and(X, CondList, RefList, Class),
%	get_argument(X, RefList, object, OutX, _).

% BUG: Experimental
% Ignore 'something', 'thing', 'X', etc.
% The idea is that otherwise they would be typed as owl:Thing,
% i.e. as objects, but maybe the DRS uses them in data expressions.
% So we don't want to make the object vs data distinction yet.
% Test: If X = 1 + 2 then X = 3.
% Test: If there is X and there is Y then X knows Y.
% Test: For everything John likes Mary.
% Since some conditions are ignored it might happen that the
% head or the body becomes empty, but this is OK I guess.
condlist_atomlist([Condition | ConditionList], RefList, AtomList) :-
	has_dom_for_member(_Ref, [Condition], []),
	!,
	condlist_atomlist(ConditionList, RefList, AtomList).

condlist_atomlist([Condition | ConditionList], RefList, AtomList) :-
	condition_atomlist(Condition, RefList, AtomList1),
	condlist_atomlist(ConditionList, RefList, AtomList2),
	append(AtomList1, AtomList2, AtomList).


/*
% BUG: experimental
% BUG: Add a restriction that all variables used in predicate-conditions
% must be declared in object-conditions.

condlist_atomlist([], _RefList, []).

condlist_atomlist([Condition | CondList], RefList, [Atom | AtomList]) :-
	(
		Condition = object(D, Value, countable,  na, _, _)-_
	->
		get_entity(class, Value, NamedClass),
		select(Condition1, CondList, CondList1),
		condlist_class(D, Condition1, CondList1, RefList, AndClass, CondList2),
		Atom = 'description'(and(NamedClass, AndClass), 'I-variable'(D))
	;
		condition_atom(Condition, RefList, Atom),
		CondList2 = CondList
	),
	condlist_atomlist(CondList2, RefList, AtomList).
*/



%% condition_atomlist(+Condition:term, -Atoms:list) is det.
%
% @param Condition is a DRS condition
% @param RefList is a list of DRS toplevel referents
% @param Atoms is a list of SWRL atoms
%

% We allow no generalized quantifiers
condition_atomlist(Condition, _, _) :-
	is_object_with_generalized_quantifier(Condition),
	!,
	fail.

condition_atomlist(formula(Expr1, Comp, Expr2)-_, RefList, Builtins) :-
	expr_to_builtins(Expr1, Var1, B1),
	expr_to_builtins(Expr2, Var2, B1, B2),
	add_relation(Comp, Var1, Var2, RefList, B2, Builtins).

condition_atomlist(object(X, Value, _,  na, _, _)-_, _, ['ClassAtom'(NamedClass, Variable)]) :-
	get_variable(X, Variable),
	get_entity(class, Value, NamedClass).

condition_atomlist('-'([predicate(_, be, X1, X2)-_]), RefList, ['DifferentIndividualsAtom'(OutX1, OutX2)]) :-
	get_argument(X1, RefList, object, OutX1, _),
	get_argument(X2, RefList, object, OutX2, _).

condition_atomlist(predicate(_, be, X1, X2)-_, RefList, ['SameIndividualAtom'(OutX1, OutX2)]) :-
	get_argument(X1, RefList, object, OutX1, _),
	get_argument(X2, RefList, object, OutX2, _).

condition_atomlist(predicate(_, Value, X1, X2)-_, RefList, ['DataPropertyAtom'(DataProperty, OutX1, OutX2) | L]) :-
	Value \= be,
	get_entity(data_property, Value, DataProperty),
	get_argument(X1, RefList, object, OutX1, L1),
	get_argument(X2, RefList, data, OutX2, L2),
	append(L1, L2, L).

condition_atomlist(predicate(_, Value, X1, X2)-_, RefList, ['ObjectPropertyAtom'(ObjectProperty, OutX1, OutX2)]) :-
	Value \= be,
	get_entity(object_property, Value, ObjectProperty),
	get_argument(X1, RefList, object, OutX1, _),
	get_argument(X2, RefList, object, OutX2, _).

% Sublist
% E.g. If a man owns a car then the man likes the car and likes exactly 3 cats.
% E.g. If John owns a car then Mary likes a car and likes at most 3 cats.
condition_atomlist([C | Cs], RefList, ['ClassAtom'(SubListClass, OutX)]) :-
	condlist_and(X, [C | Cs], RefList, SubListClass),
	get_argument(X, RefList, object, OutX, _).

% Note that X can point to an Individual (i.e. not only a variable),
% see e.g. "If John does not like Mary then a man does not like a woman."
condition_atomlist('-'(Not), RefList, ['ClassAtom'('ObjectComplementOf'(NotClass), OutX)]) :-
	condlist_and(X, Not, RefList, NotClass),
	get_argument(X, RefList, object, OutX, _).

% Note that X can point to an Individual.
condition_atomlist('v'(Or1, Or2), RefList, ['ClassAtom'('ObjectUnionOf'([Or1Class, Or2Class]), OutX)]) :-
	condlist_and(X, Or1, RefList, Or1Class),
	condlist_and(X, Or2, RefList, Or2Class),
	get_argument(X, RefList, object, OutX, _).


%% get_argument(+X:term, +RefList:list, -Type:atom, -Argument:term, -Atoms:list) is det.
%
% @param X is a discourse referent, expr-term, int/1, real/1, or string/1
% @param RefList is a list of DRS toplevel referents
% @param Type is the type of the argument, i.e. one of {data, object, _}
% @param Argument is a SWRL variable, OWL individual, or OWL data value
% @param Atoms is a list of SWRL atoms (empty if X produces no builtins)
%
get_argument(expr(X, Y, Z), _RefList, data, Var, Builtins) :-
	!,
	expr_to_builtins(expr(X, Y, Z), Var, Builtins).

% int/1, real/1, string/1
% BUG: use a predicate that already returns the ^^/2 term
% to make sure that this term is used everywhere in DRS->OWL/SWRL
% where numbers are represented.
get_argument(DataItem, _RefList, data, '^^'(DataValue, DataTypeUri), []) :-
	dataitem_datavalue_datatypeuri(DataItem, DataValue, DataTypeUri),
	!.

get_argument(X, RefList, object, Individual, []) :-
	is_toplevel(X, RefList, 'ObjectOneOf'([Individual])),
	!.

get_argument('$VAR'(X), _RefList, object, Variable, []) :-
	get_variable(X, Variable).

get_argument(X, _RefList, object, X, []).


%% add_relation(+Relation:atom, +Var1, +Var2, +BIn:list, -BOut:list) is det.
%
% @param Relation is one of {'>', '<', '='}
% ...
%
% @bug try to output less code, e.g. instead of 'equal' just unify: add_relation('=', Var, Var, B, B).
%
add_relation(Relation, Var1, Var2, RefList, B, ['BuiltInAtom'(swrlb:Builtin, [OutVar1, OutVar2]) | B]) :-
	relation_builtin(Relation, Builtin),
	get_argument(Var1, RefList, _, OutVar1, _),
	get_argument(Var2, RefList, _, OutVar2, _).


% expr_to_builtins(+Expr:term, -EList:list) is det.
% expr_to_builtins(+Expr:term, -Var:atom, -EList:list) is det.
%
% Flattens the expression (a tree) into a list simple expressions
% that are connected by expression handles. E.g.
%
%==
% ?- expr_to_builtins(expr(*, expr(+, 1, 2), 3), L).
% L = [builtin(multiply, G295, G297, 3), builtin(add, G297, 1, 2)].
%==
%
% @param Expr is a term in the form expr(Op, Expr1, Expr2)
% @param Var is a variable to be used as handle for the topmost expression
% @param Builtins is a list of SWRL built-in predicates
%
expr_to_builtins(Expr, List) :-
	expr_to_builtins(Expr, _, List).

expr_to_builtins(Expr, Var, List) :-
	expr_to_builtins(Expr, Var, [], List).

expr_to_builtins(expr(Op, E1, E2), Var, LIn, ['BuiltInAtom'(swrlb:Builtin, [Var, Var1, Var2]) | LOut]) :-
	!,
	op_builtin(Op, Builtin),
	get_swrl_variable(Var),
	expr_to_builtins(E1, Var1, LIn, LTmp),
	expr_to_builtins(E2, Var2, LTmp, LOut).

% @bug: why is RefList here empty?
expr_to_builtins(Var, OutVar, List, List) :-
	get_argument(Var, [], _, OutVar, _).


%% get_swrl_variable(-SwrlVariable:term) is det.
%
% Generate a new SWRL variable (prefixed by 'g' to avoid a possible nameclash)
%
% @bug It's not nice to use gensym/2
%
get_swrl_variable(Variable) :-
	gensym(g, GenSym),
	get_variable(GenSym, Variable).


%% get_variable(+Var, -Variable)
%
%
get_variable('$VAR'(X), 'Variable'(Var)) :-
	!,
	with_output_to(atom(Var), format("urn:swrl#x~w", [X])).

get_variable(X, 'Variable'(Var)) :-
	with_output_to(atom(Var), format("urn:swrl#x~w", [X])).


%% relation_builtin(?Relation:atom, ?Builtin:atom) is det.
%
% SWRL built-ins for comparisons (complete coverage of the 6 built-ins)
%
% @param Relation is one of {'>', '<', '=', '\=', '=<', '>='}
% @param Builtin is SWRL built-in
%
relation_builtin('=', equal).
relation_builtin('\\=', notEqual).
relation_builtin('<', lessThan).
relation_builtin('=<', lessThanOrEqual).
relation_builtin('>', greaterThan).
relation_builtin('>=', greaterThanOrEqual).


%% op_builtin(?Op:atom, ?Builtin:atom) is det.
%
% SWRL math built-ins (4) and string built-ins (1).
%
% @param Op is DRS (binary) operator
% @param Builtin is SWRL built-in
%
op_builtin('+', add).
op_builtin('-', subtract).
op_builtin('*', multiply).
op_builtin('/', divide).
op_builtin('&', stringConcat).
