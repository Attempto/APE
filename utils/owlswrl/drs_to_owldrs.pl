% This file is part of the Attempto Parsing Engine (APE).
% Copyright 2008-2012, Kaarel Kaljurand <kaljurand@gmail.com>.
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


:- module(drs_to_owldrs, [
		drs_to_owldrs/2
	]).

:- use_module(ape('utils/drs_ops'), [
		unary_drs_operator/1
	]).

/** <module> DRS to OWL DRS converter

The DRS is converted to "OWL DRS" which is a more suitable format
for the conversion into OWL.
The input must be simplified DRS (drs_to_sdrs.pl) which has been numbervard.
The following steps are made:

1. Rewrite implication chains: A => (B => C) ~~> (A & B) => C.

2. Rewrite embedded implications via conjunction and negations: A => B ~~> -(A & -B)

3. Remove double negations: --A ~~> A

4. Remove toplevel be-predicate-conditions

5. Remove named-object-conditions, unify the corresponding discourse referents with named(ProperName)

6. Rewrite certain conditions and sets of conditions:

* transitive (_|located-in|_) and comparative (_|taller than|_) adjectives are turned into transitive verbs,

* relation/3 + be-predicate are used to construct data properties,

* otherwise, relation/3 is used to construct object properties.

* "which man" is turned into "what is a man"

* Map attributive and predicative positive adjectives (property/3) to nouns (object/6).
Supporting comparative and superlative ("richer man", "richest man") probably doesn't make sense
as these require more background semantics.


@author Kaarel Kaljurand
@version 2011-07-28

*/


% Operators used in the DRS.
:- op(400, fx, -).
:- op(500, xfx, =>).
:- op(500, xfx, v).


%% drs_to_owldrs(+Drs:term, -OwlDrs:term) is det.
%
% Modifies the DRS to make it easier to convert in to OWL/SWRL.
%
% @param Drs is Attempto DRS
% @param OwlDrs is a modified Attempto DRS
%
drs_to_owldrs(Drs, OwlDrs) :-
	drs_to_owldrs(0, Drs, DrsTmp),
	exclude(is_named(DrsTmp), DrsTmp, OwlDrs).


%% drs_to_owldrs(+Level:term, +Drs:term, -OwlDrs:term) is det.
%
% Modifies the DRS to make it easier to convert in to OWL/SWRL.
% The embedding-level is 0 for the top-level DRS-box, and grows
% by 0, l(0), l(l(0)), ...
%
% @param Level denotes the level of embedding of the DRS-box
% @param Drs is Attempto DRS
% @param OwlDrs is a modified Attempto DRS
%
drs_to_owldrs(_, [], []) :-
	!.

% Double negation is removed
% --A == A
drs_to_owldrs(L, [-Drs | Conds], CondsAll) :-
	drs_to_owldrs(l(L), Drs, [-DrsR]),
	!,
	drs_to_owldrs(L, Conds, CondsR),
	append(DrsR, CondsR, CondsAll).

drs_to_owldrs(L, [Drs | Conds], [NDrs | NConds]) :-
	functor(Drs, Op, 1),
	unary_drs_operator(Op),
	!,
	arg(1, Drs, DrsConds),
	drs_to_owldrs(l(L), DrsConds, NDrsConds),
	functor(NDrs, Op, 1),
	arg(1, NDrs, NDrsConds),
	drs_to_owldrs(L, Conds, NConds).


% Handle sentences like:
% For every thing X for every thing Y if X owns something that contains Y then X owns Y.
% BUG: maybe allow this for any level, i.e. not just 0
% BUG: this new handling changes the OWL representation of sentences 41, 115, 116, 117.
% Also, it makes sentences 66 and 67 correctly succeed as SRWL rules.
% Also, it would break sentence 57 "Every man likes no dog.", but we avoid this
% by requiring the embedded then-box not to be negated.
% But, then we reject this: For everything X for every thing Y if X likes Y then X does not hate Y.
% (although this could be mapped to property disjointness axiom).
%
% BUG: Think about all this in a more systematic way. (The rewriting is definitely
% correct, but it might harm completeness.)
drs_to_owldrs(0, [IfBox => [EmbeddedIfBox => EmbeddedThenBox] | Conds], DrsR) :-
	EmbeddedThenBox \= [-_],
	!,
	append(IfBox, EmbeddedIfBox, MergedIfAndEmbeddedIfBox),
	drs_to_owldrs(0, [MergedIfAndEmbeddedIfBox => EmbeddedThenBox | Conds], DrsR).

%
% Toplevel implications are kept as they are.
%
drs_to_owldrs(0, [Drs1 => Drs2 | Conds], [Drs1R => Drs2R | CondsR]) :-
	!,
	drs_to_owldrs(l(0), Drs1, Drs1R),
	drs_to_owldrs(l(0), Drs2, Drs2R),
	drs_to_owldrs(0, Conds, CondsR).

/*
% This is handled by the double negation removal rule.
% A => -B == -(A & B)
drs_to_owldrs(l(L), [IfBox => [-ThenBox] | Conds], DrsR) :-
	!,
	append(IfBox, ThenBox, IfThenBox),
	drs_to_owldrs(l(L), [-IfThenBox | Conds], DrsR).
*/


% Embedded implications are turned into double negations.
% A => B == -(A & -B)
drs_to_owldrs(l(L), [IfBox => ThenBox | Conds], DrsR) :-
	!,
	drs_to_owldrs(l(L), [-[-ThenBox | IfBox] | Conds], DrsR).

drs_to_owldrs(L, [DRS1 v DRS2 | Conds], [DRS1R v DRS2R | CondsR]) :-
	!,
	drs_to_owldrs(l(L), DRS1, DRS1R),
	drs_to_owldrs(l(L), DRS2, DRS2R),
	drs_to_owldrs(L, Conds, CondsR).

% list condition that expresses 'at most', 'less than', 'exactly'
drs_to_owldrs(L, [List | Conds], [ListR | CondsR]) :-
	is_list(List),
	!,
	drs_to_owldrs(L, List, ListR),
	drs_to_owldrs(L, Conds, CondsR).

drs_to_owldrs(L, Conds, CondsR) :-
	rewrite_relation(Conds, FilteredConds),
	drs_to_owldrs(L, FilteredConds, CondsR).

% (Fallback)
drs_to_owldrs(L, [Cond | Conds], [Cond | CondsR]) :-
	drs_to_owldrs(L, Conds, CondsR).


% of-constructions with data items
%
% John's age is 30.
% John's address is "Poland".
%
% BUG: those should be rejected I guess because data properties
% cannot have inverses. (This can be done later, by
% not allowing a data object to be a subject.)
%
% 30 is an age of John.
% "Poland" is an address of John.
%
% Note that in addition to relation/3 we also remove the object/6.
%
rewrite_relation(Conds, [predicate(C, PropertyName, A, DataItem)-SId/'' | FilteredConds]) :-
	remove_many([
		predicate(C, be, D1, DataItem)-SId/_,
		object(D2, PropertyName, countable, na, eq, 1)-SId/_,
		relation(D3, of, A)-SId/_
	], Conds, FilteredConds),
	D1 == D2, D2 == D3,
	is_dataitem(DataItem),
	!.

rewrite_relation(Conds, [predicate(C, PropertyName, A, DataItem)-SId/'' | FilteredConds]) :-
	remove_many([
		predicate(C, be, DataItem, D1)-SId/_,
		object(D2, PropertyName, countable, na, eq, 1)-SId/_,
		relation(D3, of, A)-SId/_
	], Conds, FilteredConds),
	D1 == D2, D2 == D3,
	is_dataitem(DataItem),
	!.


% of-constructions without data items
%
% We handle differently the sentences with copula and without copula.
% The following sentences
%
% John's father is Bill.
% Bill is a father of John.
%
% will be represented as:
%
% PropertyAssertion(ObjectProperty(father), Individual(John), Individual(Bill))
%
rewrite_relation(Conds, [predicate(_, RelationalNoun, A, B)-SId/'' | FilteredConds]) :-
	remove_many([
		predicate(_, be, D1, B)-SId/_,
		object(D2, RelationalNoun, countable, na, eq, 1)-SId/_,
		relation(D3, of, A)-SId/_
	], Conds, FilteredConds),
	D1 == D2, D2 == D3,
	!.

rewrite_relation(Conds, [predicate(_, RelationalNoun, A, B)-SId/'' | FilteredConds]) :-
	remove_many([
		predicate(_, be, B, D1)-SId/_,
		object(D2, RelationalNoun, countable, na, eq, 1)-SId/_,
		relation(D3, of, A)-SId/_
	], Conds, FilteredConds),
	D1 == D2, D2 == D3,
	!.

% But the following sentences
%
% John's father likes Bill.
% Bill likes John's father.
%
% will be handled by keeping the object/6 in the returned list.
%
% PropertyAssertion(ObjectProperty(father), Individual(John), nodeID($VAR(2)))
% ClassAssertion(Class(owl:Thing), nodeID($VAR(2)))
%
% Some other tests:
%
% - John's brother likes everybody.
% - John's brother likes Mary. An age of the brother is 10.
% - Everybody's age is 31. (SWRL? Currently buggy.)
% - Everybody's address is "Poland". (SWRL? Currently buggy.)
% - Everybody's ancestor is Adam.
% - Everybody's ancestor is not Adam.
% - Every part of EU is a country.
%
% Note that one should be able to derive from "Every part of EU is a country. Estonia is a part of EU."
% that "Estonia is a country." without having to assert that "Estonia is a part."
% To make it possible we currently do not create a class "part", i.e. relational nouns
% only create properties.
%
% We used to return:
%
%==
% object(D, RelationalNoun, countable, na, eq, 1)-Id
%==
%
% But now we return:
%
%==
% object(D, something, dom, na, na, na)-Id
%==
%
rewrite_relation(Conds, [
	predicate(_, RelationalNoun, A, D1)-SId/'',
	object(D1, something, dom, na, na, na)-SId/''
	| FilteredConds]) :-
	remove_many([
		object(D1, RelationalNoun, countable, na, eq, 1)-SId/_,
		relation(D2, of, A)-SId/_
	], Conds, FilteredConds),
	D1 == D2.


% predicate/4
%
% Transitive and comparative adjectives (with arguments)
% are mapped to transitive verbs. This is debatable (and not reversible).
% BUG: We do not currently check the Degree of property/4.
% Note that it can be one of {pos, pos_as, comp, comp_than, sup}
% The outcome is that "John is as rich as Mary." == "John is richer than Mary."
% which is not wanted. I.e. one should use different property names and additionally
% assert disjointness of the properties.
%
% Examples:
%
% John is fond-of Mary.        # pos
% England is located-in UK.    # pos
% John is as rich as Mary.     # pos_as
% John is fonder-of Mary.      # comp
% John is taller than Mary.    # comp_than
% John is more rich than Mary. # comp_than
% John is fondest-of Mary.     # sup

rewrite_relation(Conds, [predicate(RefPred, PropertyName, RefArg1, RefArg2)-SId/'' | FilteredConds]) :-
	remove_many([
		predicate(RefPred, be, RefArg1, RefRel1)-SId/_,
		property(RefRel2, PropertyName, _Degree, RefArg2)-SId/_
	], Conds, FilteredConds),
	RefRel1 == RefRel2.


% Which man ... ? --> What is a man that ... ?
% Turns plural into singular.
% BUG: turns mass into countable (can we have 'mass' with 'which' at all?)
%
% BUG: does not work: Which member of Attempto is a productive-developer?
rewrite_relation(Conds, [
		query(NewRef1, QueryWord)-SId/'',
		object(WhichRef1, Name, countable, na, eq, 1)-SId/'',
		predicate(_, be, NewRef1, WhichRef1)-SId/'' | FilteredConds]) :-
	remove_many([
		query(WhichRef1, QueryWord)-SId/_,
		object(WhichRef2, Name, _, na, _, _)-SId/_
	], Conds, FilteredConds),
	WhichRef1 == WhichRef2.


% Attributive positive adjectives
% E.g. Every man likes a red cat. -> Every man likes a cat that is a red.
rewrite_relation(Conds, [
	object(Ref1, Noun, Count, na, Eq, Num)-SId/TId,
	object(NewRef, Adj, countable, na, eq, 1)-SId/'',
	predicate(_, be, Ref1, NewRef)-SId/''
	| FilteredConds]) :-
	remove_many([
		object(Ref1, Noun, Count, na, Eq, Num)-SId/TId,
		property(Ref2, Adj, pos)-SId/_
	], Conds, FilteredConds),
	Ref1 == Ref2.

% Predicative positive adjectives
% E.g. John is rich. -> John is a rich.
rewrite_relation(Conds, [
	object(Ref1, Adj, countable, na, eq, 1)-SId/'',
	predicate(PRef, be, Arg1, Ref1)-SId/TId
	| FilteredConds]) :-
	remove_many([
		property(Ref1, Adj, pos)-SId/_,
		predicate(PRef, be, Arg1, Ref2)-SId/TId
	], Conds, FilteredConds),
	Ref1 == Ref2.


%% remove_many(+ConditionsToRemove:list, +AllConditions:list, RemainingConditions:list) is det.
%
%
remove_many([], Conds, Conds).

remove_many([H | T], Conds, FilteredConds) :-
	select(H, Conds, NewConds),
	remove_many(T, NewConds, FilteredConds).


%% is_dataitem(+DataItem:term) is det.
%
% Succeeds if DataItem is an ACE int, real, or string.
%
% @param DataItem is an ACE data item (a number or a string)
%
is_dataitem(X) :-
	nonvar(X),
	is_dataitem_(X).

is_dataitem_(int(_)).
is_dataitem_(real(_)).
is_dataitem_(string(_)).


%% is_named(+CondList:list, +Condition:term)
%
% This is used for excluding certain conditions.
%
is_named(CondList, predicate(_, be, X, Y)-_) :-
	member(query(A, _)-_, CondList),
	(A == X ; A == Y),
	!,
	fail.

% Filters out:
% John is a man.
% A man is manager.
% A man is John.
% BUG: also filters out: John is John. (which might not be wanted)
% Note that the rule is so complex because we want to
% avoid filtering out numbers, strings, and the like.
is_named(_, predicate(_, be, X, Y)-_) :-
	var(X),
	var(Y),
	!,
	X = Y.

is_named(_, predicate(_, be, named(N), named(N))-_).
