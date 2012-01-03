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


:- module(drs_fol_pnf, [
		drs_fol/2,
		drs_fol/3,
		drs_pnf/2,
		drs_pnf/3,
		drs_fol_pnf/3,
		drs_fol_pnf/4,
		fol_to_folpp/2
	]).

/** <module> Transform DRSs into First-order Formulas and Their Prenex Normal Forms

Technical Details:

drs_to_fol(+DRS, +World, -FOL, -PRN) converts a discourse representation structure DRS to its  
equivalent first-order formula FOL with possible worlds semantics, and to its prenex normal form PNF.

All constructs of ACE are processed with the exception of negation as failure and the modal operators
'may' and 'should' that cause the error 'Unsupported DRS construct'.

Syntax of DRS: negation '-', disjunction 'v', conjunction ',', implication '=>', negation as failure '~',
possibility 'can', necessity 'must', label ':', admission 'may', recommendation 'should', maximality 
conditions for "at most", "less than" and "exactly" are bracketed by [ ]; discourse referents are lists 
of existentially - or in preconditions of implications universally - quantified variables.

Syntax of FOL and PRNF: negation '-', disjunction 'v', conjunction '&', implication '=>', maximality 
conditions for "at most", "less than" and "exactly" are bracketed by [ ] ; universal quantification
'forall(X,Formula)', existential quantification 'exists(X,Formula)', where 'X' is a Prolog variable.

@author Norbert E. Fuchs
@author Kaarel Kaljurand
@author Tobias Kuhn
@version 2011-01-01
*/


:- op(400,  xfx, :).           % label
:- op(400,  fy, -).            % negation
:- op(400,  fy, ~).            % negation as failure
:- op(400,  fy, can).          % possibility 
:- op(400,  fy, must).         % necessity
:- op(400,  fy, may).          % admission 
:- op(400,  fy, should).       % recommendation
:- op(500, xfy, &).            % conjunction
:- op(600, xfy, v).            % disjunction
:- op(650, xfy, =>).           % implication 


%% drs_fol(+DRS:drs, +World:world, -FOL:fol) is det.
%% drs_fol(+DRS:drs, -FOL:fol) is det.
%
% @param DRS is Attempto DRS
% @param World is a variable
% @param FOL is FOL formula

drs_fol(DRS, FOL) :-
	drs_to_fol(DRS, _, FOL),
	!.

drs_fol(drs([], []), _World, '') :-
	!.

drs_fol(DRS, World, FOL) :-
	drs_to_fol(DRS, World, FOL),
	!.

drs_fol(_, _World, '').


%% drs_pnf(+DRS:drs, +World:world, -PNF:fol) is det.
%% drs_pnf(+DRS:drs, -PNF:fol) is det.
%
% @param DRS is Attempto DRS
% @param World is a variable
% @param PNF is FOL formula in PNF

drs_pnf(DRS, PNF) :-
	drs_to_fol(DRS, _, FOL),
	prenex_normal_form(FOL, PNF),
	!.

drs_pnf(drs([], []), _World, '') :-
	!.

drs_pnf(DRS, World, PNF) :-
	drs_to_fol(DRS, World, FOL),
	prenex_normal_form(FOL, PNF),
	!.

drs_pnf(_, _World, '').


%% drs_fol_pnf(+DRS:drs, +World:world, -FOL:fol, -PNF:fol) is det.
%% drs_fol_pnf(+DRS:drs, -FOL:fol, -PNF:fol) is det.
%
% Converts DRS to first-order formula FOL using possible worlds semantics, and to prenex normal form PNF.
%
% @param DRS is Attempto DRS
% @param World is a variable
% @param FOL is FOL formula
% @param PNF is FOL formula in PNF

drs_fol_pnf(DRS, FOL, PNF) :-
	drs_to_fol(DRS, _, FOL),
	prenex_normal_form(FOL, PNF),
	!.

drs_fol_pnf(drs([], []), _World, '', '') :- !.

drs_fol_pnf(DRS, World, FOL, PNF) :-
	drs_to_fol(DRS, World, FOL),
	prenex_normal_form(FOL, PNF),
	!.

drs_fol_pnf(_, _World, '', '').


%% fol_to_folpp(+Fol:term, -FolPp:atom) is det.
%
% Prints the FOL formula (which can be in PNF) into an atom.
% The printing is done with write/1 which respects the
% operator definitions, i.e. binary operators are printed in infix form.
%
% @param Fol is a FOL formula (possibly in PNF)
% @param FolPp is the FOL formula pretty-printed

fol_to_folpp(Fol, FolPp) :-
	copy_term(Fol, FolCopy),
	numbervars(FolCopy, 0, _),
	with_output_to(atom(FolPp), write_term(FolCopy, [numbervars(true), quoted(true), module(drs_fol_pnf)])).


%% drs_to_fol(+DRS, +World, -FOL)
%
% convert DRS to first-order formula FOL using possible worlds semantics
%
% based on drs2fol.pl (P. Blackburn & J. Bos, From DRSs to First-Order Logic, 1999) and on Johan Bos, 
% Computational Semantics in Discourse: Underspecification, Resolution, and Inference, Journal of Logic, 
% Language and Information 13: 139–157, 2004.
%
% deviating from standard DRT, disjuncts are not translated independently, but similarly to implications to 
% allow for anaphoric references to precedings disjuncts permitted in ACE
%
% extended to handle maximality scopes (derived from exactly, at most, less than), questions and commands
%
% modal operators (may, should) and negation as failure (~) are flagged as being outside of FOL

drs_to_fol(drs([],[Condition]), World, Formula) :-
  !,
  cond_to_fol(Condition, World, Formula).

drs_to_fol(drs([],[Condition1,Condition2|Conditions]), World, Formula1 & Formula2) :-
  !,
  cond_to_fol(Condition1, World, Formula1),
  drs_to_fol(drs([],[Condition2|Conditions]), World, Formula2).

drs_to_fol(drs([X|Referents],Conditions), World, exists(X,Formula)) :-
  !,
  drs_to_fol(drs(Referents,Conditions), World, Formula).

cond_to_fol(-DRS, World, -Formula) :-
  !,
  drs_to_fol(DRS, World, Formula).

cond_to_fol(question(DRS), World, Formula) :-
  !,
  drs_to_fol(DRS, World, Formula).

cond_to_fol(command(DRS), World, Formula) :-
  !,
  drs_to_fol(DRS, World, Formula).

cond_to_fol(drs([],Conditions) v DRS2, World, Formula1 v Formula2) :-
  !,
  drs_to_fol(drs([],Conditions), World, Formula1),
  drs_to_fol(DRS2, World, Formula2).

cond_to_fol(drs([X|Referents],Conditions) v DRS2, World, exists(X,Formula)) :-
  !,
  cond_to_fol(drs(Referents,Conditions) v DRS2, World, Formula).

cond_to_fol(drs([],Conditions) => DRS2, World, Formula1 => Formula2) :-
  !,
  drs_to_fol(drs([],Conditions), World, Formula1),
  drs_to_fol(DRS2, World, Formula2).

cond_to_fol(drs([X|Referents],Conditions) => DRS2, World, forall(X,Formula)) :-
  !,
  cond_to_fol(drs(Referents,Conditions) => DRS2, World, Formula).

cond_to_fol(can(DRS), World1, exists(World2, (accessibility_relation(World1,World2) - accessibility_relation/0) & Formula)) :-
  !,
  drs_to_fol(DRS, World2, Formula).

cond_to_fol(must(DRS), World1, forall(World2, (accessibility_relation(World1,World2) - accessibility_relation/0) => Formula)) :-
  !,
  drs_to_fol(DRS, World2, Formula).

cond_to_fol(World2:DRS, World1, (accessibility_relation(World1,World2) - accessibility_relation/0) & Formula) :-
  !,
  drs_to_fol(DRS, World2, Formula).

cond_to_fol(List, World, NewList) :-
  is_list(List),
  !,
  transform_list(List, World, NewList).

cond_to_fol(BasicCondition - Index, World, NewBasicCondition - Index) :-
  BasicCondition =.. [Functor|Arguments],
  NewBasicCondition =..[Functor|[World|Arguments]].

cond_to_fol(Input, _World, _Formula) :-
   illegal_condition(Input),
   !,
   throw(error('Unsupported DRS construct', context(cond_to_fol/3, Input))).

illegal_condition(~(_DRS)).
illegal_condition(may(_DRS)).
illegal_condition(must(_DRS)).


%% transform_list(+List, +World, -NewList)
%
% transform each element of List

transform_list([], _World, []).

transform_list([Element|Elements], World, [NewElement|NewElements]) :-
  cond_to_fol(Element, World, NewElement),
  transform_list(Elements, World, NewElements).


%% prenex_normal_form(+FOL, -PNF)
%
% convert first-order formula FOL into its prenex normal form PNF
%
% based on transform/2 (P. A. Flach, Simply Logical: Intelligent Reasoning by Example, John Wiley & Sons, 1994)

prenex_normal_form(FOL, PNF):-
	rewrite_implications(FOL, F1),
	negations_inside(F1, F2),
	prenex_form(F2, PNF).


%% rewrite implications(+In, -Out)
% 

rewrite_implications(A, A):-						
	literal(A).

rewrite_implications(A => B, -C v D):-			
	rewrite_implications(A, C),
	rewrite_implications(B, D).

rewrite_implications(A & B, C & D):-			
	rewrite_implications(A, C),					
	rewrite_implications(B, D).

rewrite_implications(A v B, C v D):-
	rewrite_implications(A, C),
	rewrite_implications(B, D).

rewrite_implications(-A, -C):-
	rewrite_implications(A, C).

rewrite_implications(forall(X, B), forall(X, D)):-
	rewrite_implications(B, D).

rewrite_implications(exists(X, B), exists(X, D)):-
	rewrite_implications(B, D).


%% negations_inside(+In, -Out)
%
% move negations inside

negations_inside(A, A):-							
	literal(A).

negations_inside(-(A & B), C v D):-				
	negations_inside(-A, C),
	negations_inside(-B, D).

negations_inside(-(A v B), C & D):-				
	negations_inside(-A, C),
	negations_inside(-B, D).

negations_inside(-(-A), B):-						
	negations_inside(A, B).

negations_inside(-exists(X, A), forall(X, B)):-	
	negations_inside(-A, B).

negations_inside(-forall(X, A), exists(X, B)):-
	negations_inside(-A, B).

negations_inside(A & B, C & D):-				
	negations_inside(A, C),						
	negations_inside(B, D).

negations_inside(A v B, C v  D):-
	negations_inside(A, C),
	negations_inside(B, D).

negations_inside(exists(X, A), exists(X, B)):-
	negations_inside(A, B).

negations_inside(forall(X, A), forall(X, B)):-
	negations_inside(A, B).


%% prenex_form(+In, -Out)
%
% generate prenex normal form

prenex_form(F, PNF):-
	pnf(F, PNF, B, B).


pnf(A, V, V, A):-									
	literal(A).

pnf(forall(X, F), forall(X, Quants), V, Body):-
	pnf(F, Quants, V, Body).

pnf(exists(X, F), exists(X, Quants), V, Body):-
	pnf(F, Quants, V, Body).

pnf(A & B, Quants, V, BodyA & BodyB):-
	pnf(A, Quants, QB, BodyA),
	pnf(B, QB, V, BodyB).

pnf(A v B, Quants, V, BodyA v BodyB):-
	pnf(A, Quants, QB, BodyA),
	pnf(B, QB, V, BodyB).


%% disjunction_of_literals(+Term)
%

disjunction_of_literals(A) :-
	literal(A).

disjunction_of_literals(A v B):-
	disjunction_of_literals(A),
	disjunction_of_literals(B).


%% literal(+Term)
%

literal(_A - _Index).

literal(-(_A - _Index)).
