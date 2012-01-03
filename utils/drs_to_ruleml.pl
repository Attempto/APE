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


:- module(drs_to_ruleml, [
		drs_to_ruleml/2
		]).

/** <module> Attempto DRS to RuleML/folog converter

This module converts the Attempto DRS into RuleML/folog as
specified by David Hirtle in his thesis.

@author Kaarel Kaljurand
@version 2006-10-05
@see http://www.ruleml.org/translator/

@bug Test RuleML validity.
@bug Test correspondence to Hirtle's specification. E.g. DRSs resulting from
query sentences are currently incorrectly supported. (Note that Hirtle just
rejects such DRSs.)
@bug Support ACE 5/5.5/6 extensions (e.g. RuleML supports NAF).
*/


% The following operators are used in the DRS.
:- op(400, fx, -).
:- op(500, xfx, =>).
:- op(500, xfx, v).


%% drs_to_ruleml(+Drs:drs, -RuleML:functor) is det.
%
% Turn the DRS into RuleML/folog (in SWI Prolog's internal XML format)
% as specified in David Hirtle's thesis.

drs_to_ruleml(
	drs(Dom, Conds),
	element('RuleML', [], [
		element('Assert', [], [
			element('Exists', [], Elements)
		])
	])
	) :-
	args_els(Dom, DomElements),
	conds_and(Conds, Element),
	append(DomElements, [Element], Elements).


%% conds_and(+Conds:list, -Element:functor) is det.
%
% Converts a list of DRS conditions into a RuleML and-element.

conds_and(Conds, element('And', [], Elements)) :-
	conds_elements(Conds, Elements).


%% conds_elements(+Conds:list, -Elements:list) is det.
%
% Converts a list of DRS conditions into RuleML elements.

conds_elements([], []).

conds_elements([Cond | Tail], [SCond | STail]) :-
	cond_element(Cond, SCond),
	conds_elements(Tail, STail).


%% cond_element(+Condition:functor, -Element:functor) is det.
%
% Converts a DRS condition into a RuleML element.
%
% As the structure of the DRS doesn't match exactly to the structure
% of the RuleML element (as specified by Hirtle),
% we have to do some ugly appending.

cond_element(drs(Dom1, Conds1) => drs(Dom2, Conds2),
	element('Forall', [], SubElements0)
	) :-
	args_els(Dom1, VarElements1),
	args_els(Dom2, VarElements2),
	conds_and(Conds1, Element1),
	conds_and(Conds2, Element2),
	append(VarElements2, [Element2], SubElements2),
	append([Element1], [element('Exists', [], SubElements2)], SubElements1),
	append(VarElements1, [element('Implies', [], SubElements1)], SubElements0).


cond_element(drs(Dom1, Conds1) v drs(Dom2, Conds2),
	element('Or', [], SubElements0)
	) :-
	args_els(Dom1, VarElements1),
	args_els(Dom2, VarElements2),
	conds_and(Conds1, Element1),
	conds_and(Conds2, Element2),
	append(VarElements2, [Element2], SubElements2),
	append(VarElements1, [Element1], SubElements1),
	append([element('Exists', [], SubElements1)], [element('Exists', [], SubElements2)], SubElements0).


cond_element(-drs(Dom, Conds), element('Neg', [], [element('Exists', [], SubElements)])) :-
	args_els(Dom, VarElements),
	conds_and(Conds, Element),
	append(VarElements, [Element], SubElements).


cond_element(Conds, Element) :-
	is_list(Conds),
	conds_and(Conds, Element).


cond_element(Condition-_, element('Atom', [], [element('Rel', [], [Functor]) | Els])) :-
	Condition =.. [Functor | Args],
	args_els(Args, Els).


%% args_els(+List:list, -Elements:list) is det.
%
% Converts the arguments of a DRS condition into RuleML elements.
%
% Note that all terms (e.g. variables and numbers) must be converted into atoms
% to be compatible with the way how SWI represents XML
% documents internally.

args_els([], []).

args_els([H | T], [element('Var', [], [HH]) | ElsTail]) :-
	var(H),
	!,
	term_to_atom(H, HH),
	args_els(T, ElsTail).

args_els([H | T], [element('Data', [], [HH]) | ElsTail]) :-
	number(H),
	!,
	term_to_atom(H, HH), % alternatively: atom_number(HH, H)
	args_els(T, ElsTail).

args_els([H | T], [element('Ind', [], [H]) | ElsTail]) :-
	args_els(T, ElsTail).


%% to_xml(+Elements:list, -Xml:atom) is det.
%
% @param Elements is a list of XML elements
% @param Xml is an XML document as an atom
%
% @deprecated use SWI's xml_write/3 instead
%
% Converts SWI Prolog's XML represenation into an XML atom.
% Something like xml_write/3 but very simple.
%
to_xml([], '').

to_xml([element(Name, _, Elements) | T], Xml) :-
	to_xml(Elements, ElXml),
	to_xml(T, TXml),
	format(atom(Xml), '<~w>~w</~w>~n~w', [Name, ElXml, Name, TXml]).

to_xml([Text | T], Xml) :-
	atom(Text),
	to_xml(T, TXml),
	format(atom(Xml), '~w~w', [Text, TXml]).
