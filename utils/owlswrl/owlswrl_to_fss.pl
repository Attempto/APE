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

:- module(owlswrl_to_fss, [
		owlswrl_to_fss/1,
		owlswrl_to_fss/2
	]).

/** <module> OWL/SWRL serializer into OWL/SWRL Functional-Style Syntax

@author Kaarel Kaljurand
@version 2010-11-26

@tbd Escaping

@tbd Test on the regression test set if OWL-API can load the output produced here

*/

%% owlswrl_to_fss(+Ontology)
%% owlswrl_to_fss(+Ontology, -Atom)
%
% Writes the OWL/SWRL ontology in Functional-Style Syntax into the current stream.
%
owlswrl_to_fss('Ontology'(OntologyIri, Axioms)) :-
	format("Prefix(:=<~w#>)~n", [OntologyIri]),
	format("Ontology(<~w>~n", [OntologyIri]),
	print_list(Axioms, 1),
	format(")~n").

%
owlswrl_to_fss(Ontology, OntologyFss) :-
	with_output_to(atom(OntologyFss), owlswrl_to_fss(Ontology)).


%% print_compound
%
%
print_compound(OwlFss, Level) :-
	OwlFss =.. [Name | Args],
	PrettyIndent is Level * 3,
	writef('%r%w(\n', [' ', PrettyIndent, Name]),
	NewLevel is Level + 1,
	print_list(Args, NewLevel),
	writef('%r)\n', [' ', PrettyIndent]).


%% print_list
%
% Don't change the order, e.g. an empty list should
% not be "mistaken" for an atom.
%
print_list([], _).

print_list([Head | Tail], Level) :-
	is_list(Head),
	!,
	print_list(Head, Level),
	print_list(Tail, Level).

print_list([Expression | Tail], Level) :-
	print_terminal(Expression, Level),
	!,
	print_list(Tail, Level).

print_list([Head | Tail], Level) :-
	compound(Head),
	!,
	print_compound(Head, Level),
	print_list(Tail, Level).


%% print_terminal
%
%
print_terminal(Number, Level) :-
	number(Number),
	!,
	PrettyIndent is Level * 3,
	writef('%r%w\n', [' ', PrettyIndent, Number]).

print_terminal(Iri, Level) :-
	atom(Iri),
	!,
	PrettyIndent is Level * 3,
	writef('%r<%w>\n', [' ', PrettyIndent, Iri]).

print_terminal('Variable'(Iri), Level) :-
	PrettyIndent is Level * 3,
	writef('%rVariable(<%w>)\n', [' ', PrettyIndent, Iri]).

print_terminal('BuiltInAtom'(Iri, DArgList), Level) :-
	PrettyIndent is Level * 3,
	writef('%rBuiltInAtom(<%w>\n', [' ', PrettyIndent, Iri]),
	NewLevel is Level + 1,
	print_list(DArgList, NewLevel),
	writef('%r)\n', [' ', PrettyIndent]).

print_terminal('^^'(DataValue, DataType), Level) :-
	PrettyIndent is Level * 3,
	writef('%r"%w"^^<%w>\n', [' ', PrettyIndent, DataValue, DataType]).

print_terminal(Terminal, Level) :-
	pretty_print(Terminal, PrettyExpression),
	!,
	PrettyIndent is Level * 3,
	writef('%r%w\n', [' ', PrettyIndent, PrettyExpression]).


pretty_print(nodeID(Number), '_':Number).
pretty_print(NS:Name, NS:Name).
