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


:- module(owlswrl_to_xml, [
		owlswrl_to_xml/2
	]).


/** <module> OWL/SWRL Functional-Style Syntax to OW/SWRL XML converter

@author Kaarel Kaljurand
@version 2010-11-26

@bug support for datatype properties is not implemented or buggy
@tbd anonymous individuals should not be turned into regular individuals

% It would be good to output also AnonymousIndividual, but this needs more work,
% e.g. "There is a woman that every man likes." is rejected by the OWL-API OWLXMLParser:
% Element not found: Expected at least one individual in object oneOf
% Note sure why this is not allowed...

@tbd Use abbreviated IRIs and prefix declarations, no need for atom concat

@tbd Cover all of OWL/SWRL not just the constructs that ACE->OWL/SWRL generates

*/


%% owlswrl_to_xml(+OWL:term, -XML:term) is det.
%
% @param OWL is OWL ontology in Functional-Style Syntax (Prolog notation)
% @param XML is OWL ontology in XML (SWI-Prolog's XML notation)
%
owlswrl_to_xml(
	'Ontology'(OntologyIri, AxiomList),
	element('Ontology', [
		'xml:base' = 'http://www.w3.org/2002/07/owl#',
		'xmlns' = 'http://www.w3.org/2002/07/owl#',
		'ontologyIRI' = OntologyIri
		], ElList)
	) :-
	atom_concat(OntologyIri, '#', Prefix),
	axiomlist_to_xml(AxiomList, Prefix, ElList).


%% axiomlist_to_xml(+Axioms:list, Prefix:atom, -TermList:list) is det.
%
% Converts a list of OWL/SWRL axioms in Prolog notation into a list of XML
% elements in Prolog notation.
%
% @param Axioms is a list of Prolog terms (representing OWL/SWRL axioms)
% @param Prefix IRI that is used in case of the default namespace as a prefix
% @param ElementList is a list of XML elements
%
axiomlist_to_xml([], _, []).

axiomlist_to_xml([Axiom | Axioms], Prefix, [element(ElName, [], Children) | ElList]) :-
	axiom_to_xml(Axiom, Prefix, ElName, Children),
	!,
	axiomlist_to_xml(Axioms, Prefix, ElList).


%% axiom_to_xml(+Axiom:term, +Prefix:atom, -ElName:atom, -Children:list) is det.
%
% Axiom
%
axiom_to_xml('SubClassOf'(CE1, CE2), Prefix, 'SubClassOf', [CE1x, CE2x]) :-
	!,
	ce_to_xml(CE1, Prefix, CE1x),
	ce_to_xml(CE2, Prefix, CE2x).

axiom_to_xml('ClassAssertion'(CE, I), Prefix, 'ClassAssertion', [CEx, Ix]) :-
	!,
	ce_to_xml(CE, Prefix, CEx),
	ie_to_xml(I, Prefix, Ix).

axiom_to_xml('ObjectPropertyAssertion'(OPE, I1, I2), Prefix, 'ObjectPropertyAssertion', [OPEx, I1x, I2x]) :-
	!,
	ope_to_xml(OPE, Prefix, OPEx),
	ie_to_xml(I1, Prefix, I1x),
	ie_to_xml(I2, Prefix, I2x).

axiom_to_xml('DataPropertyAssertion'(DPE, I, L), Prefix, 'DataPropertyAssertion', [DPEx, Ix, Lx]) :-
	!,
	dpe_to_xml(DPE, Prefix, DPEx),
	ie_to_xml(I, Prefix, Ix),
	l_to_xml(L, Lx).

axiom_to_xml('SubObjectPropertyOf'('ObjectPropertyChain'(E_list), OPE), Prefix, 'SubObjectPropertyOf', [element('ObjectPropertyChain', [], Ex_list), OPEx]) :-
	!,
	elist_to_xml(ope, E_list, Prefix, Ex_list),
	ope_to_xml(OPE, Prefix, OPEx).

axiom_to_xml('SubObjectPropertyOf'(OPE1, OPE2), Prefix, 'SubObjectPropertyOf', [OPE1x, OPE2x]) :-
	!,
	ope_to_xml(OPE1, Prefix, OPE1x),
	ope_to_xml(OPE2, Prefix, OPE2x).

axiom_to_xml('TransitiveObjectProperty'(OPE), Prefix, 'TransitiveObjectProperty', [OPEx]) :-
	!,
	ope_to_xml(OPE, Prefix, OPEx).

axiom_to_xml('ObjectPropertyDomain'(OPE, CE), Prefix, 'ObjectPropertyDomain', [OPEx, CEx]) :-
	!,
	ope_to_xml(OPE, Prefix, OPEx),
	ce_to_xml(CE, Prefix, CEx).

axiom_to_xml('ObjectPropertyRange'(OPE, CE), Prefix, 'ObjectPropertyRange', [OPEx, CEx]) :-
	!,
	ope_to_xml(OPE, Prefix, OPEx),
	ce_to_xml(CE, Prefix, CEx).

axiom_to_xml('DLSafeRule'('Body'(Atom_list1), 'Head'(Atom_list2)), Prefix, 'DLSafeRule', Ex_list) :-
	!,
	elist_to_xml(a, Atom_list1, Prefix, Atomx_list1),
	elist_to_xml(a, Atom_list2, Prefix, Atomx_list2),
	Ex_list = [
		element('Body', [], Atomx_list1),
		element('Head', [], Atomx_list2)
	].

axiom_to_xml(Axiom, Prefix, Name, List_x) :-
	axiom_is_owl_list(Axiom, Type, Name, List),
	!,
	elist_to_xml(Type, List, Prefix, List_x).

axiom_to_xml(IllegalArg, _, _, _) :-
	throw(error('Not an axiom', context(axiom_to_xml/3, IllegalArg))).


%% axiom_is_owl_list(?Term:term, ?Type:atom, ?Functor:atom, ?Argument:list)
%% ce_is_owl_list(?Term:term, ?Type:atom, ?Functor:atom, ?Argument:list)
%
% @param Type is one of {ce, ope, ie, ...}
%
axiom_is_owl_list('DisjointClasses'(List), ce, 'DisjointClasses', List).
axiom_is_owl_list('DisjointObjectProperties'(List), ope, 'DisjointObjectProperties', List).
axiom_is_owl_list('SameIndividual'(List), ie, 'SameIndividual', List).
axiom_is_owl_list('DifferentIndividuals'(List), ie, 'DifferentIndividuals', List).

ce_is_owl_list('ObjectIntersectionOf'(List), ce, 'ObjectIntersectionOf', List).
ce_is_owl_list('ObjectUnionOf'(List), ce, 'ObjectUnionOf', List).
ce_is_owl_list('ObjectOneOf'(List), ie, 'ObjectOneOf', List).


%% elist_to_xml(CE_list, Prefix, CEx_list),
%
% List of OWL/SWRL expressions
%
elist_to_xml(_Type, [], _Prefix, []).

elist_to_xml(Type, [CE | Tail], Prefix, [CEx | Tailx]) :-
	e_to_xml(Type, CE, Prefix, CEx),
	elist_to_xml(Type, Tail, Prefix, Tailx).


%% e_to_xml(ce, CE, Prefix, CEx)
e_to_xml(ce, E, Prefix, Ex) :- ce_to_xml(E, Prefix, Ex).
e_to_xml(ope, E, Prefix, Ex) :- ope_to_xml(E, Prefix, Ex).
e_to_xml(dpe, E, Prefix, Ex) :- dpe_to_xml(E, Prefix, Ex).
e_to_xml(ie, E, Prefix, Ex) :- ie_to_xml(E, Prefix, Ex).
e_to_xml(a, E, Prefix, Ex) :- a_to_xml(E, Prefix, Ex).
e_to_xml(v_or_l, E, Prefix, Ex) :- v_or_l_to_xml(E, Prefix, Ex).


%% ce_to_xml('ObjectSomeValuesFrom'(OPE, CE), _Prefix, 'Class'(CE))
%
% Class Expression
%
ce_to_xml(CE, Prefix, Xml) :-
	ce_is_owl_list(CE, Type, Name, List),
	!,
	elist_to_xml(Type, List, Prefix, List_x),
	get_xml(Name, List_x, Xml).

ce_to_xml('ObjectComplementOf'(CE), Prefix, Xml) :-
	!,
	ce_to_xml(CE, Prefix, CEx),
	get_xml('ObjectComplementOf', [CEx], Xml).

ce_to_xml('ObjectHasSelf'(OPE), Prefix, Xml) :-
	!,
	ope_to_xml(OPE, Prefix, OPEx),
	get_xml('ObjectHasSelf', [OPEx], Xml).

ce_to_xml('DataHasValue'(DPE, L), Prefix, Xml) :-
	!,
	dpe_to_xml(DPE, Prefix, DPEx),
	l_to_xml(L, Lx),
	get_xml('DataHasValue', [DPEx, Lx], Xml).

ce_to_xml('ObjectSomeValuesFrom'(OPE, CE), Prefix, Xml) :-
	!,
	ope_to_xml(OPE, Prefix, OPEx),
	ce_to_xml(CE, Prefix, CEx),
	get_xml('ObjectSomeValuesFrom', [OPEx, CEx], Xml).

ce_to_xml('ObjectMinCardinality'(N, OPE, CE), Prefix, Xml) :-
	!,
	number(N),
	ope_to_xml(OPE, Prefix, OPEx),
	ce_to_xml(CE, Prefix, CEx),
	get_xml('ObjectMinCardinality', [cardinality = N], [OPEx, CEx], Xml).

ce_to_xml('ObjectMaxCardinality'(N, OPE, CE), Prefix, Xml) :-
	!,
	number(N),
	ope_to_xml(OPE, Prefix, OPEx),
	ce_to_xml(CE, Prefix, CEx),
	get_xml('ObjectMaxCardinality', [cardinality = N], [OPEx, CEx], Xml).

ce_to_xml('ObjectExactCardinality'(N, OPE, CE), Prefix, Xml) :-
	!,
	number(N),
	ope_to_xml(OPE, Prefix, OPEx),
	ce_to_xml(CE, Prefix, CEx),
	get_xml('ObjectExactCardinality', [cardinality = N], [OPEx, CEx], Xml).

ce_to_xml(EntityRef, Prefix, Xml) :-
	entity_to_xml('Class', EntityRef, Prefix, Xml),
	!.

ce_to_xml(IllegalArg, _, _) :-
	throw(error('Not a class expression', context(ce_to_xml/3, IllegalArg))).


%% ope_to_xml(EntityRef, Prefix, Xml)
%
% Object Property Expression
%
ope_to_xml('ObjectInverseOf'(OPE), Prefix, Xml) :-
	ope_to_xml(OPE, Prefix, OPEx),
	get_xml('ObjectInverseOf', [OPEx], Xml).

ope_to_xml(EntityRef, Prefix, Xml) :-
	entity_to_xml('ObjectProperty', EntityRef, Prefix, Xml),
	!.

ope_to_xml(IllegalArg, _, _) :-
	throw(error('Not an object property expression', context(ope_to_xml/3, IllegalArg))).


%% dpe_to_xml(EntityRef, Prefix, Xml)
%
% Data Property Expression
%
dpe_to_xml(EntityRef, Prefix, Xml) :-
	entity_to_xml('DataProperty', EntityRef, Prefix, Xml),
	!.

dpe_to_xml(IllegalArg, _, _) :-
	throw(error('Not a data property expression', context(dpe_to_xml/3, IllegalArg))).


%% ie_to_xml(EntityRef, Prefix, Xml)
%
% Individual Expression (i.e. named or anonymous individual)
%
% BUG: anonymous individuals are currently converted into named
% individuals. In order to ouput them as anonymous, use instead:
% ie_to_xml(nodeID(Name), _, element('AnonymousIndividual', ['nodeID' = Name], [])).
%
ie_to_xml('nodeID'(NodeId), Prefix, Xml) :-
	!,
	atom_concat('Ind', NodeId, IName),
	entity_to_xml('NamedIndividual', '':IName, Prefix, Xml).

ie_to_xml(EntityRef, Prefix, Xml) :-
	entity_to_xml('NamedIndividual', EntityRef, Prefix, Xml),
	!.

ie_to_xml(IllegalArg, _, _) :-
	throw(error('Not an individual', context(ie_to_xml/3, IllegalArg))).


%% a_to_xml(+Atom, +Prefix, -Xml)
%
% SWRL Atoms
%
a_to_xml('ClassAtom'(CE, IArg), Prefix, Xml) :-
	ce_to_xml(CE, Prefix, CEx),
	v_or_ie_to_xml(IArg, Prefix, IArgx),
	get_xml('ClassAtom', [CEx, IArgx], Xml).

% a_to_xml('DataRangeAtom'(...), Prefix, Xml).

a_to_xml('ObjectPropertyAtom'(OPE, IArg1, IArg2), Prefix, Xml) :-
	ope_to_xml(OPE, Prefix, OPEx),
	v_or_ie_to_xml(IArg1, Prefix, IArg1x),
	v_or_ie_to_xml(IArg2, Prefix, IArg2x),
	get_xml('ObjectPropertyAtom', [OPEx, IArg1x, IArg2x], Xml).

a_to_xml('DataPropertyAtom'(DPE, IArg, DArg), Prefix, Xml) :-
	dpe_to_xml(DPE, Prefix, DPEx),
	v_or_ie_to_xml(IArg, Prefix, IArgx),
	v_or_l_to_xml(DArg, Prefix, DArgx),
	get_xml('DataPropertyAtom', [DPEx, IArgx, DArgx], Xml).

a_to_xml('BuiltInAtom'(Op, DArgList), Prefix, element('BuiltInAtom', ['IRI' = Iri], DArgList_x)) :-
	get_iri(Op, Prefix, Iri),
	elist_to_xml(v_or_l, DArgList, Prefix, DArgList_x).

a_to_xml('SameIndividualAtom'(IArg1, IArg2), Prefix, Xml) :-
	v_or_ie_to_xml(IArg1, Prefix, IArg1x),
	v_or_ie_to_xml(IArg2, Prefix, IArg2x),
	get_xml('SameIndividualAtom', [IArg1x, IArg2x], Xml).

a_to_xml('DifferentIndividualsAtom'(IArg1, IArg2), Prefix, Xml) :-
	v_or_ie_to_xml(IArg1, Prefix, IArg1x),
	v_or_ie_to_xml(IArg2, Prefix, IArg2x),
	get_xml('DifferentIndividualsAtom', [IArg1x, IArg2x], Xml).

a_to_xml(IllegalArg, _, _) :-
	throw(error('Not a SWRL atom', context(a_to_xml/3, IllegalArg))).


%% v_or_l_to_xml(+Variable_or_Literal, +Prefix, -Xml)
%
% Literal or SWRL Variable
%
v_or_l_to_xml(Literal, _Prefix, Ex) :-
	catch(l_to_xml(Literal, Ex), _, fail),
	!.

v_or_l_to_xml(Variable, Prefix, Ex) :-
	catch(v_to_xml(Variable, Prefix, Ex), _, fail),
	!.

v_or_l_to_xml(IllegalArg, _, _) :-
	throw(error('Not a variable nor a literal', context(v_or_l_to_xml/3, IllegalArg))).


%% v_or_ie_to_xml(+Individual_or_Variable, +Prefix, -Xml)
%
% Individual or SWRL Variable
%
v_or_ie_to_xml(Individual, Prefix, Ex) :-
	catch(ie_to_xml(Individual, Prefix, Ex), _, fail),
	!.

v_or_ie_to_xml(Variable, Prefix, Ex) :-
	catch(v_to_xml(Variable, Prefix, Ex), _, fail),
	!.

v_or_ie_to_xml(IllegalArg, _, _) :-
	throw(error('Not an individual nor a SWRL variable', context(v_or_ie_to_xml/3, IllegalArg))).


%% l_to_xml
%
% Literal
%
l_to_xml('^^'(Data, Datatype), element('Literal', ['datatypeIRI' = Datatype], [PCDATA])) :-
	!,
	datatype_pcdata_data(PCDATA, Datatype, Data).


%% v_to_xml(+Variable:term, +Prefix:atom, -VariableX:term)
%
% SWRL Variable
%
v_to_xml('Variable'(Iri), _, element('Variable', ['IRI' = Iri], [])).


%% entity_to_xml(Type, NS:Name, Prefix, element(Type, ['IRI' = Iri], [])) :-
%
entity_to_xml(Type, Name, Prefix, element(Type, ['IRI' = Iri], [])) :-
	get_iri(Name, Prefix, Iri).


%% get_xml(+Name, +Attr, +Children, -Xml)
%% get_xml(+Name, +Children, -Xml)
%
% Builds the SWI-Prolog's XML-term given the XML element name,
% the attributes, and child elements.
%
get_xml(El, Els, element(El, [], Els)).
get_xml(El, Attrs, Els, element(El, Attrs, Els)).


%% get_iri(+Name:term, +Prefix:atom, -Iri:atom) is det.
%
% If Name is an atom then considers it an IRI and returns it as it is.
% Otherwise expects that Name has the form ':'(NS,LocalName)
% and uses the default prefix, namespace abbreviation and the
% local name to create the IRI.
%
get_iri(Iri, _, Iri) :-
	atom(Iri).

get_iri(owl:'Thing', _, 'http://www.w3.org/2002/07/owl#Thing').
get_iri(owl:'Nothing', _, 'http://www.w3.org/2002/07/owl#Nothing').

get_iri(ace:'Universe', _, 'http://attempto.ifi.uzh.ch/ace#Universe').
get_iri(ace:'contain', _, 'http://attempto.ifi.uzh.ch/ace#contain').

get_iri('':Name, DefaultPrefix, Iri) :-
	atom_concat(DefaultPrefix, Name, Iri).

get_iri(NS:Name, _, Iri) :-
	prefix_map(NS, Prefix),
	atom_concat(Prefix, Name, Iri).


%% prefix_map(?PrefixName:atom, ?Prefix:atom)
%
%
prefix_map(owl, 'http://www.w3.org/2002/07/owl#').
prefix_map(ace, 'http://attempto.ifi.uzh.ch/ace#').
prefix_map(swrlb, 'http://www.w3.org/2003/11/swrlb#').


%% datatype_pcdata_data(+PCDATA:atom, +Datatype:atom, -Data:atomic) is det.
%
% The purpose of this rule is to convert some of the PCDATA
% into Prolog numbers. In case of failure, the PCDATA is returned
% as it is, i.e. it will be represented as an atom.
%
% Note that if the PCDATA (i.e. an atom) cannot be converted
% into a number then atom_number/2 throws an exception.
% This is caught at higher level (for the time being).
%
% @param PCDATA is a data value (atom)
% @param Datatype is an XML Schema datatype
% @param Data is a data value (atom or number)
%
datatype_pcdata_data(PCDATA, Datatype, Data) :-
	is_numbertype(Datatype),
	!,
	atom_number(PCDATA, Data).

datatype_pcdata_data(PCDATA, _, PCDATA).


%% is_numbertype(+Datatype:atom) is det.
%
% @param Datatype is an XML Schema datatype
%
is_numbertype('http://www.w3.org/2001/XMLSchema#int').
is_numbertype('http://www.w3.org/2001/XMLSchema#integer').
is_numbertype('http://www.w3.org/2001/XMLSchema#nonNegativeInteger').
is_numbertype('http://www.w3.org/2001/XMLSchema#float').
is_numbertype('http://www.w3.org/2001/XMLSchema#double').
is_numbertype('http://www.w3.org/2001/XMLSchema#short').
is_numbertype('http://www.w3.org/2001/XMLSchema#long').
