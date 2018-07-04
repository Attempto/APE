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


:- module(owlfss_owlrdfxml, [
		owlfss_owlrdfxml/2
	]).


/** <module> OWL 2/SWRL Functional-Style Syntax to OWL 2/SWRL RDF/XML converter

@author Kaarel Kaljurand
@author Jean-Marc Vanel (jmv)
@version 2010-11-01

@deprecated

@tbd Cover all the OWL/SWRL axioms here, not only those that the DRS->OWL/SWRL generates.

@tbd Complete support for property chains.

@tbd Complete support for SWRL data properties, built-ins.
*/


%% owlfss_owlrdfxml(+Ontology:term, -RDFXML:term) is det.
%
% @param Ontology is an OWL/SWRL ontology in Functional-Style Syntax (Prolog notation)
% @param RDFXML is the OWL/SWRL ontology in RDF/XML (SWI-Prolog's XML notation)
%
owlfss_owlrdfxml('Ontology'(Name, AxiomList), RDFXML) :-

	atom_concat(Name, '#', NameHash),

	axiomlist_rdf(AxiomList, AxiomListRdf, DescriptionList),

	list_to_set(DescriptionList, DescriptionListSorted),

	findall(element('rdf:Description', ['rdf:about'=About], [
		element('rdf:type', ['rdf:resource'=Resource], [])
		]
	), member(about(About, Resource), DescriptionListSorted), DescriptionListRdf),


	append(AxiomListRdf, DescriptionListRdf, Everything),

	RDFXML = element('rdf:RDF', [
		'xml:base'=Name,
		xmlns=NameHash,
		'xmlns:owl'='http://www.w3.org/2002/07/owl#',
		'xmlns:owl11'='http://www.w3.org/2006/12/owl11#',
		'xmlns:swrl'='http://www.w3.org/2003/11/swrl#',
		'xmlns:swrlb'='http://www.w3.org/2003/11/swrlb#',
		'xmlns:rdf'='http://www.w3.org/1999/02/22-rdf-syntax-ns#',
		'xmlns:rdfs'='http://www.w3.org/2000/01/rdf-schema#'
		], [
		element('owl:Ontology', ['rdf:about'=''], []) | Everything
		]
	).


%% axiomlist_rdf(+AxiomList:list, -XML:xml, -DescriptionList:list) is det.
%
%
axiomlist_rdf([], [], []).

axiomlist_rdf([Axiom | AxiomList], [AxiomRdf | AxiomListRdf], DescriptionList) :-
	axiom_rdf(Axiom, AxiomRdf, D1),
	axiomlist_rdf(AxiomList, AxiomListRdf, D2),
	append(D1, D2, DescriptionList).


%% axiom_rdf(+Axiom:term, -Rdf:xml) is det.
%
axiom_rdf('Declaration'(Individual),
	element('owl:Thing', IndividualRdf, []),
	[]
) :-
	individual_rdf(Individual, about, IndividualRdf).


axiom_rdf('ClassAssertion'(Class, Individual),
	element('owl:Thing', IndividualRdf, [
		element('rdf:type', [], [ClassRdf])
		]
	),
	DescriptionList
) :-
	individual_rdf(Individual, about, IndividualRdf),
	class_rdf(Class, ClassRdf, DescriptionList).


% @bug cleanup the NS-stuff, we assume here that NS=''
axiom_rdf('ObjectPropertyAssertion'('ObjectProperty'(NS:NamedProperty), Individual1, Individual2),
	element('owl:Thing', Individual1Rdf, [
		element(NamedProperty, Individual2Rdf, [])
		]
	),
	[about(RdfNamedProperty, 'http://www.w3.org/2002/07/owl#ObjectProperty')]
) :-
	get_uri(NS:NamedProperty, RdfNamedProperty),
	individual_rdf(Individual1, about, Individual1Rdf),
	individual_rdf(Individual2, resource, Individual2Rdf).


% @bug cleanup the NS-stuff, we assume here that NS=''
axiom_rdf('DataPropertyAssertion'('DataProperty'(NS:NamedProperty), Individual, '^^'(Value, Type)),
	element('owl:Thing', IndividualRdf, [
		element(NamedProperty, ['rdf:datatype'=Type], [ValueRdf])
		]
	),
	[about(RdfNamedProperty, 'http://www.w3.org/2002/07/owl#DatatypeProperty')]
) :-
	get_uri(NS:NamedProperty, RdfNamedProperty),
	individual_rdf(Individual, about, IndividualRdf),
	datavalue_rdf(Value, ValueRdf).


axiom_rdf('SameIndividual'([Individual1, Individual2]),
	element('owl:Thing', Individual1Rdf, [
		element('owl:sameAs', Individual2Rdf, [])
		]
	),
	[]
) :-
	individual_rdf(Individual1, about, Individual1Rdf),
	individual_rdf(Individual2, resource, Individual2Rdf).


axiom_rdf('DifferentIndividuals'([Individual1, Individual2]),
	element('owl:Thing', Individual1Rdf, [
		element('owl:differentFrom', Individual2Rdf, [])
		]
	),
	[]
) :-
	individual_rdf(Individual1, about, Individual1Rdf),
	individual_rdf(Individual2, resource, Individual2Rdf).


axiom_rdf('SubClassOf'('Class'(owl:'Thing'), D),
	element('owl:Class', ['rdf:about'='http://www.w3.org/2002/07/owl#Thing'], [
		element('rdfs:subClassOf', [], [DRdf])
		]
	),
	DescriptionList
) :-
	!,
	class_rdf(D, DRdf, DescriptionList).


axiom_rdf('SubClassOf'('Class'(C), D),
	element('owl:Class', ['rdf:about'=RdfC], [
		element('rdfs:subClassOf', [], [DRdf])
		]
	),
	DescriptionList
) :-
	!,
	get_uri(C, RdfC),
	class_rdf(D, DRdf, DescriptionList).


axiom_rdf('SubClassOf'(C, D),
	element('owl:Class', [], [
		CRdf,
		element('rdfs:subClassOf', [], [DRdf])
		]
	),
	DescriptionList
) :-
	class_rdf(C, element('owl:Class', [], [CRdf]), D1),
	class_rdf(D, DRdf, D2),
	append(D1, D2, DescriptionList).

% BUG: incomplete chain support
axiom_rdf('SubObjectPropertyOf'('ObjectPropertyChain'(['ObjectProperty'(SubProperty1), 'ObjectProperty'(SubProperty2)]), 'ObjectProperty'(SuperProperty)),
	element('rdf:Description', [], [
		element('rdf:type', ['rdf:resource'='http://www.w3.org/1999/02/22-rdf-syntax-ns#List'], []),
		element('rdfs:subPropertyOf', ['rdf:resource'=RdfSuperProperty], []),
		element('rdf:first', ['rdf:resource'=RdfSubProperty1], []),
		element('rdf:rest', ['rdf:parseType'='Collection'], [
			element('rdf:Description', ['rdf:about'=RdfSubProperty2], [])
			]
		)
		]
	),
	[
	about(RdfSubProperty1, 'http://www.w3.org/2002/07/owl#ObjectProperty'),
	about(RdfSubProperty2, 'http://www.w3.org/2002/07/owl#ObjectProperty'),
	about(RdfSuperProperty, 'http://www.w3.org/2002/07/owl#ObjectProperty')
	]
) :-
	!,
	get_uri(SubProperty1, RdfSubProperty1),
	get_uri(SubProperty2, RdfSubProperty2),
	get_uri(SuperProperty, RdfSuperProperty).


axiom_rdf('SubObjectPropertyOf'('ObjectProperty'(SubProperty), 'ObjectInverseOf'('ObjectProperty'(SuperProperty))),
	element('owl:ObjectProperty', ['rdf:about'=RdfSubProperty], [
		element('rdfs:subPropertyOf', ['rdf:parseType'='Resource'], [
			element('rdf:type', ['rdf:resource'='http://www.w3.org/2002/07/owl#ObjectProperty'], []),
			element('owl:inverseOf', ['rdf:resource'=RdfSuperProperty], [])
			]
		)
		]
	),
	[about(RdfSuperProperty, 'http://www.w3.org/2002/07/owl#ObjectProperty')]
) :-
	!,
	get_uri(SubProperty, RdfSubProperty),
	get_uri(SuperProperty, RdfSuperProperty).


axiom_rdf('SubObjectPropertyOf'('ObjectProperty'(SubProperty), 'ObjectProperty'(SuperProperty)),
	element('owl:ObjectProperty', ['rdf:about'=RdfSubProperty], [
		element('rdfs:subPropertyOf', ['rdf:resource'=RdfSuperProperty], [])
		]
	),
	[about(RdfSuperProperty, 'http://www.w3.org/2002/07/owl#ObjectProperty')]
) :-
	get_uri(SubProperty, RdfSubProperty),
	get_uri(SuperProperty, RdfSuperProperty).


% BUG: inverse is transitive IFF the named property is transitive
axiom_rdf('TransitiveObjectProperty'('ObjectInverseOf'('ObjectProperty'(Property))),
	element('owl:ObjectProperty', ['rdf:about'=RdfProperty], [
		element('rdf:type', ['rdf:resource'='http://www.w3.org/2002/07/owl#TransitiveProperty'], [])
		]
	),
	[]
) :-
	get_uri(Property, RdfProperty).


axiom_rdf('TransitiveObjectProperty'('ObjectProperty'(Property)),
	element('owl:ObjectProperty', ['rdf:about'=RdfProperty], [
		element('rdf:type', ['rdf:resource'='http://www.w3.org/2002/07/owl#TransitiveProperty'], [])
		]
	),
	[]
) :-
	get_uri(Property, RdfProperty).


axiom_rdf('DisjointObjectProperties'(['ObjectProperty'(R), 'ObjectProperty'(S)]),
	element('owl:ObjectProperty', ['rdf:about'=RdfR], [
		element('owl11:disjointObjectProperties', ['rdf:resource'=RdfS], [])
		]
	),
	[about(RdfS, 'http://www.w3.org/2002/07/owl#ObjectProperty')]
) :-
	get_uri(R, RdfR),
	get_uri(S, RdfS).


% BUG: does not work
% TODO: implement also Domain and Range support
axiom_rdf('DisjointClasses'(['Class'(C), 'Class'(D)]),
	element('owl:Class', ['rdf:about'=RdfC], [
		element('owl:disjointWith', [], [RdfD])
		]
	),
	AboutList
) :-
	get_uri(C, RdfC),
	class_rdf(D, RdfD, AboutList).


% Finally, we can generate SWRL/RDF/XML from our own functional SWRL representation
axiom_rdf(
	'DLSafeRule'('Body'(DescriptionList1), 'Head'(DescriptionList2)),
	element('swrl:Imp', [], [
		element('swrl:body', [], AtomList1),
		element('swrl:head', [], AtomList2)
	]),
	AboutList
):-
	descriptionlist_atomlist(DescriptionList1, AtomList1, _, AboutList1),
	descriptionlist_atomlist(DescriptionList2, AtomList2, _, AboutList2),
	append(AboutList1, AboutList2, AboutList).


%% classlist_rdf(+ClassList:list, -Rdf:term, -DescriptionList:list) is det.
%
%
classlist_rdf([], [], []).

classlist_rdf([Class | ClassList], [ClassRdf | RdfList], DescriptionList) :-
	class_rdf(Class, ClassRdf, D1),
	classlist_rdf(ClassList, RdfList, D2),
	append(D1, D2, DescriptionList).


%% class_rdf(+Class:term, -Rdf:term, -DescriptionList:list) is det.
%
%
class_rdf('Class'(owl:'Thing'),
	element('owl:Class', ['rdf:about'='http://www.w3.org/2002/07/owl#Thing'], []),
	[]
) :- !.


class_rdf(
	'ObjectIntersectionOf'(ClassList),
	element('owl:Class', [], [element('owl:intersectionOf', ['rdf:parseType'='Collection'], RdfList)]),
	DescriptionList
) :-
	classlist_rdf(ClassList, RdfList, DescriptionList).


class_rdf(
	'ObjectUnionOf'(ClassList),
	element('owl:Class', [], [element('owl:unionOf', ['rdf:parseType'='Collection'], RdfList)]),
	DescriptionList
) :-
	classlist_rdf(ClassList, RdfList, DescriptionList).


class_rdf(
	'ObjectOneOf'(IndividualList),
	element('owl:Class', [], [element('owl:oneOf', ['rdf:parseType'='Collection'], RdfList)]),
	[]
) :-
	individuallist_rdf(IndividualList, RdfList).


class_rdf(
	'ObjectComplementOf'(Class),
	element('owl:Class', [], [element('owl:complementOf', [], [ClassRdf])]),
	DescriptionList
) :-
	class_rdf(Class, ClassRdf, DescriptionList).


class_rdf(
	'ObjectSomeValuesFrom'(Property, Class),
	element('owl:Restriction', [], [
		OnPropertyRdf,
		element('owl:someValuesFrom', [], [ClassRdf])
	]),
	[about(NamedProperty, 'http://www.w3.org/2002/07/owl#ObjectProperty') | DescriptionList]
) :-
	onproperty_rdf(Property, OnPropertyRdf, NamedProperty),
	class_rdf(Class, ClassRdf, DescriptionList).


class_rdf(
	'ObjectHasSelf'(Property),
	element('owl11:SelfRestriction', [], [OnPropertyRdf]),
	[about(NamedProperty, 'http://www.w3.org/2002/07/owl#ObjectProperty')]
) :-
	onproperty_rdf(Property, OnPropertyRdf, NamedProperty).


% Note: this is added for backwards compatibility (with OWL DL) reasons
class_rdf(
	'ObjectMinCardinality'(Number, Property, 'Class'(owl:'Thing')),
	element('owl:Restriction', [], [
		OnPropertyRdf,
		element('owl:minCardinality', ['rdf:datatype'='http://www.w3.org/2001/XMLSchema#nonNegativeInteger'], [NumberRdf])
	]),
	[about(NamedProperty, 'http://www.w3.org/2002/07/owl#ObjectProperty')]
) :-
	datavalue_rdf(Number, NumberRdf),
	onproperty_rdf(Property, OnPropertyRdf, NamedProperty).


class_rdf(
	'ObjectMinCardinality'(Number, Property, Class),
	element('owl:Restriction', [], [
		OnPropertyRdf,
		element('owl:minCardinality', ['rdf:datatype'='http://www.w3.org/2001/XMLSchema#nonNegativeInteger'], [NumberRdf]),
		element('owl11:onClass', [], [ClassRdf])
	]),
	[about(NamedProperty, 'http://www.w3.org/2002/07/owl#ObjectProperty') | DescriptionList]
) :-
	datavalue_rdf(Number, NumberRdf),
	onproperty_rdf(Property, OnPropertyRdf, NamedProperty),
	class_rdf(Class, ClassRdf, DescriptionList).


% Note: this is added for backwards compatibility (with OWL DL) reasons
class_rdf(
	'ObjectMaxCardinality'(Number, Property, 'Class'(owl:'Thing')),
	element('owl:Restriction', [], [
		OnPropertyRdf,
		element('owl:maxCardinality', ['rdf:datatype'='http://www.w3.org/2001/XMLSchema#nonNegativeInteger'], [NumberRdf])
	]),
	[about(NamedProperty, 'http://www.w3.org/2002/07/owl#ObjectProperty')]
) :-
	datavalue_rdf(Number, NumberRdf),
	onproperty_rdf(Property, OnPropertyRdf, NamedProperty).


class_rdf(
	'ObjectMaxCardinality'(Number, Property, Class),
	element('owl:Restriction', [], [
		OnPropertyRdf,
		element('owl:maxCardinality', ['rdf:datatype'='http://www.w3.org/2001/XMLSchema#nonNegativeInteger'], [NumberRdf]),
		element('owl11:onClass', [], [ClassRdf])
	]),
	[about(NamedProperty, 'http://www.w3.org/2002/07/owl#ObjectProperty') | DescriptionList]
) :-
	datavalue_rdf(Number, NumberRdf),
	onproperty_rdf(Property, OnPropertyRdf, NamedProperty),
	class_rdf(Class, ClassRdf, DescriptionList).


% Note: this is added for backwards compatibility (with OWL DL) reasons
class_rdf(
	'ObjectExactCardinality'(Number, Property, 'Class'(owl:'Thing')),
	element('owl:Restriction', [], [
		OnPropertyRdf,
		element('owl:cardinality', ['rdf:datatype'='http://www.w3.org/2001/XMLSchema#nonNegativeInteger'], [NumberRdf])
	]),
	[about(NamedProperty, 'http://www.w3.org/2002/07/owl#ObjectProperty')]
) :-
	datavalue_rdf(Number, NumberRdf),
	onproperty_rdf(Property, OnPropertyRdf, NamedProperty).


class_rdf(
	'ObjectExactCardinality'(Number, Property, Class),
	element('owl:Restriction', [], [
		OnPropertyRdf,
		element('owl:cardinality', ['rdf:datatype'='http://www.w3.org/2001/XMLSchema#nonNegativeInteger'], [NumberRdf]),
		element('owl11:onClass', [], [ClassRdf])
	]),
	[about(NamedProperty, 'http://www.w3.org/2002/07/owl#ObjectProperty') | DescriptionList]
) :-
	datavalue_rdf(Number, NumberRdf),
	onproperty_rdf(Property, OnPropertyRdf, NamedProperty),
	class_rdf(Class, ClassRdf, DescriptionList).


class_rdf(
	'ObjectHasValue'(Property, Individual),
	element('owl:Restriction', [], [
		OnPropertyRdf,
		element('owl:hasValue', IndividualRdf, [])
	]),
	[about(NamedProperty, 'http://www.w3.org/2002/07/owl#ObjectProperty')]
) :-
	onproperty_rdf(Property, OnPropertyRdf, NamedProperty),
	individual_rdf(Individual, resource, IndividualRdf).


class_rdf(
	'DataHasValue'(Property, '^^'(Value, Type)),
	element('owl:Restriction', [], [
		OnPropertyRdf,
		element('owl:hasValue', ['rdf:datatype'=Type], [ValueRdf])
	]),
	[about(NamedProperty, 'http://www.w3.org/2002/07/owl#DatatypeProperty')]
) :-
	onproperty_rdf(Property, OnPropertyRdf, NamedProperty),
	datavalue_rdf(Value, ValueRdf).


class_rdf('Class'(NamedClass), element('owl:Class', ['rdf:about'=RdfNamedClass], []), []) :-
	get_uri(NamedClass, RdfNamedClass).



%% onproperty_rdf(+PropertyExpression:term, -RdfXml:term, -NamedProperty:atom) is semidet.
%
% Note: data properties cannot have inverses
%
onproperty_rdf('ObjectInverseOf'('ObjectProperty'(NamedProperty)),
	element('owl:onProperty', ['rdf:parseType'='Resource'], [
		element('rdf:type', ['rdf:resource'='http://www.w3.org/2002/07/owl#ObjectProperty'], []),
		element('owl:inverseOf', ['rdf:resource'=RdfNamedProperty], [])
		]
	),
	RdfNamedProperty
) :-
	!,
	get_uri(NamedProperty, RdfNamedProperty).

onproperty_rdf('ObjectProperty'(NamedProperty), element('owl:onProperty', ['rdf:resource'=RdfNamedProperty], []), RdfNamedProperty) :-
	get_uri(NamedProperty, RdfNamedProperty).

onproperty_rdf('DataProperty'(NamedProperty), element('owl:onProperty', ['rdf:resource'=RdfNamedProperty], []), RdfNamedProperty) :-
	get_uri(NamedProperty, RdfNamedProperty).


%% individuallist_rdf(+IndividualList:list, -Rdf:term) is det.
%
%
individuallist_rdf([], []).

individuallist_rdf([Individual | IndividualList], [element('owl:Thing', RdfNamedIndividual, []) | RdfList]) :-
	individual_rdf(Individual, about, RdfNamedIndividual),
	individuallist_rdf(IndividualList, RdfList).


%% individual_rdf(+Individual:term, +Type:atom, -Rdf:term) is det.
%
%

% @deprecated
individual_rdf('AnonymousIndividual'('$VAR'(Number)), _, ['rdf:nodeID'=AnonymousIndividual]) :-
	!,
	concat_atom(['Ind', Number], AnonymousIndividual).

individual_rdf('AnonymousIndividual'(Number), _, ['rdf:nodeID'=AnonymousIndividual]) :-
	concat_atom(['Ind', Number], AnonymousIndividual).

individual_rdf('NamedIndividual'(Individual), about, ['rdf:about'=NamedIndividual]) :-
	get_uri(Individual, NamedIndividual).

individual_rdf('NamedIndividual'(Individual), resource, ['rdf:resource'=NamedIndividual]) :-
	get_uri(Individual, NamedIndividual).


%% descriptionlist_atomlist(+DescriptionList:list, -AtomList:list) is det.
%
%
descriptionlist_atomlist([], [], ['rdf:resource'='http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'], []).

descriptionlist_atomlist(
	[Description | DescriptionList],
	[
		element('swrl:AtomList', [], [
			element('rdf:first', [], [Atom]),
			element('rdf:rest', AttrList, AtomList)
			]
		)
	],
	[],
	AboutList
) :-
	description_atom(Description, Atom, AboutList1),
	descriptionlist_atomlist(DescriptionList, AtomList, AttrList, AboutList2),
	append(AboutList1, AboutList2, AboutList).


%% description_atom(+Description:term, -Atom:term, -AboutList:list) is det.
%
% @tbd data properties, built-ins
% 
description_atom(
	'ClassAtom'('Class'(Class), Argument),
	element('swrl:ClassAtom', [], [
		element('swrl:argument1', ['rdf:resource'=RdfArgument], []),
		element('swrl:classPredicate', ['rdf:resource'=RdfClass], [])
	]),
	[about(RdfClass, 'http://www.w3.org/2002/07/owl#Class') | AboutList]
) :-
	!,
	get_uri(Class, RdfClass),
	argument_rdf(Argument, RdfArgument, AboutList).

description_atom(
	'ObjectPropertyAtom'('ObjectProperty'(Property), Argument1, Argument2),
	element('swrl:IndividualPropertyAtom', [], [
		element('swrl:argument1', ['rdf:resource'=RdfArgument1], []),
		element('swrl:argument2', ['rdf:resource'=RdfArgument2], []),
		element('swrl:propertyPredicate', ['rdf:resource'=RdfProperty], [])
	]),
	[about(RdfProperty, 'http://www.w3.org/2002/07/owl#ObjectProperty') | AboutList]
) :-
	!,
	get_uri(Property, RdfProperty),
	argument_rdf(Argument1, RdfArgument1, AboutList1),
	argument_rdf(Argument2, RdfArgument2, AboutList2),
	append(AboutList1, AboutList2, AboutList).

% jmv
description_atom(
	'DataPropertyAtom'('DataProperty'(Property), Argument1, Argument2),
	element('swrl:IndividualPropertyAtom', [], [
		element('swrl:argument1', ['rdf:resource'=RdfArgument1], []),
		% TODO: case when Argument2 is an expression; maybe create an expression with =
		element('swrl:argument2', ['rdf:resource'=RdfArgument2], []),
		element('swrl:propertyPredicate', ['rdf:resource'=RdfProperty], [])
	]),
	[about(RdfProperty, 'http://www.w3.org/2002/07/owl#ObjectProperty') | AboutList]
) :-
	!,
	get_uri(Property, RdfProperty),
	argument_rdf(Argument1, RdfArgument1, AboutList1),
	argument_rdf(Argument2, RdfArgument2, AboutList2),
	append(AboutList1, AboutList2, AboutList).

description_atom(
	'SameIndividualAtom'(Argument1, Argument2),
	element('swrl:SameIndividualAtom', [], [
		element('swrl:argument1', ['rdf:resource'=RdfArgument1], []),
		element('swrl:argument2', ['rdf:resource'=RdfArgument2], [])
	]),
	AboutList
) :-
	!,
	argument_rdf(Argument1, RdfArgument1, AboutList1),
	argument_rdf(Argument2, RdfArgument2, AboutList2),
	append(AboutList1, AboutList2, AboutList).

description_atom(
	'DifferentIndividualsAtom'(Argument1, Argument2),
	element('swrl:DifferentIndividualsAtom', [], [
		element('swrl:argument1', ['rdf:resource'=RdfArgument1], []),
		element('swrl:argument2', ['rdf:resource'=RdfArgument2], [])
	]),
	AboutList
) :-
	!,
	argument_rdf(Argument1, RdfArgument1, AboutList1),
	argument_rdf(Argument2, RdfArgument2, AboutList2),
	append(AboutList1, AboutList2, AboutList).

description_atom(
	'ClassAtom'(ComplexClass, Argument),
	element('swrl:ClassAtom', [], [
		element('swrl:argument1', ['rdf:resource'=RdfArgument], []),
		element('swrl:classPredicate', [], [ComplexClassRdf])
	]),
	AboutList
) :-
	class_rdf(ComplexClass, ComplexClassRdf, AboutList1),
	argument_rdf(Argument, RdfArgument, AboutList2),
	append(AboutList1, AboutList2, AboutList).

description_atom(
	builtIn(Op, X1, X2),
	element('swrl:BuiltinAtom', [], [
		element('swrl:builtin', ['rdf:resource'=SWRL_OP], []),
		element('swrl:arguments', [], [
			element('rdf:Description', [], [
				element('rdf:first', ['rdf:resource'=RdfArgument1], []),
				element('rdf:rest', [], [
					element('rdf:Description', [], [
						element('rdf:first', ['rdf:resource'=RdfArgument2], []),
						element('rdf:rest', ['rdf:resource'='http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'], [])
					])
				])
			])
		])
	]),
	AboutList
) :-
	concat_atom(['http://www.w3.org/2003/11/swrlb#', Op], SWRL_OP),
	argument_rdf(X1, RdfArgument1, AboutList1),
	argument_rdf(X2, RdfArgument2, AboutList2),
	append([AboutList1, AboutList2], AboutList).


% added by jmv
description_atom(
	builtIn(Op, X1, X2, X3),
	element('swrl:BuiltinAtom', [], [
		element('swrl:builtin', ['rdf:resource'=SWRL_OP], []),
		element('swrl:arguments', [], [
			element('rdf:Description', [], [
				element('rdf:first', ['rdf:resource'=RdfArgument1], []),
				element('rdf:rest', [], [
					element('rdf:Description', [], [
						element('rdf:first', ['rdf:resource'=RdfArgument2], []),
						element('rdf:rest', [], [
							element('rdf:Description', [], [
								element('rdf:first', ['rdf:resource'=RdfArgument3], []),
								element('rdf:rest', ['rdf:resource'='http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'], [])
							])
						])
					])
				])
			])
		])
	]),
	AboutList
) :-
	concat_atom(['http://www.w3.org/2003/11/swrlb#', Op], SWRL_OP),
	argument_rdf(X1, RdfArgument1, AboutList1),
	argument_rdf(X2, RdfArgument2, AboutList2),
	argument_rdf(X3, RdfArgument3, AboutList3),
	append([AboutList1, AboutList2, AboutList3], AboutList).


%% argument_rdf(+VarOrInd:term, -VarOrIndName:atom, -About:list) is det.
%
%
% @deprecated
argument_rdf('Variable'('$VAR'(Variable)), RdfVariable, [about(RdfVariable, 'http://www.w3.org/2003/11/swrl#Variable')]) :-
	!,
	concat_atom(['#x', Variable], RdfVariable).

argument_rdf('Variable'(Variable), RdfVariable, [about(RdfVariable, 'http://www.w3.org/2003/11/swrl#Variable')]) :-
	concat_atom(['#x', Variable], RdfVariable).

argument_rdf('NamedIndividual'(Individual), RdfIndividual, []) :-
	get_uri(Individual, RdfIndividual).


%% datavalue_rdf(+Value:number, -ValueRdf:atom) is det.
%
% Converts a number into an atom.
%
datavalue_rdf(Value, ValueRdf) :-
	number(Value),
	!,
	atom_number(ValueRdf, Value).

datavalue_rdf(Value, Value).


%% get_uri(+Name:term, -RdfName:atom) is det.
%
% @bug we assume that 'owl' always means 'http://www.w3.org/2002/07/owl', etc.
%
get_uri(':'(owl, 'Thing'), 'http://www.w3.org/2002/07/owl#Thing') :-
	!.

get_uri(':'(ace, 'Universe'), 'http://attempto.ifi.uzh.ch/ace#Universe') :-
	!.

get_uri(':'(ace, 'contain'), 'http://attempto.ifi.uzh.ch/ace#contain') :-
	!.

get_uri(':'(NS, C), RdfC) :-
	concat_atom([NS, '#', C], RdfC).
