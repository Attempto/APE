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


:- module(drs_to_owlswrl, [
		drs_to_owlswrl/2,
		drs_to_owlswrl/4
	]).

:- use_module(drs_to_owlswrl_core, [
		condlist_to_dlquery/2,
		condition_oneof/3,
		condlist_axiomlist_with_cheat/3
	]).

:- use_module('../drs_to_drslist', [
		drs_to_drslist/2
	]).

:- use_module('../drs_to_sdrs', [
		drs_to_sdrs/2
	]).

:- use_module('../../logger/error_logger', [
		add_error_message/4,
		clear_messages/1
	]).

:- use_module(drs_to_owldrs, [
		drs_to_owldrs/2
	]).

:- use_module(transform_anonymous, [
		transform_anonymous/2
	]).


/** <module> Attempto DRS to OWL 2/SWRL translator

Translate an Attempto DRS into Web Ontology Language (OWL 2),
or if this fails then to Semantic Web Rule Language (SWRL).

If the translation fails then we search for errors by traversing
the respective structure (e.g. implication) again. Note that the error
capture is not completely implemented. Sometimes the translation simply
fails and no explanatory messages are asserted.

@author Kaarel Kaljurand
@version 2010-11-14
@license LGPLv3

TODO:

==
- general:
	- put "tricks" into a separate module
	- low priority: add a trick: if a DRS maps to a SWRL rule which uses sameAs/differentFrom,
	but replacing these with normal object properties would make the rule expressible
	directly in OWL (probably SubPropertyOf with property composition),
	then replace these with ace:equal/ace:differ-from (with equivalent 
	definitions) and express the rule in OWL syntax.

- allow:
	- URGENT: John likes at least 3 cars that Mary likes. (wrong translation)
	- URGENT: Which member of Attempto is a productive-developer? (currently fails)
	- URGENT: support: What are the countries that contain Paris?
	- Every man is a human and sees the human.
	- John's age is not 21.
	- John's age is 21 or is 22.
	- Every man owns something that is "abc". (because we do allow: Every man owns "abc".)
	- For everything X for every thing Y if X likes Y then X does not hate Y.
	- Every dog hates every cat. (MAYBE)
	- Mary eats some grass. (because we do allow: Everybody that eats some meat is a carnivore.)
	- Every lady's pets are nothing but cats. (instead to/in addition to: Every lady's pet is nothing but cats.)
	- Every lady is somebody whose pets are more than 3 cats. (instead/in addition to: Every lady is somebody whose pet is more than 3 cats.)
	- John likes more than 3 women that own a car.
	- Every man is himself. (currently generates SWRL, but we could generate nothing in this case)

- check:
	- if the syntax is according to the spec: DataProperty, DataValue, DataType
	- Are the following equivalent, if so then handle all of them (currently some are rejected):
	-- t(166, 'No city overlaps-with a city that is not the city.').
	-- t(167, 'Every city overlaps-with itself or does not overlap-with a city.').
	-- t(168, 'If a city overlaps-with something X then X is the city or X is not a city.').

- better error messages for:
	- No card is valid.
	- Every man does not have to cook a chicken.
	- Every singular is every singular. (from the log)
	- top-level negation
	- top-level disjunction

- RDF/XML (deprecated):
	- There is something X. If X likes Mary then John sees Bill. (fails to be translated to RDF/XML)
	- What borders itself? (otherwise OK, but fails to be translated into RDF/XML)

- improve:
	- implement namespaces support, i.e. each name should actually be a term ':'(NS, Name)
	- better support for anonymous individuals (don't numbervar the DRS, this would make things easier)

- seems to be fixed:
	- John owns a car. John is a man. What is it?
==
*/



%:- debug(d).
%:- debug(owldrs).
%:- debug(sentence).


% Operators used in the DRS.
:- op(400, fx, -).
:- op(500, xfx, =>).
:- op(500, xfx, v).


%% drs_to_owlswrl(+Drs:term, -Owl:term) is semidet.
%% drs_to_owlswrl(+Drs:term, +IRI:atom, -Owl:term) is semidet.
%% drs_to_owlswrl(+Drs:term, +IRI:atom, +Comment:atom, -Owl:term) is semidet.
%
% Converts an Attempto DRS into OWL 2/SWRL.
% In the beginning, the DRS is modified by drs_to_owldrs/2 in order to make
% the processing more straight-forward.
%
% @param Drs is an Attempto DRS
% @param IRI is a IRI relative to which all class names are to be interpreted
% @param Comment is a comment to be inserted into the resulting ontology (currently ignored)
% @param Owl is an OWL ontology in the form 'Ontology'(IRI, Axioms) where Axioms
% is a list of axioms that correspond to the DRS conditions, in OWL FSS (Prolog notation)

drs_to_owlswrl(Drs, Owl) :-
	ontology_iri(IRI),
	drs_to_owlswrl(Drs, IRI, Owl).

drs_to_owlswrl(Drs, IRI, Owl) :-
	drs_to_owlswrl(Drs, IRI, 'Ontology from an ACE text.', Owl).

drs_to_owlswrl(Drs, IRI, Comment, 'Ontology'(IRI, Axioms)) :-
	debug(sentence, "ACE: ~w~n", [Comment]),
	drs_to_drslist(Drs, DrsList),
	maplist(drs_to_owlswrl_x, DrsList, AxiomList),
	append(AxiomList, Axioms).

drs_to_owlswrl(_, _, _, 'Ontology'('', [])).


drs_to_owlswrl_x(Drs, Axioms) :-
	copy_term(Drs, DrsCopy),
	clear_messages(owl),
	drs_to_sdrs(DrsCopy, SDrsCopy),
	drs_to_owldrs(SDrsCopy, OwlDrs),
	numbervars(OwlDrs, 1, _),
	debug(owldrs, "OWL DRS: ~q~n", [OwlDrs]),
	drs_to_axioms(OwlDrs, Axioms),
	!.


% If the DRS corresponds to a DL-Query
drs_to_axioms(Drs, Axioms) :-
	process_question(Drs, Axioms).

% If the DRS corresponds to a set of OWL and/or SWRL axioms
drs_to_axioms(Drs, Axioms) :-
	findall(
		ref_oneof(X, OneOf),
		(
			member(Condition, Drs),
			condition_oneof(Condition, X, OneOf)
		),
		RefList
	),
	debug(sentence, "Toplevel: ~w~n", [RefList]),
	condlist_axiomlist_with_cheat(Drs, RefList, AL),
	transform_anonymous(AL, Axioms).


% Only a single question is accepted, but it
% can be preceded and followed by declarative sentences.
% Examples:
% John owns a car. Mary owns the car. What is it?
% John owns a car. Mary owns the car. What is it? Bill sees the car.
% John owns a car. Mary owns the car. What is it? Bill sees John.
process_question(Conds, AxiomList) :-
	select(question(QConds), Conds, RConds),
	!,
	append(QConds, RConds, FlatConds),
	debug(d, "~w~n", [FlatConds]),
	clear_messages(owl),
	catch(
		(
			condlist_to_dlquery(FlatConds, Class),
			AxiomList = ['SubClassOf'(Class, owl:'Thing')]
		),
		Catcher,
		(
			AxiomList = [],
			parse_exception(Catcher, Message, Lemma, SId/TId),
			add_error_message(owl, SId-TId, Lemma, Message)
		)
	).

parse_exception(error(Message, context(_Pred, query(_, Lemma)-Id)), Message, Lemma, Id) :- !.
parse_exception(error(Message, context(_Pred, _Arg)), Message, '', ''/'').


%% ontology_iri(?IRI:atom)
%
% @param IRI is the prefix for ACE words used as OWL names
%
ontology_iri('http://attempto.ifi.uzh.ch/ontologies/owlswrl/test').
