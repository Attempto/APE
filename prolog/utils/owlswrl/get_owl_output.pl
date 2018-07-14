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


:- module(get_owl_output, [
		get_owl_output/6
	]).


/** <module> Interface for the OWL outputs

@author Kaarel Kaljurand
@version 2010-11-26

*/

:- use_module(drs_to_owlswrl).
:- use_module(owlfss_owlrdfxml).
:- use_module(owlswrl_to_xml).
:- use_module(owlswrl_to_fss).
:- use_module('../../logger/error_logger').

:- use_module('../xmlterm_to_xmlatom', [
		xmlterm_to_xmlatom/2
	]).

:- use_module('../serialize_term', [
		serialize_term_into_atom/2
	]).


%% get_owl_output(+OwlOutputType:atom, +Comment:atom, +Drs:term, +Uri:atom, -ContentType:atom, -Output:atom) is det.
%
% @param OwlOutputType is one of {owlfss, owlfsspp, owlxml}
% @param Comment is a comment to be included in the ontology (currently ignored)
% @param Drs is an Attempto DRS
% @param Uri is the name of the ontology to be created
% @param ContentType is the content type of the result, one of {text/xml, text/plain}
% @param Output is the output of the conversion to OWL
%
get_owl_output(owlrdf, Comment, Drs, Uri, 'text/xml', Output) :-
	ignore(drs_to_owlswrl:drs_to_owlswrl(Drs, Uri, Comment, OwlFss)),
	(
		get_messages_with_type(owl, [])
	->
		owlfss_owlrdfxml:owlfss_owlrdfxml(OwlFss, OwlRdfxml),
		xmlterm_to_xmlatom(OwlRdfxml, Output)
	;
		Output = ''
	).

get_owl_output(owlfss, Comment, Drs, Uri, 'text/plain', Output) :-
	ignore(drs_to_owlswrl:drs_to_owlswrl(Drs, Uri, Comment, OwlFss)),
	(
		get_messages_with_type(owl, [])
	->
		serialize_term_into_atom(OwlFss, Output)
	;
		Output = ''
	).

get_owl_output(owlfsspp, Comment, Drs, Uri, 'text/plain', Output) :-
	ignore(drs_to_owlswrl:drs_to_owlswrl(Drs, Uri, Comment, OwlFss)),
	(
		get_messages_with_type(owl, [])
	->
		owlswrl_to_fss:owlswrl_to_fss(OwlFss, Output)
	;
		Output = ''
	).

get_owl_output(owlxml, Comment, Drs, Uri, 'text/xml', Output) :-
	ignore(drs_to_owlswrl:drs_to_owlswrl(Drs, Uri, Comment, OwlFss)),
	(
		get_messages_with_type(owl, [])
	->
		owlswrl_to_xml:owlswrl_to_xml(OwlFss, OwlXml),
		xmlterm_to_xmlatom(OwlXml, Output)
	;
		Output = ''
	).
