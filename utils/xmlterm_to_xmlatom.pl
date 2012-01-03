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


:- module(xmlterm_to_xmlatom, [
		xmlterm_to_xmlatom/2, % +XMLTerm, -XMLAtom
		xmlterm_to_xmlatom/3  % +XMLTerm, +Options, -XMLAtom
	]).

/** <module> XML term to XML atom converter

@author Tobias Kuhn
@author Kaarel Kaljurand
@version 2008-03-26
*/

%% xmlterm_to_xmlatom(+XMLTerm, -XMLAtom) is det.
%% xmlterm_to_xmlatom(+XMLTerm, +Options, -XMLAtom) is det.
%
% Transforms the term representation of an XML structure (using
% element/3) into an atom.
%
% @param XMLTerm is an XML document as SWI's XML term
% @param Options is a list of options for xml_write/3
% @param XMLAtom is an XML document as an atom
%
xmlterm_to_xmlatom(XMLTerm, XMLAtom) :-
	xmlterm_to_xmlatom(XMLTerm, [], XMLAtom).

xmlterm_to_xmlatom(XMLTerm, Options, XMLAtom) :-
	new_memory_file(MemHandle),
	open_memory_file(MemHandle, write, S),
	xml_write(S, XMLTerm, Options),
	close(S),
	memory_file_to_atom(MemHandle, XMLAtom).
