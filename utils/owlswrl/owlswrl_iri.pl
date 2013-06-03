% This file is part of the Attempto Parsing Engine (APE).
% Copyright 2013, Kaarel Kaljurand <kaljurand@gmail.com>.
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

:- module(owlswrl_iri, [
		iri_to_prefix/2,
		builtin_iri/2
	]).

/** <module> Some helper predicates related to IRIs

@author Kaarel Kaljurand
@version 2013-06-03

*/


%% iri_to_prefix(+Iri:atom, -Prefix:atom) is det.
%
% Attaches the #-character to the given IRI, unless
% it already has it.
%
iri_to_prefix(Iri, Iri) :-
	sub_atom(Iri, _, 1, 0, '#'),
	!.

iri_to_prefix(Iri, Prefix) :-
	atom_concat(Iri, '#', Prefix).


%% builtin_iri
%
% Lists all the builtin / hardcoded IRI prefixes,
% and their abbreviations.
%
builtin_iri(ace, 'http://attempto.ifi.uzh.ch/ace#').
