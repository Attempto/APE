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


:- module(serialize_term, [
		serialize_term/1,            % +Term
		serialize_term/2,            % +Stream, +Term
		serialize_term_into_atom/2   % +Term, -Atom
	]).

/** <module> Term serializer

The purpose of this module is to provide a single official
predicate for serializing Prolog terms that the Attempto
tools produce (DRSs, token lists, OWL FSS, etc.).

Serialized terms can be stored in files, sent over HTTP, etc.,
so that they can be later read back into the exact same term.

Essentially we provide a customized version of Prolog built-ins
like write_canonical/[1,2] and writeq/[1,2].

@author Kaarel Kaljurand
@version 2009-03-20

TODO:

* test how \= (ACE non-equality) is serialized, maybe check character_escapes(bool, changeable)
* find a way to print terms which contain variables so that the output
has nice variable names (A vs _G123) but without the detour of numbervars,
maybe check: print, portray
* should we serialize singletons as '_'
* we make an extra effort to locally undefine some operators, there must be a cleaner way

*/

%% serialize_term(+Stream:stream, +Term:term) is det.
%% serialize_term(+Term:term) is det.
%
% @param Stream is the output stream
% @param Term is a term to be serialized
%
serialize_term(Stream, Term) :-
	numbervars(Term, 0, _),
	op(0, fy, -),
	op(0, fy, ~),
	op(0, xfx, =>),
	op(0, xfx, v),
	op(0, xfx, &),
	write_term(Stream, Term, [numbervars(true), quoted(true)]),
	fail ; true.


serialize_term(Term) :-
	numbervars(Term, 0, _),
	op(0, fy, -),
	op(0, fy, ~),
	op(0, xfx, =>),
	op(0, xfx, v),
	op(0, xfx, &),
	write_term(Term, [numbervars(true), quoted(true)]),
	fail ; true.


%% serialize_term_into_atom(+Term:term, -SerializedTerm:atom) is det.
%
% @param Term is a term to be serialized
% @param SerializedTerm is the term serialized as an atom
%
serialize_term_into_atom(Term, SerializedTerm) :-
	with_output_to(atom(SerializedTerm), serialize_term(Term)).
