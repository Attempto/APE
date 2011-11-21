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


:- module(simplify_axiom, [
		simplify_axiom/2
	]).


/** <module> Maps the given axiom to a syntactically simpler form

The given axiom is mapped to a syntactically simpler form
in order to achieve better compatibility with OWL tools
and OWL fragments (which are defined based on syntax).
In most cases the axiom is preserved as it is, we only target the
following forms:

- ObjectPropertyDomain
- ObjectPropertyRange
- DisjointClasses

@author Kaarel Kaljurand
@version 2010-12-11
@license LGPLv3

*/


%% simplify_axiom(+Axiom:term, -SimplerAxiom:term) is det.
%
% Note: rule order is important
%
% @param Axiom is an OWL axiom
% @param SimplerAxiom is the same axiom possibly in a simpler form
%

% disjoint classes
simplify_axiom(
	'SubClassOf'(CE1, 'ObjectComplementOf'(CE2)),
	'DisjointClasses'([CE1, CE2])
	) :- !.

% range
simplify_axiom(
	'SubClassOf'('ObjectIntersectionOf'([owl:'Thing', 'ObjectSomeValuesFrom'('ObjectInverseOf'(OPE), owl:'Thing')]), CE),
	'ObjectPropertyRange'(OPE, CE)
	) :- !.

simplify_axiom(
	'SubClassOf'('ObjectSomeValuesFrom'('ObjectInverseOf'(OPE), owl:'Thing'), CE),
	'ObjectPropertyRange'(OPE, CE)
	) :- !.

% domain
simplify_axiom(
	'SubClassOf'('ObjectIntersectionOf'([owl:'Thing', 'ObjectSomeValuesFrom'(OPE, owl:'Thing')]), CE),
	'ObjectPropertyDomain'(OPE, CE)
	) :- !.

simplify_axiom(
	'SubClassOf'('ObjectSomeValuesFrom'(OPE, owl:'Thing'), CE),
	'ObjectPropertyDomain'(OPE, CE)
	) :- !.

% Converts OWL 2 property axioms into a semantically equivalent yet
% syntactically simplified form, so that the resulting axiom is more
% backwards compatible with OWL 1. E.g. in case a property chain stands
% for a transitivity then we represent it with the TransitiveObjectProperty-axiom.
simplify_axiom(
	'SubObjectPropertyOf'('ObjectPropertyChain'(['ObjectInverseOf'(R)]), 'ObjectInverseOf'(S)),
	'SubObjectPropertyOf'(R, S)
	) :- !.

simplify_axiom(
	'SubObjectPropertyOf'('ObjectPropertyChain'(['ObjectInverseOf'(R)]), S),
	'SubObjectPropertyOf'(R, 'ObjectInverseOf'(S))
	) :- !.

simplify_axiom(
	'SubObjectPropertyOf'('ObjectPropertyChain'([R]), S),
	'SubObjectPropertyOf'(R, S)
	) :- !.

simplify_axiom(
	'SubObjectPropertyOf'('ObjectPropertyChain'([R, R]), R),
	'TransitiveObjectProperty'(R)
	) :- !.


% no change
simplify_axiom(Axiom, Axiom).
