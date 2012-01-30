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


:- module(functionwords, [
	functionword/1,
	variable/1,
	rawnumber_number/2,
	propername_prefix/2,
	noun_prefix/2,
	verb_prefix/1,
	modif_prefix/1
	]).

/** <module> Function Words

This module stores the different kinds of function words.

@author Kaarel Kaljurand
@author Tobias Kuhn
@version 2008-06-26
*/


%% functionword(?FunctionWord) is det.
%

functionword(whose).
functionword(for).
functionword('There').
functionword(there).
functionword(and).
functionword(or).
functionword(not).
functionword(that).
functionword(than).
functionword(of).
functionword(s).
functionword('\'').
functionword('"').
functionword('/').
functionword('\\').
functionword('-').
functionword('+').
functionword('&').
functionword('*').
functionword('(').
functionword(')').
functionword('[').
functionword(']').
functionword('}').
functionword('{').
functionword('<').
functionword('=').
functionword('>').
functionword('.').
functionword('?').
functionword('!').
functionword(',').
functionword(':').
functionword('If').
functionword(if).
functionword(then).
functionword('such').
functionword('be').
functionword('isn').
functionword('aren').
functionword('doesn').
functionword('don').
functionword('provably').
functionword(more).
functionword(most).
functionword(least).
functionword(less).
functionword(but).
functionword(true).
functionword(false).
functionword(possible).
functionword(necessary).
functionword(recommended).
functionword(admissible).
functionword('-thing').
functionword('-body').
functionword('-one').
functionword('something').
functionword('somebody').
functionword('someone').
functionword('nothing').
functionword('nobody').
functionword('noone').
functionword('everything').
functionword('everybody').
functionword('everyone').
functionword('one').
functionword('A').
functionword('All').
functionword('An').
functionword('Are').
functionword('Can').
functionword('Do').
functionword('Does').
functionword('Each').
functionword('Every').
functionword('Exactly').
functionword('He').
functionword('Her').
functionword('His').
functionword('How').
functionword('Is').
functionword('It').
functionword('Its').
functionword('May').
functionword('Must').
functionword('No').
functionword('She').
functionword('Should').
functionword('Some').
functionword('The').
functionword('Their').
functionword('They').
functionword('What').
functionword('When').
functionword('Where').
functionword('Which').
functionword('Who').
functionword('Whose').
functionword(a).
functionword(all).
functionword(an).
functionword(are).
functionword(can).
functionword(do).
functionword(does).
functionword(each).
functionword(every).
functionword(exactly).
functionword(he).
functionword(her).
functionword(herself).
functionword(him).
functionword(himself).
functionword(his).
functionword(how).
functionword(is).
functionword(it).
functionword(its).
functionword(itself).
functionword(may).
functionword(must).
functionword(no).
functionword(she).
functionword(should).
functionword(some).
functionword(the).
functionword(their).
functionword(them).
functionword(themselves).
functionword(they).
functionword(what).
functionword(when).
functionword(where).
functionword(which).
functionword(who).
functionword(at).
functionword(by).
functionword(^).  % used interally to mark the beginning of a sentence
functionword('For').
functionword('At').
functionword('Less').
functionword('More').
functionword(you).
functionword('You').
functionword(your).
functionword('Your').
functionword(yourself).
functionword(yourselves).
functionword(to).  % e.g. "wants to"
functionword(own).  % e.g. "his own"
functionword(many).  % e.g. "how many"
functionword(much).  % e.g. "how much"


%% variable(+Word) is det.
%

variable(Word) :-
    atom(Word),
	atom_codes(Word, [First|Rest]),
	65 =< First,
	First =< 90,
	digits(Rest).


%% digits(+String) is det.
%

digits([]).

digits([D|Rest]) :-
	48 =< D,
	D =< 57,
	digits(Rest).


%% rawnumber_number(+RawNumber:term, -Number:integer) is det.
%
% @param RawNumber is either an integer or an English word denoting a small positive integer
% @param Number is an integer
%
% Only integers 0-12 are supported as words.

rawnumber_number(RawNumber, RawNumber) :-
	number(RawNumber).

rawnumber_number(null, 0).
rawnumber_number(zero, 0).
rawnumber_number(one, 1).
rawnumber_number(two, 2).
rawnumber_number(three, 3).
rawnumber_number(four, 4).
rawnumber_number(five, 5).
rawnumber_number(six, 6).
rawnumber_number(seven, 7).
rawnumber_number(eight, 8).
rawnumber_number(nine, 9).
rawnumber_number(ten, 10).
rawnumber_number(eleven, 11).
rawnumber_number(twelve, 12).
rawnumber_number(dozen, 12).

% Capitalized versions of the number words
% as numbers can also be used at the beginning of
% the sentences, e.g. 'Four men wait.'
rawnumber_number('Null', 0).
rawnumber_number('Zero', 0).
rawnumber_number('One', 1).
rawnumber_number('Two', 2).
rawnumber_number('Three', 3).
rawnumber_number('Four', 4).
rawnumber_number('Five', 5).
rawnumber_number('Six', 6).
rawnumber_number('Seven', 7).
rawnumber_number('Eight', 8).
rawnumber_number('Nine', 9).
rawnumber_number('Ten', 10).
rawnumber_number('Eleven', 11).
rawnumber_number('Twelve', 12).
rawnumber_number('Dozen', 12).


%% propername_prefix(+Prefix:atom, +Gender:atom, +Type:atom) is det.
%% noun_prefix(+Prefix:atom, +Gender:atom, +Type:atom) is det.
%% verb_prefix(+Prefix:atom, +Type:atom) is det.
%% modif_prefix(+Prefix:atom) is det.
%
% Definition of prefixes.
% Support for words which are not in the lexicon.
% Undefined words have to start with a prefix (e.g. `n' or `v'), e.g.
% ==
% A man v:backs-up the n:web-page of the n:pizza-delivery-service.
% ==
%
% Notes:
% * syntax disambiguates (and not morphology):
% the n:blah runs. AND the n:blah run. get different readings (singular vs plural respectively)
% the n:blah v:qwerty. (gets only singular reading, since this is arrived at first)
%
propername_prefix(pn, neutr).
propername_prefix(human, human).
propername_prefix(masc, masc).
propername_prefix(fem, fem).
propername_prefix(p, neutr).
propername_prefix(h, human).
propername_prefix(m, masc).
propername_prefix(f, fem).
propername_prefix(unknowncat, neutr).
propername_prefix(unknowncat, human).
propername_prefix(unknowncat, masc).
propername_prefix(unknowncat, fem).

noun_prefix(noun, neutr).
noun_prefix(human, human).
noun_prefix(masc, masc).
noun_prefix(fem, fem).
noun_prefix(n, neutr).
noun_prefix(h, human).
noun_prefix(m, masc).
noun_prefix(f, fem).
noun_prefix(unknowncat, neutr).
noun_prefix(unknowncat, human).
noun_prefix(unknowncat, masc).
noun_prefix(unknowncat, fem).

verb_prefix(verb).
verb_prefix(v).
verb_prefix(unknowncat).

modif_prefix(a).
modif_prefix(unknowncat).
