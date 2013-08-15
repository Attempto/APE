% This file is part of the Attempto Parsing Engine (APE).
% Copyright 2010-2013, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
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

/**
This program outputs

==
acetext_drs(Id, ACEText, DRS).
==

for every ACE text in the regression test set that passes the test.

Configuration: change user:file_search_path/2 (first two rules in this program)
to specify the path to APE and to the regression test set.


BUG: there is a lot of code overlap with test_ape.pl.


Usage:

==
swipl -f make_acetext_drs.pl -g main -t halt -q > acetext_drs.pl
==

@author Kaarel Kaljurand
@version 2011-07-21

*/

% We point to the directory where APE modules and the lexicons are located.
:- assert(user:file_search_path(ape, '..')).

% We point to the directory where the regression test set is located.
:- assert(user:file_search_path(rt, '.')).


:- use_module(ape(parser/ace_to_drs), [
		acetext_to_drs/5
	]).

:- use_module(ape(utils/are_equivalent), [
		are_equivalent/2
	]).

:- use_module(ape(utils/serialize_term), [
		serialize_term/1
	]).

% Consult the regression test set.
:- style_check(-singleton).
:- consult(rt(acetexts)).
:- style_check(+singleton).

% Everything which takes longer than 1 second is considered a bug.
time_limit(1).

%% main is det.
%
main :-
	time_limit(TimeLimit),
	catch(set_stream(user_output, encoding(utf8)), _, true),
	writeln(':- encoding(utf8).'),
	forall(
		text_drs_eval(0, Number, Text, DrsPre, _Syntax, _TestDate, _Author, _Comment),
		(
			remove_pn_conditions(DrsPre, Drs),
			run_test(TimeLimit, Number, Text, Drs)
		)
	).


% Removes the object conditions for proper names from the old DRSs of the test set.
% BUG: This should be done at a different place!
remove_pn_conditions(drs(DomIn,CondsIn), drs(DomOut,CondsOut)) :-
	exclude(is_named, CondsIn, CondsOut),
	exclude(ground, DomIn, DomOut).

is_named(object(named(Name), Name, named, _, _, _)-_).


run_test(TimeLimit, Number, Text, Drs) :-
	catch(
		call_with_time_limit(
			TimeLimit,
			(
				acetext_to_drs(Text, _, _, RealDrs, _),
				result(Drs, RealDrs, Result)
			)
		),
		CatchType,
		Result = CatchType
	),
	show_result(Result, Number, Text, RealDrs).


%% result(+Drs1:term, +Drs2:term, -ResultCode:atom) is det.
%
% Compares two DRSs and returns a ResultCode which is to be interpreted as follows:
%
% * =|0000|= - parsing fails as it should
% * =|0->#|= - parsing should fail but instead a non-empty DRS is produced
% * =|#->0|= - parsing should produce a non-empty DRS but instead it fails
% * =|####|= - parsing produces a non-empty DRS but it is different from what it should be
% * =|----|= - parsing produces a DRS that is equivalent to the one stored in the regression test set
%
% Note: it is important that Drs1 and Drs2 contain variables for discourse referents.
%
% @param Drs1 is Attempto DRS
% @param Drs2 is Attempto DRS
% @param ResultCode is one of {=|0000|=, =|0->#|=, =|#->0|=, =|####|=, =|----|=}
%
result(drs([], []), drs([], []), '0000') :- !.

result(drs([], []), Drs, '0->#') :- Drs \= drs([], []), !.

result(Drs, drs([], []), '#->0') :- Drs \= drs([], []), !.

result(Drs1, Drs2, '----') :-
	are_equivalent(Drs1, Drs2),
	!.

result(_, _, '####').


%
%
show_result('####', _Number, _Text, _Drs) :- !.
show_result('0->#', _Number, _Text, _Drs) :- !.
show_result('#->0', _Number, _Text, _Drs) :- !.

show_result(_, Number, Text, Drs) :-
	serialize_term(acetext_drs(Number, Text, Drs)), writeln('.').
