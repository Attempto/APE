/**
* @title Test the duration of APE, Refres, Drace.
* @author Kaarel Kaljurand
* @version 2008-02-15
*
* echo "[test_duration]. test_duration." | swipl -q > testruns/duration_051013-2000.txt
*
* TODO:
* - recover from situations where the automatic generation of the DRS loops or craches
* - better formatting of results
* - benchmark all the main components separately
*/

% We point to the directory where APE modules and the lexicons are located.
:- assert(user:file_search_path(ape, '../prolog')).

% We point to the directory where the regression test set is located.
:- assert(user:file_search_path(rt, '.')).

:- use_module(ape('parser/ace_to_drs'), [
		acetext_to_drs/5
	]).

:- use_module(ape('parser/ape_utils'), [
		cpu_time/2
	]).

% Consult the regression test set.
:- style_check(-singleton).
:- consult(rt(acetexts)).
:- style_check(+singleton).

% Import the lexicons
:- style_check(-discontiguous).
:- consult(clex:clex_lexicon).
:- style_check(+discontiguous).

:- set_prolog_flag(float_format, '%.11g').

test_duration :-
	forall(
		text_drs_eval(0, Number, Text, DRS, _Syntax, _Date, _Author, _Comment),
		execute_test(Number, Text, DRS, _TimeOutLimit)
	).

execute_test(Number, Text, _Drs1, _TimeOutLimit) :-
	cpu_time(acetext_to_drs(Text, _, _, _Drs2, _Messages), Duration),
	display_result(Number, Text, Duration).

display_result(Number, Text, Duration) :-
	format('~f\t~d\t~w~n', [Duration, Number, Text]).
