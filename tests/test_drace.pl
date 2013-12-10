#!/opt/local/bin/swipl -f none -g test_drace(core) -t halt -s

/**
* @title Test Drace Core or Drace NP
* @author Kaarel Kaljurand
* @version 2009-04-08
*
* This program tests the module drs_to_coreace.pl (or drs_to_npace.pl)
* by applying the composition
*
* ace_to_drs o drs_to_coreace (or drs_to_npace)
*
* to all the correct DRSs in the regression testset.
*
* The test on a given DRS is correct if the composition acts as identity.
*
*
* Usage:
*
* echo "[test_drace]. test_drace(core)." | swipl > drace_test_results.txt
* echo "[test_drace]. test_drace(np)." | swipl > dracenp_test_results.txt
*
* and then compare the results by diff, e.g.:
*
* diff testruns/drace_test_results.txt drace_test_results.txt
* diff testruns/dracenp_test_results.txt dracenp_test_results.txt
*
* Note that the files in the directory testruns/ are under version control.
* They reflect a stable state of the verbalizer. Comparing against them helps
* to detect regression in the verbalizer.
*/

% We point to the directory where APE modules and the lexicons are located.
:- assert(user:file_search_path(ape, '../')).


:- compile(acetexts).

:- use_module(ape('parser/ace_to_drs'), [
		acetext_to_drs/5
	]).

:- use_module(ape('utils/morphgen'), [
		acesentencelist_pp/2
	]).

:- use_module(ape('utils/are_equivalent')).
:- use_module(ape('utils/drs_to_coreace')).
:- use_module(ape('utils/drs_to_npace')).
:- use_module(ape('utils/drs_to_ascii')).

% Import the lexicons
:- style_check(-discontiguous).
:- consult(clex:clex_lexicon).
:- style_check(+discontiguous).

:- set_prolog_flag(float_format, '%.11g').

time_limit(3).

test_drace(DraceType) :-
	time_limit(TimeLimit),
	set_stream(user_output, encoding(utf8)),
	forall(
		text_drs_eval(0, Number, Text, PreDrs, _Syntax, _Date, _Author, _Comment),
		(
			remove_pn_conditions(PreDrs, Drs),
			run_test(TimeLimit, DraceType, Number, Text, Drs)
		)
	).


% Removes the object conditions for proper names from the old DRSs of the test set.
% BUG: This should be done at a different place!
remove_pn_conditions(drs(DomIn,CondsIn), drs(DomOut,CondsOut)) :-
	exclude(is_named, CondsIn, CondsOut),
	exclude(ground, DomIn, DomOut).

is_named(object(named(Name), Name, named, _, _, _)-_).


run_test(TimeLimit, DraceType, Number, Text1, Drs1) :-
	catch(
	call_with_time_limit(
		TimeLimit,
		(
			execute_test(DraceType, Drs1, Text2, Drs2),
			compare_results(Drs1, Drs2, Result)
		)
	),
	CatchType,
	(
		Result = CatchType,
		Text2 = ''
	)),
	display_result(Result, Number, Text1, Text2).


execute_test(core, Drs1, AceText, Drs2) :-
	bigdrs_to_coreace(Drs1, AceSentenceList),
	get_acetext_or_error(AceSentenceList, AceText),
	acetext_to_drs(AceText, _, _, Drs2, _Messages).

execute_test(np, Drs1, AceText, Drs2) :-
	drs_to_npace(Drs1, AceSentenceList),
	get_acetext_or_error(AceSentenceList, AceText),
	acetext_to_drs(AceText, _, _, Drs2, _Messages).


% In case the DRS (correctly) fails, we do not report it as ----.
compare_results(drs([], []), _, 'ZERO') :-
	!.

compare_results(Drs1, Drs2, '----') :-
	are_equivalent(Drs1, Drs2, [ignore_sid(true)]),
	!.

compare_results(_, _, 'FAIL').


%% display_result(+Result:atom, +Number:integer, +Text1:atom, +Text2:atom)
%
%
display_result('ZERO', Number, Text1, _) :-
	!,
	format('~*t~d~6| [~w] ~w~n', [0'0, Number, 'ZERO', Text1]).

display_result('----', Number, Text1, Text2) :-
	Text1 = Text2,
	!,
	format('~*t~d~6| [~w] ~w~n', [0'0, Number, 'SAME', Text1]).

display_result(Result, Number, Text1, Text2) :-
	format('~*t~d~6| [~w] ~w --> ~w~n', [0'0, Number, Result, Text1, Text2]).


%%
%
%
get_acetext_or_error([], 'NOT IMPLEMENTED') :-
	!.

get_acetext_or_error(AceSentenceList, AceText) :-
	acesentencelist_pp(AceSentenceList, AceText).
