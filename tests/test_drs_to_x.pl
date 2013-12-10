#!/opt/local/bin/swipl -f none -g main -t halt -s

/**
*
* @title Test the conversion of DRS to other formats
* @author Kaarel Kaljurand
* @version 2009-06-24
*
* This is an attempt to write a more general testing framework
* for testing DRS->X converters on the APE regression test set.
* Currently it just tests the TPTP converter.
*
* TODO: Integrate result codes into the header.
*
*/

% We point to the directory where APE modules and the lexicons are located.
:- assert(user:file_search_path(ape, '../')).

:- compile(acetexts).

:- use_module(ape('parser/ace_to_drs'), [
		acetext_to_drs/5
	]).

:- use_module(ape('utils/drs_to_tptp'), [
		drs_to_tptp/2,
		drs_to_tptplist/2,
		tptp_pp/1,
		tptplist_pp/1
	]).

% Import the lexicons
:- style_check(-discontiguous).
:- consult(clex:clex_lexicon).
:- style_check(+discontiguous).

:- set_prolog_flag(float_format, '%.11g').

time_limit(1).
drs_converter(drs_to_tptplist, tptplist_pp).

main :-
	time_limit(TimeLimit),
	drs_converter(Converter, Pp),
	set_stream(user_output, encoding(utf8)),
	forall(
		text_drs_eval(0, Number, Text, DrsPre, _Syntax, _Date, _Author, _Comment),
		(
			remove_pn_conditions(DrsPre, Drs),
			run_test(TimeLimit, Converter, Pp, Number, Text, Drs)
		)
	).

% Removes the object conditions for proper names from the old DRSs of the test set.
% BUG: This should be done at a different place!
remove_pn_conditions(drs(DomIn,CondsIn), drs(DomOut,CondsOut)) :-
	exclude(is_named, CondsIn, CondsOut),
	exclude(ground, DomIn, DomOut).

is_named(object(named(Name), Name, named, _, _, _)-_).


run_test(TimeLimit, Converter, Pp, Number, Text, Drs) :-
	display_header(Number, Text),
	catch(
		call_with_time_limit(TimeLimit, apply_converter(Converter, Drs, Result)),
		Catcher,
		Result = Catcher
	),
	display_result(Pp, Result).


apply_converter(_Converter, drs(Dom, []), _) :-
	throw(error('DRS is empty', context(apply_converter/2, drs(Dom, [])))).

apply_converter(Converter, Drs, Result) :-
	call(Converter, Drs, Result),
	!.

apply_converter(_, Drs, _) :-
	throw(error('Converter failed', context(apply_converter/2, Drs))).


display_header(Number, Text) :-
	format('~*t~d~6| ~w~n', [0'0, Number, Text]).


display_result(_, Result) :-
	check_and_output_error(Result),
	!.

display_result(Pp, Result) :-
	call(Pp, Result),
	nl.

display_result(_, Result) :-
	format("ERROR\tWrong exception-term or non-printable result\t~w~n", [Result]).


check_and_output_error(time_limit_exceeded) :-
	format("ERROR\t~w~n", ['Time limit exceeded']).

check_and_output_error(error(Message, context(Pred, Arg))) :-
	format("ERROR\t~w\t~w\t~w~n", [Message, Pred, Arg]).
