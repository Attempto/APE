/**
This is an example that shows how to convert ACE sentences into OWL/SWRL.
A simple Prolog commandline interface ('cli') is provided.

@author Kaarel Kaljurand
@version 2010-11-14

*/

% We point to the directory where APE modules and the lexicons are located.
:- assert(user:file_search_path(ape, '..')).

:- use_module(ape('parser/ace_to_drs'), [
		acetext_to_drs/5
	]).

:- use_module(ape('utils/drs_to_ascii'), [
		drs_to_ascii/2
	]).

:- use_module(ape('utils/owlswrl/drs_to_owlswrl'), [
		drs_to_owlswrl/4
	]).

% Import the lexicons
:- style_check(-singleton).
:- style_check(-discontiguous).
:- use_module(ape('lexicon/clex')).
:- use_module(ape('lexicon/ulex')).
:- style_check(+discontiguous).
:- style_check(+singleton).

:- use_module(ape('logger/error_logger'), [
		clear_messages/0,
		get_messages/1
	]).

:- use_module(ape('utils/owlswrl/owlswrl_to_fss'), [
		owlswrl_to_fss/1
	]).

:- use_module(ape('utils/owlswrl/owlswrl_to_xml'), [
		owlswrl_to_xml/2
	]).



t(AceText, Owl) :-
	clear_messages,
	acetext_to_drs(AceText, _, _, Drs, _),
	drs_to_owlswrl(Drs, test, 'Ontology from an ACE text.', Owl).

t(AceText) :-
	clear_messages,
	acetext_to_drs(AceText, _, _, Drs, _),
	drs_to_ascii(Drs, DrsAscii),
	format("~w~n", [DrsAscii]),
	drs_to_owlswrl(Drs, test, 'Ontology from an ACE text.', Owl),
	get_messages(Messages),
	format("Messages: ~w~n~n", [Messages]),
	owlswrl_to_fss(Owl), nl,
	owlswrl_to_xml(Owl, XML),
	xml_write(user, XML, [layout(true), indent(0), header(false), net(true)]),
	format("~n~n"),
	!.

t(_) :-
	format("Something failed.~n~n").


% Commandline interface
cli :-
	clear_messages,
	prompt(Old, 'ACE to OWL> '),
	read_text_from_commandline(AceText),
	prompt(_, Old),
	t(AceText),
	cli.

read_text_from_commandline([C | R]) :-
	get0(C),
	line_continues(C),
	!,
	read_text_from_commandline(R).

read_text_from_commandline([]).


multiline(false).
line_continues(C) :- C \== 10, C \== -1, !.
line_continues(C) :- multiline(true), C \== -1.
