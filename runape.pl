% This file is part of the Attempto Parsing Engine (APE).
% Copyright 2008-2013, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
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


:- use_module(utils/drs_to_drslist).

:- use_module(parser/ace_to_drs).
:- use_module(parser/ape_utils).
:- use_module(parser/tokenizer).
:- use_module(utils/morphgen, [
	acesentencelist_pp/2
	]).
:- use_module(utils/is_wellformed, [
	is_wellformed/1
	]).
:- use_module(utils/drs_to_ascii).
:- use_module(utils/trees_to_ascii).
:- use_module(utils/drs_to_ace, [
	drs_to_ace/2
	]).
:- use_module(logger/error_logger, [
	clear_messages/0,
	get_messages/1,
	is_error_message/4
	]).

:- set_prolog_flag(float_format, '%.11g').

% Import the lexicons
:- style_check(-singleton).
:- style_check(-discontiguous).
:- use_module(lexicon/clex).
:- use_module(lexicon/ulex).
:- style_check(+discontiguous).
:- style_check(+singleton).


%% ape is det.
%
%
ape :-
	format('
Attempto Parsing Engine for ACE 6.7~nCopyright 2008-2013, Attempto Group, University of Zurich
This program comes with ABSOLUTELY NO WARRANTY.
This is free software, and you are welcome to redistribute it under certain conditions.
Please visit http://attempto.ifi.uzh.ch for details.

For help, enter "help".~n~n', []),
	ape_input.


%% ape_input is det.
%
%
ape_input :-
	prompt(Old, 'APE> '),
	clear_messages,
	ape_utils:read_text_from_commandline(Text),
	prompt(_, Old),
	tokenizer:tokenize(Text, TokenList),
	!,
	call_parser(Text, TokenList).


%% call_parser(Text:atom, Tokens:list) is det.
%
% @param Text is an ACE text or a commandline command
% @param Tokens is a list of tokens (either of the ACE text or the command)
%
call_parser(_, [Quit]) :-
	(
			Quit = quit
	;
			Quit = q
	),
	!,
	nl, write('*** APE has been quit. ***'), nl.

call_parser(_, [help]) :-
	nl, write('APE Prolog-commandline client\'s commands:'),
	nl, write('   spy X            - spy predicate X'),
	nl, write('   gr               - (re)compile grammar and contentwords'),
	nl, write('   trace            - switch Prolog trace-mode on'),
	nl, write('   notrace          - switch Prolog trace-mode off'),
	nl, write('   help             - show this help'),
	nl, write('   quit/q           - quit APE interface (relaunch with "ape.")'),
	nl, nl,
	!,
	ape_input.

call_parser(_, [gr]) :-
	style_check(-discontiguous),
	style_check(-singleton),
	compile('parser/grammar.plp'),
	compile('parser/grammar_functionwords.plp'),
	compile('parser/grammar_contentwords.plp'),
	style_check(+discontiguous),
	style_check(+singleton),
	!,
	ape_input.

call_parser(_, [trace]) :-
	trace,
	!,
	ape_input.

call_parser(_, [notrace]) :-
	notrace,
	!,
	ape_input.

call_parser(_, [spy, X]) :-
	spy(X),
	!,
	ape_input.

call_parser(AceTextCodes, _) :-
	atom_codes(AceText, AceTextCodes),
	parse_and_format(AceText),
	!,
	ape_input.

call_parser(_, _) :-
	ape_input.


parse_and_format(AceText) :-
	ace_to_drs:aceparagraph_to_drs(AceText, Sentences, Syntaxtrees, UnresolvedDrs, Drs, Messages),
	format('~w~n~n', [Sentences]),
	format('Unresolved DRS:~n~n'),
	drs_to_ascii(UnresolvedDrs, UnresolvedDrsAscii),
	format('~w~n~n', [UnresolvedDrsAscii]),
	format('DRS:~n~n'),
	drs_to_ascii(Drs, DrsAscii),
	format('~w~n~n', [DrsAscii]),

	format('DRS List:~n~n'),
	drs_to_drslist:drs_to_drslist(Drs, DrsList),
	maplist(drs_to_ascii, DrsList, DrsAsciiList),
	maplist(writeln, DrsAsciiList),

	trees_to_ascii(Syntaxtrees, SyntaxtreesAscii),
	format('~w~n~n', [SyntaxtreesAscii]),
	(
		is_wellformed(Drs)
	->
		true
	;
		format("WARNING: The DRS is not well-formed!!!~n~n", [])
	),
	drs_to_ace(Drs, AceSentenceList),
	acesentencelist_pp(AceSentenceList, Paraphrase),
	format('~w~n~n', [Paraphrase]),
	print_messages(Messages).


%% print_messages(+MessageList:list) is det.
%
% @param MessageList is a list of messages
%
print_messages([]) :- nl.

print_messages([H | T]) :-
	format('~w~n~n', [H]),
	print_messages(T).

%%
%
%
%
make_ape :-
	working_directory(Old, parser),
	compile(fit_to_plp),
	working_directory(_, Old),
	make.


% Note: we load the interface at startup
:- ape.
