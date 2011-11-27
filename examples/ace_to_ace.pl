/**
This is an example that shows how to paraphrase ACE sentences as
(syntactically different) ACE sentences.
A simple Prolog commandline interface ('cli') is provided.

@author Kaarel Kaljurand
@version 2011-11-26

Usage examples:

==
?- t("Every man waits.").
[If there is a man X1 then the man X1 waits.]

?- t("Every man waits.", CoreAce).
CoreAce = ['If there is a man X1 then the man X1 waits.'].

?- tnp("Every man waits.", NpAce).
NpAce = ['Every man waits.'].

?- cli.
ACE to ACE> Every man waits.
[If there is a man X1 then the man X1 waits.]
==

*/

% We point to the directory where APE modules and the lexicons are located.
:- assert(user:file_search_path(ape, '..')).

:- use_module(ape('parser/ace_to_drs'), [
		acetext_to_drs/5
	]).

:- use_module(ape('logger/error_logger'), [
		clear_messages/0
	]).

:- use_module(ape('utils/drs_to_coreace'), [
		drs_to_coreace/2
	]).

:- use_module(ape('utils/drs_to_npace'), [
		drs_to_npace/2
	]).

% Import the lexicons
:- style_check(-singleton).
:- style_check(-discontiguous).
:- use_module(ape('lexicon/clex')).
:- use_module(ape('lexicon/ulex')).
:- style_check(+discontiguous).
:- style_check(+singleton).


%% t(+AceText:atom) is det.
%% t(+AceText:atom, -Paraphrase:list) is det.
%
% @param AceText is an ACE text (as an atom)
% @param Paraphrase is a paraphrase of ACE text (as a list of sentences)
%
t(AceText) :-
	t(AceText, Paraphrase),
	format("~w~n", [Paraphrase]).

t(AceText, Paraphrase) :-
	acetext_to_drs(AceText, _, _, Drs, _),
	drs_to_coreace(Drs, Paraphrase).

tnp(AceText, Paraphrase) :-
	acetext_to_drs(AceText, _, _, Drs, _),
	drs_to_npace(Drs, Paraphrase).

% Commandline interface
cli :-
	clear_messages,
	prompt(Old, 'ACE to ACE> '),
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
