/**

This script outputs the APE regression test set positive testcases
separated by newlines. Each testcase is normalized first:

- the testcase is tokenized with the APE tokenizer
- the first character of every sentence is lowercased if it belongs to a function word
- tokens like `every-' and `thing' are glued
- the Saxon Genitive marker is glued to the previous token

TODO:

- handle paragraph breaks somehow

@author Kaarel Kaljurand
@version 2012-01-04

Usage example:

==
swipl -s output_tests.pl -t halt -g main
==

*/

% We point to the directory where APE modules and the lexicons are located.
:- assert(user:file_search_path(ape, '../prolog')).

:- use_module(ape('parser/tokenizer'), [
		tokenize/2
	]).

:- use_module(ape('parser/tokens_to_sentences'), [
		tokens_to_sentences/2
	]).

:- use_module(ape('utils/ace_niceace'), [
		atom_capitalize/2,
		pronoun_split/3
	]).

:- assert(user:file_search_path(ape, '..')).

:- consult(ape('tests/acetexts')).


main :-
	get_test(Text),
	text_to_sentences(Text, Sentences),
	process(Sentences, SentencesN),
	concat(SentencesN, Formatted),
	writeln(Formatted),
	fail ; true.


process -->
	maplist(sentence_lowercase),
	maplist(merge_some_tokens),
	maplist(concat).


%% text_to_sentences is det.
%
text_to_sentences(Text, Sentences) :-
	tokenize(Text, Tokens),
	tokens_to_sentences(Tokens, Sentences).


%% get_test
%
% Picks a test case that correctly (0) produces a non-empty DRS ([_ | _]).
%
get_test(Test) :-
	text_drs_eval(0, _Id, Test, drs(_, [_ | _]), _Syntax, _Date, _User, _Comment).


%% sentence_lowercase
%
sentence_lowercase([^, Upper | Rest], [Lower | Rest]) :-
	atom_capitalize(Lower, Upper).


%% merge_some_tokens
%
merge_some_tokens([], []).

merge_some_tokens([T, '\'', s | Rest], [Atom | RestMerged]) :-
	!,
	atomic_list_concat([T, '\'', s], Atom),
	merge_some_tokens(Rest, RestMerged).

merge_some_tokens([T, '\'' | Rest], [Atom | RestMerged]) :-
	!,
	atomic_list_concat([T, '\''], Atom),
	merge_some_tokens(Rest, RestMerged).

merge_some_tokens([T1, T2 | Rest], [Atom | RestMerged]) :-
	pronoun_split(Atom, lower, (T1, T2)),
	!,
	merge_some_tokens(Rest, RestMerged).

merge_some_tokens([H | Rest], [H | RestMerged]) :-
	merge_some_tokens(Rest, RestMerged).


%% concat
%
concat(Tokens, Atom) :-
	atomic_list_concat(Tokens, ' ', Atom).
