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


:- module(sentence_failure, [
		get_all_sentence_errors/2, % +SentenceTokens, -ErrorTextList
		get_sentence_error_text/2  % +SentenceTokens, -ErrorText
	]).

:- use_module('../lexicon/lexicon_interface').

:- use_module('../lexicon/chars', [
		is_capitalized/1
	]).

:- use_module('../lexicon/is_in_lexicon', [
		is_functionword/1
	]).

:- use_module('../lexicon/illegalwords').

:- style_check(-singleton).
:- style_check(-discontiguous).
:- use_module('grammar.plp').
:- style_check(+discontiguous).
:- style_check(+singleton).

/** <module> Sentence Failure

@author Norbert E. Fuchs
@author Tobias Kuhn
@author Kaarel Kaljurand
@version 2011-07-18
*/

%% get_all_sentence_errors(+SentenceTokens:list, -ErrorTextList:list) is det.
%
% @param SentenceTokens is a list of tokens in the sentence
% @param ErrorTextList is a list of error messages
%
get_all_sentence_errors(SentenceTokens, ErrorTextList) :-  
	findall(ErrorText, get_sentence_error_text(SentenceTokens, ErrorText), ErrorTextListIntermediate),
	% remove duplicate messages
	list_to_set(ErrorTextListIntermediate, ErrorTextList). 


%% get_sentence_error_text(+SentenceTokens:list, -ErrorText:atom) is nondet.
%
% @param SentenceTokens is a list of tokens in the sentence
% @param ErrorText is an error message
%

% check for illegal words
get_sentence_error_text(SentenceTokens, ErrorText) :-
	member(IllegalWord, SentenceTokens),
	% BUG: add: \+ is_contentword(IllegalWord)
	% because maybe the user defined `any' as a contentword
	is_illegalword(IllegalWord, ErrorText).

% check for repetitions of tokens
get_sentence_error_text(SentenceTokens, ErrorText) :-
    append(_, [Token, '<>', Token|_], SentenceTokens),
	\+ is_repeatable(Token),
	with_output_to(atom(ErrorText), format("Token \'~w\' repeated.", [Token])).

% check for `there is' + proper name
get_sentence_error_text(SentenceTokens, ErrorText) :-
	append(_, [There, is, Token, '<>' | _], SentenceTokens),
	(
		There = there
	;
		There = 'There'
	),
	(
		Token = the
	;
		is_capitalized(Token)
	;
		pn_sg(Token, _, _)
	),
	with_output_to(atom(ErrorText), format("The construct \'there is\' + \'~w\' is not allowed.", [Token])).

% check for `there are' + proper name
get_sentence_error_text(SentenceTokens, ErrorText) :-
	append(_, [There, are, Token, '<>' | _], SentenceTokens),
	(
		There = there
	;
		There = 'There'
	),
	(
		Token = the
	;
		is_capitalized(Token)
	;
		pn_pl(Token, _, _)
	),
	with_output_to(atom(ErrorText), format("The construct \'there are\' + \'~w\' is not allowed.", [Token])).

% check for intransitive verb followed by that-subordination
% Example: John appears that Mary waits. (`appear' is an intransitive verb that is not transitive)
get_sentence_error_text(SentenceTokens, ErrorText) :-
	append(_, [Wordform, '<>', that | _], SentenceTokens),
	(
		iv_finsg(Wordform, Verb)
	;
		iv_infpl(Wordform, Verb)
	),
	with_output_to(atom(ErrorText), format("The intransitive verb \'~w\' cannot be followed by that-subordination. Use a transitive verb.", [Verb])).

get_sentence_error_text(SentenceTokens, 'The sentence contains \'then\' but not \'if\'.') :-
	member(then, SentenceTokens),
	\+ member(if, SentenceTokens),
	\+ member('If', SentenceTokens).

get_sentence_error_text(SentenceTokens, 'The sentence contains \'if\' but not \'then\'.') :-
	(
		member(if, SentenceTokens)
	;
		member('If', SentenceTokens)
	),	
	\+ member(then, SentenceTokens).


% check for illegal use of commas
% Note that this rule does not find all illegal uses of commas, e.g.:
% [...] ... , ... [...]
% {...} ... , ... {...}
% ,!
get_sentence_error_text(SentenceTokens, 'Commas must be immediately followed by \'and\' or \'or\', or must occur at specified positions in lists, sets and commands.') :-
	append(Front, [',', NextToken | Tail], SentenceTokens),
	% comma not immediately followed by 'and'
	NextToken \= and,
	% comma not immediately followed by 'or'
	NextToken \= or,
	% comma not followed by an exclamation mark (command)
	\+ member('!', Tail),
	% comma not between [ and ] (list)
	\+ (member('[', Front), member(']', Tail)),
	% comma not between { and } (set)
	\+ (member('{', Front), member('}', Tail)).


get_sentence_error_text(_, 'This is the first sentence that was not ACE. The sign <> indicates the position where parsing failed.').


%% is_repeatable(?Token) is nondet.
%
% @param Token is an ACE token that can be repeated
%
% Example: ((1+2)-3) = 0.
%
is_repeatable('{').
is_repeatable('}').
is_repeatable('(').
is_repeatable(')').
is_repeatable('[').
is_repeatable(']').
is_repeatable('"').
