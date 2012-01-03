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


:- module(tokens_to_sentences, [
		tokens_to_sentences/2,
		tokens_to_paragraphs/2
	]).


:- use_module('../lexicon/chars', [
		is_sentence_end_symbol/1
	]).


/** <module> APE Sentence splitter

Converts a flat list of tokens into a list of sentences,
each of which is a list of tokens. Sentences end with
one of the three symbols: '.', '?', and '!'.

For example, the following list of tokens:

==
[John, likes, Mary, ., Every, man, owns, a, car, .]
==

is converted into the following list of sentences

==
[[John, likes, Mary, .], [Every, man, owns, a, car, .]]
==

@author Kaarel Kaljurand
@author Tobias Kuhn
@version 2009-05-21

@bug should we generate error messages here (if token list does not end with ./?/!)

*/


%% tokens_to_sentences(+Tokens:list, -Sentences:list) is semidet.
%
% Succeeds if Tokens is a list of ACE sentences, in this case
% the sentences are returned. To succeed the token list must either
% be empty or end with a sentence end symbol. E.g. the following
% token list would cause a failure:
%
%==
%[a, b, c]
%==
%
% @param Tokens is a list of ACE tokens
% @param Sentences is a list of sentences (where a sentence is a list of ACE tokens)
%
tokens_to_sentences([], []).

tokens_to_sentences(Tokens, [[^|Sentence]|Sentences]) :-
	first_sentence(Tokens, Sentence, RestTokens),
	tokens_to_sentences(RestTokens, Sentences).


%% first_sentence(+Tokens:list, -Sentence:list, -RestTokens:list) is det.
%
% Note that the token list can contain variables (added by the guesser
% to match prefixed in the parser). Thus we have to make sure that a variable
% is not "mistaken" for a sentence end symbol.
%
% @param Tokens is a list of ACE tokens
% @param Sentence is an ACE sentence (a list of ACE tokens)
% @param RestTokens is a list of ACE tokens
%
first_sentence([SentenceEndSymbol | RestTokens], [SentenceEndSymbol], RestTokens) :-
	nonvar(SentenceEndSymbol),
	is_sentence_end_symbol(SentenceEndSymbol),
	!.

first_sentence([Token | RestTokens], RestSentence, RestTokens2) :-
	Token == '<p>',
	!,
	first_sentence(RestTokens, RestSentence, RestTokens2).

first_sentence([Token | RestTokens], [Token | RestSentence], RestTokens2) :-
	first_sentence(RestTokens, RestSentence, RestTokens2).


%% tokens_to_paragraphs(+Tokens, -Paragraphs).

tokens_to_paragraphs([], []).

tokens_to_paragraphs(Tokens, [Paragraph|Paragraphs]) :-
	first_paragraph(Tokens, Paragraph, RestTokens),
	tokens_to_paragraphs(RestTokens, Paragraphs).


%% first_paragraph(+Tokens, -Paragraph, -RestTokens).

first_paragraph([Token | RestTokens], [], RestTokens) :-
	Token == '<p>',
	!.

first_paragraph([Token | RestTokens], [Token | RestParagraph], RestTokens2) :-
	first_paragraph(RestTokens, RestParagraph, RestTokens2).

first_paragraph([], [], []).
