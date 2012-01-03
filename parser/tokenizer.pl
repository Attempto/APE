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

:- module(tokenizer, [
		tokenize/2
	]).

:- use_module('../lexicon/chars', [
		is_letter/1,
		is_digit/1
	]).

:- use_module('../logger/error_logger', [
		add_error_message_once/4,
		add_warning_message_once/4
	]).

:- use_module('../utils/ace_niceace', [
        pronoun_split/2
    ]).


/** <module> APE Tokenizer

@author Kaarel Kaljurand
@author Tobias Kuhn
@version 2010-03-28

Comments:

* Strings (between double quotes) are tokenized into [", content, "].
This can be misleading, e.g. in the sentence: `"1" represents 1.'
the verb `represents' is reported as the 4th token.
One could instead produce the term string(content), this would
probably also fix the buggy handling of `Every dot is ".".'.

* Saxon Genitives are tokenized as [noun, ', s] and [nouns, ']

* Digits cannot start a token that contains other symbols than digits and dots.
Consider the following input string and its tokenization.

==
123man man123 123man123 man123man

[123, man, man123, 123, man123, man123man]
==

BUG: future work: add character counting to be able to report character offsets
together with every token.

Example:

?- tokenizer:codes_to_tokens("2men;can't like\"#@\"and:everything@.", T), writeq(T).
[2, men, can, not, like, '"#@"', and, :, every, '-thing', '.']

*/


%% tokenize(+ACEText:term, -Tokens:list) is det.
%
% Breaks the ACEText (either an atom, or a list of character codes) into
% a list of tokens (atoms or numbers).
% The input ACEText can either be an atom like 'this is an example' or a list of character
% codes like [116,104,105,115,32,105,...] (possibly written as "this is an example").
%
% Tokenization will never fail. In case something goes wrong (e.g. a string
% or comment is not closed) then an error message is asserted.
%
% @param ACEText is the input text, it is either an atom or a string
% @param Tokens is a list of tokens, i.e. the tokenization of the input text
%
tokenize([], []) :- !.

tokenize(Atom, Tokens) :-
	atom(Atom),
	!,
	atom_codes(Atom, Codes),
	codes_to_tokens(Codes, Tokens).

tokenize([C | Cs], Tokens) :-
	codes_to_tokens([C | Cs], Tokens).


%% codes_to_tokens(+Codes:list, -Tokens:list) is det.
%
% Maps the given list of character codes into a list of tokens.
% Performs two steps:
%
% 1. Merges certain code sequences into single tokens (atoms or numbers).
% 2. Modifies certain token sequences, e.g. [can, ', t] -> [can, not].
%
% @param Codes is a list of character codes
% @param Tokens is a list of tokens, i.e. the tokenization of the input text
%
codes_to_tokens(Codes, Tokens) :-
	get_atomics(Codes, Atomics),
	expand_contracted_forms(Atomics, Tokens).


%% get_atomics(+Codes:list, -Tokens:list) is det.
%
% BUG: We could also preserve the comment in the token list
% to be able to show better error messages.
%
get_atomics([], []).

% A paragraph break starts with \n followed by whitespace,
% including at least one more \n.
% If another character is encountered first then the paragraph is not broken.
get_atomics([10 | Cs], AllTokens) :-
	get_whitespace_and_comments(Cs, Remaining, Newline_Count),
	!,
	add_paragraph_break_symbol(Newline_Count, Ts, AllTokens),
	get_atomics(Remaining, Ts).

% Quoted string starts with " (34)
get_atomics([34 | Cs], AllTokens) :-
	!,
	get_string(Cs, Prefix, Remaining),
	string_to_token(Prefix, Ts, AllTokens),
	get_atomics(Remaining, Ts).

% Quoted word starts with ` (96)
get_atomics([96 | Cs], [T | Ts]) :-
	!,
	get_qword(Cs, Prefix, Remaining),
	atom_codes(T, Prefix),
	get_atomics(Remaining, Ts).

% Perl-style comment starts with # (35)
get_atomics([35 | Cs], Ts) :-
	!,
	get_perl_comment(Cs, _Prefix, Remaining),
	get_atomics(Remaining, Ts).

% C-style comment starts with /* (47, 42)
get_atomics([47, 42 | Cs], Ts) :-
	!,
	get_c_comment(Cs, _Prefix, Remaining),
	get_atomics(Remaining, Ts).

% Whitespace (\t, ' ', \r) is ignored (whitespace does not include \n)
get_atomics([C | Cs], Ts) :-
	is_whitespace(C),
	!,
	get_atomics(Cs, Ts).

% Positive number starts with a digit
get_atomics([C | Cs], [Number | Ts]) :-
	is_digit(C),
	!,
	get_number(Cs, Prefix, Remaining),
	number_codes(Number, [C | Prefix]),
	get_atomics(Remaining, Ts).

% Negative number starts with a hyphen (45) and then a digit,
% e.g. numbers like `-.5' are not allowed.
get_atomics([45, C | Cs], ['-', Number | Ts]) :-
	is_digit(C),
	!,
	get_number(Cs, Prefix, Remaining),
	number_codes(Number, [C | Prefix]),
	get_atomics(Remaining, Ts).

% Word starts (see is_word_char/1)
get_atomics([C | Cs], AllTokens) :-
	is_word_char(C),
	!,
	get_word(Cs, Prefix, Remaining),
	split_token([C | Prefix], Ts, AllTokens),
	get_atomics(Remaining, Ts).

% Special character maps to a one-character token
get_atomics([C | Cs], [T | Ts]) :-
	is_special(C, T),
	!,
	get_atomics(Cs, Ts).

% All other characters (e.g. Japanese) are ignored, with a warning message.
get_atomics([C | Cs], Ts) :-
	with_output_to(atom(CharCode), format("~c (0x~16r, ~10r)", [C, C, C])),
	add_warning_message_once(character, '', CharCode, 'Unknown character(s) ignored.'),
	get_atomics(Cs, Ts).


%% split_token(+Codes:list, +Ts:list, -FinalTokens:list) is det.
%
% Builds a token (atom) and splits it into two if needed,
% e.g. 'Everything' is split into 'Every' and '-thing'.
%
split_token(Codes, Ts, FinalTokens) :-
	atom_codes(A, Codes),
	(
		pronoun_split(A, (A1, A2))
	->
		FinalTokens = [A1, A2 | Ts]
	;
		FinalTokens = [A | Ts]
	).


%% is_whitespace(?Code)
%
% Note that newlines are handled elsewhere.
%
is_whitespace(32). % ' '
is_whitespace(9).  % \t
is_whitespace(13). % \r


%% is_word_char(?Code)
%
% Characters that are allowed in ACE words.
%
% - (hyphen)
is_word_char(45).

% _ (underscore)
is_word_char(95).

% $ (dollar)
is_word_char(36).

% degree sign
is_word_char(176).

% letters
is_word_char(Code) :-
	is_letter(Code).


%% is_special(?Code, ?Atom) is det.
%
% Characters that translate into one-character atoms.
%
% @param Code is character code
% @param Atom is a one-character atom that corresponds to the code

% period, question mark, exclamation mark
is_special(46, '.').
is_special(63, '?').
is_special(33, '!').

% hyphen
is_special(45, '-').

% colon (for prefixed words' support)
is_special(58, ':').

% apostroph (for Saxon genitive support)
is_special(39, '\'').

% slash (for him/her support)
is_special(47, '/').

% comma (for comma-and, comma-or support)
is_special(44, ',').

% plus sign
is_special(43, '+').

% star
is_special(42, '*').

% parentheses ()
is_special(40, '(').
is_special(41, ')').

% square brackets []
is_special(91, '[').
is_special(93, ']').

% curly bracktes {}
is_special(123, '{').
is_special(125, '}').

% < = > \
is_special(60, '<').
is_special(61, '=').
is_special(62, '>').
is_special(92, '\\').

% ampersand
is_special(38, '&').


%% get_string(+Codes:list, -Prefix:list, -RemainingCodes:list) is det.
%
% Consumes the sequence of characters within a quoted string,
% as well as the closing quotation mark.
% The backslash (\, 92) can be used to escape the following character,
% e.g. the quotation mark or a backslash.
%
get_string([], [], []) :-
	add_error_message_once(character, '', 'EOF', 'Every quoted string must end with ".').

get_string([34 | Cs], [], Cs) :- !.

get_string([92, C | Cs], [C | Prefix], Remaining) :-
	!,
	get_string(Cs, Prefix, Remaining).

get_string([C | Cs], [C | Prefix], Remaining) :-
	get_string(Cs, Prefix, Remaining).


%% get_qword(+Codes:list, -Prefix:list, -RemainingCodes:list) is det.
%
% Consumes the sequence of characters within a quoted word,
% as well as the closing backtick.
% The backslash (\, 92) can be used to escape the following character,
% e.g. the quotation mark or a backslash.
%
get_qword([], [], []) :-
	add_error_message_once(character, '', 'EOF', 'Every quoted word must end with `.').

get_qword([96 | Cs], [], Cs) :- !.

get_qword([92, C | Cs], [C | Prefix], Remaining) :-
	!,
	get_qword(Cs, Prefix, Remaining).

get_qword([C | Cs], [C | Prefix], Remaining) :-
	get_qword(Cs, Prefix, Remaining).


%% get_word(+Codes:list, -Prefix:list, -RemainingCodes:list) is det.
%
% Consumes the word consisting of word characters
% and/or digits.
%
get_word([], [], []).

get_word([C | Cs], [C | Prefix], Remaining) :-
	is_word_char(C),
	!,
	get_word(Cs, Prefix, Remaining).

get_word([C | Cs], [C | Prefix], Remaining) :-
	is_digit(C),
	!,
	get_word(Cs, Prefix, Remaining).

get_word([C | Cs], [], [C | Cs]).


%% get_number(+Codes:list, -Prefix:list, -RemainingCodes:list) is det.
%
% Consumes a number which is a sequence of digits containing
% at most one dot (46).
% The dot (if present) must be followed by a digit.
%
get_number(Codes, Prefix, RemainingCodes) :-
	get_number_x(Codes, zero, Prefix, RemainingCodes).


get_number_x([], _, [], []).

get_number_x([46, C | Cs], zero, [46, C | Prefix], Remaining) :-
	is_digit(C),
	!,
	get_number_x(Cs, more_than_zero, Prefix, Remaining).

get_number_x([C | Cs], Dot_Count, [C | Prefix], Remaining) :-
	is_digit(C),
	!,
	get_number_x(Cs, Dot_Count, Prefix, Remaining).

get_number_x([C | Cs], _, [], [C | Cs]).


%% get_perl_comment(+Codes:list, -Prefix:list, -RemainingCodes:list) is det.
%
% Consumes the Perl-style comment excluding the final newline.
%
get_perl_comment([], [], []) :-
	add_error_message_once(character, '', 'EOF', 'Every #-comment must end with the newline.').

get_perl_comment([10 | Cs], [], [10 | Cs]) :-
	!.

get_perl_comment([C | Cs], [C | Prefix], Remaining) :-
	get_perl_comment(Cs, Prefix, Remaining).


%% get_c_comment(+Codes:list, -Prefix:list, -RemainingCodes:list) is det.
%
% Consumes the C-style comment including the final */.
%
get_c_comment([], [], []) :-
	add_error_message_once(character, '', 'EOF', 'Every /*-comment must end with */.').

get_c_comment([42, 47 | Cs], [], Cs) :-
	!.

get_c_comment([C | Cs], [C | Prefix], Remaining) :-
	get_c_comment(Cs, Prefix, Remaining).


%% string_to_token
%
% The content of the string is bordered by quotation marks
% in the token list.
%
% BUG: I'd rather prefer a representation such as
% string('') or string(content).
%
string_to_token(Prefix, Ts, [T | Ts]) :-
	atom_codes(S, Prefix),
	concat_atom(['"', S, '"'], T).


%% add_paragraph_break_symbol
%
add_paragraph_break_symbol(at_least_two, Ts, ['<p>' | Ts]) :- !.
add_paragraph_break_symbol(_, Ts, Ts).


%% get_whitespace_and_comments
%
% Consuming whitespace and counting the newlines.
%
get_whitespace_and_comments(Cs, Remaining, Newline_Count) :-
	get_whitespace_and_comments(Cs, one, Remaining, Newline_Count).

get_whitespace_and_comments([], Newline_Count, [], Newline_Count).

get_whitespace_and_comments([10 | Cs], _, Remaining, Newline_Count) :-
	!,
	get_whitespace_and_comments(Cs, at_least_two, Remaining, Newline_Count).

get_whitespace_and_comments([C | Cs], Newline_Count, Remaining, Final_Newline_Count) :-
	is_whitespace(C),
	!,
	get_whitespace_and_comments(Cs, Newline_Count, Remaining, Final_Newline_Count).

/*
% Perl-style comment starts
get_whitespace_and_comments([35 | Cs], Newline_Count, Final_Remaining, Final_Newline_Count) :-
	!,
	get_perl_comment(Cs, _Prefix, Remaining),
	get_whitespace_and_comments(Remaining, Newline_Count, Final_Remaining, Final_Newline_Count).

% C-style comment starts
get_whitespace_and_comments([47, 42 | Cs], Newline_Count, Final_Remaining, Final_Newline_Count) :-
	!,
	get_c_comment(Cs, _Prefix, Remaining),
	get_whitespace_and_comments(Remaining, Newline_Count, Final_Remaining, Final_Newline_Count).
*/

get_whitespace_and_comments([C | Cs], Newline_Count, [C | Cs], Newline_Count).


%% expand_contracted_forms(+TokenListIn:list, -TokenListOut:list) is det.
%
% @bug: `cannot' could be instead handled during (pronoun) splitting
%
expand_contracted_forms([], []).

expand_contracted_forms(['No', one | RestIn], ['No', '-one' | RestOut]) :-
	!,
	expand_contracted_forms(RestIn, RestOut).

expand_contracted_forms([no, one | RestIn], [no, '-one' | RestOut]) :-
	!,
	expand_contracted_forms(RestIn, RestOut).

expand_contracted_forms([isn, '\'', t | RestIn], [is, not | RestOut]) :-
	!,
	expand_contracted_forms(RestIn, RestOut).

expand_contracted_forms([aren, '\'', t | RestIn], [are, not | RestOut]) :-
	!,
	expand_contracted_forms(RestIn, RestOut).

expand_contracted_forms([doesn, '\'', t | RestIn], [does, not | RestOut]) :-
	!,
	expand_contracted_forms(RestIn, RestOut).

expand_contracted_forms([don, '\'', t | RestIn], [do, not | RestOut]) :-
	!,
	expand_contracted_forms(RestIn, RestOut).

expand_contracted_forms([can, '\'', t | RestIn], [can, not | RestOut]) :-
	!,
	expand_contracted_forms(RestIn, RestOut).

expand_contracted_forms([cannot | RestIn], [can, not | RestOut]) :-
	!,
	expand_contracted_forms(RestIn, RestOut).

expand_contracted_forms([shouldn, '\'', t | RestIn], [should, not | RestOut]) :-
	!,
	expand_contracted_forms(RestIn, RestOut).

expand_contracted_forms([Token | TailIn], [Token | TailOut]) :-
	expand_contracted_forms(TailIn, TailOut).
