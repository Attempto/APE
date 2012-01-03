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


:- module(chars, [
	is_digit/1,
	is_capitalized/1,
	to_lowercase/2,
	is_lowercase/1,
	is_uppercase/1,
	is_letter/1,
	is_unused/1,
	is_sentence_end_symbol/1
	]).

/** <module> Characters

@author Kaarel Kaljurand
@author Tobias Kuhn
@version 2009-04-29

Note: SWI has char_type/2 which could be used instead of the predicates defined
in this module. But Sicstus does not seem to have anything similar.
Note also: char_type/2 does not seem to be good because it does not handle anything
beyond ASCII-7.
*/


%% is_capitalized(+Token:atom) is semidet.
%
% Succeeds if Token starts with an uppercase letter.
%
% @param Token is an ACE token
%
is_capitalized(Token) :-
	atom(Token),
	atom_codes(Token, [Code | _]),
	is_uppercase(Code).


%% to_lower_case(+TokenIn, -TokenOut)
%
% Transforms the first character into a lowercase character. The same token is returned if the first
% character is not an uppercase character.
%
to_lowercase(TokenIn, TokenOut) :-
    atom(TokenIn),
    atom_codes(TokenIn, [Code|CodesRest]),
    is_uppercase(Code),
    !,
    NewCode is Code + 32,
    atom_codes(TokenOut, [NewCode|CodesRest]).

to_lowercase(Token, Token).


%% is_digit(+Code:integer) is semidet.
%
% Succeeds if Code corresponds to an ASCII-7 digit symbol.
%
% @param Code is a character code
%
is_digit(Code) :-
	48 =< Code, Code =< 57.


%% is_lowercase(+Code:integer) is semidet.
%
% Succeeds iff Code corresponds to a lowercase letter.
%
% @param Code is a character code
%
% We also test lowercase letters from the upper half of latin1.

% 0x61 -- 0x7a (a..z)
is_lowercase(Code) :-
	97 =< Code, Code =< 122.

% 0xdf -- 0xf6
is_lowercase(Code) :-
	223 =< Code, Code =< 246.

% 0xf8 -- 0xff
is_lowercase(Code) :-
	248 =< Code, Code =< 255.


%% is_uppercase(+Code:integer) is semidet.
%
% Succeeds iff Code corresponds to an uppercase letter.
%
% @param Code is a character code
%
% We also test uppercase letters from the upper half of latin1.

% 0x41 -- 0x5a (A..Z)
is_uppercase(Code) :-
	65 =< Code, Code =< 90.

% 0xc0 -- 0xd6
is_uppercase(Code) :-
	192 =< Code, Code =< 214.

% 0xd8 -- 0xde
is_uppercase(Code) :-
	216 =< Code, Code =< 222.


%% is_letter(+Code:integer) is semidet.
%
% Succeeds iff Code corresponds to a lower- or uppercase letter.
%
% @param Code is a character code
%
is_letter(Code) :-
	is_lowercase(Code).

is_letter(Code) :-
	is_uppercase(Code).


%% is_unused(+Code:integer) is semidet.
%
% Succeeds iff Code corresponds to an unused character according to
% http://www.w3.org/MarkUp/html3/latin1.html
% In addition: tab, linefeed.
%
% @param Code is a character code
%
is_unused(Code) :-
	Code =< 31.

is_unused(Code) :-
	127 =< Code, Code =< 160.


%% is_sentence_end_symbol(?Token:atom) is semidet.
%
% Tests if Token is an ACE sentence end symbol.
%
% @param Token is a token.
%
is_sentence_end_symbol('.').
is_sentence_end_symbol('?').
is_sentence_end_symbol('!').
