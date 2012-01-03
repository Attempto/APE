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


%---------------------------------------------------------------------------------------------------
%
%  The program corrects misspellings that are the result of:
%     - transposition of two letters 
%     - one letter extra
%     - one letter missing
%     - one letter wrong
%
%  Input:
%  spelling_corrector([yolandee,is,kooking,a,diner,fro,tri,persons],Candidates,NoSolutions).
%
%
%  Output (first solution):
%  Candidates  = [yolandee-yolande, kooking-cooking, diner-dinner, fro-for],
%  NoSolutions = [tri]
%
%
%  Output (all solutions):
%  Candidates  = [yolandee-[yolande], kooking-[cooking, looking], 
%                 diner-[dinner, dinar], fro-[for]],
%  NoSolutions = [tri]
%
%---------------------------------------------------------------------------------------------------

:- module(spellcheck, [
		spelling_corrector/3,
		damerau_rules/3
	]).

:- use_module(is_in_lexicon).


spelling_corrector(TokenList,Candidates,NoSolutions) :-
	lexicon_lookup(TokenList,MissingTokens),
	damerau_rules(MissingTokens,Candidates,NoSolutions).

% ------------------------------------------------------------------------------
%  lexicon_lookup/2 collects all tokens that are not in the lexicon.
% ------------------------------------------------------------------------------

lexicon_lookup([],[]).

lexicon_lookup([Token|Tokens],MissingTokens) :-
	lexicon(Token),
	lexicon_lookup(Tokens,MissingTokens).

lexicon_lookup([Token|Tokens],[Token|MissingTokens]) :-
	lexicon_lookup(Tokens,MissingTokens).

% ------------------------------------------------------------------------------
%  First solution:
%
%  For every missing token the first candidate (Token-NewToken) is generated
%  on account of the four Damerau rules. If the token is written completely
%  wrong so that no Damerau rule comes into question then the token is given 
%  back in the argument NoSolutions.
% ------------------------------------------------------------------------------
 
damerau_rules([],[],[]).
 
damerau_rules([Token|Tokens],[Token-NewToken|Candidates],NoSolutions) :-
	convert_token_char(Token,CharList),
	(
		transposition(CharList,NewCharList)
	;
		one_letter_extra(CharList,NewCharList)
	;
		one_letter_missing(CharList,NewCharList)
	;
		one_letter_wrong(CharList,NewCharList)
	),
	convert_token_char(NewToken,NewCharList),
	lexicon(NewToken),
	damerau_rules(Tokens,Candidates,NoSolutions).   

damerau_rules([Token|Tokens],Candidates,[Token|NoSolutions]) :-
	damerau_rules(Tokens,Candidates,NoSolutions).    
 
% ------------------------------------------------------------------------------
%  convert_token_char/2 converts either a token into a list of characters or 
%  vice versa.
% ------------------------------------------------------------------------------

convert_token_char(Token,CharList) :-
	var(CharList),
	atom_codes(Token,AsciiList),
	convert_ascii_char(AsciiList,CharList).

convert_token_char(Token,CharList) :-          
	var(Token),
	convert_ascii_char(AsciiList,CharList),
	atom_codes(Token,AsciiList).


convert_ascii_char([],[]).

convert_ascii_char([Ascii|Asciis],[Char|Chars]) :-
	atom_codes(Char,[Ascii]),
	convert_ascii_char(Asciis,Chars).

% ------------------------------------------------------------------------------
%  transposition/2 transposes two letters.
% ------------------------------------------------------------------------------

transposition([Char1,Char2|Chars],[Char2,Char1|Chars]).

transposition([Char|Chars1],[Char|Chars2]) :-
	transposition(Chars1,Chars2).

% ------------------------------------------------------------------------------
%  one_letter_extra/2 deletes one letter.  
% ------------------------------------------------------------------------------

one_letter_extra([_|Chars],Chars).

one_letter_extra([Char|Chars1],[Char|Chars2]) :-
	one_letter_extra(Chars1,Chars2).  

% ------------------------------------------------------------------------------
%  one_letter_missing/2 adds one letter.
% ------------------------------------------------------------------------------

one_letter_missing(Chars,[Char|Chars]) :-
	propose_letter(Char).

one_letter_missing([Char|Chars1],[Char|Chars2]) :-
	one_letter_missing(Chars1,Chars2).  

% ------------------------------------------------------------------------------
%  one_letter_wrong/2 exchanges one letter.
% ------------------------------------------------------------------------------

one_letter_wrong([_|Chars],[Char2|Chars]) :-
	propose_letter(Char2).

one_letter_wrong([Char|Chars1],[Char|Chars2]) :-
	one_letter_wrong(Chars1,Chars2).

% ------------------------------------------------------------------------------
%  propose_letter/1 provides a new letter.
% ------------------------------------------------------------------------------

/*
propose_letter(Char) :-
  member(Char,[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).
*/

propose_letter(a).
propose_letter(b).
propose_letter(c).
propose_letter(d).
propose_letter(e).
propose_letter(f).
propose_letter(g).
propose_letter(h).
propose_letter(i).
propose_letter(j).
propose_letter(k).
propose_letter(l).
propose_letter(m).
propose_letter(n).
propose_letter(o).
propose_letter(p).
propose_letter(q).
propose_letter(r).
propose_letter(s).
propose_letter(t).
propose_letter(u).
propose_letter(v).
propose_letter(w).
propose_letter(x).
propose_letter(y).
propose_letter(z).


% ------------------------------------------------------------------------------
%  Lexicon
% ------------------------------------------------------------------------------

lexicon(Word) :-
	is_in_lexicon:is_contentword(Word).
