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


:- module(trees_to_ascii, [
		trees_to_ascii/2  % +SyntaxLists, -Ascii
	]).

/** <module> ASCII trees

This module creates ASCII graphics for syntax trees.

@author Tobias Kuhn
@version 2008-03-17
*/


%% trees_to_ascii(+SyntaxLists, -Ascii)
%
% Returns the trees described by the syntax list as an atom (ASCII graphics).
        
trees_to_ascii([], '').
        
trees_to_ascii([FirstAndOnly], Ascii) :-
	tree_to_ascii(FirstAndOnly, Ascii),
	!.

trees_to_ascii([First | Rest], Ascii) :-
	tree_to_ascii(First, AsciiFirst),
	trees_to_ascii(Rest, AsciiRest),
	format(atom(Ascii), '~w~n~w', [AsciiFirst, AsciiRest]).


%% tree_to_ascii(+SyntaxList, -Ascii)

tree_to_ascii(Tree, Ascii) :-
	retractall(char(_,_,_)),
	retractall(final_line(_)),
	depth(Tree, Depth),
	FinalLine is Depth * 2,
	assert(final_line(FinalLine)),
	draw_tree(Tree, 0, 0, _, _),
	get_ascii(0, Ascii).


%% char(+Line, +Pos, -Char)

:- dynamic(char/3).


%% final_line(-FinalLine)

:- dynamic(final_line/1).


%% get_ascii(+Line, -Ascii)

get_ascii(Line, Ascii) :-
	setof(P, C^char(Line,P,C), Ps),
	!,
	last(Ps, MaxPos),
	get_line(Line, 0, MaxPos, Atom),
	NewLine is Line + 1,
	get_ascii(NewLine, AsciiRest),
	atom_concat(Atom, '\n', AtomT),
	atom_concat(AtomT, AsciiRest, Ascii).

get_ascii(_, '').


%% get_line(+Line, +Pos, +End, -Ascii)

get_line(_, Pos, End, '') :-
	Pos > End,
	!.

get_line(Line, Pos, End, Ascii) :-
	( char(Line, Pos, Char) ; Char = ' ' ),
	!,
	NewPos is Pos + 1,
	get_line(Line, NewPos, End, AsciiRest),
	atom_concat(Char, AsciiRest, Ascii).


%% draw_tree(+Tree, +Line, +Start, -Head, -End)

draw_tree(Tree, Line, Start, Start, End) :-
	(Tree = [Atom] ; Tree = Atom),
	atomic(Atom),
	!,
	atom_chars(Atom, AtomChars),
	final_line(FinalLine),
	draw_vertical_line(Start, Line, FinalLine-1),
	draw_chars(FinalLine, Start, AtomChars),
	length(AtomChars, Length),
	End is Start + Length.

draw_tree([Parent|Children], Line, Start, Head, End) :-
	draw_children(Children, Line + 2, Start, ChildrenEnd, [FirstHead|HeadList]),
	last([FirstHead|HeadList], LastHead),
	draw_horizontal_line(Line + 1, FirstHead, LastHead),
	Head is (FirstHead + LastHead) // 2,
	atom_chars(Parent, ParentChars),
	draw_chars(Line, Head, ParentChars),
	draw_char(Line + 1, Head, '|'),
	length(ParentChars, Length),
	(ChildrenEnd > Head + Length -> End = ChildrenEnd ; End is Head + Length).


%% draw_children(+Children, +Line, +Start, -End, -HeadList)

draw_children([Child], Line, Start, End, [Head]) :-
	!,
	draw_tree(Child, Line, Start, Head, End).

draw_children([Child|Rest], Line, Start, End, [Head|HeadList]) :-
	draw_tree(Child, Line, Start, Head, TempPos),
	draw_children(Rest, Line, TempPos + 1, End, HeadList).


%% draw_chars(+Line, +Pos, -CharList)

draw_chars(_, _, []).

draw_chars(Line, Pos, [Char|Rest]) :-
	draw_char(Line, Pos, Char),
	NewPos is Pos + 1,
	draw_chars(Line, NewPos, Rest).


%% draw_horizontal_line(+Line, +Start, +End)

draw_horizontal_line(_, Start, End) :-
	Start > End,
	!.

draw_horizontal_line(Line, Start, End) :-
	draw_char(Line, Start, '_'),
	draw_horizontal_line(Line, Start + 1, End).


%% draw_vertical_line(+Pos, +LineStart, +LineEnd)

draw_vertical_line(_, LineStart, LineEnd) :-
	LineStart > LineEnd,
	!.

draw_vertical_line(Pos, LineStart, LineEnd) :-
	draw_char(LineStart, Pos, '|'),
	draw_vertical_line(Pos, LineStart + 1, LineEnd).


%% draw_char(+Line, +Pos, +Char)

draw_char(Line, Pos, Char) :-
	LineE is Line,
	PosE is Pos,
	asserta(char(LineE, PosE, Char)).


%% depth(+List, -Depth)

depth(Atom, 0) :-
	atomic(Atom).

depth([Atom], 0) :-
	atomic(Atom),
	!.

depth([H|T], D) :-
	depth(H, HD),
	depth(T, TD),
	(HD + 1 > TD -> D is HD + 1 ; D = TD).
