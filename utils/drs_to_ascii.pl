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


:- module(drs_to_ascii, [
		drs_to_ascii/2, % +Drs, -DrsAscii
		display_drs/1   % +Drs
	]).


/** <module> DRS Pretty-printer

Creates a pretty print representation of a DRS.

@author Kaarel Kaljurand
@version 2008-03-14
*/


:- op(400, fx, -).
:- op(400, fx, ~).
:- op(500, xfx, =>).
:- op(500, xfx, v).


%% drs_to_ascii(+Drs:term, -DrsAscii:atom) is det.
%
% @param Drs is an Attempto DRS
% @param DrsAscii is an ASCII-graphics representation of the DRS
%
drs_to_ascii(Drs, DrsAscii) :-
	drs_to_drspp(Drs, PP),
	with_output_to(atom(DrsAscii), pp_to_ascii(PP)).


%% display_drs(+DRS)
%
% Makes a pretty print of a DRS and writes it out on the screen.
%
display_drs(Drs):-
	drs_to_drspp(Drs, PP),
	pp_to_ascii(PP).


%% drs_to_drspp(+Drs:term, -DrsPP:list) is det.
%
% @param Drs is an Attempto DRS
% @param DrsPP is the DRS with indentation information
%
drs_to_drspp(Drs, PP) :-
	copy_term(Drs, DrsCopy),
	numbervars(DrsCopy, 0, _),
	write_drs(DrsCopy, 0, PP).


%% pp_to_ascii(+PP:term) is det.
%
pp_to_ascii([]).

pp_to_ascii([(Indent, Term) | Rest]) :-
	with_output_to(atom(AtomTerm), format('~W', [Term, [numbervars(true)]])),
	PrettyIndent is Indent * 3, 
	writef('%r%w\n', [' ', PrettyIndent, AtomTerm]),
	pp_to_ascii(Rest).


%% write_drs(+DRS, +Indent, -PP)
%
% Creates a DRS in pretty print and indents it
% according to the value of Indent
%
write_drs(drs(Dom,[]), Indent, [(Indent,'No conditions') | PP]):-
	write_dom(Dom, Indent, PP),
	!.

write_drs(drs(Dom,Conds),Indent,PP):-
	write_dom(Dom,Indent,PP1),
	write_conds(Conds,Indent,PP2),
	append(PP1,PP2,PP),
	!.

write_drs(_Term,Indent,[(Indent,'ERROR')]).


%% write_dom(+Dom:list, +Indent:integer, -PP:list) is det.
%
%
write_dom(Dom, Indent, [(Indent, Dom)]).


%% write_conds(+ConditionsList,+Indent)
%
%
write_conds([],_Indent,[]).

write_conds([F|R],Indent,PP):-
	write_cond(F,Indent,PP1),
	write_conds(R,Indent,PP2),
	append(PP1,PP2,PP).


%% write_cond(+Condition,+Indent)
%
%
write_cond(Restr => Scope,Indent,PP):-
	!,
 	NewIndent is Indent+1,
 	write_drs(Restr,NewIndent,PP1),
 	write_drs(Scope,NewIndent,PP2),
	append(PP1,[(NewIndent,=>)|PP2],PP).

write_cond(Restr v Scope,Indent,PP):-
	!,
	NewIndent is Indent+1,
 	write_drs(Restr,NewIndent,PP1),
 	write_drs(Scope,NewIndent,PP2),
	append(PP1,[(NewIndent,v)|PP2],PP).

write_cond([FirstCond|Conds], Indent, PP) :-
	!,
	NewIndent is Indent+1,
	write_conds([FirstCond|Conds], NewIndent, PP).

write_cond(-DRS, Indent, [(NewIndent,'NOT') | PP]) :-
	!,
	NewIndent is Indent+1,
	write_drs(DRS,NewIndent,PP).

write_cond(~DRS, Indent, [(NewIndent,'NAF') | PP]) :-
	!,
	NewIndent is Indent+1,
	write_drs(DRS, NewIndent, PP).

write_cond(can(DRS), Indent, [(NewIndent, 'CAN') | PP]) :-
	!,
	NewIndent is Indent+1,
	write_drs(DRS, NewIndent, PP).

write_cond(must(DRS), Indent, [(NewIndent, 'MUST') | PP]) :-
	!,
	NewIndent is Indent+1,
	write_drs(DRS, NewIndent, PP).

write_cond(should(DRS), Indent, [(NewIndent, 'SHOULD') | PP]) :-
	!,
	NewIndent is Indent+1,
	write_drs(DRS, NewIndent, PP).

write_cond(may(DRS), Indent, [(NewIndent, 'MAY') | PP]) :-
	!,
	NewIndent is Indent+1,
	write_drs(DRS, NewIndent, PP).

write_cond(question(DRS), Indent, [(NewIndent, 'QUESTION') | PP]) :-
	!,
	NewIndent is Indent+1,
	write_drs(DRS, NewIndent, PP).

write_cond(command(DRS), Indent, [(NewIndent, 'COMMAND') | PP]) :-
	!,
	NewIndent is Indent+1,
	write_drs(DRS, NewIndent, PP).

write_cond(Label:DRS, Indent, [(NewIndent, Label) | PP]) :-
	!,
	NewIndent is Indent+1,
	write_drs(DRS, NewIndent, PP).

write_cond(drs(Dom,Cond),Indent,PP):-
	!,
	NewIndent is Indent+1,
	write_drs(drs(Dom,Cond),NewIndent,PP).

write_cond(Cond, Indent, [(Indent,Cond)]).
