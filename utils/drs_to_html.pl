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


:- module(drs_to_html, [
	drs_to_html/2,
	drs_to_html_nocontainer/2,
	header/2,
	header/1,
	footer/1
	]).

/** <module> Visualize a DRS using HTML and CSS

@author Kaarel Kaljurand
@version 2008-03-16

*/


% Operators used in the DRS.
:- op(400, fx, -).
:- op(400, fx, ~).
:- op(500, xfx, =>).
:- op(500, xfx, v).

:- style_check(-atom).


%% drs_to_html(+Drs:term, -DrsHtml:atom) is det.
%
% @param Drs is Attempto DRS
% @param DrsHtml is the DRS formatted in HTML (with HTML header and footer)
%
drs_to_html(Drs, DrsHtml) :-
	copy_term(Drs, DrsCopy),
	numbervars(DrsCopy, 0, _),
	DrsCopy = drs(Dom, Conds),

	header(Header),
	footer(Footer),
	with_output_to(atom(CondsHtml), format_condlist(Conds)),
	!,
	format(atom(DrsHtml), '~w<table class="drs"><tr><td><div class="dom">~W</div>~w</td></tr></table>~w~n',
		[Header, Dom, [numbervars(true)], CondsHtml, Footer]).


drs_to_html(_, 'ERROR').


%% drs_to_html_nocontainer(+Drs:term, -DrsHtml:atom) is det.
%
% @param Drs is Attempto DRS
% @param DrsHtml is the DRS formatted in HTML
%
drs_to_html_nocontainer(Drs, DrsHtml) :-
	copy_term(Drs, DrsCopy),
	numbervars(DrsCopy, 0, _),
	DrsCopy = drs(Dom, Conds),

	with_output_to(atom(CondsHtml), format_condlist(Conds)),
	!,
	format(atom(DrsHtml), '<table class="drs"><tr><td><div class="dom">~W</div>~w</td></tr></table>~n',
		[Dom, [numbervars(true)], CondsHtml]).

drs_to_html_nocontainer(_, 'ERROR').


%% format_condlist(+CondList:list) is det.
%
% @param CondList is a list of DRS conditions
%
format_condlist([]).

format_condlist([Condition | CondList]) :-
	format_condition(Condition),
	format_condlist(CondList).


%% format_condition(+Condition:term) is det.
%
% @param Condition is a DRS condition
%
format_condition(-drs(Dom, Conds)) :-
	!,
	format('<table><tr><td class="op">&not;</td><td><div class="dom">~W</div>', [Dom, [numbervars(true)]]),
	format_condlist(Conds),
	format('</td></tr></table>', []).

format_condition(~drs(Dom, Conds)) :-
	!,
	format('<table><tr><td class="op">&sim;</td><td><div class="dom">~W</div>', [Dom, [numbervars(true)]]),
	format_condlist(Conds),
	format('</td></tr></table>', []).

format_condition(can(drs(Dom, Conds))) :-
	!,
	format('<table><tr><td class="op">&diams;</td><td><div class="dom">~W</div>', [Dom, [numbervars(true)]]),
	format_condlist(Conds),
	format('</td></tr></table>', []).

format_condition(must(drs(Dom, Conds))) :-
	!,
	format('<table><tr><td class="op">&#x25a0;</td><td><div class="dom">~W</div>', [Dom, [numbervars(true)]]),
	format_condlist(Conds),
	format('</td></tr></table>', []).

format_condition(should(drs(Dom, Conds))) :-
	!,
	format('<table><tr><td class="op">SHOULD</td><td><div class="dom">~W</div>', [Dom, [numbervars(true)]]),
	format_condlist(Conds),
	format('</td></tr></table>', []).

format_condition(may(drs(Dom, Conds))) :-
	!,
	format('<table><tr><td class="op">MAY</td><td><div class="dom">~W</div>', [Dom, [numbervars(true)]]),
	format_condlist(Conds),
	format('</td></tr></table>', []).

format_condition(question(drs(Dom, Conds))) :-
	!,
	format('<table><tr><td class="op">QUESTION</td><td><div class="dom">~W</div>', [Dom, [numbervars(true)]]),
	format_condlist(Conds),
	format('</td></tr></table>', []).

format_condition(command(drs(Dom, Conds))) :-
	!,
	format('<table><tr><td class="op">COMMAND</td><td><div class="dom">~W</div>', [Dom, [numbervars(true)]]),
	format_condlist(Conds),
	format('</td></tr></table>', []).

format_condition(Conds) :-
	is_list(Conds),
	!,
	format('<table><tr><td>', []),
	format_condlist(Conds),
	format('</td></tr></table>', []).

format_condition(Label:drs(Dom, Conds)) :-
	!,
	format('<table><tr><td class="op">~W : </td><td><div class="dom">~W</div>', [Label, [numbervars(true)], Dom, [numbervars(true)]]),
	format_condlist(Conds),
	format('</td></tr></table>', []).

format_condition(drs(Dom1, Conds1) v drs(Dom2, Conds2)) :-
	!,
	format('<table><tr><td><div class="dom">~W</div>', [Dom1, [numbervars(true)]]),
	format_condlist(Conds1),
	format('</td><td class="op">&or;</td><td><div class="dom">~W</div>', [Dom2, [numbervars(true)]]),
	format_condlist(Conds2),
	format('</td></tr></table>', []).

format_condition(drs(Dom1, Conds1) => drs(Dom2, Conds2)) :-
	!,
	format('<table><tr><td><div class="dom">~W</div>', [Dom1, [numbervars(true)]]),
	format_condlist(Conds1),
	format('</td><td class="op">&rArr;</td><td><div class="dom">~W</div>', [Dom2, [numbervars(true)]]),
	format_condlist(Conds2),
	format('</td></tr></table>', []).

format_condition(Condition-Id) :-
	!,
	format('~W<br/>', [Condition-Id, [numbervars(true)]]).

format_condition(_, 'ERROR').


%% header(+Title:atom, -Header:atom) is det.
%% header(-Header:atom) is det.
%
% @param Title is the content for the title-element in the HTML-header
% @param Header is an HTML header
%
% Generate an HTML-header.
%
header(Header) :-
	header('', Header).

header(Title, Header) :-
	with_output_to(atom(Header), format('<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title>~w</title>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
<style type="text/css">
div.dom { background-color: #ace; margin-bottom: 0.5em }
table.drs { font-family: monospace; padding: 0.4em 0.4em 0.4em 0.4em; border: 1px solid black; margin-bottom: 2em; background-color: #eee; border-collapse: collapse }
td { vertical-align: top; padding: 0.3em 0.3em 0.3em 0.3em; border: 1px solid black }
td.op { vertical-align: middle; font-size: 110%; border: none }
</style>
</head>
<body>', [Title])).


%% footer(-Footer:atom) is det.
%
% @param Footer is an HTML footer
%
% Generate an HTML-footer.
%
footer('</body>\n</html>').
