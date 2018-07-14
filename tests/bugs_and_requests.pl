/**
* Convert all bug reports / feature requests to HTML.
* Two types of HTML output are supported
*
* "linear" (default) is simple linear output where reports follow each other
* in a linear fashion
*
* "table" is a sortable HTML-table with columns for bug ID, report date, text, DRS, ...
*
* Usage:
*
* swipl -f bugs_and_requests.pl -g main -t halt > report.html
* swipl -f bugs_and_requests.pl -g "main(table)" -t halt > report.html
*
* @author Kaarel Kaljurand
* @version 2011-06-22
*
* @tbd Escape HTML special symbols (< and &)
*/


% We point to the directory where APE modules and the lexicons are located.
:- assert(user:file_search_path(ape, '../prolog')).

% We point to the directory where the regression test set is located.
:- assert(user:file_search_path(rt, '.')).


:- use_module(ape(utils/drs_to_html), [
		drs_to_html_nocontainer/2,
		footer/1
	]).


% Consult the regression test set.
:- style_check(-singleton).
:- consult(rt(acetexts)).
:- style_check(+singleton).

:- style_check(-atom).

main :-
	main(linear).

main(Type) :-
	current_stream(1, write, Stream),
	set_stream(Stream, encoding(utf8)),
	header('APE bugs and ACE feature requests', Header),
	footer(Footer),
	write(Header),
	write('<h1>APE bugs and ACE feature requests</h1>'),
	make_another_header(Type),
	forall(
		text_drs_eval(1, Id, Text, Drs, _Syntax, Date, Author, Comment),
		ignore((
			drs_to_html_nocontainer(Drs, HtmlDrs),
			display(Type, Id, Text, Date, Author, Comment, HtmlDrs)
		))
	),
	make_another_footer(Type),
	write(Footer).


display(linear, Id, Text, Date, Author, Comment, HtmlDrs) :-
	format('<hr/>~n<b>~d</b>~n<pre style="padding-left: 1em; border-left: 1px solid silver">~w</pre>~n<p>Comment: ~w</p>~n~w~n<p>Submitted: ~w by ~w</p>~n', [Id, Text, Comment, HtmlDrs, Date, Author]).

display(table, Id, Text, Date, Author, Comment, HtmlDrs) :-
	concat_atom([Year, Month, Day | _], '-', Date),
	format('<tr>
		<td>~d</td>
		<td>~w</td>
		<td>~w</td>
		<td>~w</td>
		<td>~w</td>
		<td>~w</td></tr>', [Id, Year-Month-Day, Text, Comment, HtmlDrs, Author]).

make_another_header(table) :-
	!,
	write('<p>Click on a column header to sort by that column.</p>
<table><thead><tr>
<th title="ID of the bug/request">ID</th>
<th title="Report date">Date</th>
<th title="ACE text">ACE text</th>
<th title="Comment">Comment</th>
<th title="DRS">DRS</th>
<th title="IP of the author of the report">Author</th>
</tr></thead><tbody>').

make_another_header(_).


make_another_footer(table) :-
	!,
	write('</tbody></table>').

make_another_footer(_).


%% header(+Title:atom, -Header:atom) is det.
%
% Generate an HTML-header.
%
% @param Title is the content for the title-element in the HTML-header
% @param Header is an HTML header
%
header(Title, Header) :-
	with_output_to(atom(Header), format('<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title>~w</title>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.6.1/jquery.min.js"></script>
<script type="text/javascript" src="http://autobahn.tablesorter.com/jquery.tablesorter.min.js"></script>
<script type="text/javascript">$(document).ready(function() 
	{ 
		$("table").tablesorter(); 
	} 
);</script>
<style type="text/css">
table { empty-cells: show; border-collapse: collapse }
div.dom { background-color: #ace; margin-bottom: 0.5em }
table.drs { font-family: monospace; padding: 0.4em 0.4em 0.4em 0.4em; border: 1px solid black; margin-bottom: 2em; b
ackground-color: #eee; border-collapse: collapse }
th, td { border: 1px solid black; padding: 0.3em 0.3em 0.3em 0.3em; vertical-align: top; text-align: left }
td.op { vertical-align: middle; font-size: 110%; border: none }
thead { border: 1px black solid; font-weight: bold; background-color: silver; font-variant: small-caps }
tbody tr:nth-child(even) td { background-color: #eff1f3 }
/* Styles for the tablesorter start */
th.headerSortUp { border-top: 4px solid green; } 
th.headerSortDown { border-bottom: 4px solid green; } 
th.header { } 
/* Styles for the tablesorter end */
</style>
</head>
<body>', [Title])).
