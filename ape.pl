% This file is part of the Attempto Parsing Engine (APE).
% Copyright 2008-2013, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
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


/* APE client for the Unix command-line.

@author Tobias Kuhn
@author Kaarel Kaljurand

Building the executable file (stack sizes are in kilobytes):

==
swipl -g "working_directory(_, 'parser'), [fit_to_plp], halt."
swipl -O -F none -g "[ape], qsave_program('ape.exe', [goal(ape), toplevel(halt), local(25000), global(50000)])." -t halt
==

Note that you can use smaller stack sizes if needed. It is known that global stack of 25600 was not
enough to parse 1687 sentences of the Ordnance Survey Hydrology ontology verbalization.


TODO:

- Do better checking of which arguments can be used together. E.g. if the user uses both -server
and -httpserver, what should ape.exe do then?

- All: Should we drop ulexreload? Otherwise all the interfaces should support it, and one would
need to add also "acetextreload" for reasons of uniformity.

- All: make timelimits configurable, currently they are hard-coded

- Socket: The socket server currently supports exactly these arguments that get_ape_results supports.
This is different from the other interfaces which also support other arguments (e.g. for fetching
files from URLs). More unification is needed.

- Java: assuming that it supports the file-parameter, how are the possible errors
communicated to Java?

- HTTP: Add logging

*/

:- use_module(get_ape_results, [
		get_ape_results/2,
		get_ape_results_timelimit/3,
		get_ape_results_timelimit/4
	]).

:- use_module('logger/error_logger').

% Default encoding used for opening files in text mode.
:- set_prolog_flag(encoding, utf8).

:- initialization on_signal(int, _, default).


%% argument(?Arg, -Value, -Desc)
%
% @tbd maybe we could describe the types more formally, so that they could be checked,
% e.g. the type of the solo value is one of [drs, drsxml, ...], which could be checked
% with memberchk/2.
%

argument('-text', '"TEXT"', 'The input ACE text. If neither -text nor -file is present then the ACE text is read from stdin.').
argument('-file', 'FILENAME', 'The name or URL of the input file containing the ACE text.').
argument('-ulextext', '"TEXT"', 'The user lexicon (taken from a string).').
argument('-ulexfile', 'FILENAME', 'The user lexicon (taken from a file or URL).').
argument('-solo', 'OUTPUT', 'Output just one output component. OUTPUT has to be one of {paraphrase,paraphrase1,paraphrase2,owlfss,owlfsspp,owlrdf,owlxml,ruleml,fol,pnf,tptp,tokens,syntax,syntaxpp,syntaxd,syntaxdpp,drs,drsxml,drspp,drshtml}.').
argument('-cinput', '', hidden).
argument('-cdrs', '', 'Output the DRS as a Prolog term.').
argument('-cdrsxml', '', 'Output the DRS in XML.').
argument('-cdrspp', '', 'Output the DRS in pretty-printed form in plain text.').
argument('-cdrshtml', '', 'Output the DRS in pretty-printed form in HTML.').
argument('-cparaphrase', '', 'Output a paraphrase which is a "best-effort" combination of paraphrase1 and paraphrase2.').
argument('-cparaphrase1', '', 'Output a paraphrase which uses full sentences instead of relative clauses.').
argument('-cparaphrase2', '', 'Output a paraphrase which uses relative clauses instead of full sentences.').
argument('-ctokens', '', 'Output tokens as a Prolog list of lists.').
argument('-csentences', '', 'Output sentences as a Prolog list.').
argument('-csyntax', '', 'Output simplified syntax trees as a Prolog list.').
argument('-csyntaxpp', '', 'Output simplified syntax trees in pretty-printed form.').
argument('-csyntaxd', '', 'Output plain syntax trees as a Prolog list (for debugging).').
argument('-csyntaxdpp', '', 'Output plain syntax trees in pretty-printed form (for debugging).').
argument('-cowlfss', '', 'Output OWL/SWRL in the Functional-Style Syntax representation (as Prolog term).').
argument('-cowlfsspp', '', 'Output OWL/SWRL in the Functional-Style Syntax representation (pretty-printed).').
argument('-cowlxml', '', 'Output OWL/SWRL in the XML representation.').
argument('-cowlrdf', '', 'Output OWL/SWRL in the RDF/XML representation. DEPRECATED').
argument('-cruleml', '', 'Output RuleML representation of the DRS.').
argument('-cfol', '', 'Output standard first-order logic representations (default form) of the DRS as a Prolog term.').
argument('-cpnf', '', 'Output standard first-order logic representations (prenex normal form) of the DRS as a Prolog term.').
argument('-ctptp', '', 'Output TPTP representation of the DRS.').
argument('-uri', 'URI', 'URI for the OWL outputs.').
argument('-noclex', '', 'Ignore the lexicon entries that are compiled into the executable.').
argument('-guess', '', 'Guess the word-class of unknown words.').
argument('-server', '', 'Launch a socket interface to APE at port 2766 (0xACE).').
argument('-httpserver', '', 'Launch an HTTP interface to APE at port 8000.').
argument('-port', 'NUMBER', 'Override the default port of either the socket or the HTTP interface.').
argument('-version', '', 'Shows version information.').
argument('-help', '', 'Shows this help page.').


%% ape is det.
%
% This is the default goal of =|ape.exe|=.
% Parses the command-line arguments, processes the arguments,
% if an ACE text is specified then parses the ACE text.
% Pretty-prints an error message in case an exception was thrown.
%
ape :-
	current_prolog_flag(argv, RawArgList),
	get_arglist(RawArgList, ArgList),
	catch(
		( arglist_namevaluelist(ArgList, InputList1), process_input(InputList1) ),
		Exception,
		format_error_for_terminal(Exception)
	).


%% get_arglist(+RawArgList, -ArgList)
%
% Returns the list of arguments.
% In SWI v6.6.0+ this can be achieved simply by:
%
%     get_arglist(ArgList) :-
%       current_prolog_flag(argv, ArgList).
%
% For backwards compatibility we assume that the argument
% list can contain '--', or the path to ape.exe before the
% first argument (which must start with '-').
%
get_arglist(RawArgList, ArgList) :-
    append(_, ['--'|ArgList], RawArgList),
    !.

% TODO: on which OS is this needed?
get_arglist([NonFlag|ArgList], ArgList) :-
	\+ atom_concat('-', _, NonFlag),
	!.

get_arglist(ArgList, ArgList).


%% process_input(+InputList:list) is det.
%
% @param InputList is a list of input parameters
%
process_input(InputList) :-
	( InputList = [] ; member(help=on, InputList) ),
	!,
	show_help.

process_input(InputList) :-
	memberchk(version=on, InputList),
	!,
	show_version.

process_input([server=on]) :-
	!,
	server.

process_input([server=on, port=Port]) :-
	!,
	server(Port).

process_input([port=Port, server=on]) :-
	!,
	server(Port).

process_input([httpserver=on]) :-
	!,
	http_server.

process_input([httpserver=on, port=Port]) :-
	!,
	http_server(Port).

process_input([port=Port, httpserver=on]) :-
	!,
	http_server(Port).

% @tbd Any other usage of server, httpserver, or port is illegal

process_input(InputList1) :-
	read_ulex(InputList1, InputList2),
	read_file(InputList2, InputList3),
	get_ape_results(InputList3, Content),
	set_utf8_encoding(user_output),
	writeln(Content).


%% show_help
%
show_help :-
	show_version,
	write('Copyright 2008-2013, Attempto Group, University of Zurich\n'),
	write('This program comes with ABSOLUTELY NO WARRANTY.\n'),
	write('This is free software, and you are welcome to redistribute it under certain conditions.\n'),
	write('Please visit http://attempto.ifi.uzh.ch for details.\n'),
	nl,
	write('Command-line arguments:\n'),
	argument(Arg, Value, Desc),
	\+ Desc = hidden,
	format('~w ~w~20|~w~n', [Arg, Value, Desc]),
	fail.

show_help.


%% show_version is det.
%
% Prints the version information.
%
show_version :-
	format("Attempto Parsing Engine for ACE 6.7, version ~w~n", ['6.7-131003']).


%% arglist_namevaluelist(+ArgList:list, -NameValueList:list) is det.
%
% @param ArgList is a list of arguments
% @param NameValueList is a list of ArgumentName=ArgumentValue pairs

arglist_namevaluelist([], []).

arglist_namevaluelist([Arg|Tail1], [Name=on|Tail2]) :-
    argument(Arg, '', _),
    !,
    atom_concat('-', Name, Arg),
	arglist_namevaluelist(Tail1, Tail2).

arglist_namevaluelist([Arg,ValueAtom|Tail1], [Name=Value|Tail2]) :-
    argument(Arg, _, _),
    \+ argument(ValueAtom, _, _),
    !,
    atom_concat('-', Name, Arg),
	(
		catch(atom_number(ValueAtom, ValueNumber), _, fail)
	->
		Value = ValueNumber
	;
		Value = ValueAtom
	),
	arglist_namevaluelist(Tail1, Tail2).

arglist_namevaluelist([Arg|_], _) :-
	argument(Arg, _, _),
	!,
	throw(error('Missing value for argument', context(arglist_namevaluelist/2, Arg))).

arglist_namevaluelist([Arg|_], _) :-
	throw(error('Illegal argument', context(arglist_namevaluelist/2, Arg))).


%% read_file(+InputListIn:list, -InputListOut:list) is det.
%
% @param InputListIn is a list of APE parameters
% @param InputListOut is a modified list of APE parameters
%
read_file(InputListIn, [text=AceText | InputListOut]) :-
	select(file=AceFile, InputListIn, InputListOut),
	!,
	filename_to_filecontent(AceFile, AceText).

read_file(InputList, InputList) :-
	member(text=_, InputList),
	!.

read_file(InputList, [text=AceText | InputList]) :-
	prompt(_, ''),
	read_stream_to_codes(user_input, AceTextCodes),
	atom_codes(AceText, AceTextCodes).


%% read_ulex(+InputListIn:list, -InputListOut:list) is det.
%
% Stores the user lexicon in a local (temporary) file.
% Modifies the list of APE parameters to include the name
% of the local file.
%
% @param InputListIn is a list of APE parameters
% @param InputListOut is a modified list of APE parameters
%
read_ulex(InputListIn, [ulextext=UlexText | InputListOut]) :-
	select(ulexfile=UlexFile, InputListIn, InputListOut),
	!,
	filename_to_filecontent(UlexFile, UlexText).

read_ulex(InputList, InputList).


% Note: we use: set_stream(In, encoding(utf8))
% This makes characters travel correctly through the socket.
% There might be other (better) solutions though.

:- use_module(library(streampool)).

:- style_check(-singleton).

port(2766).  % ape (0xACE)
%port(2767).  % ape-alpha (0xACF)
%port(2768).  % ape-old (0xAD0)

server :-
	port(Port),
	server(Port).

server(Port) :-
	get_time_formatted(Time),
	format(user_error, "~w: Starting a socket interface for APE at port ~w ...~n", [Time, Port]),
	tcp_socket(Socket),
	tcp_bind(Socket, Port),
	tcp_listen(Socket, 5),
	tcp_open_socket(Socket, In, _Out),
	add_stream_to_pool(In, accept(Socket)),
	stream_pool_main_loop.

accept(Socket) :-
	tcp_accept(Socket, Slave, Peer),
	tcp_open_socket(Slave, In, Out),
	set_utf8_encoding(In),
	set_utf8_encoding(Out),
	add_stream_to_pool(In, client(In, Out, Peer)).

client(In, Out, _Peer) :-
	catch(
		client_x(In, Out, _Peer),
		CatchType,
		(
			format(user_error, "~w~n", [CatchType]),
			(is_stream(In) -> close(In), delete_stream_from_pool(In) ; true),
			(is_stream(Out) -> close(Out) ; true)
		)
	).


% NB: 'APESERVERSTREAMEND' on a separate line marks the end of the stream.
% @bug should we use write_canonical/2 here?
client_x(In, Out, _Peer) :-
	% We suppress warnings of the atom being longer than 5 lines.
	% This declaration seems to have effect only here.
	style_check(-atom),
	read(In, ClientRequest),
	close(In),
	(
		ClientRequest = get(I)
	->
		get_ape_results_timelimit(I, O, 20),  % 20 seconds timelimit
		format(Out, '~w~nAPESERVERSTREAMEND~n', [O])
	;
		format(Out, 'fail~nAPESERVERSTREAMEND~n')
	),
	close(Out),
	delete_stream_from_pool(In).


/**

The HTTP interface relies on SWI-Prolog HTTP support.

@see http://www.swi-prolog.org/packages/http.html

Note that the number of workers is set to 1.
Multiple workers would share the same assert/retract space and we do not want that
because APE is not completely thread-safe.
A better solution would be to make APE thread-safe and let the user decide on
the command-line on the number of workers (because the best-performing
number depends on the number of processor cores).
Note that the SWI default is 2 workers (which seems to make sense even with single-core processors).

At Prolog prompt, stop the server by:

==
?- http_stop_server(8000, []).
==

Note that http_stop_server/2 does not make the port immediately available,
you have to wait a few (two?) minutes. Observe it with 'netstat -na'.

*/


:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/http_client')).


% Configure the port.
http_port(8000).

% Configure the www root.
:- http_handler('/', ape, []).
%:- http_handler('/ape/', ape, []).

% Configure the APE webservice parameters.
% @bug unify this with the main argument description
% Note that while 'text' and 'file' are optional, at least one of them must be present.
% It is probably impossible to say this declaratively.
parameters([
	text(_,         [optional(true)]),
	file(_,         [optional(true)]),
	ulextext(_,     [optional(true)]),
	ulexfile(_,     [optional(true)]),
	noclex(_,       [oneof([on, off]), optional(true)]),
	guess(_,        [oneof([on, off]), optional(true)]),
	% ulexreload(_, [optional(true)]), % @tbd
	uri(_,          [default('http://attempto.ifi.uzh.ch/ontologies/owlswrl/test')]),
	cinput(_,       [oneof([on, off]), optional(true)]), % @bug deprecated
	cdrs(_,         [oneof([on, off]), optional(true)]),
	cdrsxml(_,      [oneof([on, off]), optional(true)]),
	cdrspp(_,       [oneof([on, off]), optional(true)]),
	cdrshtml(_,     [oneof([on, off]), optional(true)]),
	cparaphrase(_,  [oneof([on, off]), optional(true)]),
	cparaphrase1(_, [oneof([on, off]), optional(true)]),
	cparaphrase2(_, [oneof([on, off]), optional(true)]),
	ctokens(_,      [oneof([on, off]), optional(true)]),
	csentences(_,   [oneof([on, off]), optional(true)]),
	csyntax(_,      [oneof([on, off]), optional(true)]),
	csyntaxpp(_,    [oneof([on, off]), optional(true)]),
	csyntaxd(_,     [oneof([on, off]), optional(true)]),
	csyntaxdpp(_,   [oneof([on, off]), optional(true)]),
	cowlfss(_,      [oneof([on, off]), optional(true)]),
	cowlfsspp(_,    [oneof([on, off]), optional(true)]),
	cowlrdf(_,      [oneof([on, off]), optional(true)]),
	cowlxml(_,      [oneof([on, off]), optional(true)]),
	cruleml(_,      [oneof([on, off]), optional(true)]),
	cfol(_,         [oneof([on, off]), optional(true)]),
	ctptp(_,        [oneof([on, off]), optional(true)]),
	solo(_,         [oneof([drs, drsxml, drspp, drshtml,
					paraphrase, paraphrase1, paraphrase2,
					tokens, sentences,
					syntax, syntaxpp, syntaxd, syntaxdpp,
					owlfss, owlfsspp, owlrdf, owlxml,
					ruleml,
					fol, pnf, tptp]), optional(true)])
]).

http_server :-
	http_port(Port),
	http_server(Port).

http_server(Port) :-
	get_time_formatted(Time),
	format(user_error, "~w: Starting an HTTP interface for APE at port ~w ...~n", [Time, Port]),
	http_server(http_dispatch, [port(Port), workers(1)]),
	thread_get_message(_),
	halt.

%% ape(+Request) is det.
%
% This is the HTTP interface toplevel where input is received
% and results/errors are output.
%
ape(Request) :-
	parameters(Parameters),
	catch(
		(
			http_parameters(Request, Parameters),
			http_parameters_to_ape_parameters(Parameters, ApeParameters),
			get_ape_results_timelimit(ApeParameters, ContentType, Content, 10) % 10 second timelimit
		),	
		Exception,
		format_error_for_http(Exception, ContentType, Content)
	),
	format('Content-type: ~w\r\n\r\n~w', [ContentType, Content]).


%% http_parameters_to_ape_parameters(+HttpParameters:list, -ApeParameters:list) is det.
%
% Converts the parameters that have been instantiated by http_parameters/2
% into the parameters' format that APE accepts. Uninstantiated parameters are filtered out.
%
% @param HttpParameters is the result of http_parameters/2
% @param ApeParameters is the inputlist for get_ape_results_timelimit/4
%
http_parameters_to_ape_parameters([], []).

http_parameters_to_ape_parameters([Parameter | Parameters], Out) :-
	arg(1, Parameter, Arg1),
	(
		nonvar(Arg1)
	->
		functor(Parameter, Functor, _),
		key_value_to_parameter(Functor, Arg1, ApeParameter),
		Out = [ApeParameter | ApeParameters]
	;
		Out = ApeParameters
	),
	http_parameters_to_ape_parameters(Parameters, ApeParameters).


%% key_value_to_parameter(+Key:atom, +Value:atom, -ApeParameter:term) is det.
%
% Constructs an APE parameter which has a form Key = Value.
% Maps 'file' to 'text', 'ulextext' to 'ulexfile', etc.
%
% Note that for security reasons, the value of 'file' and 'ulexfile' cannot point
% to a local file (e.g. /etc/passwd). Only http-urls are allowed.
% The following example shows an illegal query.
%
%==
% http://attempto.ifi.uzh.ch:8000/?file=/etc/passwd
%==
%
% @param Key is a parameter name
% @param Value is the parameter value
% @param ApeParameter is the corresponding APE parameter
%
key_value_to_parameter(file, HttpUrl, text = FileContent) :-
	!,
	httpurl_to_filecontent(HttpUrl, FileContent).

key_value_to_parameter(ulexfile, HttpUrl, ulextext = FileContent) :-
	!,
	httpurl_to_filecontent(HttpUrl, FileContent).

key_value_to_parameter(Key, Value, Key = Value).


%% filename_to_filecontent(+FileName:atom, -FileContent:atom) is det.
%
% Reads the content of a file that is specified by FileName into an atom.
% FileName can be a URL or a regular file name. In case of URLs, only the http-prefix
% is allowed. The content of the file is expected to be encoded in UTF-8.
%
% @param FileName is the name (possibly a URL) of a file
% @param FileContent is the content (as an atom) of the file
%
filename_to_filecontent(FileName, FileContent) :-
	(
		is_http_url(FileName)
	->
		httpurl_to_filecontent(FileName, FileContent)
	;
		read_file_to_codes(FileName, Codes, [encoding(utf8)]),
		atom_codes(FileContent, Codes)
	).


%% httpurl_to_filecontent(+HttpUrl:atom, -FileContent:atom) is det.
%
% Makes an HTTP GET request to a http-resource.
%
% @param HttpUrl is a URL (expected to start as 'http://')
% @param FileContent is the content (as an atom) of the resource
% @throws socket_error (and other exceptions by http_get/3)
% @throws 'HTTP request failed' in case HTTP status code was not 'ok' (200)
%
% @bug Check if http_get expects UTF-8 or can handle other encodings as well
% @bug Make sure that http_get/3 never accesses local files (e.g. that it does not support 'file://')
%
httpurl_to_filecontent(HttpUrl, FileContent) :-
	http_get(HttpUrl, FileContent, [ user_agent('ape.exe (http://attempto.ifi.uzh.ch)'), reply_header(ReplyHeader) ]),
	memberchk(status(Code, Message), ReplyHeader),
	(
		Code = ok
	->
		true
	;
		with_output_to(atom(MessageAtom), format("~s: ~w", [Message, HttpUrl])),
		throw(error('HTTP request failed', context(httpurl_to_filecontent/2, MessageAtom)))
	).


%% is_http_url(+Atom:atom) is det.
%
% Tests is an atom is an HTTP URL.
% The test is quite naive.
%
% @param Atom is an atom to be tested.
%
% @bug Make use of SWI library url.pl
% @bug What is faster for substring matching sub_atom/5 or concat_atom/2,
%      e.g. concat_atom(['http://', _], FileName)
%
is_http_url(Atom) :-
	sub_atom(Atom, 0, 7, _, 'http://').


%% format_error_for_terminal(+Exception:term) is det.
%
% Pretty-prints the exception term for the terminal.
%
% @param Exception is the exception term, usually in the form
%        error(Formal, context(Module:Name/Arity, Message))
%
format_error_for_terminal(error(Formal, context(Predicate, Message))) :-
	!,
	format_message(Message, FMessage),
	format(user_error, "ERROR: ~w: ~w: ~w~n", [Formal, Predicate, FMessage]).

format_error_for_terminal(Error) :-
	format(user_error, "ERROR: ~w~n", [Error]).


%% format_error_for_http(+Exception:term, -ContentType:atom, -Content:term) is det.
%
% Generates an error message from the exception term.
%
% @param Exception is the exception term, usually in the form
%        error(Formal, context(Module:Name/Arity, Message))
% @param ContentType is the content type that message is formatted into, e.g. text/xml
% @param Content is the formatted error message
%
% @tbd return the same error messages as apews.perl
% @bug not XML-safe
%
format_error_for_http(error(Formal, context(Predicate, Message)), 'text/xml', Xml) :-
	!,
	functor(Formal, Name, _),
	format_message(Message, FMessage),
	with_output_to(atom(Xml), format("<error type=\"~w\">~w: ~w: ~w</error>", [Name, Formal, Predicate, FMessage])).

format_error_for_http(Error, 'text/xml', Xml) :-
	with_output_to(atom(Xml), format("<error>~w</error>", [Error])).


%% format_message(+Message:term, -FormattedMessage:atom)
%
% Formats the message that sometimes comes with the
% exception term. Often this message is unbound. The
% purpose of this rule is to return an empty atom if this
% is the case.
format_message(Message, '') :-
	var(Message),
	!.

format_message(Message, Message).


%% set_utf8_encoding(+Stream)
%
% Sets the encoding of the given stream to UTF-8. For some unknown reason, an error is sometimes
% thrown under Windows XP when calling set_stream/2, and this error is catched here.

set_utf8_encoding(Stream) :-
	catch(
		set_stream(Stream, encoding(utf8)),
		_,
		true
	).


%% get_time_formatted(-FormattedTimestamp)
%
% Generates a timestamp for the current time
%
get_time_formatted(FormattedTimestamp) :-
	get_time(Timestamp),
	format_time(atom(FormattedTimestamp), '%F %T%z', Timestamp).
