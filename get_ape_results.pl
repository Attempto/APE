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


:- module(get_ape_results, [
		get_ape_results_timelimit/3,
		get_ape_results_timelimit/4,
		get_ape_results/2,
		get_ape_results/3
	]).


/** <module> Interface for the ACE tools (ACE parser, DRS verbalizer, ...)

@author Kaarel Kaljurand
@author Tobias Kuhn
@version 2009-05-13

Usage with multiple results returned (i.e. multi-mode):

==
get_ape_results([text='Every man waits.', cparaphrase1=on], ContentType, Content).
get_ape_results([text='Every man waits.', cparaphrase=on, cparaphrase1=on], ContentType, Content).
get_ape_results([text='A man waits.', cinput=on, cdrs=on, cdrspp=on, cparaphrase=on, cparaphrase1=on, cparaphrase2=on, ctokens=on, csyntax=on, csyntaxpp=on, cfol=on], ContentType, Content).
==

Usage with a single result returned (i.e. solo-mode):

==
get_ape_results([text='A man sees a dog.', solo=drs], ContentType, Content).
get_ape_results([text='Every man owns a dog.', solo=owlrdf], ContentType, Content).
get_ape_results([text='Peeter likes Mary.', solo=owlfss], ContentType, Content).
==

@tbd: provide all outputs
	(1) as serialized Prolog terms,
	(2) as pretty-printed terms,
	(3) as XML, JSON, HTML, etc.
*/

% Default encoding used for opening files in text mode.
:- set_prolog_flag(encoding, utf8).

% Note: The following line fixes the testcase: 3.1415926536 approximates Pi.
% Note that using 'f' instead of 'g' does not drop the trailing zeros.
% @bug: sometimes weird digits are attached to the end.
:- set_prolog_flag(float_format, '%.11g').

% Richard A. O'Keefe: to see full precision for IEEE doubles, do
% :- set_prolog_flag(float_format, '%.18g').
% So it seems that values above 18 do not make sense.

:- assert(user:file_search_path(ape, '.')).


:- use_module(ape('utils/morphgen'), [
		acesentencelist_pp/2
	]).

:- use_module(ape('utils/ace_niceace'), [
		tokens_to_sentences/2
	]).

:- use_module(ape('utils/drs_to_xml')).
:- use_module(ape('utils/drs_to_fol_to_prenex')).
:- use_module(ape('utils/drs_to_ascii')).
:- use_module(ape('utils/drs_to_ace')).
:- use_module(ape('utils/drs_to_coreace')).
:- use_module(ape('utils/drs_to_npace')).
:- use_module(ape('utils/drs_to_html')).
:- use_module(ape('utils/drs_to_ruleml')).
:- use_module(ape('utils/tree_utils')).
:- use_module(ape('utils/trees_to_ascii')).
:- use_module(ape('utils/drs_to_tptp')).
:- use_module(ape('lexicon/clex')).
:- use_module(ape('lexicon/ulex')).
:- use_module(ape('parser/ace_to_drs')).
:- use_module(ape('logger/error_logger')).

:- use_module(ape('utils/xmlterm_to_xmlatom'), [
		xmlterm_to_xmlatom/2,
		xmlterm_to_xmlatom/3
	]).

:- use_module(ape('utils/serialize_term'), [
		serialize_term_into_atom/2
	]).

:- use_module(ape('utils/owlswrl/get_owl_output'), [
		get_owl_output/6
	]).


%% get_ape_results_timelimit(+Input:list, -Content:atom, +TimeLimit) is det.
%% get_ape_results_timelimit(+Input:list, -ContentType:atom, -Content:atom, +TimeLimit) is det.
%
% There is call_with_time_limit(+Time, :Goal) defined in library(time),
% part of clib package. On timeout this throws the exception time_limit_exceeded.
% But we catch other exceptions as well...
%
% @param Input is a list of input parameters of the form Key=Value
% @param ContentType is one of {text/plain, text/xml}
% @param Content is the returned result
% @param TimeLimit the timelimit in seconds
%
get_ape_results_timelimit(Input, Content, TimeLimit) :-
	get_ape_results_timelimit(Input, _ContentType, Content, TimeLimit).

get_ape_results_timelimit(Input, ContentType, Content, TimeLimit) :-
	catch(
		call_with_time_limit(
			TimeLimit,
			get_ape_results(Input, ContentType, Content)
		),
		CatchType,
		catchtype_errormessage(CatchType, ContentType, Content)
	).


%% catchtype_errormessage(+CatchType:atom, -ContentType:atom, -ErrorMessage:atom) is det.
%
% Returns an error message as XML. The error message is set by catch/3,
% this predicate just formats the message.
% This is a very toplevel error message indicating either that the time limit
% was exceeded or that there were resource errors (out of stack, etc.) or programmer
% errors (undefined predicate called, etc.).
%
% @param CatchType is one of {time_limit_exceeded, ...}
% @param ContentType is always 'text/xml'
% @param ErrorMessage is an end-user-level message that corresponds to the CatchType
%
catchtype_errormessage(
	time_limit_exceeded,
	'text/xml',
	'<apeResult><messages><message importance="error" type="ws" sentence="" token="" value="time_limit_exceeded"
repair="Split the text into smaller parts and parse them separately."/></messages></apeResult>'
	) :- !.

catchtype_errormessage(CatchType, 'text/xml', ErrorMessage) :-
	format(atom(CatchTypeAsAtom), "~w", [CatchType]),
	xmlterm_to_xmlatom(element(apeResult, [], [
			element(messages, [], [
				element(message, [
					importance='error',
					type='ws',
					sentence='',
					token='',
					value=CatchTypeAsAtom,
					repair='Fatal error. Please send screenshot to APE developers.'
					], [])
			])
		]), ErrorMessage).


%% get_ape_results(+Input:list, -Content:atom) is det.
%% get_ape_results(+Input:list, -ContentType:atom, -Content:atom) is det.
%
% @param Input is a list of input parameters of the form Key=Value
% @param ContentType is one of {text/plain, text/xml}
% @param Content is the returned XML
%
get_ape_results(Input, Content) :-
	get_ape_results(Input, _ContentType, Content).

get_ape_results(Input, ContentType, Content) :-
	clear_messages,
	init_clex(Input),
	load_ulex(Input),
	get_value(Input, text, ACEText),
	get_value(Input, guess, GuessOnOff),
	acetext_to_drs(ACEText, GuessOnOff, on, Tokens, Syntax, Drs, _Messages, [DT, DP, DR]),
	get_value(Input, uri, Uri, 'http://attempto.ifi.uzh.ch/ontologies/owlswrl/test'),
	TempResult = [
			time=[DT, DP, DR],
			acetext=ACEText,
			tokens=Tokens,
			drs=Drs,
			syntax=Syntax,
			uri=Uri
		],
	get_content(Input, TempResult, ContentType, Content),
	!.

% @bug: This is sometimes called, but we should know why.
% It must be here, just to catch errors.
get_ape_results(_, 'text/plain', '').


%% output_type(?Type:atom)
%
% This predicate defines the supported output types and the order of the outputs for the multi mode.
%
output_type(input).
output_type(tokens).
output_type(sentences).
output_type(drs).
output_type(drsrt).
output_type(syntax).
output_type(syntaxpp).
output_type(syntaxd).
output_type(syntaxdpp).
output_type(drspp).
output_type(drsxml).
output_type(drshtml).
output_type(paraphrase).
output_type(paraphrase1).
output_type(paraphrase2).
output_type(owlrdf).
output_type(owlfss).
output_type(owlfsspp).
output_type(owlxml).
output_type(ruleml).
output_type(fol).
output_type(pnf).
output_type(tptp).


%% get_content(+Input:list, +TempResult:list, -ContentType:atom, -Content:atom) is det.
%
%
get_content(Input, TempResult, ContentType, Content) :-
    member(solo=SoloType, Input),
	!,
	get_solo_content(SoloType, TempResult, ContentType, Content).

get_content(Input, TempResult, ContentType, Content) :-
	get_multi_content(Input, TempResult, ContentType, Content).


%% get_solo_content(+SoloType:atom, +TempResult:list, -ContentType:atom, -Content:atom) is det.
%
% @param SoloType defines which output should be returned
% @param TempResult is a list of outputs from the parser
% @param ContentType is one of {text/xml, text/plain}
% @param Content is the result (e.g. a syntax tree or a paraphrase) in XML or plain text

% If there are APE error messages
% then we print the messages and not the solo output.
% Note: we do not care about the warning messages.
get_solo_content(_, _TempResult, 'text/xml', Content) :-
	get_error_messages([M | ErrorMessages]),
	!,
	messages_xmlmessages([M | ErrorMessages], XmlErrorMessages),
	xmlterm_to_xmlatom(element(messages, [], XmlErrorMessages), Content).

get_solo_content(SoloType, TempResult, ContentType, Content) :-
	output_type(SoloType),
	get_output(SoloType, TempResult, OutputContentType, OutputContent),
	(
		get_error_messages([M | ErrorMessages])
	->
		messages_xmlmessages([M | ErrorMessages], XmlErrorMessages),
		xmlterm_to_xmlatom(element(messages, [], XmlErrorMessages), Content),
		ContentType = 'text/xml'
	;
		Content = OutputContent,
		ContentType = OutputContentType
	).

get_solo_content(SoloType, _, 'text/plain', 'ERROR: Unexpected error.') :-
	output_type(SoloType),
	!.

get_solo_content(_, _, 'text/plain', 'ERROR: Wrong solo type.').


%% get_multi_content(+Input:list, +TempResult:list, -ContentType:atom, -Content:atom) is det.
%
% @bug: we ignore the parser+refres messages, and get a fresh list
% of messages from the messages repository (it will include the parser+refres
% messages anyway).
%
get_multi_content(Input, TempResult, 'text/xml', Content) :-
    get_value(TempResult, time, [DTokenizer, DParser, DRefres]),
	with_output_to(atom(DT), format("~3f", [DTokenizer])),
	with_output_to(atom(DP), format("~3f", [DParser])),
	with_output_to(atom(DR), format("~3f", [DRefres])),
	findall(OutputElement, get_multi_output_element(Input, TempResult, OutputElement), OutputElements),
	get_messages_in_xml(Messages),
	append([element(duration, [tokenizer=DT, parser=DP, refres=DR], [])|OutputElements], [element(messages, [], Messages)], Elements),
	xmlterm_to_xmlatom(element(apeResult, [], Elements), [header(true)], Content).


%% get_multi_output_element(+Input:list, +TempResult:list, -Content:element)
%
% Returns one output element if it is required by the input in multi mode. It succeeds as many times as there
% are such multi typed outputs.
%
% In case an exception is thrown during the generation of the output,
% then an empty atom is returned as the output.
%
get_multi_output_element(Input, TempResult, element(Type, [], [Output])) :-
	output_type(Type),
	atom_concat('c', Type, Key),
	memberchk(Key=on, Input),
	catch(
		get_output(Type, TempResult, _, Output),
		_Catcher,
		Output = ''
	).


%% get_output(+OutputType:atom, +TempResult:list, -ContentType:atom, -Content:atom)
%
% Returns the required output as an atom. This is used for both modes, solo and multi.
%
get_output(input, TempResult, 'text/plain', Output) :-
    get_value(TempResult, acetext, Output),
    !.

get_output(tokens, TempResult, 'text/plain', Output) :-
    get_value(TempResult, tokens, Tokens),
	serialize_term_into_atom(Tokens, Output),
    !.

get_output(sentences, TempResult, 'text/plain', Output) :-
    get_value(TempResult, tokens, Tokens),
	tokens_to_sentences(Tokens, Sentences),
	serialize_term_into_atom(Sentences, Output),
    !.

get_output(syntax, TempResult, 'text/plain', Output) :-
    get_value(TempResult, syntax, Syntax1),
	unsplit_pronouns_in_tree(Syntax1, Syntax2),
	remove_gaps_in_tree(Syntax2, Syntax3),
	unify_coords_in_tree(Syntax3, Syntax),
	serialize_term_into_atom(Syntax, Output),
    !.

get_output(syntaxpp, TempResult, 'text/plain', Output) :-
    get_value(TempResult, syntax, Syntax1),
	unsplit_pronouns_in_tree(Syntax1, Syntax2),
	remove_gaps_in_tree(Syntax2, Syntax3),
	unify_coords_in_tree(Syntax3, Syntax),
	trees_to_ascii:trees_to_ascii(Syntax, Output),
    !.

get_output(syntaxd, TempResult, 'text/plain', Output) :-
    get_value(TempResult, syntax, Syntax),
	serialize_term_into_atom(Syntax, Output),
    !.

get_output(syntaxdpp, TempResult, 'text/plain', Output) :-
    get_value(TempResult, syntax, Syntax),
	trees_to_ascii:trees_to_ascii(Syntax, Output),
    !.

get_output(fol, TempResult, 'text/plain', Output) :-
    get_value(TempResult, drs, Drs),
	drs_fol_pnf:drs_fol(Drs, Fol),
	serialize_term_into_atom(Fol, Output),
    !.

get_output(pnf, TempResult, 'text/plain', Output) :-
    get_value(TempResult, drs, Drs),
	drs_fol_pnf:drs_pnf(Drs, Pnf),
	serialize_term_into_atom(Pnf, Output),
    !.

get_output(paraphrase, TempResult, 'text/plain', Output) :-
    get_value(TempResult, drs, Drs),
	drs_to_ace:drs_to_ace(Drs, Sentences),
	acesentencelist_pp(Sentences, Output),
    !.

get_output(paraphrase1, TempResult, 'text/plain', Output) :-
    get_value(TempResult, drs, Drs),
	drs_to_coreace:bigdrs_to_coreace(Drs, Sentences),
	acesentencelist_pp(Sentences, Output),
    !.

get_output(paraphrase2, TempResult, 'text/plain', Output) :-
    get_value(TempResult, drs, Drs),
	drs_to_npace:drs_to_npace(Drs, Sentences),
	acesentencelist_pp(Sentences, Output),
    !.

get_output(ruleml, TempResult, 'text/xml', Output) :-
    get_value(TempResult, drs, Drs),
	drs_to_ruleml:drs_to_ruleml(Drs, Ruleml),
	xmlterm_to_xmlatom([Ruleml], Output),
    !.

get_output(drs, TempResult, 'text/plain', Output) :-
    get_value(TempResult, drs, Drs),
	serialize_term_into_atom(Drs, Output),
    !.

get_output(drspp, TempResult, 'text/plain', Output) :-
    get_value(TempResult, drs, Drs),
	drs_to_ascii:drs_to_ascii(Drs, Output),
    !.

get_output(tptp, TempResult, 'text/plain', Output) :-
	get_value(TempResult, drs, Drs),
	drs_to_tptp:drs_to_tptplist(Drs, TptpList),
	with_output_to(atom(Output), tptplist_pp(TptpList)),
    !.

get_output(drsxml, TempResult, 'text/xml', Output) :-
    get_value(TempResult, drs, Drs),
	drs_to_xmlatom(Drs, Output),
    !.

get_output(drshtml, TempResult, 'text/xml', Output) :-
    get_value(TempResult, drs, Drs),
	drs_to_html:drs_to_html(Drs, Output),
    !.

get_output(OwlOutputType, TempResult, ContentType, Output) :-
	get_value(TempResult, acetext, AceText),
	get_value(TempResult, drs, Drs),
	get_value(TempResult, uri, Uri),
	get_owl_output(OwlOutputType, AceText, Drs, Uri, ContentType, Output),
	!.


%% init_clex(+Input:list) is det.
%
%
init_clex(Input) :-
	get_value(Input, noclex, on),
	!,
	set_clex_switch(off).

init_clex(_) :-
	set_clex_switch(on).


%% load_ulex(+Input:list) is det.
%
%
load_ulex(Input) :-
	get_value(Input, ulextext, Ulex),
	!,
	discard_ulex,
    atom_to_memory_file(Ulex, UlexHandle),
    open_memory_file(UlexHandle, read, UlexStream),
    read_ulex(UlexStream),
    free_memory_file(UlexHandle).

load_ulex(_).


%% get_value(+Map:list, +Key:atom, -Value:atom, +Default:atom) is det.
%
% Returns the value for a key from a map of key/value pairs. The default value is returned if
% the key is not found.
%
% @param Map is a list key/value pairs of the form Key=Value
% @param Key is the key to look for
% @param Value is the value that is returned
% @param Default is the default value for the case that the key is not found
%
get_value(Map, Key, Value, _Default) :-
	memberchk(Key=Value, Map),
	!.

get_value(_, _, Default, Default).


%% get_value(+Map:list, +Key:atom, -Value:atom) is det.
%
% The same as get_value/4 with '' as default value
%
get_value(Map, Key, Value) :-
	get_value(Map, Key, Value, '').
