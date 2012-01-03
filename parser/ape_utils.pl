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


:- module(ape_utils, [
		cpu_time/2,
		handle_unknown_words/4,
		new_npid/1
	]).


/** <module> APE utils

@author Kaarel Kaljurand
@author Tobias Kuhn
@version 2009-11-18

*/

:- use_module(grammar_words).
:- use_module('../lexicon/spellcheck').
:- use_module('../lexicon/is_in_lexicon').
:- use_module('../lexicon/functionwords').
:- use_module('../lexicon/chars', [is_capitalized/1]).
:- use_module('../logger/error_logger', [
		add_error_messagelist/4,
		add_error_message/4
	]).

:- use_module(sentence_failure, [
		get_all_sentence_errors/2
	]).

:- use_module('../utils/ace_niceace', [
		ace_niceace/2
	]).


:- op(400, fy, -).
:- op(400, fy, ~).
:- op(500, xfx, =>).
:- op(500, xfx, v).

%:- debug(known_word).

cpu_time(Goal, Duration) :-
	statistics(runtime, [Start | _]),
	ignore(Goal),
	statistics(runtime, [Finish | _]),
	Duration is (Finish-Start) * 0.001.

% @deprecated
read_text_from_commandline([C|R]) :-
	get0(C),
	line_continues(C),
	!,
	read_text_from_commandline(R).

read_text_from_commandline([]).

% @deprecated
line_continues(C) :- C \== 10, C \== -1, !.


%% list_close(+OpenList, -ClosedList) is det.
%
% Closes an open list.

list_close([], []) :- !.

list_close([Head | Tail], [Head | ClosedTail]) :-
	list_close(Tail, ClosedTail).


%% member_of_domain(+Element:var, +List:list) is det.
%
% member/2 for variables
%
% @bug we should not catch wellformedness bugs here, but in the beginning.
% (Dom must always be a closed list)
%
% Note: Can Dom be open during parsing? (member_of_domain/2 is called
% from add_modifiers)

member_of_domain(E, [F | _]) :-
	E==F,
	!.

member_of_domain(E, [_ | R]) :-
	nonvar(R),  % catches a bug
	member_of_domain(E,R).


%% list_of_conds_and_anaphors(+List, -List, -AnaphorAnteList)
%
% Filters out functors `antecedent' and `anaphor' and closes the list.
% Copies all `anaphor' and `antecedent' functors into the extra argument.
%
% We have to do it recursively for the following tests to work.
%
% - There is a man who sees no dog. The man who sees no dog waits.
% - There is a man who sees every dog. The man who sees every dog waits.
% - There is a man who sees a dog or who sees a cat. The man who sees a dog or who sees a cat waits.
% - @bug also: can, must, can't, does not have to
%
% @bug Note that we ignore the inner anaphors for the time being.
% Some theoretical work is needed to find out if they are needed.
%
% @bug Type-argument is currently ignored.

list_of_conds_and_anaphors(Tail, [], []) :- var(Tail), !.

list_of_conds_and_anaphors([], [], []).

list_of_conds_and_anaphors([Head | Tail], CondsTail, [Head|AnaAnte]) :-
	functor(Head, antecedent, _),
	!,
	list_of_conds_and_anaphors(Tail, CondsTail, AnaAnte).

list_of_conds_and_anaphors([Head | Tail], CondsTail, [Head|AnaAnte]) :-
	Head = anaphor(_, _, _, AnaphorConds, _, _, _, _, _, _, _),
	!,
	list_of_conds_and_anaphors(Tail, CondsTailTemp1, AnaAnte),
	append(AnaphorConds, CondsTailTemp1, CondsTailTemp2),
	sort(CondsTailTemp2, CondsTail).

list_of_conds_and_anaphors([Condition | CondsTail], [ConditionPruned | CondsTailPruned], AnaAnte) :-
	handle_condition(Condition, ConditionPruned),
	!,
	list_of_conds_and_anaphors(CondsTail, CondsTailPruned, AnaAnte).

% Simple condition.
list_of_conds_and_anaphors([Head | Tail], [Head | CondsTail], AnaAnte) :-
	list_of_conds_and_anaphors(Tail, CondsTail, AnaAnte).


%% handle_condition(+Condition, -ConditionPruned)
%
%

handle_condition([First|Rest], CondsPruned) :-
	list_of_conds_and_anaphors([First|Rest], CondsPruned, _).

handle_condition(-drs(Dom, Conds), -drs(Dom, CondsPruned)) :-
	list_of_conds_and_anaphors(Conds, CondsPruned, _).

handle_condition(Label:drs(Dom, Conds), Label:drs(Dom, CondsPruned)) :-
	list_of_conds_and_anaphors(Conds, CondsPruned, _).

handle_condition(can(drs(Dom, Conds)), can(drs(Dom, CondsPruned))) :-
	list_of_conds_and_anaphors(Conds, CondsPruned, _).

handle_condition(must(drs(Dom, Conds)), must(drs(Dom, CondsPruned))) :-
	list_of_conds_and_anaphors(Conds, CondsPruned, _).

handle_condition(should(drs(Dom, Conds)), should(drs(Dom, CondsPruned))) :-
	list_of_conds_and_anaphors(Conds, CondsPruned, _).

handle_condition(may(drs(Dom, Conds)), may(drs(Dom, CondsPruned))) :-
	list_of_conds_and_anaphors(Conds, CondsPruned, _).

handle_condition(~drs(Dom, Conds), ~drs(Dom, CondsPruned)) :-
	list_of_conds_and_anaphors(Conds, CondsPruned, _).

handle_condition(drs(Dom1, Conds1) => drs(Dom2, Conds2), drs(Dom1, Conds1Pruned) => drs(Dom2, Conds2Pruned)) :-
	list_of_conds_and_anaphors(Conds1, Conds1Pruned, _),
	list_of_conds_and_anaphors(Conds2, Conds2Pruned, _).

handle_condition(drs(Dom1, Conds1) v drs(Dom2, Conds2), drs(Dom1, Conds1Pruned) v drs(Dom2, Conds2Pruned)) :-
	list_of_conds_and_anaphors(Conds1, Conds1Pruned, _),
	list_of_conds_and_anaphors(Conds2, Conds2Pruned, _).


%% report_failed_sentence(+SentenceID:number, +FirstUnparsedSentence:list) is det.
%
% @param SentenceID is a sentence ID
% @param UnparsedSentences is a list of (remaining) tokens that were not accepted
%
% @bug We also return the token ID that identifies the first unparsed token.
% This ID is misleading though as it references a token in the internal token
% list which contains things like '^', '-thing', 'n', ':', etc. Thus the number can
% point to a location further than the actual location perceived by the user.
% (We do subtract 1 to account for the '^' symbol present in every token list, i.e. otherwise
% the formula would be: TokenID is (AllTokensNumber - UnparsedTokensNumber) + 1).
%
report_failed_sentence(SentenceID, FirstSentence) :-
	get_unparsed_tokens_number(UnparsedTokensNumber),
	length(UnparsedTokens, UnparsedTokensNumber),
	append(ParsedTokens, UnparsedTokens, FirstSentence),
	append(ParsedTokens, ['<>'|UnparsedTokens], FirstSentenceX),
	get_all_sentence_errors(FirstSentenceX, ErrorTextList),
	ace_niceace(FirstSentenceX, FirstSentenceNice),
	concat_atom(FirstSentenceNice, ' ', FirstSentenceAtom),
	length(FirstSentence, AllTokensNumber),
	TokenID is (AllTokensNumber - UnparsedTokensNumber),
	add_error_messagelist(sentence, SentenceID-TokenID, FirstSentenceAtom, ErrorTextList).


%% spellcheck_and_report_unknown_words(+UnknownTokenList)
%
% @param UnknownTokenList is a list of unknown tokens.
%
spellcheck_and_report_unknown_words(UnknownTokenList, N) :-
	spellcheck:damerau_rules(UnknownTokenList, Candidates, NoSolutions),
	!,
	report_candidates(Candidates, N),
	report_nosolutions(NoSolutions, N).

report_candidates([], _).

report_candidates([Bad-Good | T], N) :-
	add_error_message(word, N-'', Bad, Good),
	report_candidates(T, N).

report_nosolutions([], _).

report_nosolutions([Bad | T], N) :-
	add_error_message(word, N-'', Bad, 'Use the prefix n:, v:, a: or p:.'),
	report_nosolutions(T, N).


%% handle_unknown_words(+GuessOnOff:atom, +Tokens:list, -TokensWithPrefixes:list) is det.

handle_unknown_words(_, [], [], _).

handle_unknown_words(on, [SentenceIn|RestIn], [SentenceOut|RestOut], _) :-
	!,
	prefix_unknown_words(SentenceIn, SentenceOut),
	handle_unknown_words(on, RestIn, RestOut, _).

handle_unknown_words(G, [Sentence|Rest], [Sentence|Rest], N) :-
	get_unknown_words(Sentence, UnknownTokens),
	spellcheck_and_report_unknown_words(UnknownTokens, N),
	NextN is N + 1,
	handle_unknown_words(G, Rest, Rest, NextN).


%% get_unknown_words(+TokenList, -UnknownTokenList)
%
% @bug repeated unknowns are not removed. Ways to fix it:
% - sort/2 the list in the end
% - use some hashing technique

get_unknown_words(TokenList, UnknownTokenList) :-
	get_unknown_words(TokenList, [x, x], UnknownTokenList).

get_unknown_words([], _, []).

get_unknown_words([Token | TokensTail], [Left2, Left1], UnknownTokenList) :-
	get_right_context(TokensTail, Right1),
	(
	known_or_capitalized(Left2, Left1, Token, Right1) -> UnknownTokenList = UnknownTokenListTail
	;
	UnknownTokenList = [Token | UnknownTokenListTail]
	),
	get_unknown_words(TokensTail, [Left1, Token], UnknownTokenListTail).


%% known_or_capitalized(+Left2:atom, +Left1:atom, +WordForm:atom, +Right1:atom) is det.
%

known_or_capitalized(_, _, WordForm, _) :-
	chars:is_capitalized(WordForm),
	!.

known_or_capitalized(Left2, Left1, WordForm, Right1) :-
	debug(known_word, "~w ~w *~w* ~w~n", [Left2, Left1, WordForm, Right1]),
	known_word(Left2, Left1, WordForm, Right1),
	debug(known_word, "ok~n", []).


%% prefix_unknown_words(+TokenList, -TokenListWithPrefixes)
%
% Add variable prefixes to unknown words. The prefixes are grounded
% by the first successful parse. This essentially amounts to guessing
% the word class (noun, verb, adjective, adverb) of unknown words.
%
% Note that propernames are difficult to guess since they are not introduced
% by a marker (like 'a' or 'every'). Adding a variable prefix makes the variable
% unify also with function words (e.g. 'if'). This causes a performance problem
% and doesn't work anyway. So, for the time being, we simply don't guess
% propernames (eventhough propernames deserve guessing the most, being a very
% open class).

prefix_unknown_words(TokenList, TokenListWithPrefixes) :-
	prefix_unknown_words(TokenList, [x, x], TokenListWithPrefixes).
 
prefix_unknown_words([], _, []).

prefix_unknown_words([Token | TokensTail], [Left2, Left1], TokenListWithPrefixes1) :-
	get_right_context(TokensTail, Right1),
	(
	known_or_capitalized(Left2, Left1, Token, Right1) ->
	TokenListWithPrefixes1 = [Token | TokenListWithPrefixes]
	;
	TokenListWithPrefixes1 = [unknowncat, :, Token | TokenListWithPrefixes]
	),
	prefix_unknown_words(TokensTail, [Left1, Token], TokenListWithPrefixes).


%% get_right_context(+List, -FirstElement).
%
% Returns the first element of the list, or 'x' if the list is empty.
%
% @bug This predicate is experimental.

get_right_context([], x).

get_right_context([Head | _], Head).


%% known_word(+Left2:atom, +Left1:atom, +Token:atom, +Right1:atom) is det.
%
% Token is "known" iff:
%
% it is in quotation marks;
% it is prefixed;
% it is a prefix symbol;
% it is in the lexicon.
%
known_word(_, _, String, _) :-
    atom_concat('"', S, String),
    atom_concat(_, '"', S).

known_word(Left2, ':', _, _) :-
	(
		functionwords:propername_prefix(Left2, _)
	;
		functionwords:noun_prefix(Left2, _)
	;
		functionwords:verb_prefix(Left2)
	;
		functionwords:modif_prefix(Left2)
	),
	!.

known_word(_, _, Prefix, ':') :-
	(
		functionwords:propername_prefix(Prefix, _)
	;
		functionwords:noun_prefix(Prefix, _)
	;
		functionwords:verb_prefix(Prefix)
	;
		functionwords:modif_prefix(Prefix)
	),
	!.

known_word(_, _, WordForm, _) :-
	is_in_lexicon:is_in_lexicon(WordForm).


%----------------------------------------------------------------------------
% new_npid(-NPID:number) is det.
%
% new_npid delivers the next idenfifier (ID) for an NP.
% NP IDs are used for anaphora resolution recency calculations.
%----------------------------------------------------------------------------

new_npid(NewNPID) :-
	b_getval(npid, OldNPID),
	NewNPID is OldNPID + 1,
	b_setval(npid, NewNPID).
