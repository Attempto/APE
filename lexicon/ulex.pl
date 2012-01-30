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


:- module(ulex, [
		read_ulex/1,            % +LexiconStream
		add_lexicon_entries/1,  % +LexEntryList
		add_lexicon_entry/1,    % +LexEntry
		discard_ulex/0
	]).

:- use_module(functionwords).
:- use_module('../logger/error_logger').

/** <module> User Lexicon Interface

This module contains the predicates for the dynamic management of the user lexicon that is not
compiled into the executable.

@author Tobias Kuhn
@version 2009-11-18
*/


% The predicates for the lexicon entries are declared dynamic.

:- dynamic adv/2.
:- dynamic adv_comp/2.
:- dynamic adv_sup/2.
:- dynamic adj_itr/2.
:- dynamic adj_itr_comp/2.
:- dynamic adj_itr_sup/2.
:- dynamic adj_tr/3.
:- dynamic adj_tr_comp/3.
:- dynamic adj_tr_sup/3.
:- dynamic noun_sg/3.
:- dynamic noun_pl/3.
:- dynamic noun_mass/3.
:- dynamic mn_sg/2.
:- dynamic mn_pl/2.
:- dynamic pn_sg/3.
:- dynamic pn_pl/3.
:- dynamic pndef_sg/3.
:- dynamic pndef_pl/3.
:- dynamic iv_finsg/2.
:- dynamic iv_infpl/2.
:- dynamic tv_finsg/2.
:- dynamic tv_infpl/2.
:- dynamic tv_pp/2.
:- dynamic dv_finsg/3.
:- dynamic dv_infpl/3.
:- dynamic dv_pp/3.
:- dynamic prep/2.


%% read_ulex(+LexiconStream)
%
% Reads a lexicon file as a stream and loads all the contained entries.

read_ulex(LexiconStream) :-
	catch(
		(
			read_lexicon_entries(LexiconStream)
		),
		_CatchType,
		(
			with_output_to(atom(Message), format("The user lexicon file is not a valid Prolog file.", [])),
			add_error_message_once(lexicon, '', 'Malformed file.', Message)
		)
	),
	close(LexiconStream).

read_ulex(LexiconStream) :-
	close(LexiconStream).


%% read_lexicon_entries(+Stream)
%
% Loads the lexicon entries reading from the stream.

read_lexicon_entries(Stream) :-
    read(Stream, LexEntry),
    ( LexEntry == end_of_file ->
    	true
    ;
    	add_lexicon_entry(LexEntry),
    	read_lexicon_entries(Stream)
    ).


%% add_lexicon_entries(+LexEntryList)
%
% @param LexEntryList
%
% Adds all the lexicon entries of the list to the dynamic lexicon.

add_lexicon_entries([]).

add_lexicon_entries([LexEntry|Rest]) :-
    add_lexicon_entry(LexEntry),
    add_lexicon_entries(Rest).


%% add_lexicon_entry(+LexEntry)
%
% @param LexEntry
%
% Adds the lexicon entry to the dynamic lexicon.

add_lexicon_entry(LexEntry) :-
    ground(LexEntry),
	lexicon_template(LexEntry),
	!,
    check_intersections(LexEntry),
    assert(LexEntry).

add_lexicon_entry(LexEntry) :-
	with_output_to(atom(Message), format("User lexicon entry is malformed: ~w", [LexEntry])),
	add_error_message_once(lexicon, '', 'Malformed entry.', Message).


%% discard_ulex
%
% Discards all the lexicon entries.

discard_ulex :-
    lexicon_template(LexiconTemplate),
    retractall(LexiconTemplate),
    fail.

discard_ulex.


%% lexicon_template(+LexiconTemplate)
%

lexicon_template(adv(_, _)).
lexicon_template(adv_comp(_, _)).
lexicon_template(adv_sup(_, _)).
lexicon_template(adj_itr(_, _)).
lexicon_template(adj_itr_comp(_, _)).
lexicon_template(adj_itr_sup(_, _)).
lexicon_template(adj_tr(_, _, _)).
lexicon_template(adj_tr_comp(_, _, _)).
lexicon_template(adj_tr_sup(_, _, _)).
lexicon_template(noun_sg(_, _, _)).
lexicon_template(noun_pl(_, _, _)).
lexicon_template(noun_mass(_, _, _)).
lexicon_template(mn_sg(_, _)).
lexicon_template(mn_pl(_, _)).
lexicon_template(pn_sg(_, _, _)).
lexicon_template(pn_pl(_, _, _)).
lexicon_template(pndef_sg(_, _, _)).
lexicon_template(pndef_pl(_, _, _)).
lexicon_template(iv_finsg(_, _)).
lexicon_template(iv_infpl(_, _)).
lexicon_template(tv_finsg(_, _)).
lexicon_template(tv_infpl(_, _)).
lexicon_template(tv_pp(_, _)).
lexicon_template(dv_finsg(_, _, _)).
lexicon_template(dv_infpl(_, _, _)).
lexicon_template(dv_pp(_, _, _)).
lexicon_template(prep(_, _)).


%% check_intersection(+Entry)
%
% Checks if the new entry leads to a conflict. There is a conflict if
% - the word is a function word, or
% - the word is duplicated in the user lexicon, or
% - there is a harmful word class intersection.

check_intersections(Entry) :-
    Entry =.. [_,Word|_],
    unredefinable_fw(Word),
    add_warning_message(lexicon, '', Word, 'This function word should not be redefined in the user lexicon.').

check_intersections(adv(Word,_)) :-
    ( adv(Word,_) ; adv_comp(Word,_) ; adv_sup(Word,_) ),
    add_warning_message(lexicon, '', Word, 'This adverb is defined twice.').

check_intersections(adv_comp(Word,_)) :-
    ( adv(Word,_) ; adv_comp(Word,_) ; adv_sup(Word,_) ),
    add_warning_message(lexicon, '', Word, 'This adverb is defined twice.').

check_intersections(adv_sup(Word,_)) :-
    ( adv(Word,_) ; adv_comp(Word,_) ; adv_sup(Word,_) ),
    add_warning_message(lexicon, '', Word, 'This adverb is defined twice.').

check_intersections(adj_itr(Word,_)) :-
    ( adj_itr(Word,_) ; adj_itr_comp(Word,_) ; adj_itr_sup(Word,_) ),
    add_warning_message(lexicon, '', Word, 'This intransitive adjective is defined twice.').

check_intersections(adj_itr_comp(Word,_)) :-
    ( adj_itr(Word,_) ; adj_itr_comp(Word,_) ; adj_itr_sup(Word,_) ),
    add_warning_message(lexicon, '', Word, 'This intransitive adjective is defined twice.').

check_intersections(adj_itr_sup(Word,_)) :-
    ( adj_itr(Word,_) ; adj_itr_comp(Word,_) ; adj_itr_sup(Word,_) ),
    add_warning_message(lexicon, '', Word, 'This intransitive adjective is defined twice.').

check_intersections(adj_tr(Word,_,_)) :-
    ( adj_tr(Word,_,_) ; adj_tr_comp(Word,_,_) ; adj_tr_sup(Word,_,_) ),
    add_warning_message(lexicon, '', Word, 'This transitive adjective is defined twice.').

check_intersections(adj_tr_comp(Word,_,_)) :-
    ( adj_tr(Word,_,_) ; adj_tr_comp(Word,_,_) ; adj_tr_sup(Word,_,_) ),
    add_warning_message(lexicon, '', Word, 'This transitive adjective is defined twice.').

check_intersections(adj_tr_sup(Word,_,_)) :-
    ( adj_tr(Word,_,_) ; adj_tr_comp(Word,_,_) ; adj_tr_sup(Word,_,_) ),
    add_warning_message(lexicon, '', Word, 'This transitive adjective is defined twice.').

check_intersections(noun_sg(Word,_,_)) :-
    noun_sg(Word,_,_),
    add_warning_message(lexicon, '', Word, 'This singular noun is defined twice.').

check_intersections(noun_pl(Word,_,_)) :-
    noun_pl(Word,_,_),
    add_warning_message(lexicon, '', Word, 'This plural noun is defined twice.').

check_intersections(noun_mass(Word,_,_)) :-
    noun_mass(Word,_,_),
    add_warning_message(lexicon, '', Word, 'This mass noun is defined twice.').

check_intersections(mn_sg(Word,_)) :-
    mn_sg(Word,_),
    add_warning_message(lexicon, '', Word, 'This singular measurement noun is defined twice.').

check_intersections(mn_pl(Word,_)) :-
    mn_pl(Word,_),
    add_warning_message(lexicon, '', Word, 'This plural measurement noun is defined twice.').

check_intersections(pn_sg(Word,_,_)) :-
    ( pn_sg(Word,_,_) ; pn_pl(Word,_,_) ),
    add_warning_message(lexicon, '', Word, 'This proper name is defined twice.').

check_intersections(pn_pl(Word,_,_)) :-
    ( pn_sg(Word,_,_) ; pn_pl(Word,_,_) ),
    add_warning_message(lexicon, '', Word, 'This proper name is defined twice.').

check_intersections(pndef_sg(Word,_,_)) :-
    ( pndef_sg(Word,_,_) ; pndef_pl(Word,_,_) ),
    add_warning_message(lexicon, '', Word, 'This proper name is defined twice.').

check_intersections(pndef_pl(Word,_,_)) :-
    ( pndef_sg(Word,_,_) ; pndef_pl(Word,_,_) ),
    add_warning_message(lexicon, '', Word, 'This proper name is defined twice.').

check_intersections(iv_finsg(Word,_)) :-
    iv_finsg(Word,_),
    add_warning_message(lexicon, '', Word, 'This singular form of an intransitive verb is defined twice.').

check_intersections(iv_infpl(Word,_)) :-
    iv_infpl(Word,_),
    add_warning_message(lexicon, '', Word, 'This plural form of an intransitive verb is defined twice.').

check_intersections(tv_finsg(Word,_)) :-
    tv_finsg(Word,_),
    add_warning_message(lexicon, '', Word, 'This singular form of a transitive verb is defined twice.').

check_intersections(tv_infpl(Word,_)) :-
    tv_infpl(Word,_),
    add_warning_message(lexicon, '', Word, 'This plural form of a transitive verb is defined twice.').

check_intersections(tv_pp(Word,_)) :-
    tv_pp(Word,_),
    add_warning_message(lexicon, '', Word, 'This past participle form of a transitive verb is defined twice.').

check_intersections(dv_finsg(Word,_,'')) :-
    dv_finsg(Word,_,''),
    add_warning_message(lexicon, '', Word, 'This singular form of a ditransitive verb is defined twice.').

check_intersections(dv_infpl(Word,_,'')) :-
    dv_infpl(Word,_,''),
    add_warning_message(lexicon, '', Word, 'This plural form of a ditransitive verb is defined twice.').

check_intersections(dv_pp(Word,_,'')) :-
    dv_pp(Word,_,''),
    add_warning_message(lexicon, '', Word, 'This past participle form of a ditransitive verb is defined twice.').

check_intersections(dv_finsg(Word,_,Prep)) :-
    dv_finsg(Word,_,Prep),
    Prep \== '',
    add_warning_message(lexicon, '', Word, 'This singular form of a ditransitive verb is defined twice.').

check_intersections(dv_infpl(Word,_,Prep)) :-
    dv_infpl(Word,_,Prep),
    Prep \== '',
    add_warning_message(lexicon, '', Word, 'This plural form of a ditransitive verb is defined twice.').

check_intersections(dv_pp(Word,_,Prep)) :-
    dv_pp(Word,_,Prep),
    Prep \== '',
    add_warning_message(lexicon, '', Word, 'This past participle form of a ditransitive verb is defined twice.').

check_intersections(prep(Word,_)) :-
    prep(Word,_),
    add_warning_message(lexicon, '', Word, 'This preposition is defined twice.').

check_intersections(adv(Word,_)) :-
    noun_sg(Word,_,_),
    add_warning_message(lexicon, '', Word, 'Bad intersection: adverb and singular noun.').

check_intersections(noun_sg(Word,_,_)) :-
    adv(Word,_),
    add_warning_message(lexicon, '', Word, 'Bad intersection: adverb and singular noun.').

check_intersections(adv(Word,_)) :-
    noun_pl(Word,_,_),
    add_warning_message(lexicon, '', Word, 'Bad intersection: adverb and plural noun.').

check_intersections(noun_pl(Word,_,_)) :-
    adv(Word,_),
    add_warning_message(lexicon, '', Word, 'Bad intersection: adverb and plural noun.').

check_intersections(adv(Word,_)) :-
    noun_mass(Word,_,_),
    add_warning_message(lexicon, '', Word, 'Bad intersection: adverb and mass noun.').

check_intersections(noun_mass(Word,_,_)) :-
    adv(Word,_),
    add_warning_message(lexicon, '', Word, 'Bad intersection: adverb and mass noun.').

check_intersections(adv(Word,_)) :-
    iv_finsg(Word,_),
    add_warning_message(lexicon, '', Word, 'Bad intersection: adverb and singular intransitive verb.').

check_intersections(iv_finsg(Word,_)) :-
    adv(Word,_),
    add_warning_message(lexicon, '', Word, 'Bad intersection: adverb and singular intransitive verb.').

check_intersections(adv(Word,_)) :-
    iv_infpl(Word,_),
    add_warning_message(lexicon, '', Word, 'Bad intersection: adverb and plural intransitive verb.').

check_intersections(iv_infpl(Word,_)) :-
    adv(Word,_),
    add_warning_message(lexicon, '', Word, 'Bad intersection: adverb and plural intransitive verb.').

check_intersections(adj_itr(Word,_)) :-
    ( adj_tr(Word,_,_) ; adj_tr_comp(Word,_,_); adj_tr_sup(Word,_,_) ),
    add_warning_message(lexicon, '', Word, 'Bad intersection: intransitive adjective and transitive adjective.').

check_intersections(adj_itr_comp(Word,_)) :-
    ( adj_tr(Word,_,_) ; adj_tr_comp(Word,_,_); adj_tr_sup(Word,_,_) ),
    add_warning_message(lexicon, '', Word, 'Bad intersection: intransitive adjective and transitive adjective.').

check_intersections(adj_itr_sup(Word,_)) :-
    ( adj_tr(Word,_,_) ; adj_tr_comp(Word,_,_); adj_tr_sup(Word,_,_) ),
    add_warning_message(lexicon, '', Word, 'Bad intersection: intransitive adjective and transitive adjective.').

check_intersections(adj_tr(Word,_,_)) :-
    ( adj_itr(Word,_) ; adj_itr_comp(Word,_) ; adj_itr_sup(Word,_) ),
    add_warning_message(lexicon, '', Word, 'Bad intersection: intransitive adjective and transitive adjective.').

check_intersections(adj_tr_comp(Word,_,_)) :-
    ( adj_itr(Word,_) ; adj_itr_comp(Word,_) ; adj_itr_sup(Word,_) ),
    add_warning_message(lexicon, '', Word, 'Bad intersection: intransitive adjective and transitive adjective.').

check_intersections(adj_tr_sup(Word,_,_)) :-
    ( adj_itr(Word,_) ; adj_itr_comp(Word,_) ; adj_itr_sup(Word,_) ),
    add_warning_message(lexicon, '', Word, 'Bad intersection: intransitive adjective and transitive adjective.').

check_intersections(prep(Word,_)) :-
    ( adj_itr(Word,_) ; adj_itr_comp(Word,_) ; adj_itr_sup(Word,_) ),
    add_warning_message(lexicon, '', Word, 'Bad intersection: preposition and intransitive adjective.').

check_intersections(adj_itr(Word,_)) :-
    prep(Word,_),
    add_warning_message(lexicon, '', Word, 'Bad intersection: preposition and intransitive adjective.').

check_intersections(adj_itr_comp(Word,_)) :-
    prep(Word,_),
    add_warning_message(lexicon, '', Word, 'Bad intersection: preposition and intransitive adjective.').

check_intersections(adj_itr_sup(Word,_)) :-
    prep(Word,_),
    add_warning_message(lexicon, '', Word, 'Bad intersection: preposition and intransitive adjective.').

check_intersections(prep(Word,_)) :-
    ( tv_finsg(Word,_) ; tv_infpl(Word,_) ),
    add_warning_message(lexicon, '', Word, 'Bad intersection: preposition and transitive verb.').

check_intersections(tv_finsg(Word,_)) :-
    prep(Word,_),
    add_warning_message(lexicon, '', Word, 'Bad intersection: preposition and transitive verb.').

check_intersections(tv_infpl(Word,_)) :-
    prep(Word,_),
    add_warning_message(lexicon, '', Word, 'Bad intersection: preposition and transitive verb.').

check_intersections(pndef_sg(Word,_,_)) :-
    ( noun_sg(Word,_,_) ; noun_pl(Word,_,_) ; noun_mass(Word,_,_) ),
    add_warning_message(lexicon, '', Word, 'Bad intersection: proper name with definite article and noun.').

check_intersections(pndef_pl(Word,_,_)) :-
    ( noun_sg(Word,_,_) ; noun_pl(Word,_,_) ; noun_mass(Word,_,_) ),
    add_warning_message(lexicon, '', Word, 'Bad intersection: proper name with definite article and noun.').

check_intersections(_).


unredefinable_fw(null).
unredefinable_fw(zero).
unredefinable_fw(one).
unredefinable_fw(two).
unredefinable_fw(three).
unredefinable_fw(four).
unredefinable_fw(five).
unredefinable_fw(six).
unredefinable_fw(seven).
unredefinable_fw(eight).
unredefinable_fw(nine).
unredefinable_fw(ten).
unredefinable_fw(eleven).
unredefinable_fw(twelve).
unredefinable_fw(dozen).
unredefinable_fw('Null').
unredefinable_fw('Zero').
unredefinable_fw('One').
unredefinable_fw('Two').
unredefinable_fw('Three').
unredefinable_fw('Four').
unredefinable_fw('Five').
unredefinable_fw('Six').
unredefinable_fw('Seven').
unredefinable_fw('Eight').
unredefinable_fw('Nine').
unredefinable_fw('Ten').
unredefinable_fw('Eleven').
unredefinable_fw('Twelve').
unredefinable_fw('Dozen').
unredefinable_fw('There').
unredefinable_fw(there).
unredefinable_fw(and).
unredefinable_fw(or).
unredefinable_fw(not).
unredefinable_fw(that).
unredefinable_fw(than).
unredefinable_fw(of).
unredefinable_fw('If').
unredefinable_fw(if).
unredefinable_fw(then).
unredefinable_fw(such).
unredefinable_fw(be).
unredefinable_fw(provably).
unredefinable_fw(more).
unredefinable_fw(most).
unredefinable_fw(least).
unredefinable_fw(less).
unredefinable_fw(are).
unredefinable_fw('Are').
unredefinable_fw(is).
unredefinable_fw('Is').
unredefinable_fw(the).
unredefinable_fw('The').
unredefinable_fw(a).
unredefinable_fw('A').
unredefinable_fw(an).
unredefinable_fw('An').
unredefinable_fw(some).
unredefinable_fw('Some').
unredefinable_fw(no).
unredefinable_fw('No').
unredefinable_fw(every).
unredefinable_fw('Every').
unredefinable_fw(all).
unredefinable_fw('All').
unredefinable_fw(each).
unredefinable_fw('Each').
unredefinable_fw(which).
unredefinable_fw('Which').
unredefinable_fw(its).
unredefinable_fw('Its').
unredefinable_fw(his).
unredefinable_fw('His').
unredefinable_fw(her).
unredefinable_fw('Her').
unredefinable_fw(their).
unredefinable_fw('Their').
unredefinable_fw(whose).
unredefinable_fw('Whose').
unredefinable_fw(it).
unredefinable_fw('It').
unredefinable_fw(he).
unredefinable_fw('He').
unredefinable_fw(she).
unredefinable_fw('She').
unredefinable_fw(they).
unredefinable_fw('They').
unredefinable_fw(him).
unredefinable_fw(them).
unredefinable_fw(itself).
unredefinable_fw(himself).
unredefinable_fw(herself).
unredefinable_fw(themselves).
unredefinable_fw(someone).
unredefinable_fw('Someone').
unredefinable_fw(somebody).
unredefinable_fw('Somebody').
unredefinable_fw(something).
unredefinable_fw('Something').
unredefinable_fw(nobody).
unredefinable_fw('Nobody').
unredefinable_fw(nothing).
unredefinable_fw('Nothing').
unredefinable_fw(everyone).
unredefinable_fw('Everyone').
unredefinable_fw(everybody).
unredefinable_fw('Everybody').
unredefinable_fw(everything).
unredefinable_fw('Everything').
unredefinable_fw(what).
unredefinable_fw('What').
unredefinable_fw(who).
unredefinable_fw('Who').
unredefinable_fw(how).
unredefinable_fw('How').
unredefinable_fw(where).
unredefinable_fw('Where').
unredefinable_fw(when).
unredefinable_fw('When').
unredefinable_fw(many).
unredefinable_fw(much).
