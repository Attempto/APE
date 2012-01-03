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


:- module(clex, [
		clex_switch/1,     % ?Switch
		set_clex_switch/1  % +Switch
	]).

/** <module> Common Lexicon Interface

This module contains the predicates for the management of the common lexicon that is compiled into
the executable.

@author Tobias Kuhn
@version 2008-07-17
*/


%% clex_file(-ClexFile)
%
% This predicate defines the clex-file that is loaded and compiled into the executable. In order to
% change this, you have to edit the source code and recompile.

clex_file(clex_lexicon).
%clex_file(clex_lexicon_small).
%clex_file('').


% The predicates for the lexicon entries are declared dynamic. In this way, they don't fail if
% no entry exists.

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


% Load the clex-file
:- style_check(-discontiguous).
:- clex_file(ClexFile), ( ClexFile == '' ; load_files(ClexFile, [encoding(utf8)]) ).
:- style_check(+discontiguous).


%% clex_switch(?Switch)
%
% This predicate returns 'on' if clex is switched on, or 'off' otherwise.

:- dynamic(clex_switch/1).

clex_switch(on).


%% set_clex_switch(+Switch)
%
% This predicate switches clex on (Switch='on') or off (Switch='off').

set_clex_switch(Switch) :-
    member(Switch, [on, off]),
    retractall(clex_switch(_)),
    assert(clex_switch(Switch)).
