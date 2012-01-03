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


:- module(is_in_lexicon, [
	is_in_lexicon/1,
	is_functionword/1,
	is_contentword/1
]).


/** <module> Is a token in the lexicon?

@author Kaarel Kaljurand
@author Tobias Kuhn
@version 2007-12-06

*/

:- use_module(functionwords).
:- use_module(lexicon_interface).
:- use_module(illegalwords).
:- use_module(chars).


%% is_in_lexicon(+WordForm:atom) is semidet.
%
% @param WordForm is an ACE wordform
%
% Succeeds if WordForm is among the ACE words, possibly
% one of the illegal words like `any' or `this'.
%
is_in_lexicon(WordForm) :-
	(
		is_functionword(WordForm)
	;
		is_contentword(WordForm)
	;
		is_illegalword(WordForm, _)
	).


%% is_functionword(+WordForm:atom) is nondet.
%
% @param WordForm is an ACE wordform
%
% Succeeds if WordForm is among the ACE function words.
%
is_functionword(WordForm) :-
	(
		functionwords:rawnumber_number(WordForm, _)
	;
		functionwords:functionword(WordForm)
	;
		functionwords:variable(WordForm)
	).


%% is_contentword(+WordForm:atom) is nondet.
%
% @param WordForm is an ACE wordform
%
% Succeeds if WordForm is in the content word lexicon.
%
is_contentword(WordForm) :-
	(
		adv(WordForm, _)
	;
		adv_comp(WordForm, _)
	;
		adv_sup(WordForm, _)
	;
		adj_itr(WordForm, _)
	;
		adj_itr_comp(WordForm, _)
	;
		adj_itr_sup(WordForm, _)
	;
		adj_tr(WordForm, _, _)
	;
		adj_tr_comp(WordForm, _, _)
	;
		adj_tr_sup(WordForm, _, _)
	;
		noun_sg(WordForm, _, _)
	;
		noun_pl(WordForm, _, _)
	;
		noun_mass(WordForm, _, _)
	;
		mn_sg(WordForm, _)
	;
		mn_pl(WordForm, _)
	;
		pn_sg(WordForm, _, _)
	;
		pn_pl(WordForm, _, _)
	;
		pndef_sg(WordForm, _, _)
	;
		pndef_pl(WordForm, _, _)
	;
		iv_finsg(WordForm, _)
	;
		iv_infpl(WordForm, _)
	;
		tv_finsg(WordForm, _)
	;
		tv_infpl(WordForm, _)
	;
		tv_pp(WordForm, _)
	;
		dv_finsg(WordForm, _, _)
	;
		dv_infpl(WordForm, _, _)
	;
		dv_pp(WordForm, _, _)
	;
		( chars:to_lowercase(WordForm, WordFormL), prep(WordFormL, _) )
	).
