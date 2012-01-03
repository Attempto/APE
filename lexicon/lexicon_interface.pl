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


:- module(lexicon_interface, [
	adv/2,
	adv_comp/2,
	adv_sup/2,
	adj_itr/2,
	adj_itr_comp/2,
	adj_itr_sup/2,
	adj_tr/3,
	adj_tr_comp/3,
	adj_tr_sup/3,
	noun_sg/3,
	noun_pl/3,
	noun_mass/3,
	mn_sg/2,
	mn_pl/2,
	pn_sg/3,
	pn_pl/3,
	pndef_sg/3,
	pndef_pl/3,
	iv_finsg/2,
	iv_infpl/2,
	tv_finsg/2,
	tv_infpl/2,
	tv_pp/2,
	dv_finsg/3,
	dv_infpl/3,
	dv_pp/3,
	prep/2,
	common_noun/1,
	verb/1,
	attributive_adjective/1
	]).

:- use_module(clex).
:- use_module(ulex).


% Calls to the lexicon.
% The user lexicon (ulex) is preferred to
% the common lexicon (clex).
%
% BUG: it looks ugly, maybe there is a nicer way to do it.

adv(WordForm, Word) :- var(WordForm), var(Word), !, fail.
adv(WordForm, Word) :- ulex:adv(WordForm, Word).
adv(WordForm, Word) :- clex_switch(on), clex:adv(WordForm, Word).

adv_comp(WordForm, Word) :- var(WordForm), var(Word), !, fail.
adv_comp(WordForm, Word) :- ulex:adv_comp(WordForm, Word).
adv_comp(WordForm, Word) :- clex_switch(on), clex:adv_comp(WordForm, Word).

adv_sup(WordForm, Word) :- var(WordForm), var(Word), !, fail.
adv_sup(WordForm, Word) :- ulex:adv_sup(WordForm, Word).
adv_sup(WordForm, Word) :- clex_switch(on), clex:adv_sup(WordForm, Word).

adj_itr(WordForm, Word) :- var(WordForm), var(Word), !, fail.
adj_itr(WordForm, Word) :- ulex:adj_itr(WordForm, Word).
adj_itr(WordForm, Word) :- clex_switch(on), clex:adj_itr(WordForm, Word).

adj_itr_comp(WordForm, Word) :- var(WordForm), var(Word), !, fail.
adj_itr_comp(WordForm, Word) :- ulex:adj_itr_comp(WordForm, Word).
adj_itr_comp(WordForm, Word) :- clex_switch(on), clex:adj_itr_comp(WordForm, Word).

adj_itr_sup(WordForm, Word) :- var(WordForm), var(Word), !, fail.
adj_itr_sup(WordForm, Word) :- ulex:adj_itr_sup(WordForm, Word).
adj_itr_sup(WordForm, Word) :- clex_switch(on), clex:adj_itr_sup(WordForm, Word).

adj_tr(WordForm, Word, _Prep) :- var(WordForm), var(Word), !, fail.
adj_tr(WordForm, Word, Prep) :-  ulex:adj_tr(WordForm, Word, Prep).
adj_tr(WordForm, Word, Prep) :-  clex_switch(on), clex:adj_tr(WordForm, Word, Prep).

adj_tr_comp(WordForm, Word, _Prep) :- var(WordForm), var(Word), !, fail.
adj_tr_comp(WordForm, Word, Prep) :- ulex:adj_tr_comp(WordForm, Word, Prep).
adj_tr_comp(WordForm, Word, Prep) :- clex_switch(on), clex:adj_tr_comp(WordForm, Word, Prep).

adj_tr_sup(WordForm, Word, _Prep) :- var(WordForm), var(Word), !, fail.
adj_tr_sup(WordForm, Word, Prep) :- ulex:adj_tr_sup(WordForm, Word, Prep).
adj_tr_sup(WordForm, Word, Prep) :- clex_switch(on), clex:adj_tr_sup(WordForm, Word, Prep).

noun_sg(WordForm, Word, _Gender) :- var(WordForm), var(Word), !, fail.
noun_sg(WordForm, Word, Gender) :- ulex:noun_sg(WordForm, Word, Gender).
noun_sg(WordForm, Word, Gender) :- clex_switch(on), clex:noun_sg(WordForm, Word, Gender).

noun_pl(WordForm, Word, _Gender) :- var(WordForm), var(Word), !, fail.
noun_pl(WordForm, Word, Gender) :- ulex:noun_pl(WordForm, Word, Gender).
noun_pl(WordForm, Word, Gender) :- clex_switch(on), clex:noun_pl(WordForm, Word, Gender).

noun_mass(WordForm, Word, _Gender) :- var(WordForm), var(Word), !, fail.
noun_mass(WordForm, Word, Gender) :- ulex:noun_mass(WordForm, Word, Gender).
noun_mass(WordForm, Word, Gender) :- clex_switch(on), clex:noun_mass(WordForm, Word, Gender).

mn_sg(WordForm, Word) :- var(WordForm), var(Word), !, fail.
mn_sg(WordForm, Word) :- ulex:mn_sg(WordForm, Word).
mn_sg(WordForm, Word) :- clex_switch(on), clex:mn_sg(WordForm, Word).

mn_pl(WordForm, Word) :- var(WordForm), var(Word), !, fail.
mn_pl(WordForm, Word) :- ulex:mn_pl(WordForm, Word).
mn_pl(WordForm, Word) :- clex_switch(on), clex:mn_pl(WordForm, Word).

pn_sg(WordForm, Word, _Gender) :- var(WordForm), var(Word), !, fail.
pn_sg(WordForm, Word, Gender) :- ulex:pn_sg(WordForm, Word, Gender).
pn_sg(WordForm, Word, Gender) :- clex_switch(on), clex:pn_sg(WordForm, Word, Gender).

pn_pl(WordForm, Word, _Gender) :- var(WordForm), var(Word), !, fail.
pn_pl(WordForm, Word, Gender) :- ulex:pn_pl(WordForm, Word, Gender).
pn_pl(WordForm, Word, Gender) :- clex_switch(on), clex:pn_pl(WordForm, Word, Gender).

pndef_sg(WordForm, Word, _Gender) :- var(WordForm), var(Word), !, fail.
pndef_sg(WordForm, Word, Gender) :- ulex:pndef_sg(WordForm, Word, Gender).
pndef_sg(WordForm, Word, Gender) :- clex_switch(on), clex:pndef_sg(WordForm, Word, Gender).

pndef_pl(WordForm, Word, _Gender) :- var(WordForm), var(Word), !, fail.
pndef_pl(WordForm, Word, Gender) :- ulex:pndef_pl(WordForm, Word, Gender).
pndef_pl(WordForm, Word, Gender) :- clex_switch(on), clex:pndef_pl(WordForm, Word, Gender).

iv_finsg(WordForm, Word) :- var(WordForm), var(Word), !, fail.
iv_finsg(WordForm, Word) :- ulex:iv_finsg(WordForm, Word).
iv_finsg(WordForm, Word) :- clex_switch(on), clex:iv_finsg(WordForm, Word).

iv_infpl(WordForm, Word) :- var(WordForm), var(Word), !, fail.
iv_infpl(WordForm, Word) :- ulex:iv_infpl(WordForm, Word).
iv_infpl(WordForm, Word) :- clex_switch(on), clex:iv_infpl(WordForm, Word).

tv_finsg(WordForm, Word) :- var(WordForm), var(Word), !, fail.
tv_finsg(WordForm, Word) :- ulex:tv_finsg(WordForm, Word).
tv_finsg(WordForm, Word) :- clex_switch(on), clex:tv_finsg(WordForm, Word).

tv_infpl(WordForm, Word) :- var(WordForm), var(Word), !, fail.
tv_infpl(WordForm, Word) :- ulex:tv_infpl(WordForm, Word).
tv_infpl(WordForm, Word) :- clex_switch(on), clex:tv_infpl(WordForm, Word).

tv_pp(WordForm, Word) :- var(WordForm), var(Word), !, fail.
tv_pp(WordForm, Word) :- ulex:tv_pp(WordForm, Word).
tv_pp(WordForm, Word) :- clex_switch(on), clex:tv_pp(WordForm, Word).

dv_finsg(WordForm, Word, _Prep) :- var(WordForm), var(Word), !, fail.
dv_finsg(WordForm, Word, Prep) :- ulex:dv_finsg(WordForm, Word, Prep).
dv_finsg(WordForm, Word, Prep) :- clex_switch(on), clex:dv_finsg(WordForm, Word, Prep).

dv_infpl(WordForm, Word, _Prep) :- var(WordForm), var(Word), !, fail.
dv_infpl(WordForm, Word, Prep) :- ulex:dv_infpl(WordForm, Word, Prep).
dv_infpl(WordForm, Word, Prep) :- clex_switch(on), clex:dv_infpl(WordForm, Word, Prep).

dv_pp(WordForm, Word, _Prep) :- var(WordForm), var(Word), !, fail.
dv_pp(WordForm, Word, Prep) :- ulex:dv_pp(WordForm, Word, Prep).
dv_pp(WordForm, Word, Prep) :- clex_switch(on), clex:dv_pp(WordForm, Word, Prep).

prep(WordForm, Word) :- var(WordForm), var(Word), !, fail.
prep(WordForm, Word) :- ulex:prep(WordForm, Word).
prep(WordForm, Word) :- clex_switch(on), clex:prep(WordForm, Word).


common_noun(WordForm) :-
	(
		noun_sg(WordForm, _, _)
	;
		noun_pl(WordForm, _, _)
	;
		noun_mass(WordForm, _, _)
	).


verb(WordForm) :-
	(
		iv_finsg(WordForm, _)
	;
		iv_infpl(WordForm, _)
	;
		tv_finsg(WordForm, _)
	;
		tv_infpl(WordForm, _)
	;
		dv_finsg(WordForm, _, _)
	;
		dv_infpl(WordForm, _, _)
	).


%% attributive_adjective(+WordForm:atom) is det.
%
% @param WordForm is an ACE token
%
% Succeeds if WordForm is an attributive adjective

attributive_adjective(WordForm) :-
	(
		adj_itr(WordForm, _)
	;
		adj_itr_comp(WordForm, _)
	;
		adj_itr_sup(WordForm, _)
	).
