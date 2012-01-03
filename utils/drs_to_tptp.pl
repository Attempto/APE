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

:- module(drs_to_tptp, [
		drs_to_tptp/2,
		drs_to_tptplist/2,
		tptp_pp/1,
		tptplist_pp/1
	]).

:- use_module(drs_to_drslist, [
		drs_to_drslist/2
	]).

/** <module> DRS to TPTP converter

Converts DRSs to fof-axioms, except for yes/no question-conditions which
are converted to fof-conjectures. Some DRS conditions are not supported,
causing an exception to be thrown if encountered.

@author Kaarel Kaljurand
@version 2009-06-27

TODO:

- get rid of the ugly pretty-printer, use the one from tptp2X.main (TPTP2X.tgz)

- support arith. expressions

- TEST: every declared variable is used

- TEST: every used variable is declared

- TEST: the resulting Prolog term is syntactically correct if
serialized with writeq using the TPTP operator definitions

- TEST: every DRS that is not supported causes a useful exception-term
to be thrown

- Think about: ``John is Mary in the park.''

- Think about: representation of plurals

- Think about: "Therefore" (proposed by G. Sutcliffe), depends on ACE

*/


%% drs_to_tptplist(+Drs:drs, -TptpList:list) is det.
%
% @param Drs is an Attempto DRS
% @param TptpList is a list of TPTP formulas
%
drs_to_tptplist(Drs, TptpList) :-
	drs_to_drslist(Drs, DrsList),
	maplist(drs_to_tptp, DrsList, TptpList).


%% drs_to_tptp(+Drs:drs, -Tptp:tptp) is det.
%
% @param Drs is an Attempto DRS
% @param Tptp is a TPTP formula
%
drs_to_tptp(drs([], [question(drs(Dom, Conds))]), fof(conjecture, Expr)) :-
	!,
	drs_to_tptp_x(drs(Dom, Conds), Expr).

drs_to_tptp(drs(Dom, Conds), fof(axiom, Expr)) :-
	drs_to_tptp_x(drs(Dom, Conds), Expr).


drs_to_tptp_x(drs(Dom, Conds), Expr) :-
	conds_tptp(Dom, Conds, NDom, NConds),
	get_quantifier_expression('?', NDom, NConds, Expr).


%% conds_tptp(+Dom:list, +Conds:list, -NDom:list, -Tptp:term) is det.
%
conds_tptp(Dom, [Cond], NDom, NCond) :-
	tptp(Dom, Cond, NDom, NCond).

conds_tptp(Dom, [Cond1, Cond2 | Tail], NDom, '&'(NCond1, NTail)) :-
	tptp(Dom, Cond1, NDomTmp, NCond1),
	conds_tptp(NDomTmp, [Cond2 | Tail], NDom, NTail).


%% get_quantifier_expression(Quant, Vars, Expr, QExpr)
%
get_quantifier_expression(_, [], Expr, Expr) :- !.
get_quantifier_expression('?', Vars, Expr, ':'('?'(Vars), Expr)).
get_quantifier_expression('!', Vars, Expr, ':'('!'(Vars), Expr)).


%% tptp
%
tptp(
	UpperDom,
	'=>'(drs(Dom1, Conds1), drs(Dom2, Conds2)),
	UpperDom,
	Expr
) :-
	!,
	conds_tptp(Dom1, Conds1, NDom1, NConds1),
	conds_tptp(Dom2, Conds2, NDom2, NConds2),
	get_quantifier_expression('?', NDom2, NConds2, Expr2),
	get_quantifier_expression('!', NDom1, '=>'(NConds1, Expr2), Expr).

tptp(
	UpperDom,
	'v'(drs(Dom1, Conds1), drs(Dom2, Conds2)),
	UpperDom,
	Expr
) :-
	!,
	conds_tptp(Dom1, Conds1, NDom1, NConds1),
	conds_tptp(Dom2, Conds2, NDom2, NConds2),
	get_quantifier_expression('?', NDom2, NConds2, Expr2),
	get_quantifier_expression('?', NDom1, '|'(NConds1, Expr2), Expr).

tptp(
	UpperDom,
	'-'(drs(Dom, Conds)),
	UpperDom,
	'~'(QExpr)
) :-
	!,
	conds_tptp(Dom, Conds, NDom, NConds),
	get_quantifier_expression('?', NDom, NConds, QExpr).

tptp(Dom, predicate(X, be, A, B)-_, NDom, '='(NA, NB)) :-
	!,
	exclude(==(X), Dom, NDom),
	arg_to_tptp(A, NA),
	arg_to_tptp(B, NB).

tptp(Dom, object(_, Lemma, _, _, _, _)-_, Dom, $true) :-
	is_thing(Lemma),
	!.

tptp(Dom, object(Var, Lemma, _, _, eq, 1)-_, Dom, Pred) :-
	!,
	Pred =.. [Lemma, Var].

% The rest of the atomic conditions are kept as they are.
tptp(Dom, Cond-_, Dom, Tptp) :-
	!,
	cond_to_tptp(Cond, Tptp).

tptp(_, Term, _, _) :-
	functor(Term, Functor, _),
	concat_atom(['DRS condition not supported', Functor], ': ', Message),
	throw(error(Message, context(tptp/4, Term))).


%% cond_to_tptp
%
%
cond_to_tptp(formula(R1, Symbol, R2), Pred) :-
	arg_to_tptp(R1, NR1),
	arg_to_tptp(R2, NR2),
	atom(Symbol),
	member(Symbol, ['<', '>', '=', '\\=', '>=', '=<']),
	Pred =.. [Symbol, NR1, NR2].

cond_to_tptp(object(A, B, C, D, E, F), object(A, B, C, D, E, F)).

cond_to_tptp(modifier_adv(R, Lexem, Degree), modifier_adv(R, Lexem, Degree)) :-
	var(R),
	atom(Degree),
	member(Degree, [pos, comp, sup]),
	atom(Lexem).

cond_to_tptp(modifier_pp(R1, Prep, R2), modifier_pp(R1, Prep, NR2)) :-
	var(R1),
	atom(Prep),
	arg_to_tptp(R2, NR2).

cond_to_tptp(property(R, Lexem, Comp), property1(R, Lexem, Comp)) :-
	var(R),
	atom(Comp),
	member(Comp, [pos, comp, sup]),
	atom(Lexem).

cond_to_tptp(property(R1, Lexem, Comp, R2), property2(R1, Lexem, Comp, NR2)) :-
	var(R1),
	arg_to_tptp(R2, NR2),
	atom(Comp),
	member(Comp, [pos, pos_as, comp, comp_than, sup]),
	atom(Lexem).

cond_to_tptp(property(R1, Lexem, R2, Comp, SubjObj, R3), property3(R1, Lexem, NR2, Comp, SubjObj, NR3)) :-
	var(R1),
	arg_to_tptp(R2, NR2),
	arg_to_tptp(R3, NR3),
	atom(Comp),
	member(Comp, [pos_as, comp_than]),
	atom(SubjObj),
	member(SubjObj, [subj, obj]),
	atom(Lexem).

cond_to_tptp(predicate(R1, Lexem, R2), predicate1(R1, Lexem, NR2)) :-
	var(R1),
	arg_to_tptp(R2, NR2),
	atom(Lexem).

cond_to_tptp(predicate(R1, Lexem, R2, R3), predicate2(R1, Lexem, NR2, NR3)) :-
	var(R1),
	arg_to_tptp(R2, NR2),
	arg_to_tptp(R3, NR3),
	atom(Lexem).

cond_to_tptp(predicate(R1, Lexem, R2, R3, R4), predicate3(R1, Lexem, NR2, NR3, NR4)) :-
	var(R1),
	arg_to_tptp(R2, NR2),
	arg_to_tptp(R3, NR3),
	arg_to_tptp(R4, NR4),
	atom(Lexem).

cond_to_tptp(relation(R1, of, R2), relation(R1, of, NR2)) :-
	var(R1),
	arg_to_tptp(R2, NR2).

cond_to_tptp(has_part(R1, R2), has_part(R1, NR2)) :-
	var(R1),
	arg_to_tptp(R2, NR2).

cond_to_tptp(query(R, Lexem), _) :-
	throw(error('DRS query/2 not supported', context(cond_to_tptp/2, query(R, Lexem)))).


%% arg_to_tptp
%
%
arg_to_tptp(Arg, Arg) :-
	var(Arg),
	!.

arg_to_tptp(named(Arg), Arg) :-
	atom(Arg).

arg_to_tptp(int(Arg), Arg) :-
	integer(Arg).

arg_to_tptp(real(Arg), Arg) :-
	float(Arg).

arg_to_tptp(int(Arg, Unit), int(Arg, Unit)) :-
	integer(Arg),
	atom(Unit).

arg_to_tptp(real(Arg, Unit), real(Arg, Unit)) :-
	float(Arg),
	atom(Unit).

arg_to_tptp(string(Arg), string(Arg)) :-
	atomic(Arg).

arg_to_tptp(list(List), _) :-
	throw(error('DRS function list/1 not supported', context(arg_to_tptp/2, list(List)))).

arg_to_tptp(set(Set), _) :-
	throw(error('DRS function set/1 not supported', context(arg_to_tptp/2, set(Set)))).

arg_to_tptp(expr('&', Arg1, Arg2), _) :-
	throw(error('DRS operator \'&\' not supported', context(arg_to_tptp/2, expr('&', Arg1, Arg2)))).

arg_to_tptp(expr(Op, Arg1, Arg2), Expr) :-
	arg_to_tptp(Arg1, NArg1),
	arg_to_tptp(Arg2, NArg2),
	atom(Op),
	member(Op, ['+', '-', '*', '/']),
	Expr =.. [Op, NArg1, NArg2].


%% is_thing
%
%
is_thing(somebody).
is_thing(something).


%% tptplist_pp(+TptpList:list)
%
% Pretty-prints a list of TPTP formulas.
% The input formulas are expected not to have names,
% the names will be assigned using the scheme:
% 'f1', 'f2', 'f3', ...
%
tptplist_pp(TptpList) :-
	tptplist_pp(1, f, TptpList).


%% tptplist_pp(+Index:integer, +Prefix:atom, +TptpList:list)
%
% Pretty-prints a list of TPTP formulas.
% The input formulas are expected not to have names, i.e.
% they must have the form fof(Role, Expr), where Role
% is either 'axiom' or 'conjecture', and Expr is the FOL formula.
% Names are assigned to the formulas during pretty-printing.
% The names are constructed by concatenating Prefix with Index.
% The Index is increased by one for the next formula.
%
tptplist_pp(_, _, []).

tptplist_pp(Index, Prefix, [Tptp]) :-
	tptp_pp(Index, Prefix, Tptp).

tptplist_pp(Index, Prefix, [Tptp1, Tptp2 | Rest]) :-
	tptp_pp(Index, Prefix, Tptp1),
	nl, nl,
	NewIndex is Index + 1,
	tptplist_pp(NewIndex, Prefix, [Tptp2 | Rest]).


%% tptp_pp(+Index:integer, +Prefix:atom, +Tptp:term)
tptp_pp(Index, Prefix, fof(Role, Expr)) :-
	concat_atom([Prefix, Index], Name),
	tptp_pp(fof(Name, Role, Expr)).

%% tptp_pp(+Tptp:term)
%
% TPTP pretty-printer.
% BUG: Should be replaced with something prettier.
%
tptp_pp(fof(Role, Expr)) :-
	tptp_pp(fof(name, Role, Expr)).

tptp_pp(fof(Name, Role, Expr)) :-
	format("fof(~w, ~w, (~n", [Name, Role]),
	numbervars(Expr, 0, _),
	tptp_pp_(Expr),
	format(")).", []),
	fail ; true.

tptp_pp_(':'('?'(Vars), F)) :-
	!,
	write_term('?', [quoted(true)]),
	write(' '),
	write_term(Vars, [numbervars(true), quoted(true)]),
	write(' : '),
	tptp_pp_wrap(F).

tptp_pp_(':'('!'(Vars), F)) :-
	!,
	write_term('!', [quoted(true)]),
	format(" ", []),
	write_term(Vars, [numbervars(true), quoted(true)]),
	format(" : ", []),
	tptp_pp_wrap(F).

tptp_pp_('=>'(A, B)) :-
	!,
	tptp_pp_wrap(A),
	format("~n=> ", []),
	tptp_pp_wrap(B).

tptp_pp_('|'(A, B)) :-
	!,
	tptp_pp_wrap(A),
	format("~n| ", []),
	tptp_pp_wrap(B).

tptp_pp_('&'(A, B)) :-
	!,
	tptp_pp_wrap(A),
	format(" &~n", []),
	tptp_pp_wrap(B).

tptp_pp_('~'(A)) :-
	!,
	write('~'),
	tptp_pp_wrap(A).

tptp_pp_(A) :-
	write_term(A, [numbervars(true), quoted(true)]).


tptp_pp_wrap(A) :-
	write('('),
	tptp_pp_(A),
	write(')').
