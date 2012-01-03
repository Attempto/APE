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


:- module(prologfeatures, [
		resolve_features/1  % +Sourcefiles
	]).

/** <module> Prolog-Features

This module reimplements a part of the ProFIT transformation that is introduced in [1]. It
transforms Prolog programms that contain feature structures into plain Prolog programs. The
following constructs are supported (the numbers refer to the paper):

==
(1) Super > [Sub1, ..., Subn].
(3) Sort intro [Feature1:Restr1, ..., Featuren:Restrn].
(4) < Sort
(5) Feature ! Value
(6) Term & Term
(9) TemplateName := TemplateValue
( ) @ Template
( ) Term or Term
==


[1] Gregor Erbach. ProFIT: Prolog with Features, Inheritance and Templates. In Proceedings of
the seventh conference on European chapter of the Association for Computational Linguistics.
Morgan Kaufmann Publishers Inc., 1995.

@author Tobias Kuhn
@version 2008-02-15
*/


:- op(700, xfx, :=).
:- op(695, xfx, intro).
:- op(580, xfy, &).
:- op(570, xfy, !).
:- op(550, fx, @).
:- op(550, fx, <).
:- op(590, xfy, or).


%% resolve_features(+Sourcefiles)
%
% Reads the sourcefiles and transforms them by resolving the contained feature structures.

resolve_features(Sourcefiles) :-
    cleanup,
    read_sourcefiles(Sourcefiles),
    write_targetfiles.


%% sort_features(+Sort, -Features)
%
% Returns the list of features (without feature restrictions) for a certain sort. It fails if
% the sort does not have features.

:- dynamic(sort_feature/2).


%% feature(+Feature, -Sort, -FeatureRestr)
%
% The sort and the feature value are returned for a certain feature. This predicate might
% succeed several times.

:- dynamic(feature/3).


%% subsort(?Subsort, ?Supersort)
%
% Stores the subsort-supersort relations.

:- dynamic(subsort/2).


%% template(+TemplateName, -Template)
%
% Stores the templates. Several templates can have the same name.

:- dynamic(template/2).


%% term(-Term)
%
% Stores all the other terms that are not considered commands.

:- dynamic(term/1).


%% error_occurred
%
% Succeeds if an error occurred.

:- dynamic(error_occurred/0).


%% cleanup
%
% Retracts all the dynamic predicates.

cleanup :-
    retractall(sort_features(_,_)),
    retractall(feature(_,_,_)),
    retractall(subsort(_,_)),
    retractall(term(_)),
    retractall(template(_,_)),
    retractall(error_occurred).


%% read_sourcefiles(+Sourcefiles)
%
% Reads and processes the sourcefile(s).

read_sourcefiles([]).

read_sourcefiles([File|Rest]) :-
    read_sourcefiles(File),
    read_sourcefiles(Rest).

read_sourcefiles(File) :-
    \+ is_list(File),
    atom_concat(File, '.fit', FileC),
    open(FileC, read, In),
    write(user_error, 'Reading "'),
    write(user_error, FileC),
    write(user_error, '"\n'),
    assert(term('*** file ***'(File))),
    repeat,
      read_term(In, Term, [module(prologfeatures)]),
      store_term(Term),
      Term = end_of_file,
    close(In).


%% store_term(+Term)
%
% Stores the term in this module using the dynamic predicates.

store_term(end_of_file) :-
    !.

store_term(Sort > Subsorts intro Features) :-
    atom(Sort),
    \+ sort_features(Sort, _),
    \+ subsort(_, Sort),
    ground(Features),
    Features = [_|_],
    prune_features(Features, FeaturesT),
    ground(Subsorts),
    Subsorts = [_|_],
    store_subsorts(Subsorts, Sort),
    !,
    assert(sort_features(Sort, FeaturesT)),
    store_features(Features, Sort).

store_term(Sort > Subsorts intro Features) :-
    !,
    write_error_message('Invalid command', Sort > Subsorts intro Features).

store_term(Sort intro Features) :-
    atom(Sort),
    \+ sort_features(Sort, _),
    ground(Features),
    Features = [_|_],
    prune_features(Features, FeaturesT),
    !,
    assert(sort_features(Sort,FeaturesT)),
    store_features(Features, Sort).

store_term(Sort intro Features) :-
    !,
    write_error_message('Invalid command', Sort intro Features).

store_term(Sort > Subsorts) :-
    atom(Sort),
    \+ subsort(_, Sort),
    ground(Subsorts),
    Subsorts = [_|_],
    store_subsorts(Subsorts, Sort),
    !.

store_term(Sort > Subsorts) :-
    !,
    write_error_message('Invalid command', Sort > Subsorts).

store_term(TemplateName := Template) :-
    atom(TemplateName),
    !,
    assert(template(TemplateName, Template)).

store_term(TemplateName := Template) :-
    !,
    write_error_message('Invalid command', TemplateName := Template).

store_term(Term) :-
    assert(term(Term)).


%% prune_features(+FeaturesIn, -FeaturesOut)
%
% Removes the feature-values and keeps only the feature-names. The input is checked for
% wellformedness.

prune_features([], []).

prune_features([Feature|FeaturesRestIn], [Feature|FeaturesRestOut]) :-
    atom(Feature),
    \+ member(Feature, FeaturesRestIn),
    \+ member(Feature:_, FeaturesRestIn),
    !,
    prune_features(FeaturesRestIn, FeaturesRestOut).

prune_features([Feature:FeatureRestr|FeaturesRestIn], [Feature|FeaturesRestOut]) :-
    atom(Feature),
    atom(FeatureRestr),
    \+ member(Feature, FeaturesRestIn),
    \+ member(Feature:_, FeaturesRestIn),
    !,
    prune_features(FeaturesRestIn, FeaturesRestOut).

prune_features([Feature|_], _) :-
    write_error_message('Invalid or duplicated feature', Feature),
    fail.


%% store_features(+Features, +Sort)
%
% Stores the features using the dynamic feature/3 predicate.

store_features([], _).

store_features([Feature:FeatureRestr|FeaturesRest], Sort) :-
    !,
    assert(feature(Feature, Sort, FeatureRestr)),
    store_features(FeaturesRest, Sort).

store_features([Feature|FeaturesRest], Sort) :-
    assert(feature(Feature, Sort, '')),
    store_features(FeaturesRest, Sort).


%% store_subsorts(+Subsorts, +Supersort)
%
% Stores the subsort-supersort relations using the subsort/2 predicate.

store_subsorts([], _).

store_subsorts([Subsort|_], Sort) :-
    is_subsort_of(Sort, Subsort),
    !,
    write_error_message('Cycle introduced in sort hierarchy', Sort > Subsort),
    fail.

store_subsorts([Subsort|SubsortsRest], Sort) :-
    assert(subsort(Subsort, Sort)),
    store_subsorts(SubsortsRest, Sort).


%% write_targetfiles
%
% Transforms the input terms and writes the target files. If an error occurred
% in an ealier stage, the execution is aborted.

write_targetfiles :-
    error_occurred,
    !,
    write(user_error, 'EXECUTION ABORTED.\n').

write_targetfiles :-
    term(Term),
    process_term(Term),
    fail.

write_targetfiles :-
    told.


%% process_term(+Term)
%
% Transforms the term and writes the translated term(s) to the file.

process_term('*** file ***'(FileName)) :-
    !,
    told,
    atom_concat(FileName, '.plp', OutFile),
    write(user_error, 'Writing "'),
    write(user_error, OutFile),
    write(user_error, '"\n'),
    tell(OutFile).

process_term(Term) :-
    findall(TermT, transform(Term, TermT, []), TermsT),
    TermsT = [_|_],  % must have at least one element
    !,
    write_terms(TermsT).

process_term(Term) :-
    write_error_message('Invalid term', Term).


%% write_terms(+TermList)
%
% Writes each of the terms to the current output device.

write_terms([]).

write_terms([Term|TermsRest]) :-
    numbervars(Term, 0, _),
    write_term(Term, [character_escapes(true), quoted(true), numbervars(true), module(prologfeatures)]),
    write('.\n'),
    write_terms(TermsRest).


%% transform(+TermIn, -TermOut, +Templates)
%
% Transforms a term with feature structure into a term without. This predicate can succeed
% several times. The argument Templates stores the path of visited templates in order to detect
% loops.

transform(Var, Var, _) :-
    var(Var),
    !.

transform([], [], _) :-
	!.

transform([H1|T1], [H2|T2], Templates) :-
	!,
	transform(H1, H2, Templates),
	transform(T1, T2, Templates).

transform(Term, Term, _) :-
	Term =.. [Term],
	!.

transform(Feature ! Value, Term, Templates) :-
    !,
    atom(Feature),
    build_feature(Feature, Value, Term, Templates).

transform(< Sort, Term, _) :-
    !,
    build_sort(Sort, _, Term, _, _).

transform(@ TemplateName, Term, Templates) :-
    \+ member(TemplateName, Templates),
    !,
    template(TemplateName, Template),
    transform(Template, Term, [TemplateName|Templates]).

transform(@ TemplateName, _, Templates) :-
    !,
    write_error_message('Cyclic template definition', [TemplateName|Templates]),
    fail.

transform(T1 & T2, Term, Templates) :-
    !,
    transform(T1, Term, Templates),
    transform(T2, Term, Templates).

transform(T1 or T2, Term, Templates) :-
    !,
    (
    	transform(T1, Term, Templates)
    ;
    	transform(T2, Term, Templates)
    ).

transform(Term1, Term2, Templates) :-
	!,
	Term1 =.. List1,
	transform(List1, List2, Templates),
	Term2 =.. List2.


%% build_feature(+Feature, +Value, -Term, +Templates)
%
% Transforms a feature. This predicate can succeed several times. The argument Templates stores
% the path of visited templates in order to detect loops.

build_feature(Feature, Value, Term, Templates) :-
    feature(Feature, Sort, FeatureRestr),
    build_sort(Sort, _, Term, Feature, FeatureVar),
    transform(Value, ValueTerm, Templates),
    build_sort(FeatureRestr, _, ValueTerm, _, _),
    FeatureVar = ValueTerm.


%% build_sort(?Sort, +Content, -Term, +Feature, -FeatureVar)
%
% Transforms a sort. This predicate can succeed several times.

build_sort(Sort, _, _, _, _) :-
    Sort == '',
    !.

build_sort(Sort, Content, Term, Feature, FeatureVar) :-
    subsort(Sort, top),
    build_sort_args(1, Sort, Content, Args, Feature, FeatureVar),
    atom_concat('$', Sort, SortT),
    Term =.. [SortT,_|Args].

build_sort(Sort, Content, Term, Feature, FeatureVar) :-
    subsort(Sort, SuperSort),
    SuperSort \= top,
    build_sort_args(1, Sort, Content, Args, Feature, FeatureVar),
    atom_concat('$', Sort, SortT),
    TempTerm =.. [SortT|Args],
    build_sort(SuperSort, TempTerm, Term, _, _).


%% build_sort_args(+StepNumber, +Sort, +Content, -Args, +Feature, -FeatureVar)
%
% Builds the arguments for the transformed term.

build_sort_args(1, Sort, Content, [Content|Args], Feature, FeatureVar) :-
    subsort(_, Sort),
    !,
    build_sort_args(2, Sort, Content, Args, Feature, FeatureVar).

build_sort_args(1, Sort, Content, Args, Feature, FeatureVar) :-
    build_sort_args(2, Sort, Content, Args, Feature, FeatureVar).

build_sort_args(2, Sort, _, FeatureVarList, Feature, FeatureVar) :-
    sort_features(Sort, Features),
    !,
    create_featurevarlist(Features, FeatureVarList, Feature, FeatureVar).

build_sort_args(2, _, _, [], _, _).


%% create_featurevarlist(+FeatureList, -FeatureVarList, +Feature, -FeatureVar)
%
% Transforms a list of features (i.e. atoms) into a list of variables. The argument FeatureVar returns
% the variable for Feature.

create_featurevarlist([], [], _, _).

create_featurevarlist([Feature|FeaturesRest], [FeatureVar|FeatureVarListRest], Feature, FeatureVar) :-
    !,
    create_featurevarlist(FeaturesRest, FeatureVarListRest, Feature, FeatureVar).

create_featurevarlist([_|FeaturesRest], [_|FeatureVarListRest], Feature, FeatureVar) :-
    !,
    create_featurevarlist(FeaturesRest, FeatureVarListRest, Feature, FeatureVar).


%% is_subsort_of(+Subsort, +Sort)
%
% Succeeds if Subsort is a direct or indirect subsort of Sort.

is_subsort_of(Sort, Sort).

is_subsort_of(Subsort, Sort) :-
    subsort(Subsort, T),
    is_subsort_of(T, Sort).


%% write_error_message(+Message, +Term)
%
% Writes the error message to the error device.

write_error_message(Message, Term) :-
    assert(error_occurred),
    write(user_error, 'ERROR: '),
    write(user_error, Message),
    write(user_error, ': '),
    write_term(user_error, Term, [module(prologfeatures), quoted(true)]),
    write(user_error, '\n').
