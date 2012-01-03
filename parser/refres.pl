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


%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  Resolution of anaphors in discourse representation structures
%
%  N. E. Fuchs, IFI University of Zurich
%
%  May 31, 2005
%
%  Version 110724
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  Resolution of Anaphors
%
%    Resolution is governed by accessibility, closest textual precedence, specificity, and reflexivity.
%
%    Accessibility rules: a discourse referent is accessible from a DRS D if the discourse referent is in D, in a DRS enclosing D, in the antecedent of an 
%    ifthen-DRS with D as consequent, or in a disjunct that precedes D in an or-DRS. 
%
%    If the anaphor is a non-reflexive personal pronoun or a non-reflexive possessive pronoun the resolution algorithm picks the closest preceding accessible 
%    noun phrase that agrees in genus, numerus and person with the anaphor, and that is not the subject of the sentence. If resolution fails, an error situation 
%    arises.
%
%    Reflexive personal and possessive pronouns are resolved by APE that picks the subject of the sentence in which the pronoun occurs if the subject agrees in 
%    genus, numerus and person with the anaphor. If resolution fails, APE generates an error message.
%
%    If the anaphor is a definite noun phrase the resolution algorithm picks the closest preceding accessible noun phrase with matching genus and person and matching 
%    numerus - where a singular anaphor matches a singular countable or a mass antecedent, and a plual anaphor matches a plural countable antecedent. Either all 
%    anaphor conditions are a subset of the antecedent conditions, or a part of the anaphor conditions including the condition of the main noun is a subset of 
%    the  antecedent conditions, while the remaining anaphor conditions match conditions found in the DRS. Complex conditions of the anaphor that lead to sub-DRSs 
%    have to exactly unify with respective sub-DRSs in the antecedent. If resolution fails, the anaphor is treated as a new indefinite noun phrase, and a warning 
%    is generated.
%
%    If the anaphor is a variable the resolution algorithm picks the closest preceding accessible identical bare variable, or the closest preceding accessible 
%    noun phrase that has the variable as opposition. If variables are redefined in the same accsibility range, or if resolution fails, an error situation arises.
%
%    Proper nouns are unique and accessible from everywhere.
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  Representation of Antecedents and Anaphors
%
%    A proper name is represented in the DRS by the single condition
%
%    anaphor(Anaphortype, AnaphorID, AnaphorReferent, AnaphorConditions, AnaphorGenus, AnaphorNumerus, AnaphorPerson, SentenceID, TokenID, Tokens, SentenceSubject)
%
%    where
%
%    - Anaphortype is proper_name
%    - AnaphorID is the number of the noun phrase
%    - AnaphorReferent is named(proper name)
%    - AnaphorConditions is []
%    - AnaphorGenus is one of masc, fem, human, neutr, encoded in a way that supports the gender hierarchy
%    - AnaphorNumerus is one of sg, pl, mass, encoded in a way that supports the number hierarchy
%    - AnaphorPerson is one of second, third
%    - SentenceID is the number of the sentence in which the anaphor occurs
%    - TokenID is the number of the first token of the anaphor
%    - Tokens is the proper name without a possibly occurring definite determiner
%    - SentenceSubject is ''
%
%
%    An indefinite noun phrase is represented in the DRS by its DRS conditions plus one additional condition
%
%    antecedent(AntecedentID, AntecedentReferent, AntecedentConditions, AntecedentGenus, AntecedentNumerus, AntecedentPerson, AntecedentSID, AntecedentTID, AntecedentTokens)
%
%    if the indefinite noun phase has an attached variable then follows a further condition 
%
%    antecedent(AntecedentIDV, AntecedentReferent, AntecedentConditionsV, AntecedentGenus, AntecedentNumerus, AntecedentPerson, AntecedentSID, AntecedentTID, AntecedentTokensV]
%
%    where
%
%    - AntecedentID is the number of the noun phrase
%    - AntecedentIDV is AntecedentID + 1
%    - AntecedentReferent is the main discourse referent of the noun phrase
%    - AntecedentConditions is the list of the conditions that the noun phrase contributes to the DRS; minus any conditions that 
%      refer to anaphors (e.g. as in 'a man who sees himself' where 'himself' is represented by a separate anaphor/10 condition)
%    - AntecedentConditionsV contains just the condition for the variable
%    - AntecedentGenus is one of masc, fem, human, neutr, encoded in a way that supports the gender hierarchy
%    - AntecedentNumerus is one of sg, pl, mass, encoded in a way that supports the number hierarchy
%    - AntecedentPerson is one of second, third
%    - AntecedentSID is the number of the sentence in which the antecedent occurs
%    - AntecedentTID is the number of the first token of the antecedent
%    - AntecedentTokens is the noun of the main condition of the antecedent 
%    - AntecedentTokensV is the name of the variable
%
%
%    Indefinite pronouns - 'someone', 'somebody', 'something' and their negated and universal forms - are treated like an indefinite noun phrase.
%
%
%    A definite noun phrase is represented in the DRS by the single condition
%
%    anaphor(Anaphortype, AnaphorID, AnaphorReferent, AnaphorConditions, AnaphorGenus, AnaphorNumerus, AnaphorPerson, AnaphorSID, AnaphorTID, AnaphorTokens, SentenceSubject)
%
%    if the indefinite noun phase has an attached variable then follows a further condition 
%
%    antecedent(AntecedentIDV, AntecedentReferent, AntecedentConditionsV, AntecedentGenus, AntecedentNumerus, AntecedentPerson, AntecedentSID, AntecedentTID, AntecedentTokens]
%
%    where
%
%    - Anaphortype is definite_noun_phrase
%    - AnaphorID is the number of the noun phrase
%    - AntecedentIDV is AnaphorID + 1
%    - AnaphorReferent is the main discourse referent of the noun phrase
%    - AnaphorConditions is the list of the conditions that the noun phrase contributes to the DRS; minus any conditions that 
%      refer to other anaphors (e.g. as in 'a man who sees himself/the dog' where 'himself/the dog' is represented by a separate anaphor/10
%      condition)
%    - AntecedentConditionsV contains just the condition for the variable
%    - AnaphorGenus is one of masc, fem, human, neutr, encoded in a way that supports the gender hierarchy
%    - AnaphorNumerus is one of sg, pl, mass, encoded in a way that supports the number hierarchy
%    - AnaphorPerson is one of second, third
%    - AnaphorSID is the number of the sentence in which the anaphor occurs
%    - AnaphorTID is the number of the first token of the anaphor
%    - AnaphorTokens is the noun of the main condition of the anaphor 
%    - AntecedentTokensV is the name of the variable
%    - SentenceSubject is ''
%
%
%    A nonreflexive personal or possessive pronoun is represented in the DRS by the single condition
%
%    anaphor(Anaphortype, AnaphorID, AnaphorReferent, AnaphorConditions, AnaphorGenus, AnaphorNumerus, AnaphorPerson, AnaphorSID, AnaphorTID, AnaphorTokens, SentenceSubject)
%
%    where
%
%    - Anaphortype is nonreflexive_pronoun
%    - AnaphorID is the number of the noun phrase
%    - AnaphorReferent is the main discourse referent of the noun phrase
%    - AnaphorConditions is the list of the conditions that the noun phrase would contribute to the DRS; for pronouns AnaphorConditions is empty; for
%      variables there is the single condition variable/2
%    - AnaphorGenus is one of masc, fem, human, neutr, encoded in a way that supports the gender hierarchy
%    - AnaphorNumerus is one of sg, pl, mass, encoded in a way that supports the number hierarchy
%    - AnaphorPerson is one of second, third
%    - AnaphorSID is the number of the sentence in which the anaphor occurs
%    - AnaphorTID is the number of the first token of the anaphor
%    - AnaphorTokens is the text token of the anaphor 
%    - SentenceSubject is "subj(discourse referent of the subject of the sentence)", or "nosubj" if there is no subject
%
%
%    A bare variable is represented in the DRS by the single condition
%
%    anaphor(Anaphortype, AnaphorID, AnaphorReferent, AnaphorConditions, AnaphorGenus, AnaphorNumerus, AnaphorPerson, AnaphorSID, AnaphorTID, AnaphorTokens, SentenceSubject)
%
%    where
%
%    - Anaphortype is one of nonreflexive_pronoun, reflexive_pronoun, variable
%    - AnaphorID is the number of the noun phrase
%    - AnaphorReferent is the main discourse referent of the noun phrase
%    - AnaphorConditions is the list of the conditions that the noun phrase would contribute to the DRS; for pronouns AnaphorConditions is empty; for
%      variables there is the single condition variable/2
%    - AnaphorGenus is one of masc, fem, human, neutr, encoded in a way that supports the gender hierarchy
%    - AnaphorNumerus is one of sg, pl, mass, encoded in a way that supports the number hierarchy
%    - AnaphorPerson is one of second, third
%    - AnaphorSID is the number of the sentence in which the anaphor occurs
%    - AnaphorTID is the number of the first token of the anaphor
%    - AnaphorTokens is the text token of the anaphor 
%    - SentenceSubject is ''
%
%
%    Essential assumptions
%
%    - noun phrases are numbered by AntecedentID and AnaphorID in textual order
%    - textual order of antecedents and anaphors is preserved in DRS processed by refres
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  declarations
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

:- module(refres, [resolve_anaphors/2]).

%%%:- check.

:- dynamic(variable_defined/3).

:- dynamic(nesting_level/1).

:- op( 400,  xfx, :).           % label
:- op( 400,  fy, -).            % negation
:- op( 400,  fy, ~).            % negation as failure
:- op( 600, xfy, v).            % disjunction
:- op( 650, xfy, =>).           % implication 

:- use_module('../logger/error_logger', [add_error_message_once/4, add_warning_message_once/4]).
:- use_module('../lexicon/lexicon_interface', [noun_pl/3]).

%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  resolve_anaphors(+DRSIn, -DRSOut)
%
%    - resolve_anaphors/3 resolves all anaphors of DRSIn against antecedents within DRSIn
%
%    - DRSIn contains additional conditions for antecedents (antecedent/8) and for anaphors (anaphor/10)
%    - DRSOut is DRSIn 
%			without the conditions antecedent/8 and anaphor/10,
%			without the conditions variable/2,
%      		with resolved anaphors
%	 - if refres generates error messages then DRSOut should be discarded since it is possibly corrupted
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

resolve_anaphors(drs(ReferentsIn, ConditionsIn), DRSOut) :-
  catch(call(resolve_anaphors1(drs(ReferentsIn, ConditionsIn), DRSOut)), CatchType, add_error_message_once(anaphor, '', CatchType, 'Send screenshot to APE developers.')).

resolve_anaphors1(drs(ReferentsIn, ConditionsIn), drs(ReferentsOut, ConditionsOut)) :-
  initialise_DRS_nesting_level,
  % enforce ordering of this DRS nesting level
  enforce_order(ConditionsIn, ConditionsInUpdated, [], ConditionsAllOut, [], AntecedentsIn),
  % match anaphors to antecedents, and collect all proper names
  resolve_all_anaphors(drs(ReferentsIn, ConditionsInUpdated), ConditionsAllOut, AntecedentsIn, _AntecedentsOut, [], _ProperNamesOut, drs(ReferentsOut, ConditionsIntermediate)),
  % remove duplicate conditions that are artefacts of anaphors like in "There is a man. The cat of the man sleeps." 
  % where "the cat of the man" cannot be resolved, and thus leaves two conditions for "man"
  filter_conditions(ConditionsIntermediate, ConditionsOut),
  % clean up for next run
  cleanup.

  
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  resolve_all_anaphors(+DRSIn, ConditionsAll, +AntecedentsIn, -AntecedentsOut, +ProperNamesIn, -ProperNamesOut, -DRSOut)
%
%    - resolve_all_anaphors/7 processes all conditions of DRSIn in depth-first order, and identifies them as antecedents, anaphors, complex conditions, or simple 
%      conditions. Antecedents are collected in AntecedentsIn, anaphors are resolved in textual order calling resolve_one_anaphor/9, complex conditions lead to  
%      recursive calls of resolve_all_anaphors/7 taking into account the nesting of DRSs (respectively the accessibility of referents), while simple conditions ?   
%      with the exception  of variable/2 ? are just copied to DRSOut.
%
%    - DRSIn contains additional conditions for antecedents (antecedent/8) and for anaphors (anaphor/10)
%    - DRSOut is DRSIn without the additional conditions and without the conditions variable/2
%    - ConditionsAll contains all conditions of the current DRS nesting level; used in resolve_one_anaphor/9 to resolve pronouns
%    - AntecedentsIn/AntecedentsOut is a threaded pair of lists of antecedents
%    - ProperNamesIn/ProperNamesOut is a threaded pair of lists of proper names
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

resolve_all_anaphors(drs(Referents, []), _ConditionsAllIn, AntecedentsIn, AntecedentsOut, ProperNames, ProperNames, drs(Referents, [])) :-
  !,
  (
    var(AntecedentsOut)
    ->
    AntecedentsOut = AntecedentsIn
  ;
    true
  ).

resolve_all_anaphors(drs(ReferentsIn, [Condition|Conditions]), ConditionsAllIn, AntecedentsIn, AntecedentsOut, ProperNamesIn, ProperNamesOut, DRSOut) :-
  (
    % simple conditions _-_  with the exception of variable(_, _)-_ are just copied
    Condition = _-_
    ->
    (
      Condition = variable(_, _)-_
      ->
      % variables are checked for redefinition
      define_new_variable(Condition, apposition), 
      DRSOut = drs(ReferentsOut, ConditionsOut)
    ;
      DRSOut = drs(ReferentsOut, [Condition|ConditionsOut])
    ),
    resolve_all_anaphors(drs(ReferentsIn, Conditions), ConditionsAllIn, AntecedentsIn, AntecedentsOut, ProperNamesIn, ProperNamesOut, drs(ReferentsOut, ConditionsOut))
  ;
    % antecedent
    Condition = antecedent(_, _, _, _, _, _, _, _, _)
    ->
    % add antecedent in correct order
    insert_antecedent(Condition, AntecedentsIn, AntecedentsIM),
    % continue loop
    resolve_all_anaphors(drs(ReferentsIn, Conditions), ConditionsAllIn, AntecedentsIM, AntecedentsOut, ProperNamesIn, ProperNamesOut, DRSOut)
  ;
    % anaphoric reference
    Condition = anaphor(_, AnaphorID1, _, _, _, _, _, _, _, _, _)
    ->
    (
      % Conditions contains an anaphor with smaller ID
      append(Front, [anaphor(Anaphortype2, AnaphorID2, AnaphorReferent2, AnaphorConditions2, AnaphorGenus2, AnaphorNumerus2, AnaphorPerson2, SentenceID2, TokenID2, Tokens2, Subject2)|Rest], Conditions),
      AnaphorID2 < AnaphorID1
      ->
      % permute anaphors
      append(Front,[anaphor(Anaphortype2, AnaphorID2, AnaphorReferent2, AnaphorConditions2, AnaphorGenus2, AnaphorNumerus2, AnaphorPerson2, SentenceID2, TokenID2, Tokens2, Subject2), Condition|Rest], PermutedConditions),
      resolve_all_anaphors(drs(ReferentsIn, PermutedConditions), ConditionsAllIn, AntecedentsIn, AntecedentsOut, ProperNamesIn, ProperNamesOut, DRSOut)
    ;
      % anaphor is in textual order
      resolve_one_anaphor(drs(ReferentsIn, Conditions), ConditionsAllIn, ConditionsAllOut, Condition, AntecedentsIn, AntecedentsIntermediate, ProperNamesIn, ProperNamesIM, drs(ReferentsIM, ConditionsIM)),     
      % continue loop
      resolve_all_anaphors(drs(ReferentsIM, ConditionsIM), ConditionsAllOut, AntecedentsIntermediate, AntecedentsOut, ProperNamesIM, ProperNamesOut, DRSOut)
    )
  ;
    % nested list
    is_list(Condition)
    ->
    DRSOut = drs(ReferentsOut, [ConditionListOut|ConditionsOut]),
    resolve_all_anaphors(drs(ReferentsIn, Condition), ConditionsAllIn, AntecedentsIn, AntecedentsTemp, ProperNamesIn, ProperNamesTemp, drs(ReferentsTemp, ConditionListOut)),
    resolve_all_anaphors(drs(ReferentsTemp, Conditions), ConditionsAllIn, AntecedentsTemp, AntecedentsOut, ProperNamesTemp, ProperNamesOut, drs(ReferentsOut, ConditionsOut))
  ;
    % embedded DRSs 
    (
      % negation
      Condition = - drs(ReferentsNIn, ConditionsNIn)
    ;
      % negation as failure
      Condition = ~ drs(ReferentsNIn, ConditionsNIn)
    ;
      % possibility
      Condition = can(drs(ReferentsNIn, ConditionsNIn))
    ;
      % necessity
      Condition = must(drs(ReferentsNIn, ConditionsNIn))
    ;
      % recommendation
      Condition = should(drs(ReferentsNIn, ConditionsNIn))
    ;
      % admissibility
      Condition = may(drs(ReferentsNIn, ConditionsNIn))
    ;
      % single question
      Condition = question(drs(ReferentsNIn, ConditionsNIn))
    ;
      % single command
      Condition = command(drs(ReferentsNIn, ConditionsNIn))
    ;
      % sentence subordination
      Condition = _ : drs(ReferentsNIn, ConditionsNIn)
    )
    ->
    functor(Condition, Operator, _),
    arg(1, Condition, Label),
    increase_DRS_nesting_level,
    % enforce ordering of this DRS nesting level
    enforce_order(ConditionsNIn, ConditionsNInUpdated, ConditionsAllIn, ConditionsAllOut, AntecedentsIn, AntecedentsInUpdated),
    % resolve anaphors on the current DRS nesting level; do not export any antecedents
    resolve_all_anaphors(drs(ReferentsNIn, ConditionsNInUpdated), ConditionsAllOut, AntecedentsInUpdated, AntecedentsInUpdated, ProperNamesIn, ProperNamesIM, drs(ReferentsIntermediate, ConditionsIntermediate1)),
    % continue on the previous DRS nesting level with ConditionsAllIn and with AntecedentsIn extended by antecedents for proper names found on this level
    decrease_DRS_nesting_level,
    add_proper_name_antecedents(ProperNamesIn, ProperNamesIM, AntecedentsIn, AntecedentsInPlusProperNames),
    resolve_all_anaphors(drs(ReferentsIn, Conditions), ConditionsAllIn, AntecedentsInPlusProperNames, AntecedentsOut, ProperNamesIM, ProperNamesOut, drs(ReferentsOut, ConditionsOut)),
    % remove duplicate conditions that are artefacts of anaphors like "the cat of the man" that could not be resolved
    % and thus could leave two conditions for "man"
    filter_conditions(ConditionsIntermediate1, ConditionsIntermediate2),
    % build DRSOut
    build_drs(Operator, Label, drs(ReferentsIntermediate, ConditionsIntermediate2), NewDRS),
    DRSOut = drs(ReferentsOut, [NewDRS|ConditionsOut])
  ;
    % embedded DRSs: implication
    Condition = drs(Referents1In, Conditions1In) => drs(Referents2In, Conditions2In)
    ->
    increase_DRS_nesting_level,
    % enforce ordering and resolve anaphors on the current DRS nesting level
    enforce_order(Conditions1In, Conditions1InUpdated, ConditionsAllIn, ConditionsAllIM1, AntecedentsIn, AntecedentsIM1),
    % to resolve the anaphora in the precondition the conditions of the consequence may be needed (example: A clerk enters every card of himself. )
    append(ConditionsAllIM1, Conditions2In, ConditionsAllPlusConsequence),
    resolve_all_anaphors(drs(Referents1In, Conditions1InUpdated), ConditionsAllPlusConsequence, AntecedentsIM1, AntecedentsIM2, ProperNamesIn, ProperNamesIM1, drs(Referents1Out, Conditions1Intermediate)), 
    enforce_order(Conditions2In, Conditions2InUpdated, ConditionsAllIM1, ConditionsAllIM2, AntecedentsIM2, AntecedentsIM3),
    resolve_all_anaphors(drs(Referents2In, Conditions2InUpdated), ConditionsAllIM2, AntecedentsIM3, _AntecedentsOut, ProperNamesIM1, ProperNamesIM2, drs(Referents2Out, Conditions2Intermediate1)),
    % continue on the previous DRS nesting level with ConditionsAllIn and with AntecedentsIn extended by antecedents for proper names found on this level
    decrease_DRS_nesting_level,
    add_proper_name_antecedents(ProperNamesIn, ProperNamesIM2, AntecedentsIn, AntecedentsInPlusProperNames),
    resolve_all_anaphors(drs(ReferentsIn, Conditions), ConditionsAllIn, AntecedentsInPlusProperNames, AntecedentsOut, ProperNamesIM2, ProperNamesOut, drs(ReferentsOut, ConditionsOut)),
    % remove duplicate conditions that are artefacts of anaphors like "the cat of the man" that could not be resolved
    % and thus could leave two conditions for "man"
    filter_conditions(Conditions1Intermediate, Conditions1Out),
    filter_conditions(Conditions2Intermediate1, Conditions2Intermediate2),
    % remove from the consequence of the implication any conditions that already occur in the precondition - with the exception of formulas
    subtract_conditions(Conditions2Intermediate2, Conditions1Out, Conditions2Out),
    % build DRSOut
    DRSOut = drs(ReferentsOut, [drs(Referents1Out, Conditions1Out) => drs(Referents2Out, Conditions2Out)|ConditionsOut])    
  ;
    % embedded DRSs: disjunction
    % 'v' is interpreted as a right-associative binary operator
    % for instance, a disjunction of three disjuncts is represented as D1 v drs([], [D2 v D3]) where each disjunct D is a DRS
    Condition = drs(Referents1In, Conditions1In) v DisjunctRestIn
    ->
    increase_DRS_nesting_level,
    % enforce ordering for first disjunct
    enforce_order(Conditions1In, Conditions1InUpdated, ConditionsAllIn, ConditionsAllIM, AntecedentsIn, AntecedentsInUpdated),
    % resolve anaphors of first disjunct
    resolve_all_anaphors(drs(Referents1In, Conditions1InUpdated), ConditionsAllIM, AntecedentsInUpdated, AntecedentsIM, ProperNamesIn, ProperNamesIM1, drs(Referents1Out, Conditions1Intermediate)),
    % process rest of disjuncts
    DisjunctRestIn = drs(_ReferentsDisjunctRestIn, ConditionsDisjunctRestIn),
    enforce_order(ConditionsDisjunctRestIn, _ConditionsDisjunctRestInUpdated, ConditionsAllIM, ConditionsAllOut, AntecedentsIM, AntecedentsIMUpdated),
    resolve_all_anaphors(DisjunctRestIn, ConditionsAllOut, AntecedentsIMUpdated, _AntecedentsOut, ProperNamesIM1, ProperNamesIM2, drs(ReferentsRestOut, ConditionsRestIntermediate)),
    % continue on the previous DRS nesting level with ConditionsAllIn and with AntecedentsIn extended by antecedents for proper names found on this level
    decrease_DRS_nesting_level,
    add_proper_name_antecedents(ProperNamesIn, ProperNamesIM2, AntecedentsIn, AntecedentsInPlusProperNames),
    resolve_all_anaphors(drs(ReferentsIn, Conditions), ConditionsAllIn, AntecedentsInPlusProperNames, AntecedentsOut, ProperNamesIM2, ProperNamesOut, drs(ReferentsOut, ConditionsOut)),
    % remove duplicate conditions that are artefacts of anaphors like "the cat of the man" that could not be resolved
    % and thus could leave two conditions for "man"
    filter_conditions(Conditions1Intermediate, Conditions1Out),
    filter_conditions(ConditionsRestIntermediate, ConditionsRestOut),
    % build DRSOut
    DRSOut = drs(ReferentsOut, [drs(Referents1Out, Conditions1Out) v drs(ReferentsRestOut, ConditionsRestOut)|ConditionsOut])
  ).


%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  resolve_one_anaphor(+drs(ReferentsIn, ConditionsIn), ConditionsAllIn, ConditionsAllOut, +Anaphor, +AntecedentsIn, -AntecedentsOut, +ProperNamesIn, -ProperNamesOut, -drs(ReferentsOut, ConditionsOut))
%
%    - resolve_one_anaphor/9 matches an Anaphor against the Antecedents, and then adapts the ReferentsIn and ConditionsIn of the DRS containing Anaphor generating 
%      ReferentsOut and ConditionsOut; furthermore it collects the proper names in the threaded pair of lists ProperNamesIn/ProperNamesOut
%
%    - ConditionsAllIn/ConditionsAllOut is a threaded pair of all conditions of the current DRS level
%    - Anaphor is the anaphor being processed
%    - AntecedentsIn/AntecedentsOut  is a threaded pair of lists of antecedents
%    - ProperNamesIn/ProperNamesOut is a threaded pair of lists of proper names
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

resolve_one_anaphor(drs(ReferentsIn, ConditionsIn), ConditionsAllIn, ConditionsAllOut, Anaphor, AntecedentsIn, AntecedentsOut, ProperNamesIn, ProperNamesOut, drs(ReferentsOut, ConditionsOut)) :-
  (
    % non-reflexive personal pronoun or non-reflexive possessive pronoun
    Anaphor = anaphor(nonreflexive_pronoun, AnaphorID, AnaphorReferent, AnaphorConditions, AnaphorGenus, AnaphorNumerus, AnaphorPerson, AnaphorSID, AnaphorTID, AnaphorTokens, SentenceSubject)
    ->
    (
      % non-reflexive personal pronoun or non-reflexive possessive pronoun can be resolved 
      % constraints: AnaphorID > AntecedentID, AnaphorGenus = AntecedentGenus, AnaphorNumerus = AntecedentNumerus, AnaphorPerson = AntecedentPerson; a non-reflexive pronoun can occur as subject, 
      % as object or in a prepositional phrase, but it must not refer to the subject of the sentence in which it occurs
      member(antecedent(AntecedentID,  AntecedentReferent, _AntecedentConditions, AnaphorGenus, AnaphorNumerus, AnaphorPerson, AntecedentSID, AntecedentTID, AntecedentTokens), AntecedentsIn),
      AnaphorID > AntecedentID,
      SentenceSubject \== subj(AntecedentReferent)
      ->
      delete_all_occurrences_of_one_discourse_referent(ReferentsIn, AnaphorReferent, ReferentsOut),
      (
        % add anaphors "he", "she", "he/she", "it", "they" to antecedents to handle cases like 'He sees himself.' ...
        (AnaphorTokens = 'he' ; AnaphorTokens = 'she' ; AnaphorTokens = 'he/she' ; AnaphorTokens = 'it' ; AnaphorTokens = 'they')
        ->
        ConditionsOut = [antecedent(AnaphorID, AnaphorReferent, AnaphorConditions, AnaphorGenus, AnaphorNumerus, AnaphorPerson, AnaphorSID, AnaphorTID, AnaphorTokens)|ConditionsIn]
      ;
        % ... but not the anaphors "him", "her", "him/her", "them", "his", "her", "his/her", "its", "their"
        ConditionsOut = ConditionsIn
      ),
      ConditionsAllOut = ConditionsAllIn,
      AnaphorReferent = AntecedentReferent,
      AntecedentsOut = AntecedentsIn,
      ProperNamesOut = ProperNamesIn
    ;
      % non-reflexive personal pronoun or non-reflexive possessive pronoun could not be resolved
      % create error message and continue resolution
      ReferentsOut = ReferentsIn,
      ConditionsOut = ConditionsIn,
      ConditionsAllOut = ConditionsAllIn,
      ProperNamesOut = ProperNamesIn,
      AntecedentsOut = AntecedentsIn,
      atom_concat('Unresolved anaphor: ', AnaphorTokens, ErrorText),
	  add_error_message_once(anaphor, AnaphorSID-AnaphorTID, ErrorText, 'Identify correct accessible antecedent.')
    )
  ;
    % definite noun phrase
    Anaphor = anaphor(definite_noun_phrase, AnaphorID, AnaphorReferent, AnaphorConditions, AnaphorGenus, AnaphorNumerus, AnaphorPerson, AnaphorSID, AnaphorTID, AnaphorTokens, SentenceSubject)
    ->
    % if AnaphorConditions contains a condition for a variable then check for variable redefinition
    (
      member(variable(VariableReferent, VariableName)-AnaphorSID/_, AnaphorConditions),
      VariableReferent == AnaphorReferent
      ->
      (
        % variable is redefined on the same or a higher nesting level
        variable_defined(VariableName, DRSNestingLevel, bare),
        nesting_level(CurrentNestingLevel),
        CurrentNestingLevel >= DRSNestingLevel
        ->
        atom_concat('Redefined variable: ', VariableName, ErrorText),
        add_error_message_once(anaphor, AnaphorSID - AnaphorTID, ErrorText, 'Assign unique variables.')
      ;
        true
      )
    ;
      true
    ),
    (
      % definite noun phrase can be completely resolved against one antecedent
      % examples: "a red man who owns a dog" -> "the red man who owns a dog", "a who man does not work patiently" -> "the man who does not work patiently"
      % constraints: AnaphorID > AntecedentID, AnaphorGenus = AntecedentGenus, AnaphorPerson = AntecedentPerson, and ...   
      member(antecedent(AntecedentID, AntecedentReferent, AntecedentConditions, AnaphorGenus, _AntecedentNumerus, AnaphorPerson, AntecedentSID, AntecedentTID, AntecedentTokens), AntecedentsIn),
      AnaphorID > AntecedentID,
      % ... main noun of anaphor matches main noun of antecedent and ...
      AnaphorTokens = AntecedentTokens,
      % ... anaphor is not part of antecedent - as in "a card of the card X1" - and ...
      \+ (member(relation(ReferentX, of, ReferentY)-AntecedentSID/_, AntecedentConditions), ReferentX == AntecedentReferent, ReferentY == AnaphorReferent), 
      % ... does not wrongly refer to a variable - as the last "the card X1" in "If a card X1 is a card of the card X1 then a man enters the card X1." - and ...
      (
        member(variable(ReferentZ, _X1)-AntecedentSID/_, AntecedentConditions)
        ->
        ReferentZ == AntecedentReferent
      ;
        true
      ),
      % ... AnaphorConditions are a subset of the AntecedentConditions
      \+ \+ (
              % decouple variables of antecedent and anaphor and ...
              Antecedent = antecedent(AntecedentID, AntecedentReferent, AntecedentConditions, AnaphorGenus, AntecedentNumerus, AnaphorPerson, AntecedentSID, AntecedentTID, AntecedentTokens),
              copy_term(Antecedent, CopiedAntecedent),
              % ... ground variables of CopiedAntecedent to preserve their correct syntactic relations and ...
              numbervars(CopiedAntecedent, 1, _),
			  % ... unify AnaphorReferent with ground CopiedAntecedentReferent and ...
              CopiedAntecedent = antecedent(_, CopiedAntecedentReferent, CopiedAntecedentConditions, _, _, _, _, _, _),
			  AnaphorReferent = CopiedAntecedentReferent, 
			  % ... match all AnaphorConditions with CopiedAntecedentConditions
              match_elements(AnaphorConditions, CopiedAntecedentConditions)
            )
      ->
      % Before eliminating the referents of AnaphorConditions from the domain make sure that there is not the case "the card of who" that leads
      % to the anaphor conditions [relation(E, of, B)-1, object(E, card, countable, na, eq, 1)-1]. Removing all referents of AnaphorConditions would
      % also incorrectly remove the referent B that is defined elsewhere.
      (
        \+ \+ member(relation(AnaphorReferent, of, Owner1)-AnaphorSID/_, AnaphorConditions),
        \+ (member(object(Owner2, _, _, _, _, _)-AnaphorSID/_, AnaphorConditions), Owner1 == Owner2)
        ->
        select(relation(_SomeObject, of, _Owner)-AnaphorSID/_, AnaphorConditions, RestAnaphorConditions)
      ;
        RestAnaphorConditions = AnaphorConditions
      ), 
      % eliminate referents of RestAnaphorConditions
      term_variables(RestAnaphorConditions, AnaphorReferents),
      delete_all_occurrences_of_all_discourse_referents(ReferentsIn, AnaphorReferents, ReferentsOut),
      % eliminate from AntecedentsIn all antecedents following Anaphor whose conditions are a subset of AnaphorConditions
      eliminate_spurious_antecedents(AntecedentsIn, AnaphorID, AnaphorConditions, AntecedentsOut),
      ConditionsOut = ConditionsIn,
      ConditionsAllOut = ConditionsAllIn,
      AnaphorReferent = AntecedentReferent,
      ProperNamesOut = ProperNamesIn
    ;
	  % definite noun phrase can be resolved  partially against an antecedent and partially against other DRS conditions
      % examples: "a red man owns a dog" -> "the red man who owns a dog", "a man does not work patiently" -> "the man who does not work patiently"
      % constraints: AnaphorID > AntecedentID, AnaphorGenus = AntecedentGenus, AnaphorPerson = AntecedentPerson, and ...   
      member(antecedent(AntecedentID, AntecedentReferent, AntecedentConditions, AnaphorGenus, _AntecedentNumerus, AnaphorPerson, AntecedentSID, AntecedentTID, AntecedentTokens), AntecedentsIn),
      AnaphorID > AntecedentID,
      % ... main noun of anaphor matches main noun of antecedent and ...
      AnaphorTokens = AntecedentTokens,
      % ... AnaphorConditions are a subset of the AntecedentConditions and ConditionsAllIn
      \+ \+ (
              % decouple variables of Antecedent and ConditionsAllIn from those of Anaphor and ...
              Antecedent = antecedent(AntecedentID, AntecedentReferent, AntecedentConditions, AnaphorGenus, AntecedentNumerus, AnaphorPerson, AntecedentSID, AntecedentTID, AntecedentTokens),
              copy_term((Antecedent, ConditionsAllIn), (CopiedAntecedent, CopiedConditionsAllIn)),
              % ... ground variables of CopiedAntecedent and CopiedConditionsAllIn to preserve their correct syntactic relations and ...
              numbervars((CopiedAntecedent, CopiedConditionsAllIn), 1, _),
			  % ... unify AnaphorReferent with ground CopiedAntecedentReferent and ...
              CopiedAntecedent = antecedent(_, CopiedAntecedentReferent, CopiedAntecedentConditions, _, _, _, _, _, _),
			  AnaphorReferent = CopiedAntecedentReferent, 
              % ... identify main noun of anaphor conditions and ...
              once(append(Front, [object(AnaphorReferent, AnaphorTokens, Quant, Unit, Op, Count)-AnaphorSID/TID|Tail], AnaphorConditions)), 
              once(append(Front, Tail, RemainingAnaphorConditions)),
              MainAnaphorCondition = object(AnaphorReferent, AnaphorTokens, Quant, Unit, Op, Count)-AnaphorSID/TID,
              % ... match MainAnaphorCondition with AntecedentConditions and ...
              match_elements([MainAnaphorCondition], AntecedentConditions),
			  % ... match RemainingAnaphorConditions with concatenation of CopiedAntecedentConditions and CopiedConditionsAllIn
              append(CopiedAntecedentConditions, CopiedConditionsAllIn, CopiedAntecedentConditionsAndCopiedConditionsAllIn),
              match_elements(RemainingAnaphorConditions, CopiedAntecedentConditionsAndCopiedConditionsAllIn)
            )
      ->
      term_variables(AnaphorConditions, AnaphorReferents),
      delete_all_occurrences_of_all_discourse_referents(ReferentsIn, AnaphorReferents, ReferentsOut),
      % eliminate from AntecedentsIn all antecedents following Anaphor whose conditions are a subset of AnaphorConditions
      eliminate_spurious_antecedents(AntecedentsIn, AnaphorID, AnaphorConditions, AntecedentsOut),
      ConditionsOut = ConditionsIn,
      ConditionsAllOut = ConditionsAllIn,
      AnaphorReferent = AntecedentReferent,
      ProperNamesOut = ProperNamesIn
   ;
      % definite noun phrase could not be resolved
      % treat it as indefinite noun phrase
      ReferentsOut = ReferentsIn,
      ProperNamesOut = ProperNamesIn,
      AntecedentsOut = AntecedentsIn,
      % add its conditions without variable conditions to the DRS
      delete_all_occurrences_of_all_elements(AnaphorConditions, [variable(_, _)-_], AnaphorConditionsWithoutVariables),
      append(AnaphorConditionsWithoutVariables, ConditionsIn, ConditionsIM),
      % add anaphor condition as antecedent condition to ConditionsOut so that it will be added to antecedents in the next round
      % necessary to handle cases like "The man sees himself."
      ConditionsOut = [antecedent(AnaphorID, AnaphorReferent, AnaphorConditions, AnaphorGenus, AnaphorNumerus, AnaphorPerson, AnaphorSID, AnaphorTID, AnaphorTokens)|ConditionsIM],
      % add  the anaphor conditions to ConditionsAll
      append(AnaphorConditions, ConditionsAllIn, ConditionsAllOut),
      % create a warning message 
      (
        % countable anaphor: distinguish between singular or plural
        \+ \+ member(object(AnaphorReferent, AnaphorTokens, countable, na, _Op, AnaphorCount)-AnaphorSID/_, AnaphorConditions)
        ->
        (
          % singular anaphor
          AnaphorCount = 1
          ->
          AnaphorNoun = AnaphorTokens
        ;
          % plural anaphor
          % AnaphorCount > 1
          lexicon_interface:noun_pl(AnaphorNoun, AnaphorTokens, _Gender)
          ->
          true
        ;
          % anaphor not in any lexicon
          AnaphorNoun = AnaphorTokens
        ),
        concat_atom(['The definite noun phrase ''the ', AnaphorNoun, ''' does not have an antecedent and thus is not interpreted as anaphoric reference, but as a new indefinite noun phrase.'], ErrorText1),
        concat_atom(['If the definite noun phrase ''the ', AnaphorNoun, ''' should be an anaphoric reference then you must introduce an appropriate antecedent.'], ErrorText2),
        add_warning_message_once(anaphor, AnaphorSID-AnaphorTID, ErrorText1, ErrorText2)
      ;
        % mass anaphor
        member(object(AnaphorReferent, AnaphorTokens, mass, na, na, na)-AnaphorSID/_, AnaphorConditions)
        ->
        concat_atom(['The definite noun phrase ''the ', AnaphorTokens, ''' does not have an antecedent and thus is not interpreted as anaphoric reference, but as a new indefinite noun phrase.'], ErrorText1),
        concat_atom(['If the definite noun phrase ''the ', AnaphorTokens, ''' should be an anaphoric reference then you must introduce an appropriate antecedent.'], ErrorText2),
        add_warning_message_once(anaphor, AnaphorSID-AnaphorTID, ErrorText1, ErrorText2)
      )
    )
  ;
    % variable
    Anaphor = anaphor(variable, AnaphorID, AnaphorReferent, AnaphorConditions, _AnaphorGenus, AnaphorNumerus, AnaphorPerson, AnaphorSID, AnaphorTID, AnaphorTokens, SentenceSubject)
    ->
    (
      % variable can be resolved
      % constraints: AnaphorID > AntecedentID, AnaphorNumerus = AntecedentNumerus, AnaphorPerson = AntecedentPerson, and AnaphorConditions are identical to the AntecedentConditions
      member(antecedent(AntecedentID, AntecedentReferent, AntecedentConditions, _AntecedentGenus, AnaphorNumerus, AnaphorPerson, AntecedentSID, AntecedentTID, AntecedentTokens), AntecedentsIn),
      AnaphorID > AntecedentID,
      % AnaphorGenus = AntecedentGenus, % variables are assigned a numerus but not a genus
      AntecedentConditions = [variable(AntecedentReferent, AntecedentVariable)-_AntecedentIndex],
      AnaphorConditions = [variable(AnaphorReferent, AnaphorVariable)-_AnaphorIndex],
      AntecedentVariable == AnaphorVariable
      ->
      delete_all_occurrences_of_one_discourse_referent(ReferentsIn, AnaphorReferent, ReferentsOut),
      ConditionsOut = ConditionsIn,
      ConditionsAllOut = ConditionsAllIn,
      AntecedentsOut = AntecedentsIn,
      AnaphorReferent = AntecedentReferent,
      ProperNamesOut = ProperNamesIn      
    ;
      % variable could not be resolved
      % treat variable as new bare variable
      % store variable for later check of possible redefinition (as in "X sees a man X")
      AnaphorConditions = [Variable],
	  define_new_variable(Variable, bare),
      % insert "something" and an antecedent to allow anaphoric references to this new bare variable
      ConditionsOut = [antecedent(AnaphorID, AnaphorReferent, AnaphorConditions, _AnaphorGenus, AnaphorNumerus, AnaphorPerson, AnaphorSID, AnaphorTID, AnaphorTokens), 
                       object(AnaphorReferent,something,dom,na,na,na)-AnaphorSID/AnaphorTID|ConditionsIn],
      ReferentsOut = ReferentsIn,
      ConditionsAllOut = ConditionsAllIn,
      AntecedentsOut = AntecedentsIn,
      ProperNamesOut = ProperNamesIn      
    )
  ;
    % proper name
    Anaphor = anaphor(proper_name, AnaphorID, AnaphorReferent, AnaphorConditions, AnaphorGenus, AnaphorNumerus, AnaphorPerson, AnaphorSID, AnaphorTID, AnaphorTokens, SentenceSubject)
    ->
    (
      % proper name occurred previously
      % constraints: ProperNamesIn contains an entry matching AnaphorReferent, AnaphorConditions, AnaphorGenus, AnaphorNumerus, AnaphorPerson and AnaphorTokens
      member(proper_name(_ID, AnaphorReferent, AnaphorConditions, AnaphorGenus, AnaphorNumerus, AnaphorPerson, _SID, _TID, AnaphorTokens), ProperNamesIn) 
      -> 
      ReferentsOut = ReferentsIn,
      ConditionsOut = ConditionsIn,
      ConditionsAllOut = ConditionsAllIn,
      AntecedentsOut = AntecedentsIn,
      ProperNamesOut = ProperNamesIn
    ;
      % new proper name
      ReferentsOut = ReferentsIn,
      % an appropriate antecedent condition is added to allow references to the new proper name by pronouns 
      ConditionsOut = [antecedent(AnaphorID, AnaphorReferent, AnaphorConditions, AnaphorGenus, AnaphorNumerus, AnaphorPerson, AnaphorSID, AnaphorTID, AnaphorTokens)|ConditionsIn],
      ConditionsAllOut = ConditionsAllIn,
      AntecedentsOut = AntecedentsIn,
      % store proper name
      ProperNamesOut = [proper_name(AnaphorID, AnaphorReferent, AnaphorConditions, AnaphorGenus, AnaphorNumerus, AnaphorPerson, AnaphorSID, AnaphorTID, AnaphorTokens)|ProperNamesIn]
    )
 ;
    % unrecognised anaphor
    Anaphor = anaphor(_UnrecognizedAnaphorType, AnaphorID, AnaphorReferent, AnaphorConditions, AnaphorGenus, AnaphorNumerus, AnaphorPerson, AnaphorSID, AnaphorTID, AnaphorTokens, SentenceSubject) 
    ->
    % create error message and continue resolution
    ReferentsOut = ReferentsIn,
    ConditionsOut = [unresolved(Anaphor)|ConditionsIn],
    ConditionsAllOut = ConditionsAllIn,
    AntecedentsOut = AntecedentsIn,
    ProperNamesOut = ProperNamesIn,
    add_error_message_once(anaphor, AnaphorSID-AnaphorTID, 'Unrecognised anaphor.', 'Send screenshot to APE developers.')
  ).


%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  Groups of Supporting Predicates in Approximately Alphabetical Order
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  add_proper_name_antecedents(+ProperNamesThisLevel, +ProperNamesNestedLevel, +AntecedentsThisLevelIn, -AntecedentsThisLevelOut) 
%
%    - a nested level can introduce proper name anaphors, i.e. ProperNamesNestedLevel contains new elements compared to ProperNamesThisLevel
%    - these proper name anaphors are inserted as antecedents into AntecedentsThisLevelIn resulting in AntecedentsThisLevelOut
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

add_proper_name_antecedents(ProperNamesThisLevel, ProperNamesNestedLevel, AntecedentsThisLevelIn, AntecedentsThisLevelOut) :-
  % new proper names encountered on nested level?
  delete_all_occurrences_of_all_elements(ProperNamesNestedLevel, ProperNamesThisLevel, NewProperNames),
  (
    % no
    NewProperNames = []
    ->
    AntecedentsThisLevelOut = AntecedentsThisLevelIn
  ;
    % yes
    prepend_proper_name_antecedents(NewProperNames, AntecedentsThisLevelIn, AntecedentsThisLevelIntermediate1),
    sort(AntecedentsThisLevelIntermediate1, AntecedentsThisLevelIntermediate2),
    reverse(AntecedentsThisLevelIntermediate2, AntecedentsThisLevelOut)
  ).
  
prepend_proper_name_antecedents([], Antecedents, Antecedents).

prepend_proper_name_antecedents([proper_name(ID, Referent, Conditions, Genus, Numerus, Person, SID, TID, Tokens)|MoreProperNames], Antecedents, NewAntecedents) :-
  prepend_proper_name_antecedents(MoreProperNames, [antecedent(ID, Referent, Conditions, Genus, Numerus, Person, SID, TID, Tokens)|Antecedents], NewAntecedents).
  
  
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  build_drs(Operator, Label, DRS, NewDRS)
%
%    - for negation, negation as failure, possibility, necessity: NewDRS = Operator(DRS)
%    - for sentence subordination: NewDRS = Label : DRS
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

build_drs(Operator, Label, DRS, NewDRS) :-
  (
    var(Label)
	->
	NewDRS =.. [Operator, Label, DRS]
  ;
    % nonvar(Label)
	NewDRS =.. [Operator, DRS]
  ).


%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  check_for_redefined_variables(+Antecedents)
%
%    - checks whether the list of Antecedents contains redefined variables
%    - redefined variables can occur in antecedents that stand for indefinte noun phrases, or in antecedents that stand just for variables
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

check_for_redefined_variables([]).

check_for_redefined_variables([_Antecedent]).

% skip divider between DRS nesting levels
check_for_redefined_variables([divider_between_DRS_nesting_levels|Antecedents]) :-
  check_for_redefined_variables(Antecedents).

check_for_redefined_variables([Antecedent|Antecedents]) :-
  Antecedent = antecedent(AntecedentID1, AntecedentReferent1, AntecedentConditions1, _AnaphorGenus1, _AntecedentNumerus1, _AntecedentPerson1, AntecedentSID1, AntecedentTID1, _AntecedentTokens1), 
  (
    % variable is already defined in previous indefinite NP antecedent
    select(variable(Referent1, VariableName)-AntecedentSID1/_, AntecedentConditions1, [_|_]),
    Referent1 == AntecedentReferent1,
    member(antecedent(AntecedentID2, AntecedentReferent2, AntecedentConditions2, _AnaphorGenus2, _AntecedentNumerus2, _AntecedentPerson2, AntecedentSID2, _AntecedentTID2, _AntecedentTokens2), Antecedents),
    AntecedentID2 < AntecedentID1,
    select(variable(Referent2, VariableName)-AntecedentSID2/_, AntecedentConditions2, [_|_]),
    Referent2 == AntecedentReferent2
    ->
    atom_concat('Redefined variable: ', VariableName, ErrorText),
    add_error_message_once(anaphor, AntecedentSID1 - AntecedentTID1, ErrorText, 'Assign unique variables.')
  ;
    % variable is not redefined
    true
  ),
  % check remaining antecedents for redefinition
  check_for_redefined_variables(Antecedents).


%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  cleanup/0
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
    
cleanup :-
  retractall(variable_defined(_, _, _)).


%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  define_new_variable(+VariableCondition, +VariableType)
%
%    - each newly defined variable is asserted as variable_defined(VariableName, DRSNestingLevel, VariableType)
%    - VariableType is "bare" or "apposition"
%    - create an error message if a variable is redefined within its range of accessibility
%    - when the DRS nesting level is decreased all facts variable_defined/3 asserted for that level are retracted (cf. decrease_DRS_nesting_level)
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

define_new_variable(variable(_Referent, VariableName)-SentenceIndex/TokenIndex, VariableType) :-
  nesting_level(CurrentNestingLevel),
  (
    % variable is redefined on the same or a higher nesting level
    variable_defined(VariableName, DRSNestingLevel, _VariableType),
    CurrentNestingLevel >= DRSNestingLevel
    ->
    atom_concat('Redefined variable: ', VariableName, ErrorText),
    add_error_message_once(anaphor, SentenceIndex-TokenIndex, ErrorText, 'Assign unique variables.')
  ;
    true
  ),
  assert(variable_defined(VariableName, CurrentNestingLevel, VariableType)).


%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  delete_all_occurrences_of_all_elements(+ListIn, +ElementsToDelete, -ListOut)
%
%    - ListOut is ListIn without all occurrences of all the members of ElementsToDelete
%
%
%  delete_all(+ListIn, +ElementToDelete, -ListOut)
%
%    - ListOut is ListIn without all elements that unify with ElementsToDelete; i.e. ElementsToDelete works as a pattern against which the elements are matched
%    - notice that delete_all/3 differs from SWI Prologs built-in delete/3 that deletes all elements of ListIn that *simultaneously* unify with ElementToDelete
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
		
delete_all_occurrences_of_all_elements(ListIn, [], ListIn) :-
  !.

delete_all_occurrences_of_all_elements(ListIn, [ElementToDelete|ElementsToDelete], ListOut) :-
  delete_all(ListIn, ElementToDelete, ListIM),
  !,
  delete_all_occurrences_of_all_elements(ListIM, ElementsToDelete, ListOut).
  

delete_all([X|Xs],Z,Ys) :-
  \+ \+ (X = Z), 
  delete_all(Xs,Z,Ys).

delete_all([X|Xs],Z,[X|Ys]) :- 
  \+ \+ (X \= Z), 
  delete_all(Xs,Z,Ys).

delete_all([],_X,[]).
  
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  delete_all_occurrences_of_all_discourse_referents(+ReferentsIn, +ReferentsToDelete, -ReferentsOut)
%
%    - ReferentsOut is the list ReferentsIn without all occurrences of the elements of the list ReferentsToDelete
%
%
%  delete_all_occurrences_of_one_discourse_referent(+ReferentsIn, +ReferentToDelete, -ReferentsOut)
%
%    - ReferentsOut is the list ReferentsIn without all occurrences of ReferentToDelete
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

delete_all_occurrences_of_all_discourse_referents(Referents, [], Referents) :-
  !.

delete_all_occurrences_of_all_discourse_referents(ReferentsIn, [ReferentToDelete|ReferentsToDelete], ReferentsOut) :-
  delete_all_occurrences_of_one_discourse_referent(ReferentsIn, ReferentToDelete, ReferentsIM),
  delete_all_occurrences_of_all_discourse_referents(ReferentsIM, ReferentsToDelete, ReferentsOut).


delete_all_occurrences_of_one_discourse_referent([], _ReferentToDelete, []).

delete_all_occurrences_of_one_discourse_referent([Referent|Referents], ReferentToDelete, ReferentsOut) :-
  (
    Referent == ReferentToDelete
    -> 
    delete_all_occurrences_of_one_discourse_referent(Referents, ReferentToDelete, ReferentsOut)
  ;
    delete_all_occurrences_of_one_discourse_referent(Referents, ReferentToDelete, ReferentsRest),
    ReferentsOut = [Referent|ReferentsRest]
  ).

%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  eliminate_spurious_antecedents(+AntecedentsIn, +AnaphorID, +AnaphorConditions, -AntecedentsOut)
%
%    - AntecedentsOut is AntecedentsIn without spurious antecedents
%    - if a noun phrase like "the price of a resource" generates the anaphor "the price of a resource" and the antecedent "a resource", and the anaphor 
%      can be resolved then the antecedent is spurious
%    - spurious antecedents have an ID that is larger than AnaphorID and non-empty conditions that are a subset of AnaphorConditions 
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

eliminate_spurious_antecedents(AntecedentsIn, AnaphorID, AnaphorConditions, AntecedentsOut) :-
  (
    member(antecedent(AntecedentID, AntecedentReferent, AntecedentConditions, AntecedentGenus, AntecedentNumerus, AntecedentPerson, AntecedentSID, AntecedentTID, AntecedentTokens), AntecedentsIn),
    AnaphorID < AntecedentID, 
    \+ AntecedentConditions = [],
    \+ \+ (numbervars((AnaphorConditions, AntecedentConditions), 1, _), forall(member(Condition, AntecedentConditions), member(Condition, AnaphorConditions)))
    ->
    delete(AntecedentsIn, antecedent(AntecedentID, AntecedentReferent, AntecedentConditions, AntecedentGenus, AntecedentNumerus, AntecedentPerson, AntecedentSID, AntecedentTID, AntecedentTokens), AntecedentsIntermediate),
    eliminate_spurious_antecedents(AntecedentsIntermediate, AnaphorID, AnaphorConditions, AntecedentsOut)
  ;
    AntecedentsOut = AntecedentsIn
  ).

%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  DRS nesting level: initialise_DRS_nesting_level/0, increase_DRS_nesting_level/0, decrease_DRS_nesting_level/0
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

initialise_DRS_nesting_level :-
  retractall(nesting_level(_)),
  assert(nesting_level(1)).
  

increase_DRS_nesting_level :-
  retract(nesting_level(CurrentNestingLevel)),
  NewNestingLevel is CurrentNestingLevel + 1,
  assert(nesting_level(NewNestingLevel)).
  

decrease_DRS_nesting_level :-
  retract(nesting_level(CurrentNestingLevel)),
  NewNestingLevel is CurrentNestingLevel - 1,
  assert(nesting_level(NewNestingLevel)),
  retractall(variable_defined(_VariableName, CurrentNestingLevel, _VariableType)).


%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  enforce_order(+ConditionsIn, -ConditionsOut, +ConditionsAllIn, -ConditionsAllOut, +AntecedentsIn, -AntecedentsOut)
%
%    - enforce_order/6 
%        removes all antecedent/8 conditions from ConditionsIn giving ConditionsOut, and
%        prepends them in reverse textual order to AntecedentsIn resulting in AntecedentsOut (this guarantees the selection of the closest preceding antecedent)  
%        collects all simple conditions of ConditionsIn and adds them to the front of ConditionsAllIn resulting in ConditionsAllOut
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

enforce_order(ConditionsIn, ConditionsOut, ConditionsAllIn, ConditionsAllOut, AntecedentsIn, AntecedentsOut) :-
  % remove all antecedents from ConditionsIn and ...
  remove_and_collect_antecedents(ConditionsIn, [], AllAntecedents, [], ConditionsRest),
  % ... prepend them in inverse order to AntecedentsIn to get AntecedentsOut
  sort(AllAntecedents, AllAntecedentsSorted),
  reverse(AllAntecedentsSorted, AllAntecedentsSortedReversed),
  (
    AntecedentsIn = []
    ->
    AntecedentsOut = AllAntecedentsSortedReversed
  ;
    % for non-empty AntecedentsIn add divider between DRS nesting levels
    append(AllAntecedentsSortedReversed, [divider_between_DRS_nesting_levels|AntecedentsIn], AntecedentsOut)
  ),
  check_for_redefined_variables(AntecedentsOut),
  % reverse remaining conditions
  reverse(ConditionsRest, ConditionsOut),
  % prepend the remaining DRS conditions in reverse order to ConditionsAllIn to get ConditionsAllOut
  append(ConditionsOut, ConditionsAllIn, ConditionsAllOut).
  

remove_and_collect_antecedents([], Antecedents, Antecedents, ConditionsOut, ConditionsOut).

remove_and_collect_antecedents([ConditionIn|ConditionsIn], AntecedentsSofar, Antecedents, ConditionsOutSofor, ConditionsOut) :-
  (
    ConditionIn = antecedent(_ID, _Referent, _Conditions, _Genus, _Numerus, _Person, _SentenceID, _TokenID, _Tokens)
    -> 
    transform_allquantified_nouns(ConditionIn, TransformedConditionIn),
    remove_and_collect_antecedents(ConditionsIn, [TransformedConditionIn|AntecedentsSofar], Antecedents, ConditionsOutSofor, ConditionsOut)
  ;
    % generalised quantors 'exactly', 'less than' and 'at most' have additional bracketing that must be ...
    is_list(ConditionIn)
    -> 
    % ... processed recursively
    remove_and_collect_antecedents(ConditionIn, [], NewAntecedents, [], NewConditions),
    append(NewAntecedents, AntecedentsSofar, NewAntecedentsSofar),
    remove_and_collect_antecedents(ConditionsIn, NewAntecedentsSofar, Antecedents, [NewConditions|ConditionsOutSofor], ConditionsOut)
  ;
    remove_and_collect_antecedents(ConditionsIn, AntecedentsSofar, Antecedents, [ConditionIn|ConditionsOutSofor], ConditionsOut)
  ).
  

transform_allquantified_nouns(Condition, TransformedCondition) :-
  % phrases like "all men" generate an "antecedent(1, A, [object(A, man, countable, na, eq, 1)-1], $gen(B, $human(C)), $num(D, $pl), 1,'' , man)" that
  % on the one side is plural and on the other side has the count "eq 1"
  % to allow anaphoric reference by "the men" and "they" the count is replaced by "geq 2"
  (
    Condition = antecedent(AntecedentID, AntecedentReferent, AntecedentConditions, AntecedentGenus, '$num'(SP, '$pl'), AntecedentPerson, AntecedentSID, AntecedentTID, AntecedentTokens),
    select(object(Referent, Lemma, countable, na, eq, 1)-AntecedentSID/_, AntecedentConditions, RestAntecedentConditions),
    Referent == AntecedentReferent
    ->
    NewAntecedentConditions = [object(Referent, Lemma, countable, na, geq, 2)-AntecedentSID/_|RestAntecedentConditions],
    TransformedCondition = antecedent(AntecedentID, AntecedentReferent, NewAntecedentConditions, AntecedentGenus, '$num'(SP, '$pl'), AntecedentPerson, AntecedentSID, AntecedentTID, AntecedentTokens)
  ;
    TransformedCondition = Condition
  ).


%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  filter_conditions(+(ConditionsIn, -ConditionsOut)
%
%    - ConditionsOut is ConditionsIn without duplicate conditions
%    - create an error message if there are inconsistent noun phrase conjunctions, i.e. if they contain fewer elements than the plural object states
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

filter_conditions(ConditionsIn, ConditionsOut) :-
  % remove duplicate conditions
  remove_duplicate_conditions(ConditionsIn, ConditionsWithoutDuplicates),
  % check for noun phrase conjunctions that refer anaphorically to themselves
  noun_phrase_conjunction_refers_to_itself(ConditionsWithoutDuplicates),
  % establish original order of conditions
  reverse(ConditionsWithoutDuplicates, ConditionsOut).


remove_duplicate_conditions(Conditions, ConditionsWithoutDuplicates) :-
  remove_duplicate_conditions(Conditions, [], ConditionsWithoutDuplicates).

remove_duplicate_conditions([], ConditionsWithoutDuplicates, ConditionsWithoutDuplicates).

remove_duplicate_conditions([Condition|RestConditions], Singles, ConditionsWithoutDuplicates) :-
  (
    % simple Condition occurs identically – but possibly with different sentence index and different token index – in RestConditions
    Condition = ConditionProper - _/_,
    member(DuplicateSimpleCondition, RestConditions),
    DuplicateSimpleCondition = DuplicateSimpleConditionProper - _/_,
    ConditionProper == DuplicateSimpleConditionProper
    ->
    remove_duplicate_conditions(RestConditions, Singles, ConditionsWithoutDuplicates)
  ;
    % complex Condition occurs identically in RestConditions
    member(DuplicateComplexCondition, RestConditions),
    Condition == DuplicateComplexCondition
    ->
    remove_duplicate_conditions(RestConditions, Singles, ConditionsWithoutDuplicates)
  ;
    % Condition does not occur in RestConditions
    remove_duplicate_conditions(RestConditions, [Condition|Singles], ConditionsWithoutDuplicates)
  ).

   
noun_phrase_conjunction_refers_to_itself(Conditions) :-
  (
    % get head of noun phrase conjunction
    select(object(Whole,na,countable,na,eq,N)-Sentence/'', Conditions, RestConditions)
    ->
    % count the has_part/2 branches of the noun phrase conjunction 
    % use subterm/2 instead of member/2 since RestConditions can contain a list derived from a generalised quantifier
    findall(1, (subterm(has_part(Whole1,_Part)-Sentence/'', RestConditions), Whole1 == Whole), Branches),
    (
      % all N has_part/2 branches exist
      length(Branches, N)
      ->
      true
    ;
      % there are less than N has_part/2 branches meaning that there are anaphoric references within the noun phrase conjunction
      add_error_message_once(anaphor, Sentence - '', 'Noun phrase conjunction refers anaphorically to itself.', 'Remove anaphoric references from noun phrase conjunction.')
    ),
    noun_phrase_conjunction_refers_to_itself(RestConditions)
  ;
   % there are no (further) noun phrase conjunctions
   true
  ).


%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  insert_antecedent(+Antecedent, +AntecedentsIn, -AntecedentsOut)
%
%    - insert_antecedent/3 inserts Antecedent into the list AntecedentsIn generating AntecedentOut while the orderedness of the antecedents is preserved
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

insert_antecedent(Antecedent, AntecedentsIn, AntecedentsOut) :-
  (
    % insert Antecedent before divider between DRS nesting levels
    member(divider_between_DRS_nesting_levels, AntecedentsIn) 
    ->
    append(Front, [divider_between_DRS_nesting_levels|Tail], AntecedentsIn),
    insert_antecedent1(Antecedent, Front, FrontOut),
    append(FrontOut, [divider_between_DRS_nesting_levels|Tail], AntecedentsOut)
  ;
    % no divider
    insert_antecedent1(Antecedent, AntecedentsIn, AntecedentsOut)
  ),
  check_for_redefined_variables(AntecedentsOut).

  
insert_antecedent1(Antecedent, [], [Antecedent]) :-
  !.			

insert_antecedent1(antecedent(ID1, R1, C1, G1, N1, P1, SID1, TID1, T1), [antecedent(ID2, R2, C2, G2, N2, P2, SID2, TID2, T2)|Antecedents], [antecedent(ID2, R2, C2, G2, N2, P2, SID2, TID2, T2)|AntecedentsIM]) :-	
  ID1 < ID2,
  !,
  insert_antecedent1(antecedent(ID1, R1, C1, G1, N1, P1, SID1, TID1, T1), Antecedents, AntecedentsIM).		

insert_antecedent1(antecedent(ID1, R1, C1, G1, N1, P1, SID1, TID1, T1),[antecedent(ID2, R2, C2, G2, N2, P2, SID2, TID2, T2)|Antecedents], [antecedent(ID1, R1, C1, G1, N1, P1, SID1, TID1, T1), antecedent(ID2, R2, C2, G2, N2, P2, SID2, TID2, T2)|Antecedents]) :-	
  ID1 >= ID2.				


%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  match_elements(+Subset, +Set)
%
%    - match_elements(Subset, Set) succeeds if all elements of Subset  - with the exception of antecedents and anaphors - occur in Set that is ground
%    - complex elements - represented by sub-DRSs - must occur identically in both sets
%
%  match_element_against_disjunction(+Element, +Disjunction)
%
%    - match_element_against_disjunction(Element, Disjunction) succeeds if Element matches one of the disjuncts of Disjunction
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

match_elements([], _Set).

match_elements([Element|Elements], Set) :-
  (
    % singular noun anaphor 'the object' refers to ...
    Element = object(R1, Noun, countable, na, eq, 1) - _Index1
    ->
    (
      % ... singular countable noun antecedent 'an/1 object', 'at least/more than/exactly 1 object', or to ...
      member(object(R2, Noun, countable, na, _Op, 1) - _Index2, Set),  
      R1 = R2
    ;
      % ... mass noun antecedent occurring by itself (e.g. some water) or with measurement noun (e.g. 2 kg of water)
      member(object(R2, Noun, mass, _Unit, _Op, _Count) - _Index2, Set), 
      R1 = R2
    )
  ;
	% plural noun anaphor 'the objects' represented as 'at least 2 objects' refers to ...
    Element = object(R1, Noun, countable, na, geq, 2) - _Index1
    ->
    (
      % ... plural countable noun antecedent antecedent 'N2 objects', 'at least/more than/exactly N2 objects', where N2>=2, or to ...
      member(object(R2, Noun, countable, na, _Op, N2) - _Index2, Set), 
      N2 >= 2, 
      R1 = R2
    ;
      % ... antecedent 'measurement noun of objects'
      member(object(R2, Noun, countable, Unit, _Op, _Count) - _Index2, Set),	
      \+ Unit = na, 
      R1 = R2
    )
  ;
    % modifier/4
    Element = modifier_pp(Arg11, Arg12, Arg13) - _Index1
    ->
    member(modifier_pp(Arg21, Arg22, Arg23) - _Index2, Set),
    % must allow for backtracking in member/2 since groups of modifier/4 conditions must be matched in any order
    Arg11 = Arg21,
    Arg12 = Arg22,
    Arg13 = Arg23
  ;
    % has_part/2
    Element = has_part(Whole1, Part1) - _Index1
    ->
    member(has_part(Whole2, Part2) - _Index2, Set),
    % must allow for backtracking in member/2 since groups of has_part/2 conditions must be matched in any order
    Whole1 = Whole2,
    Part1 = Part2
  ;
    % other simple conditions
    Element = Condition1 - _Index1
    -> 
    member(Condition2 - _Index2, Set),
    Condition1 = Condition2
  ;
    % antecedents
    Element = antecedent(_, _, _ ,_, _, _, _, _)
    ->
    true
  ;
    % anaphors
    Element = anaphor(_, _, _ ,_, _, _, _, _, _, _)
    ->
    true
  ;
    % nested list
    is_list(Element)
    ->
    member(List, Set),
    is_list(List),
    match_elements(Element, List),
    match_elements(List, Element)
  ;
    % complex condition: drs
    Element = drs(_Referents1, Conditions1)
    ->
    member(drs(_Referents2, Conditions2), Set),
    match_elements(Conditions1, Conditions2),
    match_elements(Conditions2, Conditions1)
  ;
    % complex condition: negation
    Element = - drs(_Referents1, Conditions1)
    ->
    member(- drs(_Referents2, Conditions2), Set),
    match_elements(Conditions1, Conditions2),
    match_elements(Conditions2, Conditions1)
  ;
    % complex condition: negation as failure
    Element = ~ drs(_Referents1, Conditions1)
    ->
    member(~ drs(_Referents2, Conditions2), Set),
    match_elements(Conditions1, Conditions2),
    match_elements(Conditions2, Conditions1)
  ;
    % complex condition: possibility
    Element = can(drs(_Referents1, Conditions1))
    ->
    member(can(drs(_Referents2, Conditions2)), Set),
    match_elements(Conditions1, Conditions2),
    match_elements(Conditions2, Conditions1)
  ;
    % complex condition: necessity
    Element = must(drs(_Referents1, Conditions1))
    ->
    member(must(drs(_Referents2, Conditions2)), Set),
    match_elements(Conditions1, Conditions2),
    match_elements(Conditions2, Conditions1)
  ;
    % complex condition: recommendation
    Element = should(drs(_Referents1, Conditions1))
    ->
    member(should(drs(_Referents2, Conditions2)), Set),
    match_elements(Conditions1, Conditions2),
    match_elements(Conditions2, Conditions1)
  ;
    % complex condition: admissibility
    Element = may(drs(_Referents1, Conditions1))
    ->
    member(may(drs(_Referents2, Conditions2)), Set),
    match_elements(Conditions1, Conditions2),
    match_elements(Conditions2, Conditions1)
  ;
    Element = question(drs(_Referents1, Conditions1))
    ->
    member(question(drs(_Referents2, Conditions2)), Set),
    match_elements(Conditions1, Conditions2),
    match_elements(Conditions2, Conditions1)
  ;
    Element = command(drs(_Referents1, Conditions1))
    ->
    member(command(drs(_Referents2, Conditions2)), Set),
    match_elements(Conditions1, Conditions2),
    match_elements(Conditions2, Conditions1)
  ;
    % complex condition: sentence subordination
    Element = _ : drs(_Referents1, Conditions1)
    ->
    member( _ :(drs(_Referents2, Conditions2)), Set),
    match_elements(Conditions1, Conditions2),
    match_elements(Conditions2, Conditions1)
  ;
    % complex condition: implication
    Element = drs(_ReferentsP1, ConditionsP1) => drs(_ReferentsC1, ConditionsC1)
    ->
    member(drs(_ReferentsP2, ConditionsP2) => drs(_ReferentsC2, ConditionsC2), Set),
 	match_elements(ConditionsP1, ConditionsP2),
    match_elements(ConditionsP2, ConditionsP1),
 	match_elements(ConditionsC1, ConditionsC2),
    match_elements(ConditionsC2, ConditionsC1)
  ;
    % complex condition: disjunction
    % identity of Element and its counterpart in Set is established here
    Element = drs(_ReferentsD1, ConditionsD1) v DisjunctsRest1
    ->
    member(drs(_ReferentsD2, ConditionsD2) v DisjunctsRest2, Set),
    % first disjunct drs(_ReferentsD1, ConditionsD1) of Element can match any of the disjuncts of drs(_ReferentsD2, ConditionsD2) v DisjunctsRest2
    match_element_against_disjunction(drs(_ReferentsD1, ConditionsD1), drs(_ReferentsD2, ConditionsD2) v DisjunctsRest2),
    % match remaining disjuncts DisjunctsRest1 of Element against drs(_ReferentsD2, ConditionsD2) v DisjunctsRest2
    (
      % DisjunctsRest1 consists of exctly one disjunct
      \+ DisjunctsRest1 = drs([], [_ v _])
      ->
      match_element_against_disjunction(DisjunctsRest1, drs(_ReferentsD2, ConditionsD2) v DisjunctsRest2)
    ;
      % DisjunctsRest1 consists of more than one disjunct
      DisjunctsRest1 = drs([], [DisjunctsRest11 v DisjunctsRest12])
      ->
      match_elements([DisjunctsRest11 v DisjunctsRest12], [drs(_ReferentsD2, ConditionsD2) v DisjunctsRest2])
    )
  ),
  match_elements(Elements, Set).


match_element_against_disjunction(drs(_ReferentsD1, ConditionsD1), drs(_ReferentsD2, ConditionsD2) v DisjunctsRest2) :-
  % Element drs(_ReferentsD1, ConditionsD1) matches one of the disjuncts of the Disjunction drs(_ReferentsD2, ConditionsD2) v DisjunctsRest2
  (
    % Element matches first disjunct of Disjunction
    match_elements(ConditionsD1, ConditionsD2)
    ->
    true
  ;
    % Element does not match first disjunct of Disjunction
    % there is exactly one further disjunct; try it
    \+ DisjunctsRest2 = drs([], [_ v _])
    ->
    match_elements([drs(_ReferentsD1, ConditionsD1)], [DisjunctsRest2]) 
  ;
    % Element does not match first disjunct of Disjunction
    % there are at least two further disjuncts; try them in order
    DisjunctsRest2 = drs([], [DisjunctsRest21 v DisjunctsRest22])
    ->
    match_element_against_disjunction(drs(_ReferentsD1, ConditionsD1), DisjunctsRest21 v DisjunctsRest22) 
  ).


%---------------------------------------------------------------------------------------------------------
%
%  subterm(+Sub,+Term) 
%
%  	Sub is a subterm of Term
%
%---------------------------------------------------------------------------------------------------------

subterm(Term, Term) :-
  nonvar(Term).

subterm(Sub,Term):-
  nonvar(Term),
  functor(Term,_F,N),
  N > 0,
  subterm(N,Sub,Term).


subterm(N,Sub,Term):-
  arg(N,Term,Arg),       
  subterm(Sub,Arg).

subterm(N,Sub,Term):-
  N>1,
  N1 is N-1,
  subterm(N1,Sub,Term).
   
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%
%  subtract_conditions(+Conditions1, +Conditions2, -Difference)
%
%    - Difference contains all the conditions of Conditions1 that are not contained in Conditions2 - with the exception of formulas that are not affected
%    - indices of conditions are ignored
%    - no unification of conditions takes place
%
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------

subtract_conditions(Difference, [], Difference).

subtract_conditions(Conditions1, [Condition2|Conditions2], Difference) :-
  (
    Condition2 = Condition2C - _,
    Condition2C \= formula(_, _, _),
    select(Condition - _, Conditions1, RestConditions1),
    Condition == Condition2C
    ->
    subtract_conditions(RestConditions1, Conditions2, Difference)
  ;
    subtract_conditions(Conditions1, Conditions2, Difference)
  ).
  
  
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
