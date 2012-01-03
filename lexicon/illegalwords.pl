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


:- module(illegalwords, [
		is_illegalword/2           % +IllegalWord:atom, -ErrorMessage
	]).


%% is_illegalword(+IllegalWord:atom, -ErrorMessage) is semidet.
%
% @param IllegalWord is a word that is not allowed in ACE
% @param ErrorMessage is an error message explaning how to act
%
% This is a simple list of (function) words that are not allowed in ACE.
% BUG: The question remains if one could be allowed to define such words as
% content words and use them anyway. In this case, the error message would be misleading.
% Should we in ACE define a set of words that are not ACE function words and that can not
% be ACE content words either.
%
is_illegalword(any, 'The word \'any\' is not allowed. Did you mean \'every\', \'some\', or \'a\'?').
is_illegalword('Any', 'The word \'Any\' is not allowed. Did you mean \'Every\', \'Some\',  or \'A\'?').
is_illegalword(anybody, 'The word \'anybody\' is not allowed. Did you mean \'everybody\' or \'somebody\'?').
is_illegalword('Anybody', 'The word \'Anybody\' is not allowed. Did you mean \'Everybody\' or \'Somebody\'?').
is_illegalword(anything, 'The word \'anything\' is not allowed. Did you mean \'everything\' or \'something\'?').
is_illegalword('Anything', 'The word \'Anything\' is not allowed. Did you mean \'Everything\' or \'Something\'?').

is_illegalword(this, 'The word \'this\' is not allowed. Did you mean \'the\'?').
is_illegalword('This', 'The word \'This\' is not allowed. Did you mean \'The\'?').
is_illegalword(these, 'The word \'these\' is not allowed. Did you mean \'the\'?').
is_illegalword('These', 'The word \'These\' is not allowed. Did you mean \'The\'?').

is_illegalword(Pronoun, ErrorText) :-
	is_illegal_pronoun(Pronoun),
	with_output_to(atom(ErrorText), format("The pronoun \'~w\' is not allowed. Use only third person singular or plural.", [Pronoun])).


%% is_illegal_pronoun(+Pronoun:atom) is semidet.
%
% @param Pronoun is an English pronoun
%
% Succeeds if Pronoun is not allowed in ACE.
%
is_illegal_pronoun('I').
is_illegal_pronoun(me).
is_illegal_pronoun('Me').
is_illegal_pronoun(my).
is_illegal_pronoun('My').
is_illegal_pronoun(mine).
is_illegal_pronoun('Mine').
is_illegal_pronoun(yours).
is_illegal_pronoun('Yours').
is_illegal_pronoun(we).
is_illegal_pronoun('We').
is_illegal_pronoun(us).
is_illegal_pronoun('Us').
is_illegal_pronoun(our).
is_illegal_pronoun('Our').
is_illegal_pronoun(ours).
is_illegal_pronoun('Ours').
is_illegal_pronoun('Theirs').
