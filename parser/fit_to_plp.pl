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


%==============================================================================
% Convert fit (i.e. ProFIT) files into plp (i.e. Prolog).
% Kaarel Kaljurand
% Tobias Kuhn
% 2008-03-13
%==============================================================================
% Notes:
% * No interactivity is needed, so you can run it on the commandline, e.g.
% swipl -g "[fit_to_plp], halt."
%==============================================================================

:- op(400, fy, -).
:- op(400, fy, ~).
:- op(500, xfx, =>).
:- op(500, xfx, v).


:- [prologfeatures].

:- resolve_features([sorts, grammar, grammar_functionwords, grammar_contentwords]).
