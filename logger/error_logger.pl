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


:- module(error_logger,
	  [
	   clear_messages/0,
	   clear_messages/1,                     % +Type
	   clear_error_messages/0,
	   clear_error_messages_sentence/1,      % +Sentence
	   clear_warning_messages/0,
	   get_messages/1,                       % -Messages
	   get_messages_in_xml/1,                % -XmlMessages
	   get_error_messages/1,                 % -ErrorMessages
	   get_warning_messages/1,               % -WarningMessages
	   get_messages_with_type/2,             % +Type, -AllMessages
	   get_error_messages_with_type/2,       % +Type, -ErrorMessages
	   get_warning_messages_with_type/2,     % +Type, -WarningMessages
	   add_error_messagelist/4,              % +Type, +Position, +Subject, +DescriptionList
	   add_error_message/4,                  % +Type, +Position, +Subject, +Description
	   add_error_message_once/4,             % +Type, +Position, +Subject, +Description
	   add_warning_message/4,                % +Type, +Position, +Subject, +Description
	   add_warning_message_once/4,           % +Type, +Position, +Subject, +Description
	   add_messages/1,                       % +MessageList
	   is_error_message/4,                   % +Type, +Position, +Subject, +Description
	   messages_xmlmessages/2                % +Messages, -XmlMessages
	  ]).


/** <module> APE Error Logger (using global variables)

@author Tobias Kuhn
@author Kaarel Kaljurand
@version 2008-03-27
*/


%% clear_messages is det.
%
% @bug Why do we need the cut + fallback in all the clearing predicates?
%
clear_messages :-
	setval(message, []).


%% clear_messages(+Type:atom) is det.
%
% Clears all error and warnings messages of certain type.
%
% @param Type is in {syntax, anaphor, ...}
%
clear_messages(Type) :-
	getval(message, Messages),
	myexclude(message(_, Type, _, _, _), Messages, RemainingMessages),
	setval(message, RemainingMessages),
	!.

clear_messages(_).


%% clear_error_messages is det.
%
% Clears all error messages.
%
clear_error_messages :-
	getval(message, Messages),
	myexclude(message(error, _, _, _, _), Messages, RemainingMessages),
	setval(message, RemainingMessages),
	!.

clear_error_messages.


%% clear_warning_messages is det.
%
% Clears all warning messages.
%
clear_warning_messages :-
	getval(message, Messages),
	myexclude(message(warning, _, _, _, _), Messages, RemainingMessages),
	setval(message, RemainingMessages),
	!.

clear_warning_messages.


%% clear_error_messages_sentence is det.
%
% @param Sentence is a sentence number
%
% Clears all error messages of the given sentence.
%
clear_error_messages_sentence(Sentence) :-
	getval(message, Messages),
	myexclude(message(error, _, Sentence-_, _, _), Messages, RemainingMessages),
	setval(message, RemainingMessages),
	!.

clear_error_messages_sentence(_).

%% get_messages(-Messages:list) is det.
%
% @param Messages is a list of terms =|message(Importance, Type, Position, Subject, Description)|=
%
get_messages(Messages) :-
	getval(message, MessagesR),
	reverse(MessagesR, Messages).


%% get_messages_in_xml(-XmlMessages:term) is det.
%
% @param XmlMessages is list of XML elements in SWI-Prolog notation

get_messages_in_xml(XmlMessages) :-
	get_messages(Messages),
	messages_xmlmessages(Messages, XmlMessages).


%% messages_xmlmessages(+Messages:list, -XmlMessages:term) is det.
%
% @param Messages is list of messages
% @param XmlMessages is list of XML elements in SWI-Prolog notation
%
% A simple conversion of the message-terms into SWI-Prolog XML notation

messages_xmlmessages([], []).

messages_xmlmessages(
	[message(Importance, Type, SentenceId-TokenId, Subject, Description) | Messages],
	[element(message, [importance=Importance, type=Type, sentence=SentenceId, token=TokenId, value=Subject, repair=Description], [])
		| XmlMessages]
	) :-
	messages_xmlmessages(Messages, XmlMessages).



%% get_error_messages(-ErrorMessages:list) is det.
%
% @param ErrorMessages is a list of terms =|message(Importance, Type, Position, Subject, Description)|=
%
get_error_messages(ErrorMessages) :-
	get_messages(Messages),
	findall(message(error, T, P, S, D), member(message(error, T, P, S, D), Messages), ErrorMessages).


%% get_warning_messages(-WarningMessages:list) is det.
%
% @param WarningMessages is a list of terms =|message(Importance, Type, Position, Subject, Description)|=
%
get_warning_messages(WarningMessages) :-
	get_messages(Messages),
	findall(message(warning, T, P, S, D), member(message(warning, T, P, S, D), Messages), WarningMessages).


%% get_messages_with_type(+Type:atom, -AllMessages:list) is det.
%
% @param Type is a message type, one of {sentence, word, owl, ...}
% @param AllMessages is a list of terms =|message(Importance, Type, Position, Subject, Description)|=
%
get_messages_with_type(Type, AllMessages) :-
	get_messages(Messages),
	findall(message(Importance, Type, P, S, D), member(message(Importance, Type, P, S, D), Messages), AllMessages).


%% get_error_messages_with_type(+Type:atom, -ErrorMessages:list) is det.
%
% @param Type is a message type, one of {sentence, word, owl, ...}
% @param ErrorMessages is a list of terms =|message(Importance, Type, Position, Subject, Description)|=
%
get_error_messages_with_type(Type, ErrorMessages) :-
	get_messages(Messages),
	findall(message(error, Type, P, S, D), member(message(error, Type, P, S, D), Messages), ErrorMessages).


%% get_warning_messages_with_type(+Type:atom, -WarningMessages:list) is det.
%
% @param Type is a message type, one of {sentence, word, owl, ...}
% @param WarningMessages is a list of terms =|message(Importance, Type, Position, Subject, Description)|=
%
get_warning_messages_with_type(Type, WarningMessages) :-
	get_messages(Messages),
	findall(message(warning, Type, P, S, D), member(message(warning, Type, P, S, D), Messages), WarningMessages).


%% add_error_messagelist(+Type:atom, +Position:term, +Subject:atom, +DescriptionList:list) is det.
%
% @param Type is in {syntax, anaphor, ...}
% @param Position is in the form =|SentenceId|= or =|SentenceId-TokenId|=
% @param Subject is usually the lexem in the position of the error
% @param DescriptionList is a list of messages that help to repair the error
%
add_error_messagelist(_Type, _Position, _Subject, []).

add_error_messagelist(Type, Position, Subject, [H | T]) :-
	add_error_message(Type, Position, Subject, H),
	add_error_messagelist(Type, Position, Subject, T).


%% add_error_message(+Type:atom, +Position:term, +Subject:atom, +Description:atom) is det.
%
% @param Type is in {syntax, anaphor, ...}
% @param Position is in the form =|SentenceId|= or =|SentenceId-TokenId|=
% @param Subject is usually the lexem in the position of the error
% @param Description is a message that helps to repair the error

add_error_message(Type, Position, Subject, Description) :-
	assert_message(error, Type, Position, Subject, Description).


%% add_error_message_once(+Type, +Position, +Subject, +Description) is det.
%

add_error_message_once(Type, Position, Subject, Description) :-
	assert_message_once(error, Type, Position, Subject, Description).


%% add_warning_message(+Type, +Position, +Subject, +Description) is det.
%

add_warning_message(Type, Position, Subject, Description) :-
	assert_message(warning, Type, Position, Subject, Description).


%% add_warning_message_once(+Type, +Position, +Subject, +Description) is det.
%

add_warning_message_once(Type, Position, Subject, Description) :-
	assert_message_once(warning, Type, Position, Subject, Description).


%% add_messages(+MessageList).
%

add_messages(MessageList) :-
	getval(message, OldMessages),
	reverse(MessageList, MessageListR),
	append(MessageListR, OldMessages, NewMessages),
	setval(message, NewMessages).


%% assert_message(+Importance:atom, +Type:atom, +Position:term, +Subject:atom, +Description:atom) is det.
%
% @param Importance is in {error, warning}
% @param Type is in {syntax, anaphor, ...}
% @param Position is in the form =|SentenceId|= or =|SentenceId-TokenId|=
% @param Subject is usually the lexem in the position of the error
% @param Description is a message that helps to repair the error

assert_message(Importance, Type, SentenceID-TokenID, Subject, Description) :-
    ground(message(Importance, Type, SentenceID-TokenID, Subject, Description)),
	!,
	% uncomment the next line for immediate print-out of the messages
	%write(message(Importance, Type, SentenceID-TokenID, Subject, Description)), nl,
	getval(message, Messages),
	setval(message, [message(Importance, Type, SentenceID-TokenID, Subject, Description) | Messages]).

assert_message(Importance, Type, SentenceID-TokenID, Subject, Description) :-
    format(user_error, 'Malformed message: ~q\n', [message(Importance, Type, SentenceID-TokenID, Subject, Description)]),
    !.

assert_message(Importance, Type, SentenceID, Subject, Description) :-
	assert_message(Importance, Type, SentenceID-'', Subject, Description).


%% assert_message_once(+Importance, +Type, +Position, +Subject, +Description) is det.
%

assert_message_once(Importance, Type, SentenceID-TokenID, Subject, Description) :-
	getval(message, Messages),
	member(message(Importance, Type, SentenceID-TokenID, Subject, Description), Messages),
	% message already exists
	!.

assert_message_once(Importance, Type, SentenceID-TokenID, Subject, Description) :-
	!,
	assert_message(Importance, Type, SentenceID-TokenID, Subject, Description).

assert_message_once(Importance, Type, SentenceID, Subject, Description) :-
	assert_message_once(Importance, Type, SentenceID-'', Subject, Description).


%% is_error_message(+Type:atom, +Position:term, +Subject:atom, +Description:atom) is det.
%
% Succeeds if there is at least 1 error message (which the given characteristics)

is_error_message(Type, Position, Subject, Description) :-
	getval(message, Messages),
	member(message(error, Type, Position, Subject, Description), Messages),
	!.


%% myexclude(+Element:term, +List1:list, -List2:list) is det.
%
%
myexclude(_Element, [], []).

myexclude(Element, [OtherElement | List1], [OtherElement | List2]) :-
	Element \= OtherElement,
	!,
	myexclude(Element, List1, List2).

myexclude(Element, [_ | List1], List2) :-
	myexclude(Element, List1, List2).


%% setval(+Name, +Value)
% 

setval(Name, Value) :-
    nb_setval(Name, Value).


%% getval(+Name, ?Value)
% 

getval(Name, Value) :-
    nb_getval(Name, Value).


:- clear_messages.
