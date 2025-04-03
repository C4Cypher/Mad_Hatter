%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_event.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_event.

:- interface.

:- import_module mh_term.
:- import_module mh_scope.

%-----------------------------------------------------------------------------%
% Events
:- type message == string.

:- type mh_event
	--->	error(mh_scope, mh_term, message)
	;		warning(mh_scope, mh_term, message)
	;		event(mh_term, mh_scope, mh_term).
	
	
% returns atom(~ "error") for errors, atom(~ "warning") for warnings, or the
% given term for custom events.
:- func event_type(mh_event) = mh_term.

% Return the term that is the focus of the given message. (use nil if none)
:- func event_term(mh_event) = mh_term.

:- func event_scope(mh_event) = mh_scope.

% return the raw, unformatted message string
:- func event_message_string(mh_event) = string.

% format the message with the event type and term, using the given scope
% 	The message is prepended with "$event_type: "
%	"$event_type" is substituted for the type term stringified
%	"$term" is substituted for the given focused term
%	"$X" variable names prepended by the $ sign are substituted with the bound
%		term, or the name if completely unbound, using the given scope.
% I'll need to get pretty printing working before I can fully implement this
:- func event_message(mh_event) = string.

%-----------------------------------------------------------------------------%
% Errors

:- inst mh_error ---> error(ground, ground, ground).

:- type mh_error =< mh_event
	--->	error(mh_scope, mh_term, message).
	
:- mode is_error == ground >> mh_error.

:- pred is_error(mh_event::is_error) is semidet.

%-----------------------------------------------------------------------------%
% Warnings

:- inst mh_warning ---> warning(ground, ground, ground).

:- type mh_warning =< mh_event
	--->	warning(mh_scope, mh_term, message).
	
:- mode is_warning == ground >> mh_warning.

:- pred is_warning(mh_event::is_warning) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

