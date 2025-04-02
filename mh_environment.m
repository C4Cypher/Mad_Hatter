%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_environment.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_environment.

:- interface.

:- import_module mh_term.
:- import_module mh_term_map.

:- include_module mh_environment.map.

%-----------------------------------------------------------------------------%
% Environment

:- type mh_environment.

:- func empty_environment = mh_environment.

:- pred empty_environment(mh_environment::out) is det.

:- pred is_empty(mh_environment::in) is det.

:- pred ask(mh_environment::in, mh_term::in, mh_term::out) is det.

:- func ask(mh_envirronment, mh_term) = mh_term.

% Needs event logging output, need to define events
:- pred assert(mh_environment::in, mh_term::in, mh_environment::out) is det.

:- func assert(mh_environment, mh_term) = mh_environment.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mh_environment.map.

%-----------------------------------------------------------------------------%

:- type mh_environment
	--->	map_env(mh_term_map(mh_term)) % Unification map
	;		term_env(mh_term) % The context and scope of a given term.
	;		module_env(
		interface::mh_environment, %Replace with declarations
		implementation::mh_environment
		).
