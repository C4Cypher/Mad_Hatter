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

:- include_module mh_environmeent.map.

%-----------------------------------------------------------------------------%
% Environment

:- type mh_environment.

:- func empty_environment = mh_environment.

:- pred empty_environment(mh_environment::out) is det.

:- pred is_empty(mh_environment::in) is det.

%-----------------------------------------------------------------------------%
% Logic Program

% Needs event logging output, need to define events
:- pred assert(mh_term::in, mh_environment::in, mh_environment::out) is det.

:- 

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mh_environment.map.

%-----------------------------------------------------------------------------%

:- type mh_environment
	--->	map_env(mh_term_map(mh_term)).
