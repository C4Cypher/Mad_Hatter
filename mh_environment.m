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
% Environment modification

:- pred assert

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.



%-----------------------------------------------------------------------------%

:- type mh_environment
	--->	map_env(mh_term_map(mh_term)).
	
new_env(Map) = map_env(Map).
	
	
%-----------------------------------------------------------------------------%
% Environment lookup

search(Env, Term, search(Env, Term)).

search(map_env(Map), Term) = mh_tuple_map.search(Map, Term). 