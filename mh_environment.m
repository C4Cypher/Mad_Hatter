%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
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

:- import_module map.

:- import_module mh_symbol.
:- import_module mh_term.

%-----------------------------------------------------------------------------%
% Environment

:- type mh_environment.

:- func new_env(map(mh_symbol, mh_term)) = mh_environment.

%-----------------------------------------------------------------------------%
% Environment lookup

:- pred lookup(mh_environment::in, mh_symbol::in, mh_term::out) is semidet.
:- func lookup(mh_environment, mh_symbol) = mh_term is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- type mh_environment
	--->	map_env(map(mh_symbol, mh_term)).
	
new_env(Map) = map_env(Map).
	
	
%-----------------------------------------------------------------------------%
% Environment lookup

lookup(Env, Symb, lookup(Env, Symb)).

lookup(map_env(Map), Symb) = map.search(Map, Symb). 