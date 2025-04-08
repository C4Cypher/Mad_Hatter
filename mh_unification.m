%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_unification.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_unification.

:- interface.

:- import_module list.

:- import_module mh_term.
:- import_module mh_environment.
:- import_module mh_substitution.
:- import_module ordered_set.
:- import_module mh_arity.
:- import_module mh_tuple.
:- import_module mh_tuple_map.



%-----------------------------------------------------------------------------%
% Predicates

%TODO: Implement user defined equality and comparison?

:- type mh_unification 
	--->	binary_unification(mh_term, mh_term)		% X = Y
	;		set_unification(ordered_set(mh_term)).	% X = Y = Z


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%
