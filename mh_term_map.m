%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_term_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_term_map.

:- interface.


:- import_module mh_term.
:- import_module mh_var_id.

%-----------------------------------------------------------------------------%
% Term maps

:- type mh_term_map(T).

:- type key_term_func(T) == (func(T) = mh_term).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.
:- import_module array.
:- import_module hash_table.
:- import_module map.
:- import_module type_desc.
:- import_module univ.

:- import_module mh_tuple.
:- import_module mh_symbol.
:- import_module mh_value_map.
:- import_module mh_var_map.
:- import_module hashmap.

%-----------------------------------------------------------------------------%
% Term maps

:- type mh_term_map(T)
	--->	term_map(
			nil :: maybe(T),
			atom :: symbol_map(T),
			var :: mh_var_map(T),
			mr_value :: mr_value_map(T),
			cons :: mh_term_map(mh_term_map(T)), % TODO: functor map?
			tuple :: % TODO tuple map
			lazy :: mh_term_map(T),
			predicate ::
			relation ::
			function ::
	)
	

%-----------------------------------------------------------------------------%
% Nil map

%-----------------------------------------------------------------------------%
% Atom maps

:- type symbol_map(T) == hashmap(mh_symbol, T).

%-----------------------------------------------------------------------------%
% Var maps

%-----------------------------------------------------------------------------%
% Value maps

	
