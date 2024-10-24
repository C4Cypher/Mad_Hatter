%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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

:- type key_term_func(T) == func(T) = mh_term.




%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:
:- import_module map.
:- import_module type_desc.
:- import_module univ.

:- import_module mh_tuple.
:- import_module mh_symbol.
:- import_module mh_value_map.

%-----------------------------------------------------------------------------%
% Term maps

:- type mh_term_map(T)

	% un-treed ancestors
	--->	empty_map
	;		singleton_map(mh_term, T)
	
	% simple map
	;		nil_map(T)
	;		atom_map(symbol_map)
	;		var_map(var_id_offset, array(T)) % id set derived from array size
	;		var_map(var_id_offset, array(T), mh_var_map)
	;		mr_value_map(mh_value_map)
	;		simple_map.
	
%-----------------------------------------------------------------------------%
% Simple maps


%-----------------------------------------------------------------------------%
% Nil map

:- inst nil_map ---> nil_map(ground).

:- type nil_map(T) =< mh_term_map(T) ---> nil_map(T).

%-----------------------------------------------------------------------------%
% Atom maps

:- type symbol_map(T) == map(mh_symbol, T)

%-----------------------------------------------------------------------------%
% Var maps

%-----------------------------------------------------------------------------%
% Value maps
