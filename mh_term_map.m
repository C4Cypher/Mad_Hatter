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

:- import_module array.
:- import_module hash_table.
:- import_module map.
:- import_module type_desc.
:- import_module univ.

:- import_module mh_tuple.
:- import_module mh_symbol.
:- import_module mh_value_map.
:- import_module hashmap.

%-----------------------------------------------------------------------------%
% Term maps

:- type mh_term_map(T)

	% un-treed ancestors
	--->	empty_map
	;		singleton_map(mh_term, T)
	
	% simple map
	;		nil_map(T)
	;		atom_map(symbol_map(T))
	;		var_map(var_id_offset, array(T)) % id set derived from array size
	;		var_map(var_id_offset, array(T), mh_var_map(T))
	;		value_map(mh_value_map(T))
	;		simple_union( 
				nil_map 	:: mh_nil_map(T),
				atom_map 	:: symbol_map(T),
				var_map		:: mh_var_map(T),
				value_map	:: mh_value_map(T)
			).
	
%-----------------------------------------------------------------------------%
% Simple maps

:- inst simple_map
	--->	empty_map
	;		nil_map(ground)
	;		atom_map(ground)
	;		var_map(ground, ground)
	;		var_map(ground, ground, ground)
	;		value_map(ground).
	
:- type simple_map(T) =< mh_term_map(T)
	--->	empty_map
	;		nil_map(T)
	;		atom_map(symbol_map(T))
	;		var_map(var_id_offset, array(T)) % id set derived from array size
	;		var_map(var_id_offset, array(T), mh_var_map(T))
	;		value_map(mh_value_map(T)).
	

%-----------------------------------------------------------------------------%
% Nil map

:- inst nil_map ---> nil_map(ground).

:- type mh_nil_map(T) =< mh_term_map(T) ---> nil_map(T).

%-----------------------------------------------------------------------------%
% Atom maps

:- type symbol_map(T) == hashmap(mh_symbol, T).

:- inst atom_map
	---> 	empty_map
	;		atom_map(ground).
	
:- type mh_atom_map(T) =< mh_term_map(T)
	--->	empty_map
	;		atom_map(symbol_map(T)).

%-----------------------------------------------------------------------------%
% Var maps

:- inst var_map
	--->	empty_map
	;		var_map(ground, ground)
	;		var_map(ground, ground, ground).

:- type mh_var_map(T) =< mh_term_map(T)
	--->	empty_map
	;		var_map(var_id_offset, array(T)) % id set derived from array size
	;		var_map(var_id_offset, array(T), mh_var_map(T)).

%-----------------------------------------------------------------------------%
% Value maps

:- inst value_map 
	--->	empty_map
	;		value_map(ground).
	
:- type value_map(T) =< mh_term_map(T)
	---> empty_map
	;		value_map(mh_value_map(T)).
	
