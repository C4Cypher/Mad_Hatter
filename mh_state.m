%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_state.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_state.

:- interface.

:- import_module mh_type.
:- import_module mh_relation.
:- import_module mh_arity.

:- typeclass state(T) <= arity(T) where [
	pred query_state(T, relation, ground_relation),
	mode query_state(in, in, out) is nondet,
	
	pred assert(ground_relation, T, T),
	mode assert(in, in, out) is semidet,
	
	pred retract(relation, T, T),
	mode retract(in, in, out) is semidet
].


:- type state_type
	--->	some [T] state_type( 
				relation_type::relation_type,
				init_state::init_func(T)
			) => state(T).
			
			
:- type init_func(T) == ((func) = T).


