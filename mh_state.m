%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_state.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_state.

:- interface.

:- import_module mh_symbol.
:- import_module mh_type.
:- import_module mh_relation.
:- import_module mh_arity.

:- typeclass state(T) where [
	pred query_state(T, relation, ground_relation),
	mode query_state(in, in, out) is nondet
].


:- type state_type(T) 	% 	<= state(T)
	--->	state_type(
				type_name::symbol, 
				relation_type::relation_type,
				init_state::((func) = T <= state(T))
			).


