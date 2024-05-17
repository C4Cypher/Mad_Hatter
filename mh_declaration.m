%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_declaration.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_declaration.

:- interface.

:- import_module mh_symbol.
:- import_module mh_type.
:- import_module mh_term.
:- import_module mh_clause.
:- import_module mh_state.
:- import_module mh_mode.

:- import_module list.
:- import_module set.


:- type declaration
	--->	type_def(
				type_name::symbol,
				type_args::list(mh_type_term),
				type_assignment::mh_type_term,
				where_clause::where_clause
			)
	; 		data_type(symbol)
	;		primitive_type(symbol)
	;		state_relation(symbol, state_type)
	;		clause(symbol, proc_clause).
	
:- type where_clause
	--->	nothing
	;		class_constraints(list(class_constraint)).
	
:- type class_constraint
	---> 	class_constraint(symbol, proc_signature).
