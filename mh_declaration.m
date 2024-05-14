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

:- import_module list.
:- import_module set.


:- type declaration
	--->	type_def(symbol, mh_type) 
	;		type_def(symbol, list(mh_type_term), mh_type_term) %Parametric
	; 		data_type(symbol)
	;		primitive_type(symbol)
	;		state_relation(symbol, state_type)
	;		clause(symbol, proc_clause).
