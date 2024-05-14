%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_rule.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_clause.

:- interface.

:- import_module mh_term.
:- import_module mh_relation.
:- import_module mh_mode.

:- type proc_clause 
	---> 	horn_clause(proc_relation, expression_term)
	;		fact_clause(proc_relation)
	;		primitive_clause(
				operation_relation, 
				operation_signature, 
				primitive_operation
			).
			
:- type horn_clause =< proc_clause
	---> 	horn_clause(proc_relation, expression_term).
	
:- type fact_clause =< proc_clause
	--->	fact_clause(proc_relation).
	
:- type primitive_clause =< proc_clause
	--->	primitive_clause(
				operation_relation, 
				operation_signature, 
				primitive_operation
			).
			
:- type primitive_operation == (func(relation) = mh_term). 