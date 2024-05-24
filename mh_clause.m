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

:- type mh_clause 
	---> 	horn_clause(relation, expression_term)
	;		fact_clause(relation)
	;		primitive_clause(
				relation, 
				operation_signature, 
				primitive_operation
			).
			
:- type horn_clause =< mh_clause
	---> 	horn_clause(relation, expression_term).
	
:- type fact_clause =< mh_clause
	--->	fact_clause(relation).
	
:- type primitive_clause =< mh_clause
	--->	primitive_clause(
				relation, 
				operation_signature, 
				primitive_operation
			).
			
:- type primitive_operation == (func(relation) = mh_term). 