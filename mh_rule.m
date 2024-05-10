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

:- module mh_rule.

:- interface.

:- import_module mh_term.
:- import_module mh_type.
:- import_module mh_relation.
:- import_module mh_primitive.

:- type proc_rule 
	---> 	clause_rule(proc_relation, expression)
	;		primitive_rule(
				function_relation, 
				primitive_signature, 
				primitive_func
			).