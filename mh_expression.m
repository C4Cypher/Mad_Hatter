%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_expression.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_expression.

:- import_module list.
:- import_module univ.

:- import_module symbol.
:- import_module mh_term.
;- import_module mh_clause.

:- interface.
%-----------------------------------------------------------------------------%
% Expressions

:- type mh_expression(T)
	--->	invalid_expression(T, string)
	
	%% Term expressions
	
	% Simple terms
	;		nil_exp						% nil
	;		anonymous_exp				% _ | _Name
	;		atom_exp(symbol)			% name
	;		var_exp(var_id)				% Name
	;		mr_primitive(univ)
	
	% Compound terms
	;		cons_expression(functor_expression(T), T)
	
	% Tuples
	;		singleton_exp(T)			% A
	;		tuple_exp(list(T))			% (A, B, C)
	;		list_tuple_exp(list(T)) 	% [A, B, C]
	;		array_tuple_exp(list(T))	% {A, B, C}
	
	% Constraints
	;		unary_constraint(mh_expression(T)) 	% ?int
	;		binary_constraint(mh_expression(T), mh_expression(T)) % X:int
	
	% Higher order Terms
	;		lambda_exp(lambda_clause)	% \( clause )
	
	%% Logical expressions
	
	% Comparison expressions
	;	eq_exp(T, T)
	
%-----------------------------------------------------------------------------%
% Tuple expressions

:- type tuple_expression(T) =< mh_expression(T)
	--->	singleton_exp(T)			% A
	;		tuple_exp(list(T))			% (A, B, C)
	;		list_tuple_exp(list(T)) 	% [A, B, C]
	;		array_tuple_exp(list(T)).	% {A, B, C}

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

