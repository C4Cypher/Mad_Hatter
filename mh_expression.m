%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_expression.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%


:- module mh_expression. 

:- interface.

:- import_module list.
:- import_module univ.

:- import_module mh_symbol.
:- import_module mh_identifier.
:- import_module mh_type.
:- import_module mh_relation.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type mh_expression

	%	void
	--->	nil
	
	% 	-	terms
	
	%	variables
	;		var(var_id, mh_type)
	;		anonymous_var
	
	% 	atomic terms
	;		atom(symbol)
	;		some [T] mr_value(T)
	
	%	compound terms
	;		some [T] functor(mh_applicable, T) => relation(T)
	;		some [T] mr_relation(T) => relation(T)
	
	%	higher order terms
	;		some [T] lambda_pred(T, mh_clause) => relation(T)
	;		some [T] lambda_func(T, mh_term, mh_clause) => relation(T).
	
	%	-	formulas (axiomatic expressions, truth values)
	
	%TODO: Define constructors for forumulas and clauses	

%-----------------------------------------------------------------------------%
% void expression

:- type mh_nil =< mh_expression ---> nil.

%-----------------------------------------------------------------------------%
% term expressions

:- type var_id == id(mh_id_var).

:- type mh_term =< mh_expression 
	--->	var(var_id, mh_type)
	;		anonymous_var
	;		atom(symbol)
	;		some [T] mr_value(T)
	;		some [T] functor(mh_applicable, T) => relation(T)
	;		some [T] mr_relation(T) => relation(T)
	;		some [T] lambda_pred(T, mh_clause) => relation(T)
	;		some [T] lambda_func(T, mh_term, mh_clause) => relation(T).

%-----------------------------------------------------------------------------%
% variables

:- type mh_var =< mh_term 
	---> 	var(var_id, mh_type)
	;		anonymous_var.
	
:- type mh_id_var =< mh_var
	--->	var(var_id, mh_type).
	
:- type mh_anonymous_var =< mh_var
	---> 	anonymous_var.
	
%-----------------------------------------------------------------------------%
% atomic terms

:- type mh_atomic_term =< mh_term
	--->	atom(symbol)
	;		some [T] mr_value(T).
	
:- type mh_atom =< mh_atomic_term
	---> 	atom(symbol).
	
:- type mr_value =< mh_atomic_term
	--->	some [T] mr_value(T).
	
%-----------------------------------------------------------------------------%
% compound terms

:- type mh_applicable =< mh_term
	--->	var(var_id, mh_type)
	;		atom(symbol)
	;		some [T] mr_value(T)
	;		some [T] functor(mh_applicable, T) => relation(T)
	;		some [T] mr_relation(T) => relation(T)
	;		some [T] lambda_pred(T, mh_clause) => relation(T)
	;		some [T] lambda_func(T, mh_term, mh_clause) => relation(T).
	
:- type mh_compound_term =< mh_term
	--->	some [T] functor(mh_applicable, T) => relation(T)
	;		some [T] mr_relation(T) => relation(T).
	
% :- instance relation(mh_compound_term).

	
:- type mr_relation =< mh_compound_term
	---> 	some [T] mr_relation(T) => relation(T).
	
% :- instance relation(mr_relation).

%-----------------------------------------------------------------------------%
% higher order terms

%TODO:  Include constructors for direct mercury higher order terms
%TODO:	Include constructors for monomorphic functions (maybe)

:- type mh_clause ---> unimplemented.

:- type mh_higher_order_term =< mh_term
	--->	some [T] lambda_pred(T, mh_clause) => relation(T)
	;		some [T] lambda_func(T, mh_term, mh_clause) => relation(T).

:- type mh_lambda_pred =< mh_higher_order_term
	---> 	some [T] lambda_pred(T, mh_clause) => relation(T).
	
:- type mh_lambda_func =< mh_higher_order_term
	--->	some [T] lambda_func(T, mh_term, mh_clause) => relation(T).




%-----------------------------------------------------------------------------%


%-----------------------------------------------------------------------------%


	

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%
