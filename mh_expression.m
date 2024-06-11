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

% :- import_module list.
:- import_module set.
% :- import_module univ.

:- import_module mh_symbol.
:- import_module mh_identifier.
:- import_module mh_type.
:- import_module mh_relation.
:- import_module mh_predicate.
:- import_module mh_function.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type mh_expression

	%	void
	--->	nil
	
	%	errors
	;		expression_error(mh_expression, string)
	;		expression_warning(mh_expression, string)
	
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
	;		some [T] mr_predicate(T) => predicate(T)
	;		some [T] mr_function(T) => function(T)
	;		some [T] lambda_pred(T, mh_clause) => relation(T)
	;		some [T] lambda_func(T, mh_term, mh_clause) => relation(T)
	
	%	-	axioms 
	
	% 	predicate
	;		predicate(mh_term)
	
	% 	logical
	;		conjunction(set(mh_axiom))
	;		disjunction(set(mh_axiom))
	;		negation(mh_axiom)
	
	% 	clause
	;		implication(mh_fact, mh_axiom)
	
	% 	comparison
	;		equal(mh_term, mh_term)
	;		greater_than(mh_term, mh_term)
	;		less_than(mh_term, mh_term).
	
	
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
	;		some [T] mr_predicate(T) => predicate(T)
	;		some [T] mr_function(T) => function(T)
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

:- type mh_atomic_term =< mh_applicable
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
	;		some [T] functor(mh_applicable, T) => relation(T)
	;		some [T] mr_predicate(T) => predicate(T)
	;		some [T] mr_function(T) => function(T)
	;		some [T] lambda_pred(T, mh_clause) => relation(T)
	;		some [T] lambda_func(T, mh_term, mh_clause) => relation(T).
	
:- type mh_relation =< mh_term
	--->	some [T] functor(mh_applicable, T) => relation(T)
	;		some [T] mr_relation(T) => relation(T).
	
% :- instance relation(mh_compound_term).

:- type mh_functor =< mh_relation
	--->	some [T] functor(mh_applicable, T) => relation(T).
	
:- type mh_atomic_functior =< mh_functor
	---> 	some [T] functor(mh_atom, T) => relation(T).
	
:- type mh_closure =< mh_relation
	--->	some [T] functor(mh_higher_order_term, T) => relation(T).

	
:- type mr_relation =< mh_relation
	---> 	some [T] mr_relation(T) => relation(T).
	
% :- instance relation(mr_relation).

%-----------------------------------------------------------------------------%
% higher order terms

%TODO:  Include constructors for direct mercury higher order terms
%TODO:	Include constructors for monomorphic functions (maybe)
%TODO:  redefine lambadas in terms of mh_facts and rules

:- type mh_clause ---> unimplemented.

:- type mh_higher_order_term =< mh_term
	--->	some [T] mr_predicate(T) => predicate(T)
	;		some [T] mr_function(T) => function(T)
	;		some [T] lambda_pred(T, mh_clause) => relation(T)
	;		some [T] lambda_func(T, mh_term, mh_clause) => relation(T).

:- type mh_lambda_pred =< mh_higher_order_term
	---> 	some [T] lambda_pred(T, mh_clause) => relation(T).
	
:- type mh_lambda_func =< mh_higher_order_term
	--->	some [T] lambda_func(T, mh_term, mh_clause) => relation(T).

%-----------------------------------------------------------------------------%
% predicate terms

:- type mh_predicate_term
	--->	atom(symbol)
	;		some [T] mr_value(T)
	;		some [T] functor(mh_applicable, T) => relation(T)
	;		some [T] mr_relation(T) => relation(T)

%-----------------------------------------------------------------------------%
% axioms

:- type mh_axiom =< mh_expression

	% atomic
	
	
	% 	logical
	;		conjunction(set(mh_axiom))
	;		disjunction(set(mh_axiom))
	;		negation(mh_axiom)
	
	% 	clause
	;		implication(mh_fact, mh_axiom)
	
	% 	comparison
	;		equal(mh_term, mh_term)
	;		greater_than(mh_term, mh_term)
	;		less_than(mh_term, mh_term).

%-----------------------------------------------------------------------------%
% clauses

:- type mh_clause =< mh_axiom

	% facts
	--->	atom(symbol)
	;		some [T] mr_value(T)
	;		some [T] functor(mh_atom, T) => relation(T)
	;		some [T] mr_relation(T) => relation(T)
	;		equal(mh_relation, mh_term)  %function head clause

	% rules
	;		implication(mh_fact, mh_axiom).		

%-----------------------------------------------------------------------------%
% facts


:- type mh_fact =< mh_clause
	--->	atom(symbol)
	;		some [T] mr_value(T)
	;		some [T] functor(mh_atom, T) => relation(T)
	;		some [T] mr_relation(T) => relation(T)
	;		equal(mh_relation, mh_term).  %function head clause
	
:- type mh_atomic_fact =< mh_fact
	--->	atom(symbol)
	;		some [T] mr_value(T).
	
:- type mh_atom_fact =< mh_atomic_fact
	---> 	atom(symbol).
	
:- type mh_value_fact =< mh_atomic_fact
	---> 	some [T] mr_value(T).
	
:- type mh_relation_fact =< mh_fact
	--->	some [T] functor(mh_atom, T) => relation(T)
	;		some [T] mr_relation(T) => relation(T).
	
:- type mh_atomic_functor_fact =< mh_fact
	---> some[T] functor(mh_atom, T) => relation(T).
	
:- type mh_function_fact =< mh_fact
	--->	equal(mh_relation, mh_term).
	
%-----------------------------------------------------------------------------%
% rules

:- type mh_rule =< mh_clause
	--->	implication(mh_fact, mh_axiom).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%
