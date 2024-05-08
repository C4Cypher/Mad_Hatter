%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_statement.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_statement.

:- interface.

:- import_module mh_symbol.
:- import_module mh_type.
:- import_module mh_term.
:- import_module mh_expression.

:- import_module list.
:- import_module set.


:- type statement
--->	type_def(symbol, mh_type)
;		type_def(symbol, list
;		data_type(data_signature)
;		state_var(predicate_signature)
;		state_relation(predicate_signature,	   )
 
;		primitive_predicate( predicate_signature, 
			(pred(relation::in, relation::out) is nondet) )
			
;		primitive_predicate( predicate_signature,
			(pred(relation::in, relation::out) is nondet),
			promise_binds ).

;		primitive_function( function_signature,
			(func(ground_relation) = mh_term is semidet) ).
			
/* 	promises that if the given elements of a relation are ground, 
	the resulting elements of the output will be ground */
	
:- type binding_promise ---> binding_promise(set(int), set(int)).
:- type binding_promises == set(binding_promise).

:- func promise_bind(list(int), list(int)) = binding_promise.
:- func pb(list(int), list(int)) = binding_promise. % pb is an abbriviation

:- func promise_binds(list(binding_promise)) = binding_promises.

:- implementation.

promise_bind(From, To) = binding_promise(from_list(From), from_list(To)).
pb(F, T) = promise_bind(F, T).

promise_binds(List) = from_list(List).