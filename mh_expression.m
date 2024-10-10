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

:- type mh_exp(T) == mh_expression(T).

:- type mh_expression(T)
	--->	invalid_expression(T, string)
	
	%% Term expressions
	
	% Simple terms
	;		nil_exp						% nil
	;		anonymous_exp				% _ | _Name
	;		singleton_exp(T)			% A
	;		atom_exp(symbol)			% name
	;		var_exp(var_id)				% Name
	;		mr_primitive(univ)
	
	% Compound terms
	;		cons_expression(functor_expression(T), T)
	
	% Tuples
	;		tuple_exp(list(T))			% (A, B, C)
	;		list_tuple_exp(list(T)) 	% [A, B, C]
	;		array_tuple_exp(list(T))	% {A, B, C}
	
	% Constraints
	;		unary_constraint(mh_exp(T)) 	% ?int
	
	% Higher order Terms
	;		lambda_exp(lambda_clause)	% \( clause )
	
	%% Operators
	
	% Quantification operator
	;		forall_exp(tuple_expression(T), lambda_exp(T))	% forall X \()
	
	% Constraint operator
	;		binary_constraint(mh_expT), mh_exp(T)) % X:int
	
	% Comparison operators
	;		eq_exp(mh_exp(T), eq_exp(T))	% A = B
	;		gt_exp(mh_exp(T), mh_exp(T)) 	% A > B
	;		ge_exp(mh_exp(T), mh_exp(T))	% A >= B
	;		lt_exp(mh_exp(T), mh_exp(T))	% A < B
	;		le_exp(mh_exp(T), mh_exp(T))	% A =< B
	
	% Logic operators
	;		not_exp(pred_exp(T))					% not P
	;		conjunction_exp(tuple_expression(T))	% (A, B)
	;		disjunction_exp(tuple_expression(T))	% (A ; B)
	;		implication_exp(tuple_expression(T))	% (A => B)
	;		equivalence_exp(tuple_expression(T))	% (A <=> B)
	
	
% Term exp = simple + compound + tuple + urnary_constraint
	
%-----------------------------------------------------------------------------%
% Tuple expressions

:- type tuple_expression(T) =< mh_expression(T)
	--->	singleton_exp(T)			% A
	;		tuple_exp(list(T))			% (A, B, C)
	;		list_tuple_exp(list(T)) 	% [A, B, C]
	;		array_tuple_exp(list(T)).	% {A, B, C}
	
% Lambda exp
	
% Pred exp = compound + comparison + logical

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

