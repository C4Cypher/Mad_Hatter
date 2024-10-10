%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_clause.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_clause.

:- interface.

:- import_module array.

:- import_module mh_term.
:- import_module mh_scope.
:- import_module mh_symbol.
:- import_module mh_predicate.
:- import_module mh_tuple.
:- import_module mh_var_set.
:- import_module mh_arity.

%-----------------------------------------------------------------------------%
% Clauses

:- type mh_clause

	%% root clauses

	% name = \( clause ).  | name(term) = relation | name(term) :- ...
	--->	atom_clause(symbol, lambda_clause, root_scope)
	
	% forall term \( clause ).
	;		quantified_assertion_clause(mh_term, predicate_clause, root_scope)
	
	%% lambda clauses
	
	% \( predicate ).
	;		fact_clause(mh_predicate)

	
	% \(Head :- Pred). | 
	;		rule_clause(head_clause, mh_predicate)  		
	
	%% head clauses
	
	% forall term | \( term body )    
	;	quantification_clause(lhs_clause)
	
		% term = term | term @ term
	;	unification_clause(mh_term, mh_term)
	
	;
	
:- type root_clause =< mh_clause
	--->	atom_clause(symbol, lambda_clause, root_scope)
	;		quantified_assertion_clause(mh_term, lambda_clause, root_scope).
	
:- type lambda_clause =< mh_clause
	---> 	fact_clause()
	;		rule_clause()
	
	
:- type head_clause =<
	
:- type lhs_clause =<


:- type relation_clause =<

:- type predicate_clause =<
	--->	fact_clause(mh_predicate)

:- type function_clause =<
	

	
	

% TODO: apply substitutions

%-----------------------------------------------------------------------------%
% Expressions

:- type clause_expression
	--->	unification_exp(mh_term, mh_term) % (term = term)
	;		


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%
% Clauses


