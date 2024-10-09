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

	% name = ( clause ).  
	% Symbol assignment occurs above any local clause scope
	--->	atom_clause(symbol, lambda_clause, root_scope) 		
	
	% name(Term) = apply( clause, Term ).  
	%  All vars in Term are universally quantified, produces scope
	;		cons_clause(symbol, mh_term, lambda_clause, root_scope) 
	
	% \( predicate ).
	;		fact_clause(mh_predicate) 			
	
	% \(Term :- Pred). | 
	;		rule_clause(head_clause, mh_predicate) 
	

	
	

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


