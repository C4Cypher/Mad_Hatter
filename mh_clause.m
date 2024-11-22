%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_clause.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_clause.

:- interface.

% :- import_module array.

:- import_module mh_term.
:- import_module mh_scope.
:- import_module mh_symbol.
:- import_module mh_predicate.
% :- import_module mh_tuple.
:- import_module mh_var_set.
% :- import_module mh_arity.

%-----------------------------------------------------------------------------%
% Clauses

:- type mh_clause

	%% root clauses
	
	% name(term)			[ :- body].	=> named predicate clause
	% name(term) = term 	[ :- body].	=> named relation clause
	% name(term) -> term 	[ :- body].	=> named function clause
	% term:name = term 		[ :- body].	=> named constraint clause
	
	% all [terms] body. 				=> assertion_clause

	% name = \( clause ).  | name(Term) = relation(Term)
	--->	atom_clause(mh_symbol, lambda_clause, root_scope)
	
	% fact clause.
	;		assertion_clause(mh_predicate, root_scope)
	
	%% lambda clauses
	
	% \( term 				[ :- body]) => lambda predicate
	% \( term = term 		[ :- body]) => lambda relation
	% \( clause ; clause..)	[ :- body]) => lambda relation (disjunctve)
	% \( term -> term 		[ :- body]) => lambda function
	% \( term @ term = term	[ :- body]) => lambda constraint 
	
		
	% \( predicate ).
	;		fact_clause(head_clause)
	
	% \( forall term predicate_clause )
	;		quantified_fact_clause(mh_var_set, mh_predicate)

	
	% \( head :- predicate).
	;		rule_clause(head_clause, mh_predicate)

	%% head_clauses
	
	% X | { A, B, C ...}
	;		term_clause(mh_term)
	
	% X = Y
	;		unification_clause(mh_term, mh_term)
	
	% X -> Y
	;		arrow_clause(mh_term, mh_term)
	
	% X @ Y = Z => (X ; Y) = ?(_)
	;		constraint_clause(mh_term, mh_term, mh_term).
	

	
:- type root_clause =< mh_clause
	--->	atom_clause(mh_symbol, lambda_clause, root_scope)
	;		assertion_clause(mh_predicate, root_scope).
	
:- type lambda_clause =< mh_clause
	---> 	fact_clause(head_clause)
	;		quantified_fact_clause(mh_var_set, mh_predicate)
	;		rule_clause(head_clause, mh_predicate)
	;		term_clause(mh_term)
	;		unification_clause(mh_term, mh_term)
	;		arrow_clause(mh_term, mh_term)
	;		constraint_clause(mh_term, mh_term, mh_term).
	
:- type complete_lambda_clause =< lambda_clause
	---> 	fact_clause(head_clause)
	;		quantified_fact_clause(mh_var_set, mh_predicate)
	;		rule_clause(head_clause, mh_predicate).
	
:- type head_clause =< lambda_clause
	---> 	term_clause(mh_term)
	;		unification_clause(mh_term, mh_term)
	;		arrow_clause(mh_term, mh_term)
	;		constraint_clause(mh_term, mh_term, mh_term).
	
	

	

	
	

% TODO: apply substitutions


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%
% Clauses


