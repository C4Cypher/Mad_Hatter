%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
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

:- import_module mh_term.

%-----------------------------------------------------------------------------%
% Clauses

:- type mh_clause
	--->	fact_clause(clause_head)
	;		rule_clause(clause_head, mh_term).
	
:- func clause_head(mh_clause) = clause_head.

:- inst mh_clause(I)
	---> 	fact_clause(I)
	;		rule_clause(I, ground).
	
:- inst fact_clause(I) ---> fact_clause(I).

:- mode is_fact_clause(I) == ground >> fact_clause(I).

:- inst fact_clause == fact_clause(ground).

:- mode is_fact_clause == is_fact_clause(ground).

	
%-----------------------------------------------------------------------------%
% Clause head

:- type clause_head
	--->	predicate_head(mh_term)				% X 	 :-
	;		unification_head(mh_term, mh_term) 	% X = Y  :-
	;		function_head(mh_term, mh_term).	% X -> Y :-

:- inst predicate_head(I) ---> predicate_head(I).
:- inst predicate_head == predicate_head(ground).
 
:- type predicate_head =< clause_head 
	---> 	predicate_head(mh_term).

:- inst unification_head(I) ---> unification_head(I, ground).
:- inst unification_head == unification_head(ground).
	
:- type unification_head =< clause_head 
	---> unification_head(mh_term, mh_term).
	
:- type function_head =< clause_head
	---> function_head(mh_term, mh_term).
	
	
%-----------------------------------------------------------------------------%
% Clause subtypes

:- type predicate_clause =< mh_clause
	---> fact_clause(predicate_head)	% Head.
	;	 rule_clause(predicate_head, mh_term).	% Head :- Body.
	
:- type unification_clause =< mh_clause
	---> 	fact_clause(unification_head)	% X = Y.
	;		rule_clause(unification_head, mh_term). % X = Y :- Body.
	
:- type function_clause =< mh_clause
	--->	fact_clause(function_head) % X -> Y.
	;		rule_clause(function_head, mh_term). % X -> Y :- Body.



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%
% Clauses

clause_head(fact_clause(Head)) = Head.
clause_head(rule_clause(Head, _)) = Head.
