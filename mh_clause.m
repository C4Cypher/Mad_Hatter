%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
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
:- import_module mh_mode.
:- import_module mh_arity.

%-----------------------------------------------------------------------------%
% Clauses

% A clause represents a single disjunct of a relation, typically parsed
% as a single peicee of source code.  A relation *may* be represented by
% a single disjunctive clause, however, in order to represent relations in
% the way I desire (especially concerning unification and lazy evaluation of
% relations), I need to break up the relation concept into component clauses.



:- type mh_clause 
	---> 	horn_clause(predicate_term, arity, mh_scope).		
	;		relation_clause(predicate_term, arity, mh_scope)	
	;		function_clause(predicate_term, arity, mh_scope)	
	;		moded_clause(mh_mode, predicate_term, mh_scope).
	
:- func clause_arity(mh_clause) = arity.
:- func clause_scope(mh_clause) = mh_scope.
:- func clause_context(mh_clause) = scope_context.
	
	