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

:- import_module mh_tuple.
:- import_module mh_proposition.
:- import_module mh_scope.
:- import_module mh_mode.
:- import_module mh_arity.

%-----------------------------------------------------------------------------%
% Clauses

% A clause represents a single disjunct of a relation as parsed directly from
% source.  As such, it carries a refrence back to the location it was parsed 
% from. Thus, it not only serves as the origin point for relations, but
% is also part of the chain that ties events back to the original point
% in the source code that generated them.


:- type mh_clause 
			% f(X).
	---> 	fact_clause(mh_tuple, mh_scope) 
			% p(X) :- Body(X).
	;		horn_clause(mh_proposition, arity, mh_scope)
			% r(X) = Y :- Body(X, Y).
	;		relation_clause(mh_proposition, arity, mh_scope) 
			% f(X) -> Y :- Body(X, Y).
	;		function_clause(mh_proposition, arity, mh_scope)
			% m(X::M) = (Y::N) :- r([X, Y]::[M, N])
	;		moded_clause(mh_mode, mh_clause).
/*	
:- func clause_arity(mh_clause) = arity.
:- func clause_scope(mh_clause) = mh_scope.
:- func clause_context(mh_clause) = scope_context.
*/
% Unify clauses

	
	