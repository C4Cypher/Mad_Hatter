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
:- import_module mh_predicate.

%-----------------------------------------------------------------------------%
% Clauses

:- type mh_clause(T)
	--->	fact_clause(T)					% X.
	;		rule_clause(T, mh_predicate).		% X :- Y.
	
:- func clause_head(mh_clause(T)) = T.
:- func clause_body(mh_clause(_T)) = mh_predicate.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%
% Clauses

clause_head(fact_clause(Head)) = Head.
clause_head(rule_clause(Head, _)) = Head.

clause_body(fact_clause(_)) = true.
clause_body(rule_clause(_, Body)) = Body.

