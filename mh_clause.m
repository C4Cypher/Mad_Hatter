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
	--->	named_clause(symbol, mh_term) 		% name = Term.
	;		fact_clause(mh_predicate, mh_scope) % Pred.
	;		rule_clause(mh_term, mh_predicate, mh_scope) % Term :- Pred.
	;		unification_clause(mh_term, mh_term, mh_scope). % Term = Term.

% TODO: apply substitutions


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%
% Clauses


