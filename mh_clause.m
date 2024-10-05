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

:- import_module array.

:- import_module mh_term.
:- import_module mh_predicate.
:- import_module mh_tuple.
:- import_module mh_var_set.
:- import_module mh_arity.

%-----------------------------------------------------------------------------%
% Clauses

:- type mh_clause
	--->	clause(
			clause_arity:int,  %number of vars in the clause head. 
			clause_body:mh_term, 
			var_names:array(string)
).		

	% the clause head is the term that unifies with the vars less than or
	% equal to the clause's arity.  If the arity is greater than 1, the term
	% is a tuple representing the terms in the clause's head. Fails if arity
	% is zero.   TODO: throw exception if the arity is negative?
:- func clause_head(mh_clause) = mh_term is semidet.


% TODO: apply substitutions


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%
% Clauses


clause_head(_Arity, _Body, _) :- sorry($module, $pred).

