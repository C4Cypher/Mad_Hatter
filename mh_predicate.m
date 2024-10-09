%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_predicate.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_predicate.

:- interface.

:- import_module list.

:- import_module mh_term.
:- import_module mh_substitution.
:- import_module mh_arity.
:- import_module mh_tuple.

% In prolog and many other logic programming languages, a predicate is
% represented by a clause, either a fact clause 'predicate(X,Y).' or a rule clause
% 'predicate(X) :- bar(X)'.
%
% Not so in Mad Hatter, instead a 'predicate' represents a relational term
% after the arguments have been applied.  The application of arguments is the
% realm of relations, which produce predicates after evaluation.

%-----------------------------------------------------------------------------%
% Predicates

:- type mh_predicate 
	--->	invalid_predicate(mh_term, string)
 	;		pred_success(mh_substitution)
  	;		pred_failure(pred_fail_reason)
	;		some [T] mr_predicate(T) => predicate(T).
	
	
:- pred apply_predicate_substitution(mh_substitution::in, mh_predicate::in,
	mh_predicate::out) is det.
	
 :- pred ground_predicate(mh_predicate::in) is semidet.
 
 :- func predicate_arity(mh_predicate) = int.
 
 :- pred predicate_arity(mh_predicate::in, int::out) is det.
 
 :- instance arity(mh_predicate).
	
%-----------------------------------------------------------------------------%
% Predicate failure

:- type pred_fail_reason
	--->	value_unification_failure(mh_term, mh_term) % 1 \= 2
	;		constraint_unification_failure(mh_term, mh_term) % 5:string
	;		flounder(mh_term) % ?- predicate(1) where predicate(A) :- predicate(A).
	;		conjunction_failure(mh_tuple, int, pred_fail_reason)
	;		disjunction_failure(mh_tuple, list(pred_fail_reason))
	;		mr_predicate_failure(mh_term, string)
	;		mr_relation_failure(mh_term, string).
	


%-----------------------------------------------------------------------------%
% Predicate typeclass


:- typeclass predicate(T) <= arity(T)  where 
[
	% Todo:  predicate to call predicate under a module and scope
].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%

apply_predicate_substitution(_, _, _) :- sorry($module, $pred,
	"apply_predicate_substitution/3").

:- pragma no_determinism_warning(apply_predicate_substitution/3).


ground_predicate(_) :- sorry($module, $pred, "ground_predicate/1").

:- pragma no_determinism_warning(ground_predicate/1).

predicate_arity(_) = _ :- sorry($module, $pred, "predicate_arity/1").

:- pragma no_determinism_warning(predicate_arity/1).

predicate_arity(T, predicate_arity(T)).

:- instance arity(mh_predicate) where [ pred(arity/2) is predicate_arity ].