%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_relation.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_relation.

:- interface.

% :- import_module array.

:- import_module mh_clause.
:- import_module mh_proposition.
:- import_module mh_term.
:- import_module mh_scope.
:- import_module ordered_set.

%-----------------------------------------------------------------------------%
% Relation type

% A relation represents the functor of a compound term in any unifcation
% that requires more work than simple assignment of that compound term to
% a free variable.
% 
%
% X = R(Arg)
%
% At it's core, they represent the 'head clause' of any horn clause, even for
%  s. In essene, a predicate in the traditional Prolog sense is a 
% Relation that unifies with a success indicator or truth value, or to be more
% specific, a function that returns a success indicator. The substitution 
% contained in the success represents the variable bindings that resolve
% to make the fact 'true'

% Relations can be  'moded', modes being pairings of pre-conditions and post
% conditions that enforce the relation's soundness and purity.
 
% Thinking about this. I can describe relations in many  different ways.
% However I need to distill the type structure down into a way that is
% unifiable and able to encapsulate clauses before and after mode analysis
% PLUS handling foreign function calls. I need to define modes. Preconditions
% and postconditions.

% Put it another way, a relation is a disjunction of clauses


:- type mh_relation 
			% r(X) = Y :- p(X, Y).
	--->	relation_clause(mh_clause)
			% r(A, B) = C :- p({A, B, C}).
	;		prop_relation(mh_proposition, mh_scope)
			% r(X) = a(X) ; b(X). <-> r(X) = Y :- a(X) = Y ; b(X) = Y.
	;		disj_relation(ordered_set(mh_relation))
			% r(X) = a(X) , b(X). <-> r(X) = Y :- a(X) = Y , b(X) = Y. a = b.
	;		conj_relation(ordered_set(mh_relation)).
	
:- pred apply_relation_substitution(mh_substitution::in, mh_relation::in,
	mh_relation::out) is det.
	
 
:- pred ground_relation(mh_relation::in) is semidet.
 
:- func relation_arity(mh_relation, int) = mh_relation.
 
:- pred relation_arity(mh_relation::in, int::in, mh_relation::out) is det.
 



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%

apply_relation_substitution(_, _, _) :- sorry($module, $pred,
	"apply_relation_substitution/3").
	
:- pragma no_determinism_warning(apply_relation_substitution/3).

ground_relation(_) :- sorry($module, $pred, "ground_relation/1").

:- pragma no_determinism_warning(ground_relation/1).

relation_arity(invalid_relation(_, _) = 0.
relation_arity(relation(P)) = fact_arity(P) - 1.


relation_arity(T, relation_arity(T)).

:- instance arity(mh_relation) where [ pred(arity/2) is relation_arity ].
