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

:- import_module ordered_set.

:- import_module mh_clause.
:- import_module mh_proposition.
:- import_module mh_arity.
:- import_module mh_tuple.
:- import_module mh_scope.
:- import_module mh_term_map.
:- import_module mh_function.
:- import_module mh_term.
:- import_module mh_mode.
:- import_module mh_substitution.

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

% TODO: replace ordered_set/1  with mh_relation_set or mh_ordered_relation_set
:- type mh_relation 
			% Direct from parsed clause
	--->	clause_relation(mh_clause)
			% r(X) = a(X) ; b(X). <-> r(X) = Y :- a(X) = Y ; b(X) = Y.
	;		disj_relation(ordered_set(mh_relation))
			% r(X) = a(X) , b(X). <-> r(X) = Y :- a(X) = Y , b(X) = Y. r = a@b.
	;		conj_relation(ordered_set(mh_relation))
			% r(X) = Y :- not Y = n(X)
	;		negated_relation(mh_relation)
			% r(X) = Y :- f(X) -> Y.
	;		func_relation(mh_function) %Hmmm ...
			% r(X) = Y :- p(X, Y).
	;		horn_relation(arity, mh_proposition, mh_scope)
			% r(X::out(constraint), Y::out(constraint), Z::out(constraint)). 
	;		fact_relation(mh_tuple, mh_scope)
			% Curried relation based on first argument, nil maps to thunk
	;		tree_relation(mh_term_map(mh_term))
			% r1(?X::M) = (Y::N) :- r2(?[X, Y]::[M, N])
	;		moded_relation(mh_mode, mh_relation)
			% r(M:mode).
	;		norm_rleation(mh_mode, mh_scope). 
	
:- pred apply_relation_substitution(mh_substitution::in, mh_relation::in,
	mh_relation::out) is det.
	
:- func relation_scope(mh_relation) = mh_scope.
	
 
:- pred ground_relation(mh_relation::in) is semidet.
 



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