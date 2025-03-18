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

:- import_module mh_term.
:- import_module mh_scope.
:- import_module mh_predicate.
:- import_module mh_substitution.
:- import_module mh_arity.

%-----------------------------------------------------------------------------%
% Relation type


:- type mh_relation 
			% r(X) = Y :- p(X, Y).
	--->	predicate_relation(mh_scope, predicate_term, arity)	
	;		
	;		mercury_call(mr_call).
	
:- pred apply_relation_substitution(mh_substitution::in, mh_relation::in,
	mh_relation::out) is det.
	
 
:- pred ground_relation(mh_relation::in) is semidet.
 
:- func relation_arity(mh_relation) = arity.
 
:- pred relation_arity(mh_relation::in, int::out) is det.
 
:- instance arity(mh_relation).


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
relation_arity(relation(P)) = predicate_arity(P) - 1.


relation_arity(T, relation_arity(T)).

:- instance arity(mh_relation) where [ pred(arity/2) is relation_arity ].
