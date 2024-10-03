%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_relation.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_relation.

:- interface.

:- import_module array.

:- import_module mh_term.
:- import_module mh_clause.
:- import_module mh_tuple.
:- import_module mh_substitution.
:- import_module mh_arity.

%-----------------------------------------------------------------------------%
% Relation type


:- type mh_relation 
	--->	invalid_relation(mh_term, string)
	;		relation(mh_term) % X = Y
 	;		relation_disjunction(mh_tuple) % X = A ; B ; C ; ...
	;		relation_radix(mh_term, array(mh_relation)) % X = (A ; B) ; (...)
	; 		some [T] mr_relation(T) => relation(T).
	
:- pred apply_relation_substitution(mh_substitution::in, mh_relation::in,
	mh_relation::out) is det.
	
%-----------------------------------------------------------------------------%
% Radix tree

:- inst relation_leaf ---> relation_unification(ground).

:- type relation_leaf =< mh_relation
	---> relation(mh_term).
	
:- inst relation_branch
	--->	relation_disjunction(ground)
	;		relation_radix(ground, ground).
	
:- type relation_branch
	---> 	relation_disjunction(mh_tuple)
	;		relation_radix(mh_term, array(mh_relation)).
	
:- type relation_branch
	

%-----------------------------------------------------------------------------%
% Relation typeclass

:- typeclass relation(T) <= arity(T) where [
	% Todo: method to unify relations under environment
	
	% pred relation_signature(T::in, E::in, relation_signature::out) is nondet,
].


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%

apply_relation_substitution(_, _, _) :- sorry($module, $pred,
	"apply_relation_substitution/3").
	
:- pragma no_determinism_warning(apply_relation_substitution/3).
