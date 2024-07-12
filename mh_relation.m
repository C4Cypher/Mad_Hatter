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

:- import_module mh_arity.

%-----------------------------------------------------------------------------%
% Relation type

:- type mh_relation 
	--->	relation(mh_constructor, mh_term) % r(X) = Y
	;		relation(mh_constructor, mh_term, mh_term) % r(X) = Y :- Z.
	

%-----------------------------------------------------------------------------%
% Relation typeclass

:- typeclass relation(T) <= arity(T) where [
	% Todo: method to unify relations under environment
	
	% pred relation_signature(T::in, E::in, relation_signature::out) is nondet,
].

:- typeclass predicate(T) <= relation(T) where 
[
	% Todo:  predicate to call predicate under a module and scope
].

:- typeclass function(T) <= relation(T) where [
	% Todo:  predicate to apply a function under a module and scope
].

