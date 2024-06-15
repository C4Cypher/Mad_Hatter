%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_procedure.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_procedure.

:- interface.

:- import_module mh_arity.
:- import_module mh_mode.

%-----------------------------------------------------------------------------%

:- typeclass procedure(T, S) <= (arity(T), signature(T, S)) where [ ].

:- typeclass procedure(T) <= procedure(T, proc_signature) where [ ].

:- typeclass predicate(T) <= procedure(T, predicate_signature) where 
[
	% Todo:  predicate to call predicate under a module and scope
].

:- typeclass functor(T) <= procedure(T, functor_signature) where [
	% Todo: context to call a functor unification under a module and scope
].

:- typeclass function(T) <= procedure(T, function_signature) where [
	% Todo:  predicate to apply a function under a module and scope
].

