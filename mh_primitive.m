%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_primitive.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_primitive.

:- interface.

:- import_module mh_term.
:- import_module mh_relation.
:- import_module mh_type.


:- typeclass primitive(T) where [
	pred call_primitive(T::in, relation::in, mh_term::out) is det,
	pred primitive_type_signature(T::in, primitive_signature::out) is det
].

:- type primitive_func == (func(relation) = mh_term). 
