%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_arity.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_arity.

:- interface.

:- import_module int.
:- import_module list.
:- import_module array.

%-----------------------------------------------------------------------------%

:- typeclass arity(T) where [
	pred arity(T, int),
	mode arity(in, out) is det
].

:- func arity(T) = int <= arity(T).

%-----------------------------------------------------------------------------%

:- instance arity(list(T)).
:- instance arity(array(T)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

arity(T) = A :- arity(T, A).

:- instance arity(list(T)) where [ pred(arity/2) is list.length ].

:- instance arity(array(T)) where [ pred(arity/2) is array.size ].