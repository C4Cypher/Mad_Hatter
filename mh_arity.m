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

:- import_module mh_symbol.

%-----------------------------------------------------------------------------%

:- typeclass arity(T) where [
	pred arity(T::in, int::out) is det
].

:- func arity(T) = int <= arity(T).

%-----------------------------------------------------------------------------%

:- type symbol_arity ---> symbol/int.

:- func symbol_arity(symbol, T) = symbol_arity <= arity(T).

%-----------------------------------------------------------------------------%

:- instance arity(list(T)).
:- instance arity(array(T)).
:- instance arity(symbol_arity).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

arity(T) = A :- arity(T, A).

%-----------------------------------------------------------------------------%

symbol_arity(S, T) = S/arity(T).

%-----------------------------------------------------------------------------%

:- instance arity(list(T)) where [ pred(arity/2) is list.length ].

:- instance arity(array(T)) where [ pred(arity/2) is array.size ].

:- instance arity(symbol_arity) where [
		arity(_/Arity, Arity)
	].