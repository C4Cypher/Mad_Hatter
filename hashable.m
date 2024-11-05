%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: hashable.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module hashable.

:- interface.

%-----------------------------------------------------------------------------%

:- typeclass hashable(K) where [
	func hash(K) = uint
].


:- pred hash(K::in, uint::out) is det <= hashable(K) .


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

hash(K, hash(K)).

:- pragma inline(hash/2).
