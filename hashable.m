%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: hashable.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module hashable.

:- interface.

%-----------------------------------------------------------------------------%
% Hashable type class

:- typeclass hashable(K) where [
	func hash(K) = uint
].


% predicate alternative for compatability with hash_table.m

:- pred hash(K::in, uint::out) is det <= hashable(K) .

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

hash(K, hash(K)).

:- pragma inline(hash/2).
