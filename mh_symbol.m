%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_symbol.m
% Main author: C4Cypher.
% Stability: low.

:- module mh_symbol.

:- interface.

:- import_module hashable.
:- import_module hashmap.

%-----------------------------------------------------------------------------%
% Symbols

:- type mh_symbol.

:- inst mh_symbol(I) ---> ~I.



:- func symbol(string) = mh_symbol.
:- mode symbol(in) = out is det.
:- mode symbol(out) = in is det.

:- func to_string(mh_symbol) = string.

%-----------------------------------------------------------------------------%
% Symbol hashes

:- pred symbol_hash(mh_symbol::in, int::out) is det.
:- func symbol_hash(mh_symbol) = uint.

:- instance hashable(mh_symbol).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module fnv_hash.
:- import_module uint.

% TODO: Memo grades may not be thread safe or compatable.
% Revisit symbol memoization.  Hashing?

:- pragma require_feature_set([memo]). 


%-----------------------------------------------------------------------------%
% Symbols

:- type mh_symbol ---> ~string.

symbol(String) = '~'(String).

:- pragma memo(symbol(in) = out).


to_string(symbol(String)) = String.


%-----------------------------------------------------------------------------%
% Symbol hashes


symbol_hash(symbol(String), cast_to_int(Hash)) :- fnv1a_hash(String, Hash).
symbol_hash(symbol(String)) =  fnv1a_hash(String).

:- instance hashable(mh_symbol) where [ func(hash/1) is symbol_hash ].