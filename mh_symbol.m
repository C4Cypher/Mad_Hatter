%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_symbol.m
% Main author: C4Cypher.
% Stability: low.

:- module mh_symbol.

:- interface.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type mh_symbol.
:- type mh_symbols == list(mh_symbol).

:- inst mh_symbol(I) ---> ~I.



:- func symbol(string) = mh_symbol.
:- mode symbol(in) = out is det.
:- mode symbol(out) = in is det.

:- func to_string(mh_symbol) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% TODO: Revisit symbol memoization.  Hashing?

:- pragma require_feature_set([memo]). 
%-----------------------------------------------------------------------------%


:- type mh_symbol ---> ~string.

symbol(String) = '~'(String).

to_string('~'(String)) = String.

:- pragma memo(symbol(in) = out).

