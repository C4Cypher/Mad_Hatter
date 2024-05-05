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

:- type symbol.
:- type symbols == list(symbol).



:- func symbol(string) = symbol.
:- mode symbol(in) = out is det.
:- mode symbol(out) = in is det.




:- implementation.

:- pragma require_feature_set(memo).

:- type symbol = \string.

symbol(String) = \String.

:- pragma memo(symbol(in) = out).