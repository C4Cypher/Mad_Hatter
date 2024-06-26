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

:- inst symbol(I) ---> \I.



:- func symbol(string) = symbol.
:- mode symbol(in) = out is det.
:- mode symbol(out) = in is det.

:- func to_string(symbol) = string.

:- implementation.

:- pragma require_feature_set([memo]).

:- type symbol ---> \string.

symbol(String) = \String.

to_string(\String) = String.

:- pragma memo(symbol(in) = out).

% I don't like using '\' as the internal constructor for symbols, but Mercury
% doesn't have a more suitable bultin operator at present. The reason I'm
% using it is that, at this point it should result in more readable output