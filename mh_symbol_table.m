%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_symbol_table.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module symbol_table.



:- import_module mh_symbol.
:- import_module mh_type.
:- import_module mh_arity.

:- type symbol_table(T). % <= arity(T)

:- pred init_symbol_table(


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module array.

:- type symbol_table(T)	---> symbols(map(symbol, array(T))).


