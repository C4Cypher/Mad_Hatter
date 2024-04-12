%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_declaration.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_declaration.

:- interface.

:- import_module mh_symbol.
:- import_module mh_term.
:- import_module mh_expression.


:- type declaration
--->	type_def(symbol, mh_type)
;		state_var(predicate_signature)
;		some [T] state_relation(predicate_signature, T =< relation(T)). 