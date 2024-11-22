%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_type_table.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module symbol_table.

:- import_module map.
:- import_module type_desc.
:- import_module set.

:- import_module mh_symbol.
:- import_module mh_type.

:- type symbol_table == map(symbol, symbol_assignment).

:- type symbol_assignment
	---> 	primitive_type(type_desc)
	;		data_type(set(data_type_signature))
	;		type_assignment(