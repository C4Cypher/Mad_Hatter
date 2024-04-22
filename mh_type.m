%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_type.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_type.

:- interface.

:- import_module mh_symbol.
:- import_module mh_argument.
:- import_module mh_term.
:- import_module mh_mode.

:- import_module map.
:- import_module set.
:- import_module list.

:- type mh_type
	--->	free
	;		type(symbol) % type alias
	;		type(symbol, list(mh_type)) %parametric type alias
	;		type_var(mh_var)
	;		union(set(mh_type))
	;		entity
	;		number
	;		int
	;		float
	;		enum(set(symbol))
	;		symbol
	;		string
	;		predicate(symbol, relation_mode)
	;		function(symbol, relation_mode)
	;		data(symbol, list(mh_data_type))
	;		univ.

%-----------------------------------------------------------------------------%

:- type mh_ground_type =< mh_type
	--->	entity
	;		int
	;		float
	;		symbol
	;		string
	;		predicate(symbol, relation_mode)
	;		function(symbol, relation_mode)
	;		data(symbol, list(mh_data_type))
	;		univ.

%-----------------------------------------------------------------------------%



%-----------------------------------------------------------------------------%

:- type mh_data_type =< mh_type
	--->	union(set(mh_data_type))
	;		entity
	;		number
	;		symbol
	;		string
	;		data(symbol, list(mh_data_type))
	; 		univ.


%-----------------------------------------------------------------------------%




%-----------------------------------------------------------------------------%