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

:- import_module map.
:- import_module set.
:- import_module list.

:- type mh_type
	--->	free
	;		type(symbol) % type alias
	;		type(symbol, list(mh_type)) %parametric type alias
	;		union(set(mh_type))
	;		entity
	;		number
	;		int
	;		float
	;		enum(set(symbol))
	;		symbol
	;		string
	;		predicate(symbol, predicate_bindings)
	;		function(symbol, function_bindings)
	;		data(symbol, list(mh_data_type))
	;		univ.

%-----------------------------------------------------------------------------%

:- type mh_ground_type =< mh_type
	--->	entity
	;		int
	;		float
	;		symbol
	;		string
	;		predicate(symbol, predicate_bindings)
	;		function(symbol, function_bindings)
	;		data(symbol, list(mh_data_type))
	;		univ.

%-----------------------------------------------------------------------------%

:- type predicate_signature =< mh_type
	--->	predicate(symbol, predicate_bindings).

:- type function_signature =< mh_type
	---> 	function(symbol, function_bindings).

:- type data_signature =< mh_type
	---> 	data(symbol, list(mh_data_type)).

%-----------------------------------------------------------------------------%

:- type mh_data_type =< mh_type
	--->	union(set(mh_data_type))
	;		entity
	;		number
	;		enum(set(symbol))
	;		symbol
	;		string
	;		data(symbol, list(mh_data_type))
	; 		univ.


%-----------------------------------------------------------------------------%


:- type element ---> element(mh_type, field).

:- type function_element == element.

:- type predicate_element =< element
	---> element(mh_type, argument).
	


:- type binding == map(element, element).

:- type bindings == set(binding).

:- type function_binding = binding.

:- type function_bindings = set(function_binding).

:- type predicate_binding == map(predicate_element, predicate_element).

:- type predicate_bindings == set(predicate_binding).

%-----------------------------------------------------------------------------%