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
:- import_module mh_mode.

:- import_module set.
:- import_module list.
:- import_module type_desc.

%-----------------------------------------------------------------------------%


:- type mh_type
	--->	any
	;		named_type(symbol) % type alias
	;		parametric_type(symbol, list(mh_type)) %parametric type alias
	;		union(set(mh_type))
	;		mercury_type(type_desc).
	;		primitive(symbol)
	;		predicate(relation_signature) 
	;		function(function_signature)
	;		operation(operation_signature)
	;		boolean
	;		data(symbol, list(mh_type)).
	
%-----------------------------------------------------------------------------%

:- inst mh_ground_type
	--->	primitive(ground)
	;		predicate(ground)
	;		function(ground)
	;		operation(ground)
	;		boolean
	;		data(ground, list(mh_ground_type)).

:- type mh_ground_type =< mh_type
	--->	primitive(symbol)
	;		predicate(relation_signature) 
	;		function(function_signature) 
	;		operation(operation_signature)
	;		boolean
	;		data(symbol, list(mh_ground_type)).

%-----------------------------------------------------------------------------%

:- type mh_ground_union =< mh_type ---> union(set(mh_ground_type)).

%-----------------------------------------------------------------------------%

:- type proc_relation_type  %TODO: Consider using arrays instead
	--->	relation_type(list(mh_type))
	;		function_type(list(mh_type), mh_type).
	
:- type relation_type =< proc_relation_type
	---> 	relation_type(list(mh_type)).
	
:- type function_type =< proc_relation_type
	--->	function_type(list(mh_type), mh_type).



%-----------------------------------------------------------------------------%