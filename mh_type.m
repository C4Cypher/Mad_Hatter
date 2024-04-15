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

:- import_module set.
:- import_module list.

:- type mh_type
--->	free
;		type(symbol)
;		union(set(mh_type))
;		entity
;		number
;		int
;		float
;		enum(set(symbol))
;		symbol
;		string
;		predicate(symbol, list(mh_type))
;		function(symbol, list(mh_type), mh_data_type)
;		data(symbol, list(mh_data_type))
;		univ.

:- type predicate_signature =< mh_type
--->	predicate(symbol, list(mh_type)).

:- type function_signature =< mh_type
---> 	function(symbol, list(mh_type), mh_data_type).

:- type data_signature =< mh_type
---> 	data(symbol, list(mh_data_type)).



:- type mh_data_type =< mh_type
--->	union(set(mh_data_type))
;		entity
;		number
;		enum(set(symbol))
;		symbol
;		string
;		data(symbol, list(mh_data_type))
; 		univ.