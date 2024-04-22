%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_term.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%


:- module mh_term. 

:- interface.

:- import_module mh_symbol.
:- import_module mh_identifier.
:- import_module mh_var.
:- import_module mh_type.
:- import_module mh_relation.
:- import_module mh_function.

:- import_module set.
:- import_module list.
:- import_module univ.



:- type entity_id == id(entity).
:- type var_id == id(mh_var).


:- type mh_term 
	--->	var(var_id)
	;		named_var(symbol)
	;		entity(entity_id)
	;		int(int)
	;		float(float)
	; 		symbol(symbol)
	;		string(string)
	;		type(mh_type)
	;		functor(symbol, relation)
	;		function(mh_function)
	;		closure(mh_function, relation)
	;		univ(univ).



:- inst mh_var
	---> 	var(ground)
	;		named_var(ground).

:- inst mh_var(I)
	---> 	var(I)
	;		named_var(I).

:- type mh_var =< mh_term 
	---> 	var(var_id)
	;		named_var(symbol).

:- inst functor ---> functor(ground, ground).

:- type functor =< mh_term ---> functor(symbol, relation).





:- type mh_ground =< mh_term
	---> 	entity(entity_id)
	;		int(int)
	;		float(float)
	; 		symbol(symbol)
	;		string(string)
	;		type(mh_type)
	;		functor(symbol, ground_relation)
	;		function(mh_function)
	;		closure(mh_function, ground_relation)
	;		univ(univ).

:- type entity =< mh_ground ---> entity(id(entity)).

% 	Values that are not only semantically ground, but can be stored in state 
%	and can be serialized

:- type mh_stval =< mh_ground
	---> 	entity(entity_id)
	;		int(int)
	;		float(float)
	; 		symbol(symbol)
	;		string(string)
	;		type(mh_type)
	;		functor(symbol, ground_relation).

:- inst numeric_term
	--->	var(ground)
	;		named_var(ground)
	;		int(ground)
	;		float(ground)
	;		functor(ground, ground).



:- inst not_numeric_term
	--->	symbol(ground)
	;		string(ground)
	;		univ(ground).



:- type numeric_term =< mh_term
	---> 	var(var_id)
	;		named_var(symbol)
	;		int(int)
	;		float(float)
	;		functor(symbol, relation).

:- type number =< numeric_term
	--->	int(int)
	;		float(float).

:- inst number_int ---> int(ground).
:- inst number_float ---> float(ground).



%-----------------------------------------------------------------------------%

