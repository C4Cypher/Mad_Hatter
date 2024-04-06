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

:- import_module set.
:- import_module list.
:- import_module univ.

:- type mh_type
--->	entity
;		number
;		symbol
; 		enum_symbol(set(symbol))
;		string
;		functor(symbol, list(mh_type))
;		univ.


:- type functor_signature =< mh_type ---> functor(symbol, list(mh_type)).

:- type entity_id == id(entity).
:- type var_id == id(var).


:- type mh_term 
--->	var(var_id)
;		named_var(symbol)
;		entity(entity_id)
;		int(int)
;		float(float)
; 		symbol(symbol)
;		string(string)
;		f(symbol, list(mh_term))
;		univ(univ).

:- inst var
---> 	var(ground)
;		named_var(ground).

:- type var =< mh_term 
---> 	var(id(var))
;		named_var(symbol).

:- type functor =< mh_term ---> f(symbol, list(mh_term)).



:- inst ground_term
---> 	entity(ground)
;		int(ground)
;		float(ground)
; 		symbol(ground)
;		string(ground)
;		f(ground, list(ground_term))
;		univ(ground).

:- type mh_ground =< mh_term
---> 	entity(entity_id)
;		int(int)
;		float(float)
; 		symbol(symbol)
;		string(string)
;		f(symbol, list(mh_ground))
;		univ(univ).

:- type entity =< mh_ground ---> entity(id(entity)).

:- inst numeric_term
--->	var(ground)
;		named_var(ground)
;		int(ground)
;		float(ground)
;		f(ground, ground).

:- inst not_numeric_term
--->	symbol(ground)
;		string(ground)
;		univ(ground).

:- type numeric_term =< mh_term
---> 	var(id(var))
;		named_var(symbol)
;		int(int)
;		float(float)
;		f(symbol, list(mh_term)).

:- type number =< numeric_term
--->	int(int)
;		float(float).

:- inst number_int ---> int(ground).
:- inst number_float ---> float(ground).

:- inst ground_functor
---> 	f(ground, list(ground_term)).

:- type mh_ground_functor =< functor 
---> 	f(symbol, list(mh_ground)).

%-----------------------------------------------------------------------------%

