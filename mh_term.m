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

:- type mh_type
--->	entity
;		number
; 		enum(set(symbol))
;		functor(symbol, list(mh_type)).

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
;		f(symbol, list(mh_term)).

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
;		symbol(ground)
;		f(ground, list(ground_term)).

:- type mh_ground =< mh_term
---> 	entity(entity_id)
;		int(int)
;		float(float)
;		symbol(symbol)
;		f(symbol, list(mh_ground)).

:- type entity =< mh_ground ---> entity(id(entity)).

:- type number =< mh_ground 
--->	int(int)
;		float(float).

:- inst number_int ---> int(ground).
:- inst number_float ---> float(ground).

:- inst ground_functor
---> 	f(ground, list(ground_term)).

:- type ground_functor =< functor 
---> 	f(symbol, list(mh_ground)).

%-----------------------------------------------------------------------------%

