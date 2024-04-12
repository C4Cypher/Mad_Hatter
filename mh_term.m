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

:- import_module set.
:- import_module list.
:- import_module univ.



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
;		functor(symbol, relation)
;		univ(univ).



:- inst var
---> 	var(ground)
;		named_var(ground).

:- inst var(I)
---> 	var(I)
;		named_var(I).

:- type var =< mh_term 
---> 	var(var_id)
;		named_var(symbol).

:- inst functor ---> functor(ground, ground).

:- inst functor(I) ---> functor(ground, relation(I)).

:- type functor =< mh_term ---> functor(symbol, relation).



:- inst mh_ground
---> 	entity(ground)
;		int(ground)
;		float(ground)
; 		symbol(ground)
;		string(ground)
;		functor(ground, ground_relation)
;		univ(ground).

:- type mh_ground =< mh_term
---> 	entity(entity_id)
;		int(int)
;		float(float)
; 		symbol(symbol)
;		string(string)
;		functor(symbol, ground_relation)
;		univ(univ).

:- type entity =< mh_ground ---> entity(id(entity)).

:- inst numeric_term
--->	var(ground)
;		named_var(ground)
;		int(ground)
;		float(ground)
;		functor(ground, ground).

:- inst numeric_term(I)
--->	var(I)
;		named_var(I)
;		int(I)
;		float(I)
;		functor(I, relation(I)).

:- inst not_numeric_term
--->	symbol(ground)
;		string(ground)
;		univ(ground).

:- inst not_numeric_term(I)
--->	symbol(I)
;		string(I)
;		univ(I).

:- type numeric_term =< mh_term
---> 	var(id(var))
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

