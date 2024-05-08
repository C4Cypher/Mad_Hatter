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
:- import_module mh_type.
:- import_module mh_relation.
:- import_module mh_rule.

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
	;		mh_true
	;		mh_false
	;		expression(functor, proc_relation)
	;		lambda(proc_rule)
	;		univ(univ).

%-----------------------------------------------------------------------------%

:- inst mh_var
	---> 	var(ground)
	;		named_var(ground).

:- inst mh_var(I)
	---> 	var(I)
	;		named_var(I).

:- type mh_var =< mh_term 
	---> 	var(var_id)
	;		named_var(symbol).
	
%-----------------------------------------------------------------------------%	

:- type functor =< mh_term
	--->	symbol(symbol)
	;		lambda(proc_rule).

%-----------------------------------------------------------------------------%

:- inst expression ---> expression(ground, ground).

:- type expression =< mh_term ---> expression(functor, proc_relation).


%-----------------------------------------------------------------------------%


:- type mh_ground =< mh_term
	---> 	entity(entity_id)
	;		int(int)
	;		float(float)
	; 		symbol(symbol)
	;		string(string)
	;		type(mh_type)
	;		expression(functor, ground_proc_relation)
	;		lambda(proc_rule)
	;		univ(univ).

:- type entity =< mh_ground ---> entity(id(entity)).

%-----------------------------------------------------------------------------%

% 	Values that are not only semantically ground, but can be stored in state 
%	and can be serialized

:- type mh_stval =< mh_ground
	---> 	entity(entity_id)
	;		int(int)
	;		float(float)
	; 		symbol(symbol)
	;		string(string)
	;		type(mh_type)
	;		expression(functor, ground_proc_relation).
	
%-----------------------------------------------------------------------------%

:- inst numeric_term
	--->	var(ground)
	;		named_var(ground)
	;		int(ground)
	;		float(ground)
	;		expression(ground, ground).
	
:- inst numeric_term(I)
	--->	var(I)
	;		named_var(I)
	;		int(I)
	;		float(I)
	;		expression(I, I).




:- inst not_numeric_term
	--->	symbol(ground)
	;		string(ground)
	;		univ(ground).



:- type numeric_term =< mh_term
	---> 	var(var_id)
	;		named_var(symbol)
	;		int(int)
	;		float(float)
	;		expression(functor, proc_relation).

:- type number =< numeric_term
	--->	int(int)
	;		float(float).

:- inst number_int ---> int(ground).
:- inst number_float ---> float(ground).



%-----------------------------------------------------------------------------%

% I tried to implement the primitive(T) typeclass into it's own module, but
% for some reason mh_relation.m was causing visibility issues.
% I got tired of chasing the dependency issue, so I moved the implementation
% of primitive(T) here, as mh_term is the root of the type structure.


:- typeclass primitive(T) where [
	pred call_primitive(T::in, relation::in, mh_term::out) is det,
	pred primitive_type_signature(T::in, primitive_signature::out) is det
].

:- type primitive_func == (func(relation) = mh_term). 