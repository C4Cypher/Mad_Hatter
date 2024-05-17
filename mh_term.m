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

:- import_module list.

:- import_module mh_symbol.
:- import_module mh_identifier.
:- import_module mh_primitive.
:- import_module mh_type.
:- import_module mh_relation.
:- import_module mh_clause.


:- type var_id == id(mh_var).


:- type mh_term 
	--->	var(var_id)
	;		named_var(var_id, symbol)
	;		some [T] primitive(symbol, T) => primitive(T)
	;		symbol(symbol)
	;		parametric_type(mh_type, list(mh_type_term))
	;		type(mh_type)
	;		mh_true
	;		mh_false
	;		expression(functor, proc_relation)
	;		lambda(proc_clause)
	;		constrained(mh_term, constraint).

%-----------------------------------------------------------------------------%

:- inst mh_var
	---> 	var(ground)
	;		named_var(ground, ground).


:- type mh_var =< mh_term 
	---> 	var(var_id)
	;		named_var(var_id, symbol).
	
:- func var_id(mh_term) = var_id is semidet.
:- func var_name(mh_term) = symbol is semidet.
	
%-----------------------------------------------------------------------------%

:- inst primitive ---> primitive(ground, ground).

:- inst primitive(I) ---> primitive(ground, I).

:- type primitive =< mh_term
	---> 	some [T] primitive(symbol, T) => primitive(T).


%-----------------------------------------------------------------------------%	

:- type functor =< mh_term
	--->	symbol(symbol)
	;		lambda(proc_clause).

%-----------------------------------------------------------------------------%

:- inst expression_term ---> expression(ground, ground).

:- type expression_term =< mh_term ---> expression(functor, proc_relation).


%-----------------------------------------------------------------------------%

:- inst mh_ground
	--->	primitive(ground, ground)
	;		symbol(ground)
	;		parametric_type(ground, list(mh_ground_type_term))
	;		type(ground)
	;		mh_true
	;		mh_false
	;		expression(ground, ground)
	;		lambda(ground).

:- type mh_ground =< mh_term 
	--->	some [T] primitive(symbol, T) => primitive(T)
	;		symbol(symbol)
	;		parametric_type(mh_type, list(mh_ground_type_term))
	;		type(mh_type)
	;		mh_true
	;		mh_false
	;		expression(functor, ground_proc_relation)
	;		lambda(proc_clause).


%-----------------------------------------------------------------------------%

:- inst mh_type_term
	---> 	var(ground)
	;		named_var(ground, ground)
	;		parametric_type(ground, ground)
	;		type(ground).
	

:- type mh_type_term =< mh_term
	---> 	var(var_id)
	;		named_var(var_id, symbol)
	;		parametric_type(mh_type, list(mh_type_term))
	;		type(mh_type). 
	
:- inst mh_ground_type_term
	--->	parametric_type(ground, list(mh_ground_type_term))
	;		type(ground).
	
:- type mh_ground_type_term =< mh_type_term
	--->	parametric_type(mh_type, list(mh_ground_type_term))
	;		type(mh_type).
	
%-----------------------------------------------------------------------------%

:- type constraint
	--->	type_constraint(mh_type).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

var_id(var(ID)) = ID.
var_id(named_var(ID, _)) = ID.

var_name(named_var(_, Name)) = Name.