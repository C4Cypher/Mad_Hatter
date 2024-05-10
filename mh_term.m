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
:- import_module mh_primitive.
:- import_module mh_type.
:- import_module mh_relation.
:- import_module mh_rule.


:- type var_id == id(mh_var).


:- type mh_term 
	--->	var(var_id)
	;		named_var(symbol)
	;		some [T] primitive(symbol, T) => primitive(T)
	;		symbol(symbol)
	;		type(mh_type)
	;		mh_true
	;		mh_false
	;		expression(functor, proc_relation)
	;		lambda(proc_rule).

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

:- inst primitive ---> primitive(ground, ground).

:- inst primitive(I) ---> primitive(ground, I).

:- type primtive =< mh_term
	---> 	some [T] primitive(symbol, T) => primitive(T).


%-----------------------------------------------------------------------------%	

:- type functor =< mh_term
	--->	symbol(symbol)
	;		lambda(proc_rule).

%-----------------------------------------------------------------------------%

:- inst expression ---> expression(ground, ground).

:- inst expression(I) ---> expression(ground, I).

:- type expression =< mh_term ---> expression(functor, proc_relation).


%-----------------------------------------------------------------------------%


:- type mh_ground =< mh_term 
	--->	some [T] primitive(symbol, T) => primitive(T)
	;		symbol(symbol)
	;		type(mh_type)
	;		mh_true
	;		mh_false
	;		expression(functor, ground_proc_relation)
	;		lambda(proc_rule).
	

%-----------------------------------------------------------------------------%


