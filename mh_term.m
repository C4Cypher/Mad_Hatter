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
:- import_module univ.

:- import_module mh_symbol.
:- import_module mh_identifier.
:- import_module mh_primitive.
:- import_module mh_type.
:- import_module mh_relation.
:- import_module mh_clause.


:- type var_id == id(mh_var).


:- type mh_term 
	--->	var(var_id, mh_type)
	;		named_var(symbol, mh_type)
	;		some [T] primitive(symbol, T) => primitive(T)
	;		atom(symbol)
	;		expression(functor, relation)
	;		lambda(mh_clause).

%-----------------------------------------------------------------------------%

:- inst mh_var
	---> 	var(ground, ground)
	;		named_var(ground, ground).


:- type mh_var =< mh_term 
	---> 	var(var_id, mh_type)
	;		named_var(symbol, mh_type).
	
:- pred is_var(mh_term::in) is semidet.
	
:- func term_var_id(mh_term) = var_id is semidet.
:- func term_var_name(mh_term) = symbol is semidet.
:- func term_var_type(mh_term) = mh_type is semidet.

:- func var_id(mh_var) = var_id is semidet.
:- func var_name(mh_var) = symbol is semidet.
:- func var_type(mh_var) = mh_type is det.
	
%-----------------------------------------------------------------------------%

:- inst primitive ---> primitive(ground, ground).

:- inst primitive(I) ---> primitive(ground, I).

:- type primitive =< mh_term
	---> 	some [T] primitive(symbol, T) => primitive(T).


%-----------------------------------------------------------------------------%	

:- type functor =< mh_term
	--->	atom(symbol)
	;		lambda(mh_clause).

%-----------------------------------------------------------------------------%

:- inst expression_term ---> expression(ground, ground).

:- type expression_term =< mh_term ---> expression(functor, relation).


%-----------------------------------------------------------------------------%

:- inst mh_ground
	--->	primitive(ground, ground)
	;		atom(ground)
	;		expression(ground, ground)
	;		lambda(ground).

:- type mh_ground =< mh_term 
	--->	some [T] primitive(symbol, T) => primitive(T)
	;		atom(symbol)
	;		expression(functor, relation)
	;		lambda(mh_clause).


%-----------------------------------------------------------------------------%


	

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

is_var(var(_, _)).
is_var(named_var(_, _)).

term_var_id(var(ID, _)) = ID.

term_var_name(named_var(Name, _)) = Name.

term_var_type(var(_, Type)) = Type.
term_var_type(named_var(_, Type)) = Type. 

var_id(var(ID, _)) = ID.

var_name(named_var(Name, _)) = Name.

var_type(var(_, Type)) = Type.
var_type(named_var(_, Type)) = Type. 

