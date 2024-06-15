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
:- import_module mh_relation.
:- import_module mh_procedure.
:- import_module mh_type.



:- type var_id == id(mh_var).

:- type mh_term 

	% variables
	--->	var(var_id, mh_type)
	;		anonymous_var
	
	% atomic terms
	;		atom(symbol)
	
	% value
	;		some [T] mr_value(T)
	
	% compound terms
	;		some [T] compound(symbol, T) => relation(T)
	;		some [T] mr_structure(T) => relation(T)
	
	% Higher order terms
	;		some [T] predicate(T) => predicate(T)
	;		some [T] functor(T) => functor(T)
	;		some [T] function(T) => function(T).

:- type mh_clause ---> unimplemented.



%-----------------------------------------------------------------------------%
% Variables

:- inst mh_var
	---> 	var(ground, ground)
	;		anonymous_var.


:- type mh_var =< mh_term 
	---> 	var(var_id, mh_type)
	;		anonymous_var.
	
:- pred is_var(mh_term::in) is semidet.
	
:- func term_var_id(mh_term) = var_id is semidet.
:- func term_var_type(mh_term) = mh_type is semidet.

:- func var_id(mh_var) = var_id is semidet.
:- func var_type(mh_var) = mh_type is det.
	
%-----------------------------------------------------------------------------%


%-----------------------------------------------------------------------------%



%-----------------------------------------------------------------------------%


%-----------------------------------------------------------------------------%


	

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

is_var(var(_, _)).
is_var(anonymous_var).

term_var_id(var(ID, _)) = ID.

term_var_type(var(_, Type)) = Type.
term_var_type(anonymous_var) = any. 

var_id(var(ID, _)) = ID.

var_type(var(_, Type)) = Type.
var_type(anonymous_var) = any.

