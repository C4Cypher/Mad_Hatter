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

	% nil, the abscence of term
	---> 	nil

	% variables
	;		var(var_id::var_id, var_type::mh_type)
	;		anonymous
	
	% atomic terms
	;		atom(symbol)
	
	% values
	;		some [T] mr_value(T)
	
	% compound terms
	;		some [T] compound(symbol, T) => relation(T)
	;		some [T] mr_struct(T) 
	;		some [T] mr_relation(T) => relation(T)
	
	% Higher order terms
	;		some [T] predicate(T) => predicate(T)
	;		some [T] functor(T) => functor(T)
	;		some [T] function(T) => function(T).

%-----------------------------------------------------------------------------%
%	Atoms

:- type atom =< mh_term
	--->	atom(symbol).

%-----------------------------------------------------------------------------%
%	Variables

:- inst mh_var 
	--->	var(ground, ground)
	;		anonymous.
	
:- type mh_var =< mh_term 
	---> 	var(var_id::var_id, var_type::mh_type)
	;		anonymous.
	
:- mode is_var == ground >> mh_var.

:- pred is_var(mh_term::is_var) is semidet.
	
%-----------------------------------------------------------------------------%
%	Values

:- type mercury_value =< mh_term
	---> 	some [T] mr_value(T).

%-----------------------------------------------------------------------------%
%	Compound terms

:- inst compound_term
	--->	compound(ground, ground)
	;		mr_struct(ground)
	;		mr_relation(ground).

:- type compound_term =< mh_term
	--->	some [T] compound(symbol, T) => relation(T)
	;		some [T] mr_struct(T) 
	;		some [T] mr_relation(T) => relation(T).
	
:- 	inst mh_compound ---> compound(ground, ground).
	
:- type mh_compound =< compound_term
	--->	some [T] compound(symbol, T) => relation(T).

%-----------------------------------------------------------------------------%
%  Structures - Mercury values indexed via deconstruction

:- type mercury_structure =< compound_term
	---> 	some [T] mr_struct(T).
	
%-----------------------------------------------------------------------------%
%	Relations

:- inst mercury_relation ---> mr_relation(ground).

:- type mercury_relation =< compound_term
	--->	some [T] mr_relation(T) => relation(T).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%
%	Variables

is_var(T) :- T = anonymous ; T = var(_, _).

%-----------------------------------------------------------------------------%
%	Atoms


%-----------------------------------------------------------------------------%
%	Values


%-----------------------------------------------------------------------------%
% Structures

