%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_type.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_type.

:- interface.

:- import_module set.
:- import_module list.
:- import_module type_desc.

:- import_module mh_symbol.
:- import_module mh_signature.


%-----------------------------------------------------------------------------%
% Mad Hatter types

:- type mh_type
	--->	nil_type
	;		any
	;		named_type(symbol) % type alias
	;		p_type(p_type, list(mh_type)) %bound parametric type
	;		type_disjunction(set(mh_type))
	;		type_conjunction(set(mh_type))
	;		mr_type(type_desc)
	;		tuple_type(list(mh_type))
	;		algebraic_type(symbol, tuple_type)
	;		relation_signature(list(term_signature), term_signature) 
	;		predicate_signature(list(term_signature))
	;		function_type(tuple_type, mh_type).
	
% TODO logical subtypes
	
%-----------------------------------------------------------------------------%
%	Named types

:- type named_type =< mh_type
	--->	named_type(symbol).


%-----------------------------------------------------------------------------%
% Mercury types

:- type mr_type =< mh_ground_type
	---> 	mr_type(type_desc).

%-----------------------------------------------------------------------------%
% Tuple type

:- type tuple_type =< mh_type
	---> tuple_type(list(mh_type)).
	
%-----------------------------------------------------------------------------%
% Algebraic types

:- type algebraic_type =< mh_type
	--->	algebraic_type(symbol, tuple_type).
	

%-----------------------------------------------------------------------------%
% Relation type and signature

:- type relation_type
	--->	relation_type(list(mh_type), mh_type).
	

:- type relation_signature	=< mh_type
	--->	relation_signature(list(term_signature), term_signature).
	
%-----------------------------------------------------------------------------%
% Predicate type and signature

:- type predicate_type
	---> predicate_type(list(mh_type)).

:- type predicate_signature =< mh_type
	---> 	predicate_signature(list(term_signature)).
	

	
	
%-----------------------------------------------------------------------------%
% Ground types

:- type mh_ground_type =< mh_type
	--->	mr_type(type_desc)
	;		tuple_type(list(mh_ground_type))
	;		algebraic_type(symbol, ground_tuple_type). %TODO populate ground types

%TODO: Ground higher order types

%-----------------------------------------------------------------------------%
% Ground algebraic types

:- type ground_algebraic_type =< mh_ground_type
	--->	algebraic_type(symbol, ground_tuple_type).
	
%-----------------------------------------------------------------------------%
% Ground tuple types

:- type ground_tuple_type =< tuple_type
	--->	tuple_type(list(mh_ground_type)).

%-----------------------------------------------------------------------------%

% Parametric types and pseudo types
:- type p_type	
	--->	parametric_type(symbol, int)  %name, arity
	;		pseudo_type(pseudo_type_desc).

%-----------------------------------------------------------------------------%
% Function type

:- type function_type =< mh_type
	---> 	function_type(tuple_type, mh_type).

	




%-----------------------------------------------------------------------------%