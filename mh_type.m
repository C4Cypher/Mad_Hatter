%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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
:- import_module mh_mode.
:- import_module mh_signature.


%-----------------------------------------------------------------------------%
% Mad Hatter types

:- type mh_type
	--->	nil_type
	;		any
	;		named_type(symbol) % type alias
	;		bound_p_type(p_type, list(mh_type)) %bound parametric type
	;		type_disjunction(set(mh_type))
	;		type_conjunction(set(mh_type))
	;		mr_type(type_desc)
	;		tuple_type(list(mh_type))
	;		relation_signature(list(term_signature), term_signature) 
	;		predicate_signature(list(term_signature))
	;		function_type(tuple_type, mh_type)
	;		constructor_type(symbol, tuple_type).
	
%-----------------------------------------------------------------------------%
% Ground types

:- type mh_ground_type =< mh_type
	--->	mr_type(type_desc)
	;		constructor_type(symbol, tuple_type).

%TODO: Ground higher order types


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

% Parametric types and pseudo types
:- type p_type	
	--->	parametric_type(symbol, int)  %name, arity
	;		pseudo_type(pseudo_type_desc).

%-----------------------------------------------------------------------------%
% Function type

:- type function_type =< mh_type
	---> 	function_type(tuple_type, mh_type).

	




%-----------------------------------------------------------------------------%