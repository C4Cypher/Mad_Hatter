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


%-----------------------------------------------------------------------------%


:- type mh_type
	--->	nil_type
	;		any
	;		named_type(symbol) % type alias
	;		bound_p_type(p_type, list(mh_type)) %bound parametric type
	;		type_disjunction(set(mh_type))
	;		type_conjunction(set(mh_type))
	;		mr_type(type_desc)
	;		mr_tuple(type_desc, tuple_type)
	;		relation(relation_signature) 
	;		predicate(tuple_signature)
	;		function(function_signature)
	;		mh_term(symbol, tuple_type).
	
%-----------------------------------------------------------------------------%

:- type mh_ground_type =< mh_type
	--->	mr_type(type_desc)
	;		mh_term(symbol, data_type_signature).
	
:- type data_type_signature == list(mh_type).	

%TODO: Ground higher order types



%-----------------------------------------------------------------------------%
:- type tuple_type == list(mh_type).


	
%-----------------------------------------------------------------------------%

% Parametric types and pseudo types
:- type p_type	
	--->	parametric_type(symbol, int)  %name, arity
	;		pseudo_type(pseudo_type_desc).

%-----------------------------------------------------------------------------%


	




%-----------------------------------------------------------------------------%