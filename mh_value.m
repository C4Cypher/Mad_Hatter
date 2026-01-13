%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_value.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_value.

:- interface.

:- import_module univ.

%-----------------------------------------------------------------------------%
% Values

:- type mh_value  % struct union  struct { bool foreign, union {}}?

	% Mercury values
	--->	mr_value(univ).
	
	
	/*  Uneeded for getting the initial implementation running

	% Literal values
	;		mh_value(
				word::c_pointer, 
				size::uint, 	% width of the value in bytes
				literal::bool,	% word/1 is a pointer if literal is false 
				format::mh_value_format	
			)
	
	% Foreign values
	;		foreign_value(
				foreign_word::c_pointer,
				language::mh_symbol, 
				value_type::mh_symbol
			).
	
	% bytecode constructors
	
	;		bytecode_chunk(start::c_pointer, chunk_size::uint)
	;		bytecode_term(term_pointer::c_pointer, parent_chunk::mh_chunk).
	
:- type mh_chunk =< mh_value
	--->	bytecode_chunk(start::c_pointer, chunk_size::uint).
			
% Todo:   See if I can organize a c union of structs that fits nicely together
% 			for these constructors

:- type mh_value_format
	--->	void	% void *
	;		signed_int
	;		unsigned_int
	;		float
	;		string.
	
	%Uneeded for getting the initial implementation running */
/*	
:- pred unify_values(mh_value, mh_value).
:- mode unify_values(in, in) is semidet.
:- mode unify_values(in, out) is nondet.
:- mode unify_values(out, in) is nondet.
*/

%TODO: Rules on unification of different value 'kinds'


%-----------------------------------------------------------------------------%
% Mercury values

/* Redundant if mr_value is the only constructor.
:- inst mercury_value ---> mr_value(ground).

:- type mercury_value =< mh_value
	--->	mr_value(univ).
	
:- mode is_mercury_value == ground >> mercury_value.

:- pred is_mercury_value(mh_value::is_mercury_value).
*/

:- func from_mr_value(T) = mh_value is det.

:- func to_mr_value(mh_value) = T is semidet.

:- func value_type_name(mh_value) = string.

:- some [T] func to_some_mr_value(mh_value) = T is det.

:- func to_mh_value(T) = mh_value is det.

%-----------------------------------------------------------------------------%
% Literal Values

%TODO

%-----------------------------------------------------------------------------%
% Foreign values

% TODO



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module type_desc.

%-----------------------------------------------------------------------------%
% Values
%-----------------------------------------------------------------------------%
% Mercury values

%is_mercury_value(mr_value(_)).

from_mr_value(T) = mr_value(univ(T)).

to_mr_value(mr_value(univ(T))) = T.

value_type_name(mr_value(Univ)) = type_name(univ_type(Univ)).

to_some_mr_value(mr_value(Univ)) = univ_value(Univ).

to_mh_value(T) = mr_value(univ(T)).

%-----------------------------------------------------------------------------%
% Literal Values

%TODO

