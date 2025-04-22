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

:- import_module bool.
:- import_module univ.

:- import_module mh_symbol.

%-----------------------------------------------------------------------------%

:- type mh_value  % struct union  struct { bool foreign, union {}}?
	--->	mh_value(
				word::c_pointer, 
				size::uint, 	% width of the value in bytes
				literal::bool,	% word/1 is a pointer if literal is false 
				format::mh_value_format	
			)
			
	;		foreign_value(
				foreign_word::c_pointer,
				language::mh_symbol, 
				value_type::mh_symbol
			)
			
	;		mr_value(univ).
	
	/* bytecode constructors
	
	;		bytecode_chunk(start::c_pointer, chunk_size::uint)
	;		bytecode_term(term_pointer::c_pointer, parent_chunk::mh_chunk).
	
:- type mh_chunk =< mh_value
	--->	bytecode_chunk(start::c_pointer, chunk_size::uint).
	*/
			
% Todo:   See if I can organize a c union of structs that fits nicely together
% 			for these constructors

:- type mh_value_format
	--->	void	% void *
	;		signed_int
	;		unsigned_int
	;		float
	;		string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

