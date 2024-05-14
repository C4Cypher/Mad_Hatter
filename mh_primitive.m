%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_primitive.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_primitive.

:- interface.


:- import_module mh_parse_term.
:- import_module mh_to_literal.
:- import_module mh_term.

:- import_module term.


:- typeclass primitive(T) <= (parse_term(T), to_literal(T)) where [
	pred primitive_type_name(T, string),
	mode primitive_type_name(in, out) is det
].

:- pred parse_primitive(term(_)::in, primitive::out) is det.

:- func parse_primitive(term(_)) = primitive.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mh_symbol.

%-----------------------------------------------------------------------------%

parse_primitive(Term, primitive(Symbol, Primitive) ) :-
	parse_term(Term, Primitive),
	primitive_type_name(Primitive, Name),
	Symbol = symbol(Name).

parse_primitive(Term) = PrimitiveTerm :- parse_primitive(Term, PrimitiveTerm).
	
