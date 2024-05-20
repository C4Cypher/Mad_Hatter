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


:- import_module mh_to_literal.
:- import_module mh_term.

:- import_module term.


:- typeclass primitive(T) <= (to_literal(T)) where [
	pred primitive_type_name(T::in, string::out) is det,
	pred primitive_type_desc(T::in, type_desc::out) is det
].


:- pred parse_primitive_term(term(T)::in, primitive::out) is multi.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module term_conversion.

%-----------------------------------------------------------------------------%


parse_primitive_term(Term, 'new primitive'(Symbol, Primitive) ) :-
	type_to_term
	primitive_type_name(Primitive, Name),
	Symbol = symbol(Name).

	
