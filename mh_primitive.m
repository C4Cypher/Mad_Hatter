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

:- import_module mh_symbol.

:- import_module term.


:- typeclass primitive(T) where [
	pred parse_primitive(term, symbol, T),
	mode parse_primitive(in, out, out) is semidet,
	func primtive_to_literal(T) = string
].


