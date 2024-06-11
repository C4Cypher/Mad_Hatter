%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_parse.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_parse.

:- interface.

:- use_module term.

:- import_module mh_term.

:- type mr_term(T) == term.term(T).

:- typeclass parse(T) where [
	some [U] pred parse_mr_term(mr_term(U)::in, T::out) is det 
].