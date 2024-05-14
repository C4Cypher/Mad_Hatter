%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_parse_term.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_parse_term.

:- interface.

:- import_module term.

:- typeclass parse_term(T) where [
	pred parse_term(term(_), T),
	mode parse_term(in, out) is semidet
].

:- func parse_term(term(_)) = T <= parse_term(T).
:- mode parse_term(in) = out is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.


parse_term(MercTerm) = MHTerm :- parse_term(MercTerm, MHTerm). 