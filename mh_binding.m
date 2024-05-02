%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_binding.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_binding.

:- interface.

:- import_module map.

:- import_module mh_term.

:- typeclass binding(T) where [
	pred bound(T, mh_var, mh_term),
	mode bound(in, in, out) is semidet,
	mode bound(in, out, out) is nondet
].

:- type binding ---> binding(map(mh_var, mh_term)).

:- instance binding(binding).

:- implementation.

:- instance binding(binding) where [
	pred(bound/3) is map_bound
].

:- pred map_bound(binding, mh_var, mh_term).
:- mode map_bound(in, in, out) is semidet.
:- mode map_bound(in, out, out) is nondet.

:- pragma promise_equivalent_clauses(map_bound/3).

map_bound(binding(Map)::in, Var::in, Term::out) :-
	map.search(Map, Var, Term).
	
map_bound(binding(Map)::in, Var::out, Term::out) :-
	map.member(Map, Var, Term).