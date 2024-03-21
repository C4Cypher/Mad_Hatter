%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: set_util.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module set_util.

:- interface.

:- import_module set.

:- pred nondet_insert(T, set(T), set(T)).
:- mode nondet_insert(in, in, in) is semidet.
:- mode nondet_insert(in, in, out) is det.
:- mode nondet_insert(in, out, in) is semidet.
:- mode nondet_insert(out, in, in) is nondet.

:- pred nondet_remove(T, set(T), set(T)).
:- mode nondet_remove(in, in, in) is semidet.
:- mode nondet_remove(in, in, out) is semidet.
:- mode nondet_remove(in, out, in) is det.
:- mode nondet_remove(out, in, in) is nondet.

:- pred multi_superset(set(T), set(T)).
:- mode multi_superset(in, in) is semidet.
:- mode multi_superset(in, out) is multi.

:- pred multi_subset(set(T), set(T)).
:- mode multi_subset(in, in) is semidet.
:- mode multi_subset(out, in) is multi.


:- pred nondet_union(set(T), set(T), set(T)).
:- mode nondet_union(in, in, in) is semidet.
:- mode nondet_union(in, in, out) is det.
:- mode nondet_union(out, out, in) is nondet.
:- mode nondet_union(in, out, in) is nondet.
:- mode nondet_union(out, in, out) is nondet.

%-----------------------------------------------------------------------------%
:- implementation.

%-----------------------------------------------------------------------------%
:- promise_equivalent_clauses(nondet_insert/3).

nondet_insert(A::in, B::in, C::out) :- insert(A, B, C).

nondet_insert(A::in, B::out, C::in) :- remove(A, C, B).

nondet_insert(A::out, B::in, C::in) :-	member(A, C), remove(A, C, B).

nondet_remove(A, B, C) :- nondet_insert(A, C, B).

%-----------------------------------------------------------------------------%

:- promise_equivalent_clauses(multi_superset/2)

multi_superset(A::in, B::in) :- superset(A, B).

multi_superset(A::in, B::out) :-
	member(X, A), det_remove(X, A, Y), 
	(B = Y ; multi_superset(Y, B) ).

multi_superset(init::in, init::out).

multi_subset(A, B) :- multi_superset(B, A).


%-----------------------------------------------------------------------------%

:- promise_equivalent_clauses(nondet_union/3).

nondet_union(A::in, B::in, C::out) :- union(A, B, C).
	
