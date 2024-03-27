%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: list_util.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module list_util.

:- interface.

:- import_module list.

:- pred nondet_insert(T, list(T), list(T)).
:- mode nondet_insert(in, in, in) is semidet.
:- mode nondet_insert(in, in, out) is det.
:- mode nondet_insert(in, out, in) is semidet.
:- mode nondet_insert(out, in, in) is nondet.

:- pred nondet_remove(T, list(T), list(T)).
:- mode nondet_remove(in, in, in) is semidet.
:- mode nondet_remove(in, in, out) is semidet.
:- mode nondet_remove(in, out, in) is det.
:- mode nondet_remove(out, in, in) is nondet.

:- pred multi_subset(list(T), list(T)).
:- mode multi_subset(in, in) is semidet.
:- mode multi_subset(out, in) is multi.

:- pred multi_superset(list(T), list(T)).
:- mode multi_superset(in, in) is semidet.
:- mode multi_superset(in, out) is multi.

:- pred list_subset(list(T), list(T)).
:- mode list_subset(in, in) is semidet.
:- mode list_subset(out, in) is multi.

:- pred nondet_union(list(T), list(T), list(T)).
:- mode nondet_union(in, in, in) is semidet.
:- mode nondet_union(in, in, out) is det.
:- mode nondet_union(in, out, in) is nondet.
:- mode nondet_union(out, in, in) is nondet.
:- mode nondet_union(out, out, in) is multi.




%-----------------------------------------------------------------------------%
:- implementation.

:- import_module multi_map.

%-----------------------------------------------------------------------------%
:- pragma promise_equivalent_clauses(nondet_insert/3).

nondet_insert(A::in, B::in, C::in) :- insert(A, B, C).

nondet_insert(A::in, B::in, C::out) :- insert(A, B, C).

nondet_insert(A::in, B::out, C::in) :- remove(A, C, B).

nondet_insert(A::out, B::in, C::in) :-	member(A, C), remove(A, C, B).

nondet_remove(A, B, C) :- nondet_insert(A, C, B).

%-----------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(multi_subset/2).

multi_subset(A::in, B::in) :- subset(A, B).

multi_subset(list_to_list(A)::out, B::in) :- list_subset(A, to_sorted_list(B)).

multi_superset(A, B) :- multi_subset(B, A).

%-----------------------------------------------------------------------------%
 
list_subset([], []).

list_subset([X | Ys], [X | Xs]) :- list_subset(Ys, Xs).

list_subset(Ys, [_ | Xs]) :- list_subset(Ys, Xs).
	

%-----------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(nondet_union/3).

nondet_union(A::in, B::in, C::in) :- union(A, B, C).
nondet_union(A::in, B::in, C::out) :- union(A, B, C).

nondet_union(init::out, init::out, init::in).
nondet_union(A::out, init::out, A::in).
nondet_union(init::out, B::out, B::in).

nondet_union(As::out, Bs::out, Cs::in) :-
	C = to_sorted_list(Cs),
	table_subset(A, C),
	table_subset(B, C),
	merge_and_remove_dups(A, B, C),
	As = list_to_list(A),
	Bs = list_to_list(B).
	
nondet_union(As::out, Bs::in, Cs::in) :-
	subset(Bs, Cs),
	C = to_sorted_list(Cs),
	table_subset(A, C),
	merge_and_remove_dups(A, to_sorted_list(Bs), C),
	As = list_to_list(A).
	
nondet_union(As::in, Bs::out, Cs::in) :- nondet_union(Bs, As, Cs).


	
:- pred table_subset(list(T)::out, list(T)::in) is nondet.

%:- pragma minimal_model(table_subset/2).

table_subset(Sub, Super) :- list_subset(Sub, Super).



	




	
