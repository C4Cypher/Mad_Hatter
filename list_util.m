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


:- pred list_subset(list(T), list(T)).
:- mode list_subset(in, in) is semidet.
:- mode list_subset(out, in) is multi.

:- pred list_superset(list(T), list(T)).
:- mode list_superset(in, in) is semidet.
:- mode list_superset(in, out) is multi.

:- pred nondet_union(list(T), list(T), list(T)).
:- mode nondet_union(in, in, in) is semidet.
:- mode nondet_union(in, in, out) is det.
:- mode nondet_union(in, out, in) is nondet.
:- mode nondet_union(out, in, in) is nondet.
:- mode nondet_union(out, out, in) is multi.

:- pred unify_unordered(list(T), list(T)).
:- mode unify_unordered(in, in) is semidet.
:- mode unify_unordered(in, out) is multi.
:- mode unify_unordered(out, in) is multi.

:- promise all [A, B] ( unify_unordered(A, B) <=> unify_unordered(B, A) ).

:- func unify_unordered(list(T)) = list(T).
:- mode unify_unordered(in) = in is semidet.
:- mode unify_unordered(in) = out is multi.
:- mode unify_unordered(out) = in is multi.

:- promise all [A, B] ( unify_unordered(A) = B <=> unify_unordered(B) = A ).


:- pred multi_multi_remove_dups(list(T), list(T)).
:- mode multi_multi_remove_dups(in, out) is det.
:- mode multi_multi_remove_dups(out, in) is multi.




%-----------------------------------------------------------------------------%
:- implementation.

:- import_module multi_map.


%-----------------------------------------------------------------------------%

 
list_subset([], []).

list_subset([X | Xs], Y]) :- list_subset(Xs, Ys), .

list_subset(Xs, [_ | Ys]) :- list_subset(Xs, Ys).

list_superset(A, B) :- list_subset(B, A).
	

%-----------------------------------------------------------------------------%

nondet_union([], [], []).
nondet_union(A, [], A).
nondet_union([], B, B).

nondet_union(A, B, C) :-
	table_subset(A, C),
	table_subset(B, C),
	check_if_union(C, A, B).


:- pred check_if_union(list(T), list(T), list(T)).
:- mode check_if_union(in, in, in) is semidet.

check_if_union([], _, _).
check_if_union([X | Xs], A, B) :-
    ( if member(X, A) then true else member(X, B) ), 
    nondet_union_loop(Xs, A, B).
	
%-----------------------------------------------------------------------------%	

unify_unordered([], []).

unify_unordered([A | As], Bs) :-
	delete(Bs, A, Remainder),
	unify_unordered(As, Remainder).

unify_unordered(A) = B :- unify_unordered(A, B).

%-----------------------------------------------------------------------------%

multi_remove_dups(Xs) = FilteredXs :-
    multi_remove_dups(Xs, FilteredXs).

multi_remove_dups(Xs, FilteredXs) :-
    multi_remove_dups_loop(Xs, set_tree234.init, FilteredXs).

:- pred multi_remove_dups_loop(list(T), set_tree234(T), list(T)).
:- mode multi_remove_dups_loop(in, in, out) is det.
:- mode multi_remove_dups_loop(out, in, in) is multi.

multi_remove_dups_loop([], _SoFar, []).
multi_remove_dups_loop([X | Xs], SoFar0, FilteredXs) :-
    (   set_tree234.member(SoFar0, X),
        multi_remove_dups_loop(Xs, SoFar0, FilteredXs)
    ;
		not set_tree234.member(SoFar0, X),
        set_tree234.insert(X, SoFar0, SoFar),
        multi_remove_dups_loop(Xs, SoFar, FilteredXsTail),
        FilteredXs = [X | FilteredXsTail]
    ).

	



	




	
