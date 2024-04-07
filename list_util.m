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

:- pred unify_unordered(list(T), list(T)).
:- mode unify_unordered(in, in) is semidet.
:- mode unify_unordered(in, out) is multi.
:- mode unify_unordered(out, in) is multi.

:- promise all [A, B] ( unify_unordered(A, B) <=> unify_unordered(B, A) ).


%-----------------------------------------------------------------------------%
:- implementation.

:- import_module set_tree234.


%-----------------------------------------------------------------------------%

 
list_subset([], []).

list_subset([X | Xs], [X | Ys]) :- list_subset(Xs, Ys).

list_subset(Xs, [_ | Ys]) :- list_subset(Xs, Ys).

list_superset(A, B) :- list_subset(B, A).
	


	
%-----------------------------------------------------------------------------%	

unify_unordered([], []).

unify_unordered([A | As], Bs) :-
	delete(Bs, A, Remainder),
	unify_unordered(As, Remainder).
	
unify_unordered(A, B) :- unify_unordered(B, A).
	


%-----------------------------------------------------------------------------%






	




	
