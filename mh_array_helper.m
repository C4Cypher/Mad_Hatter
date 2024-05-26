%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_array_helper.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_array_helper.

:- interface.

:- import_module array.

%-----------------------------------------------------------------------------%


:- pred valid_array_index0(array(T), int).
:- mode valid_array_index0(in, in) is semidet.
:- mode valid_array_index0(in, out) is nondet. 

:- pred array_index0(array(T), int, T).
:- mode array_index0(in, in, in) is semidet.
:- mode array_index0(in, in, out) is semidet. 
:- mode array_index0(in, out, out) is nondet. 
	
:- pred set_array_index0(int, T, array(T), array(T)).
:- mode set_array_index0(in, in, in, out) is semidet. 
:- mode set_array_index0(out, in, in, out) is nondet.




:- pred uniq_set_array_index0(int::in, T::in, 
	array(T)::array_di, array(T)::array_uo) is semidet.

%-----------------------------------------------------------------------------%

:- pred valid_array_index1(array(T), int).
:- mode valid_array_index1(in, in) is semidet.
:- mode valid_array_index1(in, out) is nondet. 

:- pred array_index1(array(T), int, T).
:- mode array_index1(in, in, in) is semidet.
:- mode array_index1(in, in, out) is semidet. 
:- mode array_index1(in, out, out) is nondet. 
	
:- pred set_array_index1(int, T, array(T), array(T)).
:- mode set_array_index1(in, in, in, out) is semidet. 
:- mode set_array_index1(out, in, in, out) is nondet.


:- pred uniq_set_array_index1(int::in, T::in, 
	array(T)::array_di, array(T)::array_uo) is semidet.
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.


:- pragma promise_equivalent_clauses(valid_array_index0/2).

valid_array_index0(A::in, I::in) :- in_bounds(A, I).

valid_array_index0(A::in, I::out) :- all_ints_from_to(min(A), max(A), I).

array_index0(A, I, V) :-
	valid_array_index0(A, I),
	unsafe_lookup(A, I, V).
	
set_array_index0(I, V, !A) :-
	valid_array_index0(!.A, I),
	slow_set(I, V, !A).
	
uniq_set_array_index0(I, V, !A) :-
	valid_array_index0(!.A, I),
	set(I, V, !A).
	



%-----------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(valid_array_index1/2).

valid_array_index1(A::in, I::in) :- in_bounds(A, I - 1).

valid_array_index1(A::in, I::out) :- all_ints_from_to(min(A), max(A), I - 1).

array_index1(A, I, V) :-
	valid_array_index1(A, I),
	unsafe_lookup(A, I - 1, V).
	
set_array_index1(I, V, !A) :-
	valid_array_index1(!.A, I),
	slow_set(I - 1, V, !A).
	
uniq_set_array_index1(I, V, !A) :-
	valid_array_index1(!.A, I),
	set(I, V, !A).
	
	
%-----------------------------------------------------------------------------%

:- pred all_ints_from_to(int::in, int::in, int::out) is nondet.

all_ints_from_to(From, To, Out) :-
	From =< To,
	( Out = From ; all_ints_from_to(From + 1, To, Out) ).