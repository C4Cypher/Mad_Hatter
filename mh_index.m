%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_index.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_index.

:- interface.

:- import_module int.
:- import_module list.

:- import_module array.

%-----------------------------------------------------------------------------%

:- typeclass index(T, U) <= (T -> U) where [

	pred valid_index(T, int),
	mode valid_index(in, in) is semidet, % suceed on valid index
	mode valid_index(in, out) is nondet, % return all valid indexes

	pred index(T, int, U),
	mode index(in, in, in) is semidet, % implicit fail on inequality
	mode index(in, in, out) is semidet, % fail on invalid index
	mode index(in, out, out) is nondet, % all indexed values
	
	pred set_index(int, U, T, T),
	mode set_index(in, in, in, out) is semidet, % fail on invalid index
	mode set_index(out, in, in, out) is nondet % update any index nondet
].

% TODO: Subclass index with unique set modes 

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Helper predicates for declaring instances of index/2

:- pred valid_list_index(list(T), int).
:- mode valid_list_index(in, in) is semidet.
:- mode valid_list_index(in, out) is nondet. 

:- pred list_index(list(T), int, T).
:- mode list_index(in, in, in) is semidet.
:- mode list_index(in, in, out) is semidet. 
:- mode list_index(in, out, out) is nondet. 
	
:- pred set_list_index(int, T, list(T), list(T)).
:- mode set_list_index(in, in, in, out) is semidet. 
:- mode set_list_index(out, in, in, out) is nondet.

%-----------------------------------------------------------------------------%

:- pred valid_array_index(array(T), int).
:- mode valid_array_index(in, in) is semidet.
:- mode valid_array_index(in, out) is nondet. 

:- pred array_index(array(T), int, T).
:- mode array_index(in, in, in) is semidet.
:- mode array_index(in, in, out) is semidet. 
:- mode array_index(in, out, out) is nondet. 
	
:- pred set_array_index(int, T, array(T), array(T)).
:- mode set_array_index(in, in, in, out) is semidet. 
:- mode set_array_index(out, in, in, out) is nondet.
	

:- implementation.

%-----------------------------------------------------------------------------%
	
valid_list_index([_], 0).

valid_list_index([_ | Vs], I) :-
	I > 0,
	valid_list_index(Vs, I - 1).
	


:- pragma promise_equivalent_clauses(list_index/3).

list_index(L::in, I::in, V::in) :- index0(L, I, V).

list_index(L::in, I::in, V::out) :- index0(L, I, V). 
		
list_index([V | Vs]::in, I::out, U::out) :-
	I = 0, V = U;
	I > 0, list_index(Vs @ [_ | _], I - 1, U). 


set_list_index(I, V, !L) :-
	valid_list_index(!.L, I),
	det_replace_nth(!.L, I + 1, V, !:L).

%-----------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(valid_array_index/2).

valid_array_index(A::in, I::in) :- in_bounds(A, I).

valid_array_index(A::in, I::out) :- all_ints_from_to(min(A), max(A), I).

:- pred all_ints_from_to(int::in, int::in, int::out) is nondet.

all_ints_from_to(From, To, Out) :-
	From =< To,
	( Out = From ; all_ints_from_to(From + 1, To, Out) ).

array_index(A, I, V) :-
	valid_array_index(A, I),
	unsafe_lookup(A, I, V).
	
set_array_index(I, V, !A) :-
	valid_array_index(!.A, I),
	slow_set(I, V, !A).

%-----------------------------------------------------------------------------%