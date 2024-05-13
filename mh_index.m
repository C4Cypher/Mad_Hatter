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
	mode valid_index(in, in) is semidet, % succeed on valid index
	mode valid_index(in, out) is nondet, % return all valid indexes

	pred index(T, int, U),
	mode index(in, in, in) is semidet, % implicit fail on inequality
	mode index(in, in, out) is semidet, % fail on invalid index
	mode index(in, out, out) is nondet, % all indexed values
	
	pred set_index(int, U, T, T),
	mode set_index(in, in, in, out) is semidet, % fail on invalid index
	mode set_index(out, in, in, out) is nondet, % update any index nondet
	
	func max_index(T) = int is semidet, % fail if there are no valid indexes
	func min_index(T) = int is semidet
].


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Helper predicates for declaring instances of index/2

:- pred valid_list_index0(list(T), int).
:- mode valid_list_index0(in, in) is semidet.
:- mode valid_list_index0(in, out) is nondet. 

:- pred list_index0(list(T), int, T).
:- mode list_index0(in, in, in) is semidet.
:- mode list_index0(in, in, out) is semidet. 
:- mode list_index0(in, out, out) is nondet. 
	
:- pred set_list_index0(int, T, list(T), list(T)).
:- mode set_list_index0(in, in, in, out) is semidet. 
:- mode set_list_index0(out, in, in, out) is nondet.

:- func max_list_index0(list(T)) = int is semidet.
:- func min_list_index0(list(T)) = int is semidet.

%-----------------------------------------------------------------------------%

:- pred valid_list_index1(list(T), int).
:- mode valid_list_index1(in, in) is semidet.
:- mode valid_list_index1(in, out) is nondet. 

:- pred list_index1(list(T), int, T).
:- mode list_index1(in, in, in) is semidet.
:- mode list_index1(in, in, out) is semidet. 
:- mode list_index1(in, out, out) is nondet. 
	
:- pred set_list_index1(int, T, list(T), list(T)).
:- mode set_list_index1(in, in, in, out) is semidet. 
:- mode set_list_index1(out, in, in, out) is nondet.

:- func max_list_index1(list(T)) = int is semidet.
:- func min_list_index1(list(T)) = int is semidet.

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

:- func max_array_index0(array(T)) = int is semidet.
:- func min_array_index0(array(T)) = int is semidet.

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

:- func max_array_index1(array(T)) = int is semidet.
:- func min_array_index1(array(T)) = int is semidet.

:- pred uniq_set_array_index1(int::in, T::in, 
	array(T)::array_di, array(T)::array_uo) is semidet.
	

:- implementation.

%-----------------------------------------------------------------------------%
	
valid_list_index0([_], 0).

valid_list_index0([_ | Vs], I) :-
	I > 0,
	valid_list_index0(Vs, I - 1).
	


:- pragma promise_equivalent_clauses(list_index0/3).

list_index0(L::in, I::in, V::in) :- index0(L, I, V).

list_index0(L::in, I::in, V::out) :- index0(L, I, V). 
		
list_index0([V | Vs]::in, I::out, U::out) :-
	I = 0, V = U;
	I > 0, list_index0(Vs @ [_ | _], I - 1, U). 


set_list_index0(I, V, !L) :- 
	valid_list_index0(!.L, I),
	det_replace_nth(!.L, I + 1, V, !:L).
	
max_list_index0(List @ [_,_]) = length(List) - 1.

min_list_index0([_|_]) = 0.
	
%-----------------------------------------------------------------------------%
	
valid_list_index1([_], 1).

valid_list_index1([_ | Vs], I) :-
	I > 1,
	valid_list_index1(Vs, I - 1).
	


:- pragma promise_equivalent_clauses(list_index1/3).

list_index1(L::in, I::in, V::in) :- index1(L, I, V).

list_index1(L::in, I::in, V::out) :- index1(L, I, V). 
		
list_index1([V | Vs]::in, I::out, U::out) :-
	I = 1, V = U;
	I > 1, list_index1(Vs @ [_ | _], I - 1, U). 




set_list_index1(I, V, !L) :- 
	valid_list_index0(!.L, I),
	det_replace_nth(!.L, I, V, !:L).
	
max_list_index1(List @ [_,_]) = length(List).

min_list_index1([_|_]) = 1.

%-----------------------------------------------------------------------------%

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
	
max_array_index0(A) = max(A) :- size(A) > 0.

min_array_index0(A) = min(A) :- size(A) > 0.



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
	
max_array_index1(A) = max(A) + 1 :- size(A) > 0.

min_array_index1(A) = min(A) + 1 :- size(A) > 0.
	
%-----------------------------------------------------------------------------%

:- pred all_ints_from_to(int::in, int::in, int::out) is nondet.

all_ints_from_to(From, To, Out) :-
	From =< To,
	( Out = From ; all_ints_from_to(From + 1, To, Out) ).