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

:- import_module list.
:- import_module array.

:- import_module mh_arity.

%-----------------------------------------------------------------------------%

% Types implementing index(T, U) will store values of type U in structures of
% type T, indexed from 1 to arity(T), index/3 and set_index/4 are expected to
% throw an exception if index is out of range.

:- typeclass index(T, U) <= ((T -> U), arity(T)) where [

	pred index(T, int, U),	% index(Container, Index, Value)
	mode index(in, in, out) is det, % exception on invalid index
	mode index(in, out, out) is nondet, % all indexed values
	
	pred set_index(int, U, T, T), % set_index(Index, Value, !Container)
	mode set_index(in, in, in, out) is det, % exception on invalid index
	mode set_index(out, in, in, out) is nondet, % update any index nondet
	
	% Iterate a closure over all values in a container with an accumulator
	% from index 1 to arity(T), abort and return the accumulator early if 
	% closure fails.
	pred fold_index(pred(U, A, A), T, A, A), % fold_index(Closure, Cont, !Acc)
	mode fold_index(pred(in, in, out) is det, in, in, out) is det,
	
	pred map_index(pred(U, U), T, T), % map_index(Closure, !Container)
	mode map_index(pred(in, out) is det, in, out) is det

].

%-----------------------------------------------------------------------------%
% Valid indexes

:- pred valid_index(T, int) <= arity(T). % valid_index(Container, Index)
:- mode valid_index(in, in) is semidet. % succeed on valid index
:- mode valid_index(in, out) is nondet. % return all valid indexes, 

%-----------------------------------------------------------------------------%
% Index Operation determinism casts

:- pred semidet_valid_index(T::in, int::in) is semidet <= index(T, _).
:- pred nondet_valid_index(T::in, int::out) is nondet <= index(T, _).
:- pred cc_nondet_valid_index(T::in, int::out) is cc_nondet <= index(T, _).

:- pred det_index(T::in, int::in, U::out) is det <= index(T, U).
:- pred semidet_index(T::in, int::in, U::out) is semidet <= index(T, U).
:- pred nondet_index(T::in, int::out, U::out) is nondet <= index(T, U).
:- pred cc_nondet_index(T::in, int::out, U::out) is cc_nondet <= index(T, U).

:- pred det_set_index(int::in, U::in, T::in, T::out) is det <= index(T, U).
:- pred semidet_set_index(int::in, U::in, T::in, T::out) is semidet 
	<= index(T, U).
:- pred nondet_set_index(int::out, U::in, T::in, T::out) is nondet 
	<= index(T, U).
:- pred cc_nondet_set_index(int::out, U::in, T::in, T::out) is cc_nondet 
	<= index(T, U).

%-----------------------------------------------------------------------------%
% Function versions of index/2 methods

:- func index(int, T) = U <= index(T, U).
:- func set_index(int, U, T) = T <= index(T, U).
:- func 'index :='(int, T, U) = T <= index(T, U).

:- func semidet_index(int, T) = U is semidet <= index(T, U).
:- func semidet_set_index(int, U, T) = T is semidet <= index(T, U).
:- func 'semidet_index :='(int, T, U) = T is semidet <= index(T, U).


:- func fold_index(func(U, A) = A, T, A) = A <= index(T, U).
:- func map_index(func(U) = U, T) = T <= index(T, U).

%-----------------------------------------------------------------------------%
% Default implementation of index methods

% These default implementations of typeclass index/2 member predicates require
% implementations that iterate over the entire structure using index/3 or
% valid_index/2 resulting in O(n) complexities, some instances
% may have more efficient implementations availible.

:- pred default_fold(pred(U, A, A), T, A, A) <= index(T, U).
:- mode default_fold(pred(in, in, out) is det, in, in, out) is det.
	
:- pred default_map(pred(U, U), T, T) <= index(T, U).
:- mode default_map(pred(in, out) is det, 	in, out) is det.

% index_out_of_range_error(Module, Pred, T, U, Index).
:- pred index_out_of_range_error(string::in, string::in, T::unused, U::unused, 
	int::in) is erroneous <= index(T, U).

%-----------------------------------------------------------------------------%
% List index implementation methods

:- pred list_index(list(T), int, T).
:- mode list_index(in, in, out) is det. 
:- mode list_index(in, out, out) is nondet. 
	
:- pred set_list_index(int, T, list(T), list(T)).
:- mode set_list_index(in, in, in, out) is det. 
:- mode set_list_index(out, in, in, out) is nondet.

:- pred fold_list_index(pred(T, A, A), list(T), A, A). 
:- mode fold_list_index(pred(in, in, out) is det, in, in, out) is det.

:- pred map_list_index(pred(T, T), list(T), list(T)).
:- mode map_list_index(pred(in, out) is det, in, out) is det.
	
%-----------------------------------------------------------------------------%
% Array index implementation methods

:- pred array_index(array(T), int, T).
:- mode array_index(in, in, out) is det. 
:- mode array_index(in, out, out) is nondet. 
	
:- pred set_array_index(int, T, array(T), array(T)).
:- mode set_array_index(in, in, in, out) is det. 
:- mode set_array_index(out, in, in, out) is nondet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.
:- import_module type_desc.
:- import_module solutions.
:- import_module int.
:- import_module bool.
:- import_module maybe.
:- import_module sparse_bitset.

%-----------------------------------------------------------------------------%
% Valid indexes

:- pragma promise_equivalent_clauses(valid_index/2).

valid_index(T::in, I::in) :- I > 0, I =< arity(T).

valid_index(T::in, I::out) :- 
	Arity @ arity(T) > 0,
	generate_valid_index(1, Arity, I).
	
:- pred generate_valid_index(int::in, int::in, int::out) is multi.

generate_valid_index(I0, A, I) :-
		I = I0 
	; 	I0 < A,
		generate_valid_index(I0 + 1, A, I).

%-----------------------------------------------------------------------------%
% Index Operation determinism casts


semidet_valid_index(T, I) :- valid_index(T, I).
nondet_valid_index(T, I) :- valid_index(T, I).
cc_nondet_valid_index(T, I) :- valid_index(T, I).

det_index(T, I, U) :- index(T, I, U).
semidet_index(T, I, U) :- valid_index(T, I), index(T, I, U).
nondet_index(T, I, U) :- valid_index(T, I),index(T, I, U).
cc_nondet_index(T, I, U) :- valid_index(T, I),index(T, I, U).

det_set_index(I, U, !T) :- set_index(I, U, !T).
semidet_set_index(I, U, !T) :- valid_index(!.T, I),set_index(I, U, !T).
nondet_set_index(I, U, !T) :- valid_index(!.T, I),set_index(I, U, !T).
cc_nondet_set_index(I, U, !T) :- valid_index(!.T, I),set_index(I, U, !T).


%-----------------------------------------------------------------------------%
% Function versions of index/2 methods

T ^ index(I) = U :- index(T, I, U).

set_index(I, U, !.T) = !:T :- set_index(I, U, !T). 

(!.T ^ index(I) := U) = !:T :- set_index(I, U, !T).

T ^ semidet_index(I) = U :- semidet_index(T, I, U).

semidet_set_index(I, U, !.T) = !:T :- semidet_set_index(I, U, !T).

(!.T ^ semidet_index(I) := U) = !:T :- semidet_set_index(I, U, !T).



fold_index(Func, T, !.A) = !:A :- fold_index(function_closure(Func), T, !A).

:- pred function_closure(func(T, A) = A, T, A, A).
:- mode function_closure(func(in, in) = out is det, in, in, out) is det.

function_closure(Func, T, !A) :- Func(T, !.A) = !:A.

map_index(Func, !.T) = !:T :- map_index(function_closure(Func), !T).
	
:- pred function_closure(func(T) = T, T, T).
:- mode function_closure(func(in) = out is det, in, out) is det.

function_closure(Func, !T) :- Func(!.T) = !:T.

%-----------------------------------------------------------------------------%
% Default implementation of index methods

default_fold(Closure, T, !A) :-	promise_equivalent_solutions [!:A]
	unsorted_aggregate(	
		nondet_valid_index(T), 
		default_fold_aggregator(Closure, T),
		!A).
	
:- pred default_fold_aggregator(pred(U, A, A), T, int, A, A) <= index(T, U).
:- mode default_fold_aggregator(pred(in, in, out) is det, in, in, in, out)
	is det.

	
default_fold_aggregator(Pred, T, I, !A) :- det_index(T, I, U), Pred(U, !A).


default_map(Closure, !T) :- promise_equivalent_solutions [!:T] 
	unsorted_aggregate(
		nondet_valid_index(!.T), 
		default_map_aggregator(Closure), 
		!T).
	
:- pred default_map_aggregator(pred(U, U), int, T, T) <= index(T, U).
:- mode default_map_aggregator(pred(in, out) is det, in, in, out) is det.

default_map_aggregator(Pred, I, !T) :- 
	det_index(!.T, I, U0),
	Pred(U0, U),
	det_set_index(I, U, !T).
	
index_out_of_range_error(Module, Pred, T, U, I) :- unexpected(Module, Pred, 
				"Index " ++ string(I) ++ 
				" out of range for method call of index(" ++ 
				type_name(type_of(T)) ++ ", " ++ type_name(type_of(U)) ++
				").").
				
%-----------------------------------------------------------------------------%
% List index implementation methods

:- pragma promise_equivalent_clauses(list_index/3).

list_index(L::in, I::in, V::out) :- det_index1(L, I, V). 
		
list_index([V | Vs]::in, I::out, U::out) :-
	I = 1, V = U;
	I > 1, list_index(Vs @ [_ | _], I - 1, U). 


:- pragma promise_equivalent_clauses(set_list_index/4).

set_list_index(I::in, T::in, !.L::in, !:L::out) :-
	det_replace_nth(!.L, I, T, !:L).


set_list_index(I::out, T::in, !.L::in, !:L::out) :-
	Len = length(!.L),
	set_list_index(I, T, !L, Len).

:- pred set_list_index(int::out, T::in, list(T)::in, list(T)::out, int::in)
		is nondet.
		
set_list_index(I, T, [_ | Vs], [T | Vs], I).

set_list_index(I + 1, T, [V | !.Vs], [V | !:Vs], Len) :-
	Len > 1,
	set_list_index(I, T, !Vs, Len - 1).
	
fold_list_index(_, [], !A).

fold_list_index(Closure, [T | L], !A) :-
	Closure(T, !A),
	fold_list_index(Closure, L, !A).
	
map_list_index(_, [], []).

map_list_index(Closure, [ !.T | !.L ], [ !:T | !:L]) :-
	Closure(!T),
	map_list_index(Closure, !L).
	
%-----------------------------------------------------------------------------%
% Array index implementation methods

:- pragma promise_equivalent_clauses(array_index/3).

array_index(A::in, I::in, V::out) :- unsafe_lookup(A, I - 1, V).

array_index(A::in, I::out, V::out) :-
	valid_index(A, I),
	unsafe_lookup(A, I - 1, V).

:- pragma promise_equivalent_clauses(set_array_index/4).

set_array_index(I::in, V::in, !.A::in, !:A::out) :-
	slow_set(I - 1, V, !A).
	
set_array_index(I::out, V::in, !.A::in, !:A::out) :-
	valid_index(!.A, I),
	slow_set(I - 1, V, !A).
