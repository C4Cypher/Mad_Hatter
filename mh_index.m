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

%-----------------------------------------------------------------------------%

:- typeclass index(T, U) <= (T -> U) where [


	pred valid_index(T, int), % valid_index(Container, Index)
	mode valid_index(in, in) is semidet, % succeed on valid index
	mode valid_index(in, out) is nondet, % return all valid indexes, 
	
	% valid_index(in, out) may produce indexes in any order, but it must not
	% generate duplicate indexes, otherwise default_map/3 may produce
	% undefined behavior

	pred index(T, int, U),	% index(Container, Index, Value)
	mode index(in, in, in) is semidet, 	% implicit fail on inequality
	mode index(in, in, out) is semidet, % fail on invalid index
	mode index(in, out, out) is nondet, % all indexed values
	
	pred set_index(int, U, T, T), % set_index(Index, Value, !Container)
	mode set_index(in, in, in, out) is semidet, % fail on invalid index
	mode set_index(out, in, in, out) is nondet, % update any index nondet
	
	pred index_size(T::in, int::out) is det,  % Number of valid indexes
	
	% Index bounds, fail if there are no valid indexes
	pred max_index(T::in, int::out) is semidet,	% highest valid index
	pred min_index(T::in, int::out) is semidet, % lowest valid index
	
	% true if there are no invalid indexes between min and max
	pred contiguous_index(T::in) is semidet,
	
	% Iterate a closure over all values in a container with an accumulator
	% in the same order as index/3
	pred fold_index(pred(U, A, A), T, A, A), % fold_index(Closure, Cont, !Acc)
	mode fold_index(pred(in, in, out) is det, in, in, out) is det,
	
	pred map_index(pred(U, U), T, T), % map_index(Closure, !Container)
	mode map_index(pred(in, out) is det, in, out) is det

].

%-----------------------------------------------------------------------------%

	% Throws an exception if index is out of range.

:- pred det_index(T::in, int::in, U::out) is det <= index(T, U).

:- pred det_set_index(int::in, U::in, T::in, T::out) is det <= index(T, U).

%-----------------------------------------------------------------------------%
% Function versions of index/2 methods

:- func index(int, T) = U is semidet <= index(T, U).
:- func 'index :='(int, T, U) = T is semidet <= index(T, U).

:- func det_index(int, T) = U <= index(T, U).
:- func 'det_index :='(int, T, U) = T <= index(T, U).

:- func index_size(T) = int <= index(T, _).

:- func max_index(T) = int is semidet <= index(T, _).
:- func min_index(T) = int is semidet <= index(T, _).

:- func fold_index(func(U, A) = A, T, A) = A <= index(T, U).
:- func map_index(func(U) = U, T) = T <= index(T, U).




%-----------------------------------------------------------------------------%
% Default implementation of index methods

	% Default implmentation of map_index/2 using valid_index/2 to generate 
	% valid indexes to iterate over. Some data structures such as lists may 
	% have more efficient implementations 
:- pred default_map(pred(U, U), T, T) <= index(T, U).
:- mode default_map(pred(in, out) is det, 	in, out) is det.


:- implementation.

:- import_module require.
:- import_module string.
:- import_module type_desc.
:- import_module solutions.
:- import_module bool.

%-----------------------------------------------------------------------------%

det_index(T, I, U) :-
	if valid_index(T, I) then (
		if index(T, I, V) then U = V
		else unexpected($module, $pred, 
			"Index " ++ string(I) ++ 
			" out of range for method call of index(" ++ 
			type_name(type_of(T)) ++ ", " ++ type_name(type_of(U)) ++
			"). mh_index.valid_index/2 should not have allowed this!")
		)
		else unexpected($module, $pred, 
				"Index " ++ string(I) ++ 
				" out of range for method call of index(" ++ 
				type_name(type_of(T)) ++ ", " ++ type_name(type_of(U)) ++
				").").
				
det_set_index(I, U, !T) :-
	if valid_index(!.T, I) then (
		if set_index(I, U, !.T, NewT) 
			then !:T = NewT
			else unexpected($module, $pred, 
				"Index " ++ string(I) ++ 
				" out of range for method call of index(" ++ 
				type_name(type_of(!.T)) ++ ", " ++ type_name(type_of(U)) ++
				"). mh_index.valid_index/2 should not have allowed this!")
		)
		else unexpected($module, $pred, 
				"Index " ++ string(I) ++ 
				" out of range for method call of index(" ++ 
				type_name(type_of(!.T)) ++ ", " ++ type_name(type_of(U)) ++
				").").

%-----------------------------------------------------------------------------%

T ^ index(I) = U :- index(T, I, U).

(!.T ^ index(I) := U) = !:T :- set_index(I, U, !T).

T ^ det_index(I) = U :- det_index(T, I, U).

(!.T ^ det_index(I) := U) = !:T :- det_set_index(I, U, !T).

index_size(T) = I :- index_size(T, I).

max_index(T) = M :- max_index(T, M).
min_index(T) = M :- min_index(T, M).

%-----------------------------------------------------------------------------%

fold_index(Func, T, !.A) = !:A :- fold_index(function_closure(Func), T, !A).

:- pred function_closure(func(T, A) = A, T, A, A).
:- mode function_closure(func(in, in) = out is det, in, in, out) is det.

function_closure(Func, T, !A) :- Func(T, !.A) = !:A.

%-----------------------------------------------------------------------------%

map_index(Func, !.T) = !:T :- map_index(function_closure(Func), !T).
	
:- pred function_closure(func(T) = T, T, T).
:- mode function_closure(func(in) = out is det, in, out) is det.

function_closure(Func, !T) :- Func(!.T) = !:T.

%-----------------------------------------------------------------------------%

default_map(Closure, !T) :- 
	promise_equivalent_solutions [!:T] unsorted_aggregate(
		generate_index(!.T), 
		default_map_aggregator(Closure), 
		!T).

:- pred generate_index(T::in, int::out) is nondet <= index(T, _).

generate_index(T, I) :- valid_index(T, I).	
	
:- pred default_map_aggregator(pred(U, U), int, T, T) <= index(T, U).
:- mode default_map_aggregator(pred(in, out) is det, in, in, out) is det.

default_map_aggregator(Pred, I, !T) :- 
	det_index(!.T, I, U0),
	Pred(U0, U),
	det_set_index(I, U, !T).
	
	