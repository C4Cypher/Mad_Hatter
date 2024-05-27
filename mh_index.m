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


	pred valid_index(T, int),
	mode valid_index(in, in) is semidet, % succeed on valid index
	mode valid_index(in, out) is nondet, % return all valid indexes, no dups

	pred index(T, int, U),
	mode index(in, in, in) is semidet, % implicit fail on inequality
	mode index(in, in, out) is semidet, % fail on invalid index
	mode index(in, out, out) is nondet, % all indexed values
	
	pred set_index(int, U, T, T),
	mode set_index(in, in, in, out) is semidet, % fail on invalid index
	mode set_index(out, in, in, out) is nondet, % update any index nondet
	
	pred map(pred(U, U), T, T),
	mode map(pred(in, out) is det, 	in, out) is det

].

%-----------------------------------------------------------------------------%

	% Throws an exception if index is out of range.

:- pred det_index(T::in, int::in, U::out) is det <= index(T, U).

:- pred det_set_index(int::in, U::in, T::in, T::out) is det <= index(T, U).

%-----------------------------------------------------------------------------%

:- func index(int, T) = U is semidet <= index(T, U).

:- func 'index :='(int, T, U) = T is semidet <= index(T, U).

:- func det_index(int, T) = U <= index(T, U).

:- func 'det_index :='(int, T, U) = T <= index(T, U).

%-----------------------------------------------------------------------------%

	% function form of map/2
:- func map(func(U) = U, T) = T <= index(T, U).

	% Default implmentation of map/2 using valid_index/2 to generate valid
	% indexes to iterate over. Some data structures such as lists may have
	% More efficient implementations 
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

%-----------------------------------------------------------------------------%

map(Func, !.T) = !:T :- map(function_closure(Func), !T).
	
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
	
	