%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_util.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_util.

:- interface.

:- import_module array.
:- import_module list.
:- import_module string.


%-----------------------------------------------------------------------------%
% Deterministic dynamic casting

	% if unsafe_dynamic_cast is true, det_dynamic_cast will perform a naked
	% type cast, otherwise it will perform a normal dynamic cast, throwing
	% an exception if the cast fails.
	
	% I intend to leave unsafe_dynamic_cast as false until I get to profiling
	% Mad Hatter, to see if naked type casts work as intended, and to see
	% if there is any performance benefit to naked type casting.
	
:- pred unsafe_dynamic_cast is semidet.

	% DO NOT USE THESE PREDICATES unless you have already verified that the 
	% variables being cast are the same type. If this condititions is not
	% met, det_dynamic_cast will throw an exceptiosn IF unsafe_dynamic_cast
	% is false, otherwise you will get undefined behavior
	
:- func det_dynamic_cast(T) = V.

:- pred det_dynamic_cast(T::in, V::out) is det.



%-----------------------------------------------------------------------------%
% Array Manipulation

% array_insert(Index, T, Source, Result) 
% Copy an array and insert an element T at Index, shifting elements right
% Throws an exception if Index is out of bounds
:- pred array_insert(int::in, T::in, array(T)::in, array(T)::array_uo) is det.
:- func array_insert(int::in, T::in, array(T)::in) = (array(T)::array_uo)
	is det.

% Does not check bounds 	
:- pred unsafe_array_insert(int::in, T::in, array(T)::in, array(T)::array_uo) 
	is det.
:- func unsafe_array_insert(int::in, T::in, array(T)::in) = 
	(array(T)::array_uo) is det.


% array_copy_range(Source, SrcFirst, SrcLast, TgtFirst, !Array)
% Copy elements from a Source array ranging from indexes SrcFirst to SrcLast
% to a target Array, starting at index TgtFirst
% Throws an exception if any of the indexes are out of bounds.
:- pred array_copy_range(array(T)::in, int::in, int::in, int::in,
	array(T)::array_di, array(T)::array_uo) is det.
	
:- pred unsafe_copy_array_range(array(T)::in, int::in, int::in, int::in,
	array(T)::array_di, array(T)::array_uo) is det.
	
%-----------------------------------------------------------------------------%
% Exceptions

	
% bounds_error(Pred, Msg)
% Throw an array.index_out_of_bounds exception
:- pred bounds_error(string::in, string::in) is erroneous.

% format_error(Pred, Msg, Vars)
% Throw a software error while formatting Msg with Vars
:- pred format_error(string::in, string::in, 
	list(poly_type)::in) is erroneous.

% format_out_of_bounds_error(Pred, Msg, Vars)
% Same, but with an out of bounds exception
:- pred format_bounds_error(string::in, string::in, 
	list(poly_type)::in) is erroneous.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.


:- import_module int.
:- import_module require.
:- import_module exception.

%-----------------------------------------------------------------------------%
% Deterministic dynamic casting


% Set to true at your own risk! the Mercury designers never intended for
% the use of unchecked type casting outside of the internal operation of
% the Mercury Melbourne Compiler, and Mercury runtime

unsafe_dynamic_cast :- false.

:- pragma no_determinism_warning(unsafe_dynamic_cast/0).



det_dynamic_cast(T) = V :- 
	(if unsafe_dynamic_cast
	then
		private_builtin.unsafe_type_cast(T, V)  % !!!!!!!!!!
	else
		( if dynamic_cast(T, U)
		then V = U
		else unexpected($module, $pred, "Dynamic cast failure.")
		)
	).
	


det_dynamic_cast(T, det_dynamic_cast(T)).


%-----------------------------------------------------------------------------%
% Array Manipulation

array_insert(I, T, Src, array_insert(I, T, Src)).

array_insert(I, T, Src) = Result :-
	bounds(Src, Min, Max - 1),
	(if I >= Min, I =< Max 
	then
		Result = unsafe_array_insert(I, T, Src)
	else
		format_bounds_error($pred, "index %d not in range [%d, %d]",
        [i(I), i(Min), i(Max)])
	).

unsafe_array_insert(I, T, Src, array_insert(I, T, Src)).

unsafe_array_insert(I, T, Src) = Result :-
	Size = size(Src),
	NewSize = Size + 1,
	(if Size = 0
	then
		init(NewSize, T, Result)
	else
		init(NewSize, Src ^ elem(0), Result0),
		insert_loop(I, T, Src, 1, Size, Result0, Result)	
	).
		
:- pred insert_loop(int::in, T::in, array(T)::in, int::in, int::in,
	array(T)::array_di, array(T)::array_uo) is det.
	
insert_loop(I, T, Src, Current, Last, !Array) :-
	Next = Current + 1,
	(if I = Current
	then
		unsafe_set(I, T, !Array),
		array_copy_range(Src, Current, Last, Next, !Array)
	else
		insert_loop(I, T, Src, Next,  Last, !Array)
	).


array_copy_range(Src, SrcF, SrcL, TgtF, !Array) :-
	(if SrcF < SrcL
	then
		format_error($pred, 
			"erroneous source range, first index %d must be smaller " ++
			"than last index %d", [i(SrcF), i(SrcL)])
	else if in_bounds(Src, SrcF) then
		bounds_error($pred, "range start out of bounds of source array")
	else if in_bounds(Src, SrcL) then
		bounds_error($pred, "range end out of bounds of source array")
	else if in_bounds(!.Array, TgtF) then
		bounds_error($pred, "target index start out of bounds of target array")
	else if in_bounds(!.Array, TgtF + SrcL - SrcF) then
		bounds_error($pred, "range end out of bounds of target array")
	else
		unsafe_copy_array_range(Src, SrcF, SrcL, TgtF, !Array)
	).

unsafe_copy_array_range(Src, SrcF, SrcL, TgtF, !Array) :-
	unsafe_set(TgtF, Src ^ elem(SrcF), !Array),
	(if SrcF < SrcL
	then 
		array_copy_range(Src, SrcF + 1, SrcL, TgtF + 1, !Array)
	else
		!:Array = !.Array
	).


	
%-----------------------------------------------------------------------------%
% Exceptions
	
bounds_error(Pred, Msg) :-
    throw(array.index_out_of_bounds(Pred ++ ": " ++ Msg)).

format_error(Pred, Msg, Vars) :-
	string.format(Pred ++ ": " ++ Msg, Vars, Err),
	error(Err).
	
format_bounds_error(Pred, Msg, Vars) :-
	string.format(Msg, Vars, Err),
    bounds_error(Pred, Err).