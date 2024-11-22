%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_util.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module util.

:- interface.

:- import_module array.
:- import_module list.
:- import_module string.
:- import_module map.


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
:- func array_insert(array(T)::in, int::in, T::in) = (array(T)::array_uo)
	is det.

% Does not check bounds 	
:- pred unsafe_array_insert(int::in, T::in, array(T)::in, array(T)::array_uo) 
	is det.
:- func unsafe_array_insert(array(T)::in, int::in, T::in) = 
	(array(T)::array_uo) is det.
	
% array_delete(Index, Source, Result)
% Remove an element from an array, shifting elements left
:- pred array_delete(int::in, array(T)::in, array(T)::array_uo) is det.
:- func array_delete(array(T)::in, int::in) = (array(T)::array_uo) is det.

:- pred unsafe_array_delete(int::in, array(T)::in, array(T)::array_uo) is det.
:- func unsafe_array_delete(array(T)::in, int::in) = (array(T)::array_uo) 
	is det.

% array_[cons|snoc](T, Source, Result)
% Insert T as the [first|last] element of Source 
:- pred array_cons(T::in, array(T)::in, array(T)::array_uo) is det.
:- func array_cons(array(T)::in, T::in) = (array(T)::array_uo) is det.

:- pred array_snoc(T::in, array(T)::in, array(T)::array_uo) is det.
:- func array_snoc(array(T)::in, T::in) = (array(T)::array_uo) is det.


% array_copy_range(Source, SrcFirst, SrcLast, TgtFirst, !Array)
% Copy elements from a Source array ranging from indexes SrcFirst to SrcLast
% to a target Array, starting at index TgtFirst
% Throws an exception if any of the indexes are out of bounds.
:- pred array_copy_range(array(T)::in, int::in, int::in, int::in,
	array(T)::array_di, array(T)::array_uo) is det.
	
:- pred unsafe_array_copy_range(array(T)::in, int::in, int::in, int::in,
	array(T)::array_di, array(T)::array_uo) is det.

%-----------------------------------------------------------------------------%
% Map Manipulation

:- pred is_singleton(map(_, _)::in) is semidet.
	
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
	
% Call report_lookup_error/2 as an erroneous function.
:- func report_lookup_error(string, K) = _ is erroneous.

:- func report_lookup_error(string, K, V) = _ is erroneous.

%-----------------------------------------------------------------------------%
% Misc

:- func func_fail = _ is failure.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.


:- import_module int.
:- import_module solutions.
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

array_insert(I, T, Src, array_insert(Src, I, T)).

array_insert(Src, I, T) = Result :-
	bounds(Src, Min, Max),
	(if I >= Min, I =< Max + 1 
	then
		unsafe_array_insert(I, T, Src, Result)
	else
		format_bounds_error($pred, "index %d not in range [%d, %d]",
        [i(I), i(Min), i(Max)])
	).

unsafe_array_insert(I, T, Src, unsafe_array_insert(Src, I, T)).

unsafe_array_insert(Src, I, T) = Result :-
	First = min(Src),
	Next = First + 1,
	Size = size(Src),
	Last = max(Src),
	NewSize = Size + 1,
	(if I = First
	then
		init(NewSize, T, Result0),
		(if Size = 0
		then
			Result = Result0
		else
			%unsafe_array_copy_range(Src, First, Last, Next,
			array_copy_range(Src, First, Last, Next,
				Result0, Result)
		)		
	else
		init(NewSize, Src ^ elem(First), Result0),
		insert_loop(I, T, Src, Next, Last, Result0, Result)	
	).
		
:- pred insert_loop(int::in, T::in, array(T)::in, int::in, int::in,
	array(T)::array_di, array(T)::array_uo) is det.
	
insert_loop(I, T, Src, Current, Last, !Array) :-
	Next = Current + 1,
	(if Current < I
	then
		unsafe_set(Current, Src ^ elem(Current), !Array),
		insert_loop(I, T, Src, Next,  Last, !Array)
	else 
		unsafe_set(I, T, !Array),
		compare(Compare, I, Last),
		(
			Compare = (<),
			%unsafe_array_copy_range(Src, Current, Last, Next, !Array)
			array_copy_range(Src, Current, Last, Next, !Array)
		;
			Compare = (=),
			unsafe_set(Last + 1, Src ^ elem(Last), !Array)
		;
			Compare = (>),
			!:Array = !.Array		
		)
	).
	
array_delete(I, Src, array_delete(Src, I)).

array_delete(Src, I) = Result :-
	bounds(Src, Min, Max),
	(if I >= Min, I =< Max 
	then
		unsafe_array_delete(I, Src, Result)
	else
		format_bounds_error($pred, "index %d not in range [%d, %d]",
        [i(I), i(Min), i(Max)])
	).
	
unsafe_array_delete(I, Src, unsafe_array_delete(Src, I)).

unsafe_array_delete(Src, I) = Result :-
	Size = size(Src),
	NewSize = Size - 1,
	Last = max(Src),
	(if NewSize = 0
	then 
		Result = make_empty_array
	else if NewSize = 1
	then
		Remaining = (I = 0 -> 1 ; 0),
		init(NewSize, Src ^ elem(Remaining), Result)
	else if I = Last
	then
		init(NewSize, Src ^ elem(0), Result0),
		%unsafe_array_copy_range(Src, 1, Last, 1, Result0, Result)
		array_copy_range(Src, 1, Last - 1, 1, Result0, Result)
	else
		(if I = 0
		then
			init(NewSize, Src ^ elem(1), Result1),
			First = 2,
			CpyStart = 1
		else
			First = I + 1,
			init(NewSize, Src ^ elem(0), Result0),
			(if I = 1
			then
				Result1 = Result0,
				CpyStart = 1
			else
				%unsafe_array_copy_range(Src, 1, I - 1, 1, Result0, Result1),
				array_copy_range(Src, 1, I - 1, 1, Result0, Result1),
				CpyStart = I
			)
		),
		%unsafe_array_copy_range(Src, I + 1, Last, CpyStart, Result1, Result)
		array_copy_range(Src, First, Last, CpyStart, Result1, Result)
	).
	
array_cons(T, Src, array_cons(Src, T)).

array_cons(Src, T) = unsafe_array_insert(Src, 0, T).

array_snoc(T, Src, array_snoc(Src, T)).

array_snoc(Src, T) = unsafe_array_insert(Src, max(Src) + 1, T).


array_copy_range(Src, SrcF, SrcL, TgtF, !Array) :-
	(if SrcF > SrcL
	then
		format_error($pred, 
			"erroneous source range, first index %d must be smaller " ++
			"than last index %d", [i(SrcF), i(SrcL)])
	else if not in_bounds(Src, SrcF) then
		bounds(Src, Min, Max),
		format_bounds_error($pred, 
			"range start %d out of bounds of source array %d - %d",
			[i(SrcF), i(Min), i(Max)])
	else if not in_bounds(Src, SrcL) then
		bounds(Src, Min, Max),
		format_bounds_error($pred, 
			"range end %d out of bounds of source array %d - %d",
			[i(SrcL), i(Min), i(Max)])
	else if not in_bounds(!.Array, TgtF) then
		bounds(!.Array, Min, Max),
		format_bounds_error($pred, 
			"target index %d start out of bounds of target array %d - %d",
			[i(TgtF), i(Min), i(Max)])
	else if 
		TgtL = TgtF + SrcL - SrcF,
		not in_bounds(!.Array, TgtL) 
	then
		bounds(!.Array, Min, Max),
		format_bounds_error($pred, 
			"target index %d end out of bounds of target array %d - %d",
			[i(TgtL), i(Min), i(Max)])
	else
		unsafe_array_copy_range(Src, SrcF, SrcL, TgtF, !Array)
	).

unsafe_array_copy_range(Src, SrcF, SrcL, TgtF, !Array) :-
	unsafe_set(TgtF, Src ^ elem(SrcF), !Array),
	(if SrcF < SrcL
	then 
		unsafe_array_copy_range(Src, SrcF + 1, SrcL, TgtF + 1, !Array)
	else
		!:Array = !.Array
	).

%-----------------------------------------------------------------------------%
% Map Manipulation

is_singleton(Map) :- keys(Map, [_]).
	
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
	
report_lookup_error(Msg, K) = _ :-
	report_lookup_error(Msg, K).
	
report_lookup_error(Msg, K, V) = _ :-
	report_lookup_error(Msg, K, V).
	
%-----------------------------------------------------------------------------%
% Misc

func_fail = _ :- fail.