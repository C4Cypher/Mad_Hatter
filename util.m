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
	% variables being cast are the same type. If this conditition is not
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

% Unsafe version skips the bounds checks, may result in underfined behavior
:- pred unsafe_array_copy_range(array(T)::in, int::in, int::in, int::in,
	array(T)::array_di, array(T)::array_uo) is det.
	
% array_copy_range_rev(Source, SrcFirst, SrcL, TgtFirst, !Array)
% As above, but copy the elements in reverse ordeer.
:- pred array_copy_range_rev(array(T)::in, int::in, int::in, int::in,
	array(T)::array_di, array(T)::array_uo) is det.

% Unsafe version skips the bounds checks, may result in underfined behavior
:- pred unsafe_array_copy_range_rev(array(T)::in, int::in, int::in, int::in,
	array(T)::array_di, array(T)::array_uo) is det.

	
% remove_dups(Source, Result).
% Take a sorted array and remove duplicates, the array MUST be sorted by the
% standard ordering
:- pred remove_dups(array(T)::array_di, array(T)::array_uo) is det.
:- func remove_dups(array(T)::array_di) = (array(T)::array_uo) is det.

% Make a copy of the input array, sort and remove duplicates
:- pred sort_and_remove_dups(array(T)::in, array(T)::array_uo) is det.
:- func sort_and_remove_dups(array(T)::in) = (array(T)::array_uo) is det.

% Perform a linear search of an array, returning the index if found.

:- pred array_search(array(T)::in, T::in, int::out) is semidet.
:- func array_search(array(T), T) = int is semidet.

% Perform a sort of the input array in a manner identical to the library 
% standard sort/1 call, but provide a higher order comparison function

:- func samsort(comparison_func(T)::in(comparison_func), array(T)::array_di) = 
	(array(T)::array_uo) is det.
	
% A traditional top down merge sort, should be stable to the original order	
:- func mergesort(comparison_func(T)::in(comparison_func), 
	array(T)::array_di) = (array(T)::array_uo) is det.



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
		true
	).
	

array_copy_range_rev(Src, SrcF, SrcL, TgtF, !Array) :-
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
		unsafe_array_copy_range_rev(Src, SrcF, SrcL, TgtF, !Array)
	).

unsafe_array_copy_range_rev(Src, SrcF, SrcL, TgtF, !Array) :-
	unsafe_set(TgtF, Src ^ elem(SrcL), !Array),
	(if SrcF < SrcL
	then 
		unsafe_array_copy_range(Src, SrcF, SrcL - 1, TgtF + 1, !Array)
	else
		true
	).
	
remove_dups(!A) :- 
	(if size(!.A, 0)
	then true
	else
		unsafe_lookup(!.A, 0, First),
		remove_dups(1, First, 1, NewSize, !A),
		shrink(NewSize, !A)
	).

:- pred remove_dups(int::in, T::in, int::in, int::out, array(T)::array_di, 
	array(T)::array_uo) is det.
	
remove_dups(Index, Current, !Unique, !A) :-
	(if Index > max(!.A)
	then true
	else
		unsafe_lookup(!.A, Index, Next),
		(if Current = Next
		then remove_dups(Index + 1, Current, !Unique, !A)
		else
			unsafe_set(!.Unique, Next, !A),
			!:Unique = !.Unique + 1,
			remove_dups(Index + 1, Next, !Unique, !A)
		)
	).

remove_dups(!.A) = !:A :- remove_dups(!A).

sort_and_remove_dups(!A) :- array.copy(!A), !:A = array.sort(!.A), 
	remove_dups(!A).

sort_and_remove_dups(!.A) = !:A :- sort_and_remove_dups(!A).

array_search(A, T, I) :- array_search(A, T, 0, I).

array_search(A, T) = I :- array_search(A, T, I).

:- pred array_search(array(T)::in, T::in, int::in, int::out) is semidet.

array_search(A, T, Current, I) :-
	I < max(A),
	(if unsafe_lookup(A, Current, T)
	then
		I = Current
	else
		array_search(A, T, Current + 1, I)
	).
	
samsort(CMP, A) = samsort_subarray(CMP, A, array.min(A), array.max(A)).

mergesort(CMP, !.A) = !:A :- mergesort_subarray(CMP, !A, copy(!.A), _,
	array.min(!.A), array.max(!.A)).
	
%-----------------------------------------------------------------------------%
% SAM (Smooth) sort

% The following is copied in it's entirety and then adapted to use a higher
% order comparison function from the array.m standard library, credit due
% to fjh and bromage, the original authors of that library.

	% SAMsort (smooth applicative merge) invented by R.A. O'Keefe.
	%
	% SAMsort is a mergesort variant that works by identifying contiguous
	% monotonic sequences and merging them, thereby taking advantage of
	% any existing order in the input sequence.
	%
:- func samsort_subarray(comparison_func(T)::in(comparison_func), 
	array(T)::array_di, int::in, int::in) = (array(T)::array_uo) is det.

:- pragma type_spec(func(samsort_subarray/4), T = int).
:- pragma type_spec(func(samsort_subarray/4), T = string).

samsort_subarray(CMP, A0, Lo, Hi) = A :-
	samsort_up(CMP, 0, array.copy(A0), A, A0, _, Lo, Hi, Lo).

	% samsort_up(N, A0, A, B0, B, Lo, Hi, I):
	%
	% Precondition:
	%   We are N levels from the bottom (leaf nodes) of the tree.
	%   A0 is sorted from Lo .. I - 1.
	%   A0 and B0 are identical from I .. Hi.
	% Postcondition:
	%   A is sorted from Lo .. Hi.
	%
:- pred samsort_up(comparison_func(T)::in(comparison_func), int::in, 
	array(T)::array_di, array(T)::array_uo, array(T)::array_di, 
	array(T)::array_uo, int::in, int::in, int::in) is det.

:- pragma type_spec(pred(samsort_up/9), T = int).
:- pragma type_spec(pred(samsort_up/9), T = string).

samsort_up(CMP, N, A0, A, B0, B, Lo, Hi, I) :-
	trace [compile_time(flag("array_sort"))] (
		verify_sorted(CMP, A0, Lo, I - 1),
		verify_identical(CMP, A0, B0, I, Hi)
	),
	( if I > Hi then
		A = A0,
		B = B0
		% A is sorted from Lo .. Hi.
	else if N > 0 then
		% B0 and A0 are identical from I .. Hi.
		samsort_down(CMP, N - 1, B0, B1, A0, A1, I, Hi, J),
		% A1 is sorted from I .. J - 1.
		% B1 and A1 are identical from J .. Hi.

		merge_subarrays(CMP, A1, Lo, I - 1, I, J - 1, Lo, B1, B2),
		A2 = A1,

		% B2 is sorted from Lo .. J - 1.
		% B2 and A2 are identical from J .. Hi.
		samsort_up(CMP, N + 1, B2, B3, A2, A3, Lo, Hi, J),
		% B3 is sorted from Lo .. Hi.

		A = B3,
		B = A3
		% A is sorted from Lo .. Hi.
	else
		% N = 0, I = Lo
		copy_run_ascending(CMP, A0, B0, B1, Lo, Hi, J),

		% B1 is sorted from Lo .. J - 1.
		% B1 and A0 are identical from J .. Hi.
		samsort_up(CMP, N + 1, B1, B2, A0, A2, Lo, Hi, J),
		% B2 is sorted from Lo .. Hi.

		A = B2,
		B = A2
		% A is sorted from Lo .. Hi.
	),
	trace [compile_time(flag("array_sort"))] (
		verify_sorted(CMP, A, Lo, Hi)
	).

	% samsort_down(N, A0, A, B0, B, Lo, Hi, I):
	%
	% Precondition:
	%   We are N levels from the bottom (leaf nodes) of the tree.
	%   A0 and B0 are identical from Lo .. Hi.
	% Postcondition:
	%   B is sorted from Lo .. I - 1.
	%   A and B are identical from I .. Hi.
	%
:- pred samsort_down(comparison_func(T)::in(comparison_func), int::in, 
	array(T)::array_di, array(T)::array_uo, array(T)::array_di, 
	array(T)::array_uo, int::in, int::in, int::out) is det.

:- pragma type_spec(pred(samsort_down/9), T = int).
:- pragma type_spec(pred(samsort_down/9), T = string).

samsort_down(CMP, N, A0, A, B0, B, Lo, Hi, I) :-
	trace [compile_time(flag("array_sort"))] (
		verify_identical(CMP, A0, B0, Lo, Hi)
	),
	( if Lo > Hi then
		A = A0,
		B = B0,
		I = Lo
		% B is sorted from Lo .. I - 1.
	else if N > 0 then
		samsort_down(CMP, N - 1, B0, B1, A0, A1, Lo, Hi, J),
		samsort_down(CMP, N - 1, B1, B2, A1, A2, J,  Hi, I),
		% A2 is sorted from Lo .. J - 1.
		% A2 is sorted from J  .. I - 1.
		A = A2,
		merge_subarrays(CMP, A2, Lo, J - 1, J, I - 1, Lo, B2, B)
		% B is sorted from Lo .. I - 1.
	else
		A = A0,
		copy_run_ascending(CMP, A0, B0, B, Lo, Hi, I)
		% B is sorted from Lo .. I - 1.
	),
	trace [compile_time(flag("array_sort"))] (
		verify_sorted(CMP, B, Lo, I - 1),
		verify_identical(CMP, A, B, I, Hi)
	).

%-----------------------------------------------------------------------------%
% Merge sort

% mergesort_subarray(CMP, !A, !B, First, Last) 
:- pred mergesort_subarray(comparison_func(T)::in(comparison_func), 
	array(T)::array_di, array(T)::array_uo, 
	array(T)::array_di, array(T)::array_uo, 
	int::in, int::in) is det.

:- pragma type_spec(pred(mergesort_subarray/7), T = int).
:- pragma type_spec(pred(mergesort_subarray/7), T = string).

mergesort_subarray(CMP, !A, !B, Lo, Hi) :-
	(if Hi - Lo = 0 then
		true
	else
		Mid = Lo + (Hi - Lo) / 2,
		MidSucc = Mid + 1,
		mergesort_subarray(CMP, !B, !A, Lo, Mid), % Make this conjunction &?
		mergesort_subarray(CMP, !B, !A, MidSucc, Hi),
		merge_subarrays(CMP, !.B, Lo, Mid, MidSucc, Hi, 0, !A)
	).


%-----------------------------------------------------------------------------%
% Sorting utilities (mostly copied or adapted from the above samsort)

	% merges the two sorted consecutive subarrays Lo1 .. Hi1 and Lo2 .. Hi2
	% from A into the subarray starting at I in B.
	%
:- pred merge_subarrays(comparison_func(T)::in(comparison_func),
	array(T)::array_ui,
	int::in, int::in, int::in, int::in, int::in,
	array(T)::array_di, array(T)::array_uo) is det.

:- pragma type_spec(pred(merge_subarrays/9), T = int).
:- pragma type_spec(pred(merge_subarrays/9), T = string).

merge_subarrays(CMP, A, Lo1, Hi1, Lo2, Hi2, I, !B) :-
	( if Lo1 > Hi1 then
		unsafe_array_copy_range(A, Lo2, Hi2, I, !B)
	else if Lo2 > Hi2 then
		unsafe_array_copy_range(A, Lo1, Hi1, I, !B)
	else
		array.lookup(A, Lo1, X1),
		array.lookup(A, Lo2, X2),
		R = CMP(X1, X2),
		(
			(R = (<) ; R = (=) ),
			array.set(I, X1, !B),
			merge_subarrays(CMP, A, Lo1 + 1, Hi1, Lo2, Hi2, I + 1, !B)
		;
			R = (>),
			array.set(I, X2, !B),
			merge_subarrays(CMP, A, Lo1, Hi1, Lo2 + 1, Hi2, I + 1, !B)
		)
	).

%---------------------%

:- pred verify_sorted(comparison_func(T)::in(comparison_func), 
	array(T)::array_ui, int::in, int::in) is det.

verify_sorted(CMP, A, Lo, Hi) :-
	( if Lo >= Hi then
		true
	else if   CMP(A ^ elem(Lo + 1), A ^ elem(Lo)) = (<) then
		unexpected($pred, "array range not sorted")
	else
		verify_sorted(CMP, A, Lo + 1, Hi)
	).

:- pred verify_identical(comparison_func(T)::in(comparison_func), 
	array(T)::array_ui, array(T)::array_ui,
	int::in, int::in) is det.

verify_identical(CMP, A, B, Lo, Hi) :-
	( if Lo > Hi then
		true
	else if CMP(A ^ elem(Lo), B ^ elem(Lo)) = (=) then
		verify_identical(CMP, A, B, Lo + 1, Hi)
	else
		unexpected($pred, "array ranges not identical")
	).

%---------------------%

:- pred copy_run_ascending(comparison_func(T)::in(comparison_func), 
	array(T)::array_ui, array(T)::array_di, array(T)::array_uo, 
	int::in, int::in, int::out) is det.

:- pragma type_spec(pred(copy_run_ascending/7), T = int).
:- pragma type_spec(pred(copy_run_ascending/7), T = string).

copy_run_ascending(CMP, A, !B, Lo, Hi, I) :-
	( if
		Lo < Hi,
		(>) = CMP(A ^ elem(Lo), A ^ elem(Lo + 1))
	then
		I = search_until(CMP, (<), A, Lo, Hi),
		unsafe_array_copy_range_rev(A, Lo, I - 1, I - 1, !B)
	else
		I = search_until(CMP, (>), A, Lo, Hi),
		unsafe_array_copy_range(A, Lo, I - 1, Lo, !B)
	).

:- func search_until(comparison_func(T)::in(comparison_func), 
	comparison_result::in, array(T)::array_ui,
	int::in, int::in) = (int::out) is det.

:- pragma type_spec(func(search_until/5), T = int).
:- pragma type_spec(func(search_until/5), T = string).

search_until(CMP, R, A, Lo, Hi) =
	( if
		Lo < Hi,
		not R = CMP(A ^ elem(Lo), A ^ elem(Lo + 1))
	then
		search_until(CMP, R, A, Lo + 1, Hi)
	else
		Lo + 1
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