%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: array_util.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module array_util.

:- interface.

:- import_module array.
:- import_module map.



%-----------------------------------------------------------------------------%
% Array Manipulation

	% insert(Index, T, Source, Result) 
	% Copy an array and insert an element T at Index, shifting elements right
	% Throws an exception if Index is out of bounds
:- pred insert(int::in, T::in, array(T)::in, array(T)::array_uo) is det.
:- func insert(array(T)::in, int::in, T::in) = (array(T)::array_uo)
	is det.

	% Fails if index is out of bounds
:- pred semidet_insert(int::in, T::in, array(T)::in, array(T)::array_uo)
	is semidet.
:- func semidet_insert(array(T)::in, int::in, T::in) = (array(T)::array_uo)
	is semidet.

	% Does not check bounds 	
:- pred unsafe_insert(int::in, T::in, array(T)::in, array(T)::array_uo) 
	is det.
:- func unsafe_insert(array(T)::in, int::in, T::in) = 
	(array(T)::array_uo) is det.
	
	% delete(Index, Source, Result)
	% Remove an element from an array, shifting elements left
:- pred delete(int::in, array(T)::in, array(T)::array_uo) is det.
:- func delete(array(T)::in, int::in) = (array(T)::array_uo) is det.

:- pred unsafe_delete(int::in, array(T)::in, array(T)::array_uo) is det.
:- func unsafe_delete(array(T)::in, int::in) = (array(T)::array_uo) 
	is det.
	
	% delete_map(map, Source, Result)
	% Remove the key values of a map from an array, shifting elements left
:- pred delete_map(map(int, _)::in, array(T)::in, array(T)::array_uo)
	is det.
:- func delete_map(array(T)::in, map(int, _)::in) = (array(T)::array_uo)
	is det.

	% Does not check the bounds of indexes to be deleted, must not contain
	% duplicates
:- pred unsafe_delete_map(map(int, _)::in, array(T)::in, array(T)::array_uo)
	is det.
:- func unsafe_delete_map(array(T)::in, map(int, _)::in) = 
	(array(T)::array_uo) is det.
	
	% remove_dups_stable(Source, Result).
	% Take an unsorted array and remove duplicates, keeping the first 
	% (leftmost) instanace of an element and shifting to the right, the
	% order of unique elements is preserved
:- pred remove_dups_stable(array(T)::in, array(T)::array_uo) is det.
:- func remove_dups_stable(array(T)::in) = (array(T)::array_uo) is det.

	% array_[cons|snoc](T, Source, Result)
	% Insert T as the [first|last] element of Source 
:- pred array_cons(T::in, array(T)::in, array(T)::array_uo) is det.
:- func array_cons(array(T)::in, T::in) = (array(T)::array_uo) is det.

:- pred array_snoc(T::in, array(T)::in, array(T)::array_uo) is det.
:- func array_snoc(array(T)::in, T::in) = (array(T)::array_uo) is det.


	% copy_range(Source, SrcFirst, SrcLast, TgtFirst, !Array)
	% Copy elements from a Source array ranging from indexes SrcFirst to 
	% SrcLast to a target Array, starting at index TgtFirst
	% Throws an exception if any of the indexes are out of bounds.
:- pred copy_range(array(T)::in, int::in, int::in, int::in,
	array(T)::array_di, array(T)::array_uo) is det.

	% Unsafe version skips the bounds checks, will result in underfined 
	% behavior if any of the indexes are out of bounds
:- pred unsafe_copy_range(array(T)::in, int::in, int::in, int::in,
	array(T)::array_di, array(T)::array_uo) is det.
	
	% copy_range_rev(Source, SrcFirst, SrcL, TgtFirst, !Array)
	% As above, but copy the elements in reverse ordeer.
:- pred copy_range_rev(array(T)::in, int::in, int::in, int::in,
	array(T)::array_di, array(T)::array_uo) is det.

	% Unsafe version skips the bounds checks, may result in underfined behavior
:- pred unsafe_copy_range_rev(array(T)::in, int::in, int::in, int::in,
	array(T)::array_di, array(T)::array_uo) is det.

%-----------------------------------------------------------------------------%
% Sorting
	
	% remove_dups_sorted(Source, Result).
	% Take a sorted array and remove duplicates, the array MUST be sorted by 
	% the standard ordering
:- pred remove_dups_sorted(array(T)::array_di, array(T)::array_uo) is det.
:- func remove_dups_sorted(array(T)::array_di) = (array(T)::array_uo) is det.

	% Make a copy of the input array, sort and remove duplicates
:- pred sort_and_remove_dups(array(T)::in, array(T)::array_uo) is det.
:- func sort_and_remove_dups(array(T)::in) = (array(T)::array_uo) is det.

	% Succeed if the given array is sorted in ascending standard ordering
:- pred is_sorted(array(T)::in) is semidet.

	% Perform a linear search of an array, returning the index if found.
:- pred search(array(T)::in, T::in, int::out) is semidet.
:- func search(array(T), T) = int is semidet.

	% Perform a sort of the input array in a manner identical to the library 
	% standard sort/1 call, but provide a higher order comparison function
:- func samsort(comparison_func(T)::in(comparison_func), array(T)::array_di) = 
	(array(T)::array_uo) is det.
	
	% A traditional top down merge sort, should be stable to the original order	
:- func mergesort(comparison_func(T)::in(comparison_func), 
	array(T)::array_di) = (array(T)::array_uo) is det.
	
%-----------------------------------------------------------------------------%
% Higher order 

	% Left fold over items in an array, providing the array index
:- func index_fold(func(int, T, A) = A, array(T), A) = A.
:- mode index_fold(in(func(in, in, in) = out is det), in, in) = out is det.
:- mode index_fold(in(func(in, in, in) = out is semidet), in, in) = out 
	is semidet.
	
:- pred index_fold(func(int, T, A) = A, array(T), A, A).
:- mode index_fold(in(func(in, in, in) = out is det), in, in, out) is det.
:- mode index_fold(in(func(in, in, in) = out is semidet), in, in, out) 
	is semidet.
	
:- pred index_all_true(pred(int, T)::in(pred(in, in) is semidet), 
	array(T)::in) is semidet.

%---------------------%

:- type vfold_call(U, V, A) == (func(func(V, A) = A, U, A) = A).
:- inst vfold_call == (func(in(func(in, in) = out is det), in, in) = out 
	is det).

	% vfold_array(FoldFunction, FoldCall, Container, !.Array) = !:Array.
	% Pass Foldfunction through FoldCall as if it were a standard accumulator
	% function, workaround for the fact that arrays can not be passed via
	% modes, 
:- func vfold_array(func(V, array(T)) = array(T), vfold_call(U, V, array(T)),
	U, array(T)) = array(T).
	
:- mode vfold_array(in(func(in, array_di) = array_uo is det), 
	in(vfold_call),	in, array_di) = array_uo is det.
	
:- pred vfold_array(func(V, array(T)) = array(T), 
	vfold_call(U, V, array(T)), U, array(T), array(T)).

:- mode vfold_array(in(func(in, array_di) = array_uo is det), 
	in(vfold_call), in, array_di, array_uo) is det.
	
%---------------------%

:- type vfold2_call(U, V, A, B) == (pred(pred(V, A, A, B, B), U, A, A, B, B)).
:- inst vfold2_call == (pred(in(pred(in, in, out, in, out) is det), in, 
	in, out, in, out) is det).

	% vfold2_array(FoldFunction, FoldCall, Container, !Acc, !Array).
	% as above, but passing an additional accumulator 
:- pred vfold2_array(pred(V, A, A, array(T), array(T)), 
	vfold2_call(U, V, A, array(T)), U, A, A, array(T), array(T)).
	
:- mode vfold2_array(in(pred(in, in, out, array_di, array_uo) is det), 
	in(vfold2_call), in, in, out, array_di, array_uo) is det.

%---------------------%

:- type vfold3_call(U, V, A, B, C) == (pred(pred(V, A, A, B, B, C, C), U, 
	A, A, B, B, C, C)).
:- inst vfold3_call == 
	(pred(in(pred(in, in, out, in, out, in, out) is det), in, in, out, 
	in, out, in, out) is det).

	% vfold3_array(FoldFunction, FoldCall, Container, !Acc, !Array).
:- pred vfold3_array(pred(V, A, A, B, B, array(T), array(T)), 
	vfold3_call(U, V, A, B, array(T)), U, A, A, B, B, array(T), array(T)).
	
:- mode vfold3_array(in(pred(in, in, out, in, out, array_di, array_uo) is det), 
	in(vfold3_call), in, in, out, in, out, array_di, array_uo) is det.

%---------------------%
	
:- type kvfold_call(U, K, V, A) == (func(func(K, V, A) = A, U, A) = A).
:- inst kvfold_call == (func(in(func(in, in, in) = out is det), in, in) = out 
	is det).

	% kvfold_array(FoldFunction, FoldCall, Container, !.Array) = !:Array.
	% As above, but passing key value pairs	
:- func kvfold_array(func(K, V, array(T)) = array(T), 
	kvfold_call(U, K, V, array(T)), U, array(T)) = array(T).
	
:- mode kvfold_array(in(func(in, in, array_di) = array_uo is det), 
	in(kvfold_call), in, array_di) = array_uo is det.
	
:- pred kvfold_array(func(K, V, array(T)) = array(T), 
	kvfold_call(U, K, V, array(T)), U, array(T), array(T)).

:- mode kvfold_array(in(func(in, in, array_di) = array_uo is det), 
	in(kvfold_call), in, array_di, array_uo) is det.

%---------------------%
	
	% kvfold2_array(FoldFunction, FoldCall, Container, !.Array) = !:Array.
	% As above, but passing an additional accumulator
:- type kvfold2_call(U, K, V, A, B) == 
	(pred(pred(K, V, A, A, B, B), U, A, A, B, B)).
:- inst kvfold2_call == 
	(pred(in(pred(in, in, in, out, in, out) is det), in, in, out, in, out) 
	is det).
	
:- pred kvfold2_array(pred(K, V, A, A, array(T), array(T)), 
	kvfold2_call(U, K, V, A, array(T)), U, A, A, array(T), array(T)).

:- mode kvfold2_array(in(pred(in, in, in, out, array_di, array_uo) is det), 
	in(kvfold2_call), in, in, out, array_di, array_uo) is det.

%---------------------%
	
	% kvfold3_array(FoldFunction, FoldCall, Container, !.Array) = !:Array.
:- type kvfold3_call(U, K, V, A, B, C) == 
	(pred(pred(K, V, A, A, B, B, C, C), U, A, A, B, B, C, C)).
:- inst kvfold3_call == 
	(pred(in(pred(in, in, in, out, in, out, in, out) is det), in, in, out, 
	in, out, in, out) is det).
	
:- pred kvfold3_array(pred(K, V, A, A, B, B, array(T), array(T)), 
	kvfold3_call(U, K, V, A, B, array(T)), U, A, A, B, B, array(T), array(T)).

:- mode kvfold3_array(in(pred(in, in, in, out, in, out, array_di, array_uo) 
	is det), in(kvfold3_call), in, in, out, in, out, array_di, array_uo)
	is det.
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.


:- import_module int.
:- import_module list.
:- import_module unit.
:- import_module string.
% :- import_module solutions.
:- import_module require.
:- import_module exception.


:- import_module util.


%-----------------------------------------------------------------------------%
% Array Manipulation

insert(I, T, Src, insert(Src, I, T)).

insert(Src, I, T) = Result :-
	bounds(Src, Min, Max),
	(if I >= Min, I =< Max + 1 
	then
		unsafe_insert(I, T, Src, Result)
	else
		format_bounds_error($pred, "index %d not in range [%d, %d]",
		[i(I), i(Min), i(Max + 1)])
	).


semidet_insert(I, T, Src, semidet_insert(Src, I, T)).

semidet_insert(Src, I, T) = Result :-
	bounds(Src, Min, Max),
	I >= Min, 
	I =< Max + 1,
	unsafe_insert(I, T, Src, Result).

unsafe_insert(I, T, Src, unsafe_insert(Src, I, T)).

unsafe_insert(Src, I, T) = Result :-
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
			%unsafe_copy_range(Src, First, Last, Next,
			copy_range(Src, First, Last, Next,
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
			%unsafe_copy_range(Src, Current, Last, Next, !Array)
			copy_range(Src, Current, Last, Next, !Array)
		;
			Compare = (=),
			unsafe_set(Last + 1, Src ^ elem(Last), !Array)
		;
			Compare = (>),
			!:Array = !.Array		
		)
	).
	
delete(I, Src, delete(Src, I)).

delete(Src, I) = Result :-
	bounds(Src, Min, Max),
	(if I >= Min, I =< Max 
	then
		unsafe_delete(I, Src, Result)
	else
		format_bounds_error($pred, "index %d not in range [%d, %d]",
		[i(I), i(Min), i(Max)])
	).
	
unsafe_delete(I, Src, unsafe_delete(Src, I)).

unsafe_delete(Src, I) = Result :-
	Size = size(Src),
	NewSize = Size - 1,
	Last = max(Src),
	(if NewSize = 0
	then 
		Result = make_empty_array
	else if NewSize = 1
	then
		Remaining = (I = 0 -> 1 ; 0),
		init(1, Src ^ elem(Remaining), Result)
	else if I = Last
	then
		init(NewSize, Src ^ elem(0), Result0),
		%unsafe_copy_range(Src, 1, Last, 1, Result0, Result)
		copy_range(Src, 1, Last - 1, 1, Result0, Result)
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
				%unsafe_copy_range(Src, 1, I - 1, 1, Result0, Result1),
				copy_range(Src, 1, I - 1, 1, Result0, Result1),
				CpyStart = I
			)
		),
		%unsafe_copy_range(Src, I + 1, Last, CpyStart, Result1, Result)
		copy_range(Src, First, Last, CpyStart, Result1, Result)
	).
	
delete_map(Map, Src, delete_map(Src, Map)).	

delete_map(Src, Map) = 
	(if 
		promise_equivalent_solutions [Ob] 
			(det_min_key(Map)@Ob < Min ; det_max_key(Map)@Ob > Max)
	then
		format_bounds_error($pred, "index %d not in range [%d, %d]",
		[i(Ob), i(Min), i(Max)])
	else unsafe_delete_map(Src, Map) 
	) :- Min = min(Src), Max = max(Src).
	
unsafe_delete_map(Map, Src, unsafe_delete_map(Src, Map)).	
	
:- func first_kept_index(int, map(int, _), int) = int. 

first_kept_index(I, Map, FirstRemoval) = 
	(if Cmp = (<) ; Cmp = (>), not contains(Map, I)
	then I
	else first_kept_index(I + 1, Map, FirstRemoval)
	) 
:-
	compare(Cmp, I, FirstRemoval).

:- func last_kept_index(int, int) = int is semidet.

last_kept_index(I, FirstRemoval) = FirstRemoval - 1 :- I < FirstRemoval.
	
%- pred add_rest(Current, Last, SrcIndex, RemovedIndexes, Src, !Result).
:- pred add_rest(int::in, int::in, int::in, map(int, U)::in, map(int, U)::out,
	array(T)::in, array(T)::array_di, array(T)::array_uo) is det.
	
add_rest(Current, Last, SrcIndex, !Map, Src, !Result) :-
	FirstRemoval = det_min_key(!.Map),
	(if Current > Last then true
	else 
		(if RangeLast = last_kept_index(SrcIndex, FirstRemoval)
		then
			RangeFirst = first_kept_index(SrcIndex, !.Map, FirstRemoval),
			(if RangeFirst = RangeLast
			then
				unsafe_lookup(Src, RangeFirst, Element),
				unsafe_set(Current, Element, !Result),
				Width = 1
			else
				unsafe_copy_range(Src, RangeFirst, RangeLast, Current, 
					!Result),
				Width = RangeLast - RangeFirst + 1 %num elements in range
			),
			% We know for a fact that the next SrcIndex is at or past the next
			% element to be removed, we can skip to removing it from the map
			% we just need to properly index past the elements we copied in 
			% both the source and result arrays
			Next = Current + Width,
			%skip past elements we copied, plus the next source element, 
			% because we know it will be removed
			NextSrcIndex = SrcIndex + Width + 1 
		else
			% The SrcIndex is past the next element to be remvoved, we need to
			% remove the lowest element from the map until this is no longer the 
			% case
			Next = Current,
			NextSrcIndex = SrcIndex
		),
		% We've either copied all of the elements up to FirstRemoval, or we are
		% past it, in either cast we need to remove it from the map, and if 
		% there are no more elements in the map, yet empty elements in the Result
		% array, we can copy the rest of the source array, otherwise loop
		delete(FirstRemoval, !Map),
		(if is_empty(!.Map)
		then
			(if Next > Last then true %Stop here if we've already at the end
			else
				FinalWidth = Last - Next + 1, % number of target elements left
				FinalFirst = FirstRemoval + 1, % element after the last removed
				FinalLast = FinalFirst + FinalWidth,
				unsafe_copy_range(Src, FinalFirst, FinalLast, Next, !Result)
			)
		else
			add_rest(Next, Last, NextSrcIndex, !Map, Src, !Result)	
		)
	).

unsafe_delete_map(Src, Map) = Result :- 
	( if is_empty(Map)
	then Result = copy(Src)
	else
		Size = size(Src),
		NewSize = Size - count(Map),
		(if NewSize = 0
		then 
			Result = make_empty_array
		else 
			FirstNew = first_kept_index(0, Map, det_min_key(Map)),
			(if NewSize = 1
			then
				array.init(1, unsafe_elem(FirstNew, Src), Result)
			else 
				array.init(NewSize, unsafe_elem(FirstNew, Src), Result0),
				add_rest(1, max(Result0), FirstNew + 1, Map, _, Src, 
					Result0, Result)
			)
		)
	).
	
 :- pred remove_dups_fold(T::in, int::in, int::out, map(T, unit)::in, 
	map(T, unit)::out, array(T)::array_di, array(T)::array_uo) is det.
	
remove_dups_fold(Element, !Next, !Unique, !Array) :-
	(if insert(Element, unit, !Unique)
	then
		unsafe_set(!.Next, Element, !Array),
		!:Next = !.Next + 1
	else true
	).
	
remove_dups_stable(Src, remove_dups_stable(Src)).

remove_dups_stable(Src) = Result :-
	Size = size(Src),
	(if Size =< 1
	then Result = copy(Src)
	else
		unsafe_lookup(Src, min(Src), FirstElem),
		Array0 = init(Size, FirstElem),
		vfold3_array(remove_dups_fold, det_fold3, Src, 1, NewSize, 
			singleton(FirstElem, unit), _, Array0, Array),
		shrink(NewSize, Array, Result)
	).
	
:- pred det_fold3(pred(T, A, A, B, B, C, C), array(T), A, A, B, B, C, C).
:- mode det_fold3(in(pred(in, in, out, in, out, in, out) is det), in,
	in, out, in, out, in, out) is det.
	
det_fold3(P, Array, !A, !B, !C) :- foldl3(P, Array, !A, !B, !C).
	
array_cons(T, Src, array_cons(Src, T)).

array_cons(Src, T) = unsafe_insert(Src, 0, T).

array_snoc(T, Src, array_snoc(Src, T)).

array_snoc(Src, T) = unsafe_insert(Src, max(Src) + 1, T).

copy_range(Src, SrcF, SrcL, TgtF, !Array) :-
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
		unsafe_copy_range(Src, SrcF, SrcL, TgtF, !Array)
	).

unsafe_copy_range(Src, SrcF, SrcL, TgtF, !Array) :-
	unsafe_set(TgtF, Src ^ elem(SrcF), !Array),
	(if SrcF < SrcL
	then 
		unsafe_copy_range(Src, SrcF + 1, SrcL, TgtF + 1, !Array)
	else
		true
	).
	

copy_range_rev(Src, SrcF, SrcL, TgtF, !Array) :-
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
		unsafe_copy_range_rev(Src, SrcF, SrcL, TgtF, !Array)
	).

unsafe_copy_range_rev(Src, SrcF, SrcL, TgtF, !Array) :-
	unsafe_set(TgtF, Src ^ elem(SrcL), !Array),
	(if SrcF < SrcL
	then 
		unsafe_copy_range(Src, SrcF, SrcL - 1, TgtF + 1, !Array)
	else
		true
	).

%-----------------------------------------------------------------------------%
% Sorting
	
remove_dups_sorted(!A) :- 
	(if size(!.A, 0)
	then true
	else
		unsafe_lookup(!.A, 0, First),
		remove_dups_sorted(1, First, 1, NewSize, !A),
		shrink(NewSize, !A)
	).

:- pred remove_dups_sorted(int::in, T::in, int::in, int::out, array(T)::array_di, 
	array(T)::array_uo) is det.
	
remove_dups_sorted(Index, Current, !Unique, !A) :-
	(if Index > max(!.A)
	then true
	else
		unsafe_lookup(!.A, Index, Next),
		(if Current = Next
		then remove_dups_sorted(Index + 1, Current, !Unique, !A)
		else
			unsafe_set(!.Unique, Next, !A),
			!:Unique = !.Unique + 1,
			remove_dups_sorted(Index + 1, Next, !Unique, !A)
		)
	).

remove_dups_sorted(!.A) = !:A :- remove_dups_sorted(!A).

sort_and_remove_dups(!A) :- array.copy(!A), !:A = array.sort(!.A), 
	remove_dups_sorted(!A).

sort_and_remove_dups(!.A) = !:A :- sort_and_remove_dups(!A).


:- pred is_sorted(array(T)::in, T::in, int::in, int::in) is semidet.

is_sorted(A, Current, Index, Last) :-
	(if Index > Last
	then true
	else
		unsafe_lookup(A, Index, Next),
		Current @=< Next,
		is_sorted(A, Next, Index + 1, Last)
	).
	
is_sorted(A) :- 
	(if size(A, 0) 
	then true
	else
		unsafe_lookup(A, 0, First),
		is_sorted(A, First, 1, max(A))
	).

search(A, T, I) :- search(A, T, 0, I).

search(A, T) = I :- search(A, T, I).

:- pred search(array(T)::in, T::in, int::in, int::out) is semidet.

search(A, T, Current, I) :-
	I < max(A),
	(if unsafe_lookup(A, Current, T)
	then
		I = Current
	else
		search(A, T, Current + 1, I)
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
		unsafe_copy_range(A, Lo2, Hi2, I, !B)
	else if Lo2 > Hi2 then
		unsafe_copy_range(A, Lo1, Hi1, I, !B)
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
		unsafe_copy_range_rev(A, Lo, I - 1, I - 1, !B)
	else
		I = search_until(CMP, (>), A, Lo, Hi),
		unsafe_copy_range(A, Lo, I - 1, Lo, !B)
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
% Higher order 

:- func for_fold(int, int, func(int, T, A) = A, array(T), A) = A.
:- mode for_fold(in, in, in(func(in, in, in) = out is det), in, in) = out 
	is det.
:- mode for_fold(in, in, in(func(in, in, in) = out is semidet), in, in) = out 
	is semidet.

for_fold(Current, Last, F, Array, Acc) = 
	(if Current > Last 
	then Acc
	else for_fold(Current + 1, Last, F, Array, NextAcc)
	)
:-
	NextAcc = F(Current, unsafe_elem(Current, Array), Acc).
	
index_fold(F, Array, Acc) = for_fold(min(Array), max(Array), F, Array, Acc).

index_fold(F, Array, Acc, index_fold(F, Array, Acc)).

:- pred for_all_true(int::in, int::in, 
	pred(int, T)::in(pred(in, in) is semidet), array(T)::in) is semidet.
	
for_all_true(Current, Last, P, Array) :-
	(if Current > Last then true
	else
		P(Current, unsafe_elem(Current, Array)),
		for_all_true(Current + 1, Last, P, Array)
	).
	
index_all_true(P, Array) :- for_all_true(min(Array), max(Array), P, Array).


%---------------------%


	% Mercury cheats uniqueness for arrays, so can we.
	%
	% The compiler would *not* allow us to pull something like this for
	% actually unique insted variables, but the array library's unqiue modes
	% are a work around, aliased to the 'ground' inst, allowing us to
	% pass 'ground' insted arrays as array_uniq without complaint from the mmc
	% ... just don't do this with any array you want to treat as immutable.
	% see also:  array.m line 58
:- func coerce_uniq_array(T::in) = (T::array_uo).

coerce_uniq_array(T) = T.

%---------------------%

:- func wrap_array_acc(func(V, A) = A, V, A) = A.
:- mode wrap_array_acc(in(func(in, array_di) = array_uo is det), in, in) = out
	is det.

wrap_array_acc(F, V, A) = F(V, coerce_uniq_array(A)).

vfold_array(F, Call, U, A) = coerce_uniq_array(Call(wrap_array_acc(F), U, A)).

vfold_array(F, Call, U, A, vfold_array(F, Call, U, A)).

%---------------------%

:- pred wrap_array_acc(pred(V, A, A, B, B), V, A, A, B, B).
:- mode wrap_array_acc(in(pred(in, in, out, array_di, array_uo) is det), 
	in, in, out, in, out) is det.

wrap_array_acc(P, V, !Acc, !A) :- 
	P(V, !Acc, coerce_uniq_array(!.A), !:A).

vfold2_array(P, Call, U, !Acc, !.A, coerce_uniq_array(!:A)) :- 
	Call(wrap_array_acc(P), U, !Acc, !A).

%---------------------%

:- pred wrap_array_acc(pred(V, A, A, B, B, C, C), V, A, A, B, B, C, C).
:- mode wrap_array_acc(in(pred(in, in, out, in, out, array_di, array_uo)
	is det), in, in, out, in, out, in, out) is det.

wrap_array_acc(P, V, !Acc, !Acc2, !A) :- 
	P(V, !Acc, !Acc2, coerce_uniq_array(!.A), !:A).

vfold3_array(P, Call, U, !Acc, !Acc2, !.A, coerce_uniq_array(!:A)) :- 
	Call(wrap_array_acc(P), U, !Acc, !Acc2, !A).

%---------------------%

:- func wrap_array_acc(func(K, V, A) = A, K, V, A) = A.
:- mode wrap_array_acc(in(func(in, in, array_di) = array_uo is det), in, in, 
	in) = out is det.

wrap_array_acc(F, K, V, A) = F(K, V, coerce_uniq_array(A)).

kvfold_array(F, Call, U, A) = coerce_uniq_array(Call(wrap_array_acc(F), U, A)).

kvfold_array(F, Call, U, A, kvfold_array(F, Call, U, A)).

%---------------------%

:- pred wrap_array_acc(pred(K, V, A, A, B, B), K, V, A, A, B, B).
:- mode wrap_array_acc(in(pred(in, in, in, out, array_di, array_uo) is det), 
	in, in, in, out, in, out) is det.

wrap_array_acc(P, K, V, !Acc, !A) :- 
	P(K, V, !Acc, coerce_uniq_array(!.A), !:A).

kvfold2_array(P, Call, U, !Acc, !.A, coerce_uniq_array(!:A)) :- 
	Call(wrap_array_acc(P), U, !Acc, !A).

%---------------------%

:- pred wrap_array_acc(pred(K, V, A, A, B, B, C, C), K, V, A, A, B, B, C, C).
:- mode wrap_array_acc(in(pred(in, in, in, out, in, out, array_di, array_uo) 
	is det), in, in, in, out, in, out, in, out) is det.

wrap_array_acc(P, K, V, !Acc, !Acc2, !A) :- 
	P(K, V, !Acc, !Acc2, coerce_uniq_array(!.A), !:A).

kvfold3_array(P, Call, U, !Acc, !Acc2, !.A, coerce_uniq_array(!:A)) :- 
	Call(wrap_array_acc(P), U, !Acc, !Acc2, !A).
