%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_ordered_term_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_ordered_term_map(T).

:- interface.

:- import_module list.
:- import_module array.

:- import_module ordered_set.

:- import_module mh_tuple.

%-----------------------------------------------------------------------------%
% Ordered set

:- type mh_ordered_term_map(T). % where comparison is compare_ordered_term_sets.

%-----------------------------------------------------------------------------%
% Basic operations

% Sanity check, ensure the sorted array is actually congruent with the ordered
% Array
:- pred is_valid(mh_ordered_term_map(T)::in) is semidet.

:- func empty_set = mh_ordered_term_map(T).

:- pred empty_set(mh_ordered_term_map(T)::out) is det.

:- pred is_empty(mh_ordered_term_map(T)::in) is semidet.

:- func singleton(T) = mh_ordered_term_map(T).

:- pred singleton(T::in, mh_ordered_term_map(T)::out) is det.

:- pred is_singleton(mh_ordered_term_map(T)::in) is semidet.

% True if the order of the elements are sorted, even with duplicates.
:- pred is_sorted(mh_ordered_term_map(T)::in) is semidet.

% Sort a given set without removing elements.
:- func sort(mh_ordered_term_map(T)) = mh_ordered_term_map(T).

% If the order is sorted and has no duplicates
:- pred is_sorted_set(mh_ordered_term_map(T)::in) is semidet. 

% True if the internal ordered and sorted arrays refer to the same array, 
% Offers a cheaper incomplete alternative test to is_sorted_set/1

:- pred is_merged_set(mh_ordered_term_map(T)::in) is semidet.

% Return the number of elements in the ordered portion of the set
:- func size(mh_ordered_term_map(T)) = int.
:- pred size(mh_ordered_term_map(T)::in, int::out) is det.


% Return the number of unique elements in the set
:- func unique_elements(mh_ordered_term_map(T)) = int.
:- pred unique_elements(mh_ordered_term_map(T)::in, int::out) is det.

% Compares the ordered values in the set, including duplicates, succeeds if
% the set contains the same values in the same order. 
:- pred equal(mh_ordered_term_map(T)::in, mh_ordered_term_map(T)::in) is semidet.



% Succeeds if the sets have the same values, with duplicates removed
% Equivalent to compare_ordered_term_sets((=)).
:- pred equivalent(mh_ordered_term_map(T)::in, mh_ordered_term_map(T)::in) 
	is semidet.
	


% Sorts and removes duplicates from the sets and compares them according to
% the standard ordering. Used to implement mercury equality and comparison.
% This is neccecary in order to efficiently implement maps and other data
% structures for this type as *sets* ... if you need to map the values in
% the set by ordering, convert the set to an ordered list or array first.
:- pred compare_ordered_term_maps(comparison_result::uo, 
	mh_ordered_term_map(T)::in, mh_ordered_term_map(T)::in) is det.
	



%-----------------------------------------------------------------------------%
% Conversion

:- func from_tuple(mh_tuple) = mh_ordered_term_map(T).
:- func to_tuple(mh_ordered_term_map(T)) = mh_tuple.

:- func from_ordered_set(ordered_set(mh_term)) = mh_ordered_term_map(T).
:- func to_ordered_set(mh_ordered_term_map(T)) = ordered_set(mh_term).

:- func from_list(list(mh_term)) = mh_ordered_term_map(T).
:- func to_list(mh_ordered_term_map(T)) = list(mh_term).

:- func from_array(array(mh_term)) = mh_ordered_term_map(T).
:- func to_array(mh_ordered_term_map(T)) = array(mh_term).

% Sorts and removes duplicates (if any) from the set, the assigned ordering
% is ignored

:- func to_sorted_list(mh_ordered_term_map(T)) = list(mh_term).
:- func to_sorted_array(mh_ordered_term_map(T)) = array(mh_term).

	
%-----------------------------------------------------------------------------%
% Lookup

% Unlike arrays, these operations are 1 indexed, the 'set_' variants of each
% call are equivalent, but operate on the sorted set version, not the ordered

% Return the maximum and minimum valid indexes for the order, return -1 for 
% both values if empty set
:- pred bounds(mh_ordered_term_map(T)::in, int::out, int::out) is det.
:- pred set_bounds(mh_ordered_term_map(T)::in, int::out, int::out) is det.

% As above, but fail on emtpy set
:- pred semidet_bounds(mh_ordered_term_map(T)::in, int::out, int::out) is semidet.
:- pred semidet_set_bounds(mh_ordered_term_map(T)::in, int::out, int::out) is semidet.

% Succeed if the given index is in bounds for the given mh_ordered_term_map(T)
:- pred in_bounds(mh_ordered_term_map(T)::in, int::in) is semidet.
:- pred in_set_bounds(mh_ordered_term_map(T)::in, int::in) is semidet.

% Return the first index (should always be 1) unless the set is empty then -1
:- func min(mh_ordered_term_map(T)) = int is det.
:- pred min(mh_ordered_term_map(T)::in, int::out) is det.

:- func semidet_min(mh_ordered_term_map(T)) = int is semidet.
:- pred semidet_min(mh_ordered_term_map(T)::in, int::out) is semidet.

:- func set_min(mh_ordered_term_map(T)) = int is det.
:- pred set_min(mh_ordered_term_map(T)::in, int::out) is det.

:- func semidet_set_min(mh_ordered_term_map(T)) = int is semidet.
:- pred semidet_set_min(mh_ordered_term_map(T)::in, int::out) is semidet.

% Return the last index
:- func max(mh_ordered_term_map(T)) = int is det.
:- pred max(mh_ordered_term_map(T)::in, int::out) is det.

:- func semidet_max(mh_ordered_term_map(T)) = int is semidet.
:- pred semidet_max(mh_ordered_term_map(T)::in, int::out) is semidet.

:- func set_max(mh_ordered_term_map(T)) = int is det.
:- pred set_max(mh_ordered_term_map(T)::in, int::out) is det.

:- func semidet_set_max(mh_ordered_term_map(T)) = int is semidet.
:- pred semidet_set_max(mh_ordered_term_map(T)::in, int::out) is semidet.

:- pred contains(mh_ordered_term_map(T)::in, mh_term::in) is semidet.


% Lookup the ordered value at the given index (starting at 1), throws an
% exception if index is out of bound
:- func lookup(mh_ordered_term_map(T), int) = mh_term is det.
:- pred lookup(mh_ordered_term_map(T)::in, int::in, mh_term::out) is det.

:- func set_lookup(mh_ordered_term_map(T), int) = mh_term is det.
:- pred set_lookup(mh_ordered_term_map(T)::in, int::in, mh_term::out) is det.

% Search for the value and return it's index in the ordered array (linear)
:- func search(mh_ordered_term_map(T), mh_term) = int is semidet.
:- pred search(mh_ordered_term_map(T)::in, mh_term::in, int::out) is semidet.

% Search for the value and return it's index in the sorted set. (log N)
:- func set_search(mh_ordered_term_map(T), mh_term) = int is semidet.
:- pred set_search(mh_ordered_term_map(T)::in, mh_term::in, int::out) is semidet.



%-----------------------------------------------------------------------------%
% Ordering

% Creates a new  ordered set by sorting the members of the sorted set using the
% provided comparison function. The original ordering is discarded, and
% duplicates according to the standard ordering are removed.  
% Stable/predictable ordering where the comparison func returns equality is not
% garunteed.
:- pred order_by(comparison_func(mh_term)::in(comparison_func), 
	mh_ordered_term_map(T)::in, mh_ordered_term_map(T)::out) is det.
	
:- func order_by(comparison_func(mh_term)::in(comparison_func), 
	mh_ordered_term_map(T)::in) = (mh_ordered_term_map(T)::out) is det.

% Create a new orddered set using the
% provided comparison function that preserves the original order when the
% comparison function returns equality. Does not remove duplicates.
:- pred reorder_by(comparison_func(mh_term)::in(comparison_func),
	mh_ordered_term_map(T)::in,	mh_ordered_term_map(T)::out) is det.
	
:- func reorder_by(comparison_func(mh_term)::in(comparison_func), 
	mh_ordered_term_map(T)::in)	= (mh_ordered_term_map(T)::out) is det.

% TODO: Move ordering into it's own module?

% in order for an ordering to be valid for a given set, it must contain at
% least one index for every unique member of it's sorted set, and no indexes
% that are out of the bounds of the sorted set.

:- pred valid_ordering_for(ordering::in, mh_ordered_term_map(T)::in) is semidet.

% Return the current ordering of the given set, returns an empty array if
% The input set is empty.

:- func current_ordering(mh_ordered_term_map(T)) = ordering.
:- pred current_ordering(mh_ordered_term_map(T)::in, ordering::out) is det.

% Attempt to create a new ordered set from the sorted set of the provided set
% The original ordering of the input set will be ignored, fails if the ordering
% is not valid for the given set. 
% (Should be) More efficient than calling valid_ordering_for/2 first.

:- func apply_ordering(mh_ordered_term_map(T), ordering) = mh_ordered_term_map(T)
	is semidet.
:- pred apply_ordering(ordering::in, mh_ordered_term_map(T)::in, 
	mh_ordered_term_map(T)::out) is semidet.
	
% As above, but throws an exception if the ordering is not valid.

:- func det_apply_ordering(mh_ordered_term_map(T), ordering) = mh_ordered_term_map(T).
:- pred det_apply_ordering(ordering::in, mh_ordered_term_map(T)::in,  
	mh_ordered_term_map(T)::out) is  det. 


%-----------------------------------------------------------------------------%
% Set operations


% The union of two sets sorted and without duplicates, order is not preserved
:- pred union(mh_ordered_term_map(T)::in, mh_ordered_term_map(T)::in,
	mh_ordered_term_map(T)::out) is det.
:- func union(mh_ordered_term_map(T), mh_ordered_term_map(T)) = mh_ordered_term_map(T).


% The intersection of two sets sorted and without duplicates, order is not 
% preserved
:- pred intersect(mh_ordered_term_map(T)::in, mh_ordered_term_map(T)::in,
	mh_ordered_term_map(T)::out) is det.
:- func intersect(mh_ordered_term_map(T), mh_ordered_term_map(T))
	= mh_ordered_term_map(T).

% The difference of two sets sorted and without duplicates, 
% order is not preserved
:- pred difference(mh_ordered_term_map(T)::in, mh_ordered_term_map(T)::in, 
	mh_ordered_term_map(T)::out) is det.
:- func difference(mh_ordered_term_map(T), mh_ordered_term_map(T))
	= mh_ordered_term_map(T).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module bool.
:- import_module require.

:- import_module util.


% Variable naming
% A == Array
% O == Ordered Array
% M == Sorted Set Array
% OM == Ordered Set (not deconstructed)
% CMP == Comparison Function
% R == Comparison Result
% T == Term
% I == Index
% Or == Ordering


%-----------------------------------------------------------------------------%
% Ordered set

:- type mh_ordered_term_map(T) 
	--->	ordered_term_map(order::array(T), map::mh_term_map(T))
		where comparison is compare_ordered_term_maps.

% Deterministic constructor
:- func om(array(T), array(T)) = mh_ordered_term_map(T).
:- mode om(in, in) = out is det.
:- mode om(out, out) = in is det.

:- pragma promise_equivalent_clauses(os/2).

om(O::in, M::in) = (ordered_term_map(O, M)::out).

% Deconstructions on types with user defined equality are cc_multi
om(O::out, M::out) = (OM::in) :- 
	promise_equivalent_solutions [O, M] ordered_term_map(O, M) = OM. 

:- pragma inline(om/2).


%-----------------------------------------------------------------------------%
% Basic operations


is_valid(om(A, sort_and_remove_dups(A))).

empty_set = OM :- empty_set(OM).

empty_set(om(A, A)) :- make_empty_array(A).


is_empty(om(A, _)) :- size(A, 0).

singleton(T) = om(A, A) :- init(1, T, A).

singleton(T, singleton(T)).

is_singleton(om(A, _)) :- size(A, 1).

is_sorted(om(A, _)) :- 
	(if size(A, 0) 
	then true
	else
		unsafe_lookup(A, 0, First),
		is_sorted(A, First, 1)
	).

:- pred is_sorted(array(T)::in, T::in, int::in) is semidet.

is_sorted(A, Last, Index) :-
	(if Index > max(A)
	then true
	else
		unsafe_lookup(A, Index, Next),
		Last @=< Next,
		is_sorted(A, Next, Index + 1)
	).
	
sort(om(O, M)) = om(sort(copy(O)), M).

is_sorted_set(om(A, A)).

is_merged_set(om(O, M)) :- private_builtin.pointer_equal(O, M).

size(om(A, _)) = size(A).
size(OM, size(OM)).
	
unique_elements(om(_, A)) = size(A).
unique_elements(OM, unique_elements(OM)).

equal(om(A, _), om(A, _)).

equivalent(om(_, A), om(_, A)).

compare_ordered_term_sets(array_compare(A1, A2), om(_, A1), 	om(_, A2)).


%-----------------------------------------------------------------------------%
% Conversion

from_list(L) = from_array(array.from_list(L)).

to_list(om(O, _)) = array.to_list(O). % CC multi?

from_array(A) = om(A, sort_and_remove_dups(A)).

to_array(om(O, _)) = O.

to_sorted_list(om(_, M)) = array.to_list(M).

to_sorted_array(om(_, M)) = M.

%-----------------------------------------------------------------------------%
% Lookup

bounds(om(A, _), Min, Max) :-
	(if size(A) = 0
	then
		Min@Max = -1
	else
		Min = array.min(A) + 1,
		Max = array.max(A) + 1
	).
	
set_bounds(om(_, A), Min, Max) :-
	(if size(A) = 0
	then
		Min@Max = -1
	else
		Min = array.min(A) + 1,
		Max = array.max(A) + 1
	).
	
semidet_bounds(om(A, _), Min, Max) :-
	size(A) > 0,
	Min = array.min(A) + 1,
	Max = array.max(A) + 1.
	
semidet_set_bounds(om(_, A), Min, Max) :-
	size(A) > 0,
	Min = array.min(A) + 1,
	Max = array.max(A) + 1.

in_bounds(om(O, _), I) :- array.in_bounds(O, I - 1).
in_set_bounds(om(_, M), I) :- array.in_bounds(M, I - 1).

	
min(om(A, _)) = 
	(if size(A) = 0
	then
		-1
	else
		array.min(A) + 1
	).
	
min(OM, min(OM)).

semidet_min(om(A, _)) = array.min(A) :- size(A) > 0.

semidet_min(OM, semidet_min(OM)).

set_min(om(_, A)) = 
	(if size(A) = 0
	then
		-1
	else
		array.min(A) + 1
	).
	
set_min(OM, min(OM)).

semidet_set_min(om(_, A)) = array.min(A) :- size(A) > 0.

semidet_set_min(OM, semidet_min(OM)).
	
max(om(A, _)) = 
	(if size(A) = 0
	then
		-1
	else
		array.max(A) + 1
	).
	
max(OM, max(OM)).

semidet_max(om(A, _)) = array.max(A) :- size(A) > 0.

semidet_max(OM, semidet_max(OM)).

set_max(om(_, A)) = 
	(if size(A) = 0
	then
		-1
	else
		array.max(A) + 1
	).
	
set_max(OM, max(OM)).

semidet_set_max(om(_, A)) = array.max(A) :- size(A) > 0.

semidet_set_max(OM, semidet_max(OM)).

contains(OM, T) :- search(OM, T, _).

% index and set_index implementations here

lookup(om(A, _), Index) = array.lookup(A, Index - 1).
lookup(OM, Index, lookup(OM, Index)).
set_lookup(om(_, A), Index) = array.lookup(A, Index - 1).
set_lookup(OM, Index, set_lookup(OM, Index)).

search(om(O, _), T) = array_search(O, T) + 1.
search(OM, T, search(OM, T)).

set_search(om(_, M), T) = I + 1 :- binary_search(M, T, I). 
set_search(OM, T, set_search(OM, T)).

%-----------------------------------------------------------------------------%
% Ordering

order_by(CMP, OM, order_by(CMP, OM)).

order_by(CMP, om(_, M)) = om(O@samsort(CMP, copy(M)), sort_and_remove_dups(O)).


reorder_by(CMP, OM, reorder_by(CMP, OM)).

reorder_by(CMP, om(O0, _)) = 
	om(O@mergesort(CMP, copy(O0)), sort_and_remove_dups(O)). 
	
ordering_to_list(Or) = array.to_list(Or).
ordering_from_list(L) = array.array(L).

valid_ordering_for(Or, om(_, M)) :-
	array.size(M, SetSize),
	array.init(SetSize, no, UniqueFound),
	valid_ordering_check(0, array.max(Or), Or, 0, UniqueCount, UniqueFound, _),
	UniqueCount = SetSize. % Ordering is only valid if all are found
		
%valid_ordering_check(Index, Last, Or, !UniqueCount, !UniqueFound)
% Count every element in the Ordered array that has not yet been found to be
% in bounds of the Sorted array
:- pred valid_ordering_check(int::in, int::in, array(int)::in, int::in, 
	int::out, array(bool)::array_di, array(bool)::array_uo) is semidet.
	
valid_ordering_check(Index, Last, Or, !UniqueCount, !UniqueFound) :-
	(if Index > Last then 
		true
	else
		array.lookup(Or, Index, Is),
		%fails if out of bounds of the Sorted array
		array.semidet_lookup(!.UniqueFound, Is, Counted), 
		(if Counted = no then
			array.set(Is, yes, !UniqueFound),
			!:UniqueCount = !.UniqueCount + 1
		else
			true
		),
		valid_ordering_check(Index + 1, Last, Or, !UniqueCount, !UniqueFound)
	).
	
current_ordering(om(O, M)) = array.generate(size(O), generate_ordering(O, M)).

:- func generate_ordering(array(T), array(T), int) = int.

generate_ordering(O, M, Io) = Is :-
	array.lookup(O, Io, T),
	(if binary_search(M, T, Found) then
		Is = Found + 1
	else
		unexpected($module, $pred,
"mh_ordered_term_map(T).current_ordering: Value in ordered set not found in sorted set")
	).

current_ordering(OM, current_ordering(OM)).

apply_ordering(om(_, M), Or) = OM :-
	size(M, SortedSize),
	size(Or, OrderedSize),
	(if SortedSize = 0	then
		 OrderedSize = 0,
		OM = empty_set
	else 
		% Find first element
		semidet_lookup(Or, 0, For),
		semidet_lookup(M, Fs@For - 1, First),
		array.init(SortedSize, no, UniqueFound0),
		array.set(Fs, yes, UniqueFound0, UniqueFound), 
		(if OrderedSize = 1 then
			SortedSize = 1,
			singleton(First, OM)
		else
			array.init(OrderedSize, First, NewOrder),
			generate_order_from_ordering(Or, M, 1, max(Or), 1, SortedSize,
				UniqueFound, _, NewOrder, O),
			OM = om(O, M)
		
		)
	).
	
apply_ordering(Or, OM, apply_ordering(OM, Or)).
	
	
:- pred generate_order_from_ordering(array(int)::in, array(T)::in, int::in, 
	int::in, int::in, int::out,  array(bool)::array_di, array(bool)::array_uo,
	array(T)::array_di, array(T)::array_uo) is semidet.

generate_order_from_ordering(Or, M, I, Last, !UniqueCount, !UniqueFound, !O) :-
	(if I > Last then
		true
	else
		array.lookup(Or, I, Ior),
		array.semidet_lookup(M, Is@(Ior - 1), T),
		array.set(I, T, !O),
		array.semidet_lookup(!.UniqueFound, Is, Counted), 
		(if Counted = no then
			array.set(Is, yes, !UniqueFound),
			!:UniqueCount = !.UniqueCount + 1
		else
			true
		),
		generate_order_from_ordering(Or, M, I + 1, Last, !UniqueCount, 
			!UniqueFound, !O)
	).
	
det_apply_ordering(OS0, Or) =  
	(if apply_ordering(OS0, Or) = OM
	then
		OM
	else
		unexpected($module, $pred, "Invalid ordering.")
	).
	
det_apply_ordering(Or, OM, det_apply_ordering(OM, Or)).
	
%-----------------------------------------------------------------------------%
% Set operations

union(OS1, OS2, union(OS1, OS2)).

union(om(_, S1), om(_, S2)) = om(S3, S3) :-
	Unsorted = array.append(S1, array(difference_list(S1, S2))),
	S3 = array.sort(Unsorted).

intersect(OS1, OS2, intersect(OS1, OS2)).
	
intersect(om(_, S1), om(_, S2)) = om(S3, S3) :- 
	S3 = array(intersect_list(S1, S2)).


difference(OS1, OS2, difference(OS1, OS2)).
	
difference(om(_, S1), om(_, S2)) = om(S3, S3) :- 
	S3 = array(difference_list(S1, S2)).

% Compose a list of elements not present in the first array. 
% Arrays must be sorted.
:- func difference_list(array(T), array(T)) = list(T).

difference_list(A1, A2) = difference_list(0, max(A2), A1, A2).

:- func difference_list(int, int, array(T), array(T)) = list(T).

difference_list(I, Last, A1, A2) = 
	(if I > Last then
		[]
	else
		(if lookup(A2, I, T), not binary_search(A1, T, _) then
			[ T | difference_list(I + 1, Last, A1, A2)]
		else
			difference_list(I + 1, Last, A1, A2)
		)
	).
	
:- pragma inline(difference_list/4).

% Compose a list of elements found in both sorted arrays.
:- func intersect_list(array(T), array(T)) = list(T).

intersect_list(A1, A2) = difference_list(0, max(A2), A1, A2).

:- func intersect_list(int, int, array(T), array(T)) = list(T).

intersect_list(I, Last, A1, A2) = 
	(if I > Last then
		[]
	else
		(if lookup(A2, I, T), binary_search(A1, T, _) then
			[ T | intersect_list(I + 1, Last, A1, A2)]
		else
			intersect_list(I + 1, Last, A1, A2)
		)
	).
