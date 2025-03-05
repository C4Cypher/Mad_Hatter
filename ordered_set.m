%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: ordered_set.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module ordered_set.

:- interface.

:- import_module list.
:- import_module array.
:- import_module assoc_list.
:- import_module map.

%-----------------------------------------------------------------------------%
% Ordered set

:- type ordered_set(T) where comparison is compare_ordered_sets.

%-----------------------------------------------------------------------------%
% Basic operations

% Sanity check, ensure the sorted array is actually congruent with the ordered
% Array
:- pred is_valid(ordered_set(T)::in) is semidet.

:- func empty_set = ordered_set(T).

:- pred empty_set(ordered_set(T)::out) is det.

:- pred is_empty(ordered_set(_)::in) is semidet.

:- func singleton(T) = ordered_set(T).

:- pred singleton(T::in, ordered_set(T)::out) is det.

:- pred is_singleton(ordered_set(T)::in) is semidet.

% True if the order of the elements are sorted, even with duplicates.
:- pred is_sorted(ordered_set(T)::in) is semidet.

% Sort a given set without removing elements.
:- func sort(ordered_set(T)) = ordered_set(T).

% If the order is sorted and has no duplicates
:- pred is_sorted_set(sorted_set(T)::in) is semidet. 

% True if the internal ordered and sorted arrays refer to the same array, 
% Offers a cheaper incomplete alternative test to is_sorted_set/1

:- pred is_merged_set(sorted_set(T)::in) is semidet.

% Return the number of elements in the ordered portion of the set
:- func size(ordered_set(_)) = int.
:- pred size(ordered_set(_)::in, int::out) is det.



% Return the number of unique elements in the set
:- func unique_elements(ordered_set(_)) = int.
:- pred unique_elements(ordered_set(_)::in, int::out) is det.

% Compares the ordered values in the set, including duplicates, succeeds if
% the set contains the same values in the same order. 
:- pred equal(ordered_set(T)::in, ordered_set(T)::in) is semidet.

% Succeeds if the sets have the same values, with duplicates removed
% Equivalent to compare_ordered_sets((=)).
:- pred equivalent(ordered_set(T)::in, ordered_set(T)::in) is semidet.

% Sorts and removes duplicates from the sets and compares them according to
% the standard ordering. Used to implement mercury equality and comparison.
% This is neccecary in order to efficiently implement maps and other data
% structures for this type as *sets* ... if you need to map the values in
% the set by ordering, convert the set to an ordered list or array first.
:- pred compare_ordered_sets(comparison_result::uo, ordered_set(T)::in, 
	ordered_set::in) is det.


%-----------------------------------------------------------------------------%
% Conversion

% Converts the linear order of the set into the according container

:- func from_list(list(T)) = ordered_set(T).
:- func to_list(ordered_set(T)) = list(T).

:- func from_array(array(T)) = ordered_set(T).
:- func to_array(ordered_set(T)) = array(T).

% Sorts and removes duplicates (if any) from the set, the assigned ordering
% is ignored

:- func to_sorted_list(ordered_set(T)) = list(T).
:- func to_sorted_array(ordered_set(T)) = array(T).

	
%-----------------------------------------------------------------------------%
% Lookup

% Unlike arrays, these operations are 1 indexed, the 'set_' variants of each
% call are equivalent, but operate on the sorted set version, not the ordered

% Return the maximum and minimum valid indexes for the order, return -1 for 
% both values if empty set
:- pred bounds(ordered_set(_)::in, int::out, int::out) is det.
:- pred set_bounds(ordered_set(_)::in, int::out, int::out) is det.

% As above, but fail on emtpy set
:- pred semidet_bounds(ordered_set(_)::in, int::out, int::out) is semidet.
:- pred semidet_set_bounds(ordered_set(_)::in, int::out, int::out) is semidet.

% Return the first index (should always be 1) unless the set is empty then -1
:- func min(ordered_set(_)) = int is det.
:- pred min(ordered_set(_)::in, int::out) is det.

:- func semidet_min(ordered_set(_)) = int is semidet.
:- pred semidet_min(ordered_set(_)::in, int::out) is semidet.

% Return the last index
:- func max(ordered_set(_)) = int is det.
:- pred max(ordered_set(_)::in, int::out) is det.

:- func semidet_max(ordered_set(_)) = int is semidet.
:- pred semidet_max(ordered_set(_)::in, int::out) is semidet.

:- pred contains(ordered_set(T)::in, T::in) is semidet.

% Index of the elements by order
:- pred index(ordered_set(T), int, T).
:- mode index(in, out, in) is semidet.
:- mode index(in, in, out) is semidet.
:- mode index(in, out, out) is nondet.

% Index of the elements by sorted set
:- pred set_index(ordered_set(T), int, T).
:- mode set_index(in, out, in) is semidet.
:- mode set_index(in, in, out) is semidet.
:- mode set_index(in, out, out) is nondet.

% Lookup the ordered value at the given index (starting at 1)
:- func lookup(ordered_set(T), int) = T is semidet.
:- pred lookup(ordered_set(T)::in, int::in, ::out) is semidet.

:- func set_lookup(ordered_set(T), int) = T is semidet.
:- pred set_lookup(ordered_set(T)::in, int::in, T::out) is semidet.

% Search for the given value and return it's index in the order.
:- func search(ordered_set(T), T) = int.
:- pred search(ordered_set(T)::in, T::in, int::out) is semidet.

:- func set_search(ordered_set(T), T) = int.
:- pred set_search(ordered_set(T)::in, T::in, int::out) is semidet.


%-----------------------------------------------------------------------------%
% Ordering

% Creates a new  ordered set by sorting the members using the provided
% comparison function
:- pred order_by(comparison_func(T)::in(comparison_func), ordered_set(T)::in, 
	ordered_set(T)::out) is det.
	
:- func order_by(comparison_func(T)::in(comparison_func), ordered_set(T)::in)
	= (ordered_set(T)::out) is det.
	
:- type indexed_comparison_func(T) == 
	(func(int, int, T, T) = comparison_result).

:- inst indexed_comparison_func == (func(in, in, in, in) = out is det).

% As above, but provides the corresponding indexes in the original ordering
% to the provided comparison function.
:- pred order_by_index(
	indexed_comparison_func(T)::in(indexed_comparison_func)),
	ordered_set(T)::in, ordered_set(T)::out) is det.

:- func order_by_index(
	indexed_comparison_func(T)::in(indexed_comparison_func)),
	ordered_set(T)::in) = (ordered_set(T)::out) is det.	

%-----------------------------------------------------------------------------%
% Set operations

% The non-prefixed set operations will attempt to preserve the order of the
% first set provided, the 'set_' prefixed operations will operate only on the
% sorted sets without duplicates.  The 'set_' variants should be assumed to be
% more efficient than the default calls. The 'ordered_' prefixed operations
% will use the provided comparison function to construct the output, hopefully
% with greater efficiency than calling a 'set_' operation and then calling
% order_by/2-3. Likewise 'index_ordered_' calls are equivalent to calling the
% non-prefixed set operations and then calling order_by_index/2-3.

% Preserve the order of the first set, appending items in the second set to the
% first in-order
:- pred union(ordered_set(T)::in, ordered_set(T)::in, ordered_set::out) is det.
:- func union(ordered_set(T), ordered_set(T)) = ordered_set(T).

% The union of two sets sorted and without duplicates, order is not preserved
:- pred set_union(ordered_set(T)::in, ordered_set(T)::in, ordered_set::out)
	is det.
:- func set_union(ordered_set(T), ordered_set(T)) = ordered_set(T).

% Perform a set_union on the input sets, ordering the results using the
% provided comparison function
:- pred ordered_union(comparison_func(T)::in(comparison_func), 
	ordered_set(T)::in, ordered_set(T)::in, ordered_set::out) is det.
:- func ordered_union(comparison_func(T)::in(comparison_func), 
	ordered_set(T)::in, ordered_set(T)::in) = (ordered_set::out) is det.

% Perform a union on the input sets, ordering the results using the
% provided comparison function
:- pred index_ordered_union(
	indexed_comparison_func(T)::in(indexed_comparison_func), 
	ordered_set(T)::in, ordered_set(T)::in, ordered_set::out) is det.
:- func index_ordered_union(
	indexed_comparison_func(T)::in(indexed_comparison_func), 
	ordered_set(T)::in, ordered_set(T)::in) = (ordered_set::out) is det.
	
% Preserve the order of the first set, removing elements not found in the
% second set.
:- pred intersect(ordered_set(T)::in, ordered_set(T)::in, ordered_set::out) 
	is det.
:- func intersect(ordered_set(T), ordered_set(T)) = ordered_set(T).

% The intersection of two sets sorted and without duplicates, order is not 
% preserved
:- pred set_intersect(ordered_set(T)::in, ordered_set(T)::in, ordered_set::out) 
	is det.
:- func set_intersect(ordered_set(T), ordered_set(T)) = ordered_set(T).

% Perform a set_intersect on the input sets, ordering the results using the
% provided comparison function
:- pred ordered_intersect(comparison_func(T)::in(comparison_func), 
	ordered_set(T)::in, ordered_set(T)::in, ordered_set::out) is det.
:- func ordered_intersect(comparison_func(T)::in(comparison_func), 
	ordered_set(T)::in, ordered_set(T)::in) = (ordered_set::out) is det.

% Perform a intersect on the input sets, ordering the results using the
% provided comparison function
:- pred index_ordered_intersect(
	indexed_comparison_func(T)::in(indexed_comparison_func), 
	ordered_set(T)::in, ordered_set(T)::in, ordered_set::out) is det.
:- func index_ordered_intersect(
	indexed_comparison_func(T)::in(indexed_comparison_func), 
	ordered_set(T)::in, ordered_set(T)::in) = (ordered_set::out) is det.

% Preserve the order of the first set, removing elements found in the second
% set
:- pred difference(ordered_set(T)::in, ordered_set(T)::in, ordered_set::out) 
	is det.
:- func difference(ordered_set(T), ordered_set(T)) = ordered_set(T).

% The difference of two sets sorted and without duplicates, 
% order is not preserved
:- pred set_difference(ordered_set(T)::in, ordered_set(T)::in, 
	ordered_set::out) is det.
:- func set_difference(ordered_set(T), ordered_set(T)) = ordered_set(T).

% Perform a set_difference on the input sets, ordering the results using the
% provided comparison function
:- pred ordered_difference(comparison_func(T)::in(comparison_func), 
	ordered_set(T)::in, ordered_set(T)::in, ordered_set::out) is det.
:- func ordered_difference(comparison_func(T)::in(comparison_func), 
	ordered_set(T)::in, ordered_set(T)::in) = (ordered_set::out) is det.

% Perform a intersect on the input sets, ordering the results using the
% provided comparison function
:- pred index_ordered_difference(
	indexed_comparison_func(T)::in(indexed_comparison_func), 
	ordered_set(T)::in, ordered_set(T)::in, ordered_set::out) is det.
:- func index_ordered_difference(
	indexed_comparison_func(T)::in(indexed_comparison_func), 
	ordered_set(T)::in, ordered_set(T)::in) = (ordered_set::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- import_module util.


%-----------------------------------------------------------------------------%
% Ordered set

:- type ordered_set(T)
	--->	ordered_set(order::array(T), sorted::array(T)).

% Deterministic constructor
:- func os(array(T), array(T)) = ordered_set(T).
:- mode os(in, in) = out is det.
:- mode os(out, out) = in is det.

:- pragma promise_equivalent_clauses(os/2).

os(O::in, S::in) = (ordered_set(O, S)::out).

% Deconstructions on types with user defined equality are cc_multi
os(O::out, S::out) = (OS::in) :- 
	promise_equivalent_solutions [O, S] ordered_set(O, S) = OS. 
	
:- pragma inline(os/2).
	
/* TODO: Make the sorted array lazy when benchmarking to see if there is any
 appriciable benefit to performance. 

Efficient set operations (union, intersect, difference) or 
comparison (ord ops >,=,<) would force evaluation of the lazy set. The only 
advantage to delaying the sorting of the array would be if the ordered array 
were indexed (or converted to a non-set type) without performing any set or 
comparison operations. 
*/

%-----------------------------------------------------------------------------%
% Basic operations

% Variable naming conventions
% A == Array
% O == Ordered Array
% S == Sorted Set Array
% OS == Ordered Set (not deconstructed)
% CMP == Comparison Result

is_valid(os(A, sort_and_remove_dups(A))).

empty_set(os(A, A)) :- make_empty_array(A).

empty_set(empty_set).

is_empty(os(A, _)) :- size(A, 0).

singleton(T) = os(A, A) :- init(1, T, A).

singleton(T, singleton(T)).

is_singleton(os(A, _)) :- size(A, 1).

is_sorted(os(A, _)) :- 
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
	
sort(os(O, S)) = os(sort(copy(O)), S).

is_sorted_set(os(A, A)).

is_merged_set(os(O, S)) :- private_builtin.pointer_equal(O, S).

size(os(A, _)) = size(A).
size(OS, size(OS)).
	
unique_elements(os(_, A)) = size(A).
unique_elements(OS, unique_elements(OS)).

equal(os(A, _), os(A, _)).

equivalent(os(_, A), os(_, A)).

compare_ordered_sets(array_compare(A1, A2), os(_, A1), 	os(_, A2)).


%-----------------------------------------------------------------------------%
% Conversion

