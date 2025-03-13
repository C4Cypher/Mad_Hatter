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

:- func set_min(ordered_set(_)) = int is det.
:- pred set_min(ordered_set(_)::in, int::out) is det.

:- func semidet_set_min(ordered_set(_)) = int is semidet.
:- pred semidet_set_min(ordered_set(_)::in, int::out) is semidet.

% Return the last index
:- func max(ordered_set(_)) = int is det.
:- pred max(ordered_set(_)::in, int::out) is det.

:- func semidet_max(ordered_set(_)) = int is semidet.
:- pred semidet_max(ordered_set(_)::in, int::out) is semidet.

:- func set_max(ordered_set(_)) = int is det.
:- pred set_max(ordered_set(_)::in, int::out) is det.

:- func semidet_set_max(ordered_set(_)) = int is semidet.
:- pred semidet_set_max(ordered_set(_)::in, int::out) is semidet.

:- pred contains(ordered_set(T)::in, T::in) is semidet.


% Lookup the ordered value at the given index (starting at 1), throws an
% exception if index is out of bound
:- func lookup(ordered_set(T), int) = T is det.
:- pred lookup(ordered_set(T)::in, int::in, T::out) is det.

:- func set_lookup(ordered_set(T), int) = T is det.
:- pred set_lookup(ordered_set(T)::in, int::in, T::out) is det.

% Search for the given value and return it's index in the sorted set.
:- func search(ordered_set(T), T) = int.
:- pred search(ordered_set(T)::in, T::in, int::out) is semidet.



%-----------------------------------------------------------------------------%
% Ordering

% Creates a new  ordered set by sorting the members of the sorted set using the
% provided comparison function. The original ordering is discarded, and
% duplicates according to the standard ordering are removed.  
% Stable/predictable ordering where the comparison func returns equality is not
% garunteed.
:- pred order_by(comparison_func(T)::in(comparison_func), ordered_set(T)::in, 
	ordered_set(T)::out) is det.
	
:- func order_by(comparison_func(T)::in(comparison_func), ordered_set(T)::in)
	= (ordered_set(T)::out) is det.

% Create a new orddered set using the
% provided comparison function that preserves the original order when the
% comparison function returns equality. Does not remove duplicates.
:- pred reorder_by(comparison_func(T)::in(comparison_func),
	ordered_set(T)::in,	ordered_set(T)::out) is det.
	
:- func reorder_by(comparison_func(T)::in(comparison_func), 
	ordered_set(T)::in)	= (ordered_set(T)::out) is det.

% An ordering is the arrangement of an ordered set by the index of it's sorted
% set.  Indexes of the array are zero based, however, the elements of the array
% refer to the one based indexes of the sorted set, as they refer to logical
% order, not literal.

% For example:  Given the ordered set of floats [1.5, 0.0, 0.0, 1.0], the
% sorted set would be [0.0, 1.0, 1.5] and the ordering would be [3, 1, 1, 2].

:- type ordering == array(int).

:- func ordering_to_list(ordering) = list(int).
:- func ordering_from_list(list(int)) = ordering.

% in order for an ordering to be valid for a given set, it must contain at
% least one index for every unique member of it's sorted set, and no indexes
% that are out of the bounds of the sorted set.

:- pred valid_ordering_for(ordering::in, ordered_set(T)::in) is semidet.

% Return the current ordering of the given set, returns an empty array if
% The input set is empty.

:- func current_ordering(ordered_set(T)) = ordering.
:- pred current_ordering(ordered_set(T)::in, ordering::out) is det.

% Attempt to create a new ordered set from the sorted set of the provided set
% The original ordering of the input set will be ignored, fails if the ordering
% is not valid for the given set. 
% (Should be) More efficient than calling valid_ordering_for/2 first.

:- func apply_ordering(ordered_set(T), ordering) = ordered_set(T) is semidet.
:- pred apply_ordering(ordering::in, ordered_set(T)::in,  ordered_set(T)::out)
	is semidet.
	
% As above, but throws an exception if the ordering is not valid.

:- func det_apply_ordering(ordered_set(T), ordering) = ordered_set(T).
:- pred det_apply_ordering(ordering::in, ordered_set(T)::in,  ordered_set(T)::out)
	is  det.
	
% Reorder an ordering given a comparison function with the provided ordered set
% preserving the original order where the comparison func returns equal. May
% Be useful when simply using order_by gives unstable results. Ignores the
% ordering of of the input ordered set. May thow an exception if input ordering
% is not valid. If the input ordering is valid, the output is garunteed to be
% valid.

:- func reorder(comparison_func(T)::in(comparison_func), ordered_set(T)::in,
	ordering::in) = (ordering::out) is det.
:- pred reorder(comparison_func(T)::in(comparison_func), ordered_set(T)::in,
	ordering::in, ordering::out) is det.

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module bool.

:- import_module util.


% Variable naming
% A == Array
% O == Ordered Array
% S == Sorted Set Array
% OS == Ordered Set (not deconstructed)
% CMP == Comparison Function
% R == Comparison Result
% T == Element/Value/Type
% I == Index
% Or == Ordering


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

from_list(L) = from_array(array.from_array(L)).

to_list(OS) = array.to_list(order(OS)). % CC multi?

from_array(A) = os(A, sort_and_remove_dups(A)).

to_array(OS) = order(OS).

to_sorted_list(OS) = array.to_list(sorted(OS)).

to_sorted_array(OS) = sorted(OS).

%-----------------------------------------------------------------------------%
% Lookup

bounds(os(A, _), Min, Max) :-
	(if size(A) = 0
	then
		Min@Max = -1
	else
		Min = array.min(A) + 1,
		Max = array.max(A) + 1
	).
	
set_bounds(os(_, A), Min, Max) :-
	(if size(A) = 0
	then
		Min@Max = -1
	else
		Min = array.min(A) + 1,
		Max = array.max(A) + 1
	).
	
semidet_bounds(os(A, _), Min, Max) :-
	size(A) > 0,
	Min = array.min(A) + 1,
	Max = array.max(A) + 1.
	
semidet_set_bounds(os(_, A), Min, Max) :-
	size(A) > 0,
	Min = array.min(A) + 1,
	Max = array.max(A) + 1.
	
min(os(A, _)) = 
	(if size(A) = 0
	then
		-1
	else
		array.min(A) + 1
	).
	
min(OS, min(OS)).

semidet_min(os(A, _)) = array.min(A) :- size(A) > 0.

semidet_min(OS, semidet_min(OS)).

set_min(os(_, A)) = 
	(if size(A) = 0
	then
		-1
	else
		array.min(A) + 1
	).
	
set_min(OS, min(OS)).

semidet_set_min(os(_, A)) = array.min(A) :- size(A) > 0.

semidet_set_min(OS, semidet_min(OS)).
	
max(os(A, _)) = 
	(if size(A) = 0
	then
		-1
	else
		array.max(A) + 1
	).
	
max(OS, max(OS)).

semidet_max(os(A, _)) = array.max(A) :- size(A) > 0.

semidet_max(OS, semidet_max(OS)).

set_max(os(_, A)) = 
	(if size(A) = 0
	then
		-1
	else
		array.max(A) + 1
	).
	
set_min(OS, max(OS)).

semidet_set_max(os(_, A)) = array.max(A) :- size(A) > 0.

semidet_set_max(OS, semidet_max(OS)).

% index and set_index implementations here

lookup(os(A, _), Index) = array.lookup(A, Index).
lookup(OS, Index, lookup(OS, Index)).
set_lookup(os(_, A), Index) = array.lookup(A, Index).
set_lookup(OS, Index, set_lookup(OS, Index)).

search(OS, T) = array_search(order(OS), T).
search(OS, T, search(OS, T)).

set_search(OS, T) = I :- binary_search(sorted(OS), T, I). % array_ui??
set_search(OS, T, set_search(OS, T)).

%-----------------------------------------------------------------------------%
% Ordering

order_by(CMP, OS, order_by(CMP, OS)).

order_by(CMP, os(_, S)) = os(O@samsort(CMP, copy(S)), sort_and_remove_dups(O)).


reorder_by(CMP, OS, reorder_by(CMP, OS)).

reorder_by(CMP, os(O0, _)) = 
	os(O@mergesort(CMP, copy(O0)), sort_and_remove_dups(O)). 
	
ordering_to_list(Or) = array.to_list(Or).
ordering_from_list(L) = array.from_list(Or).

valid_ordering_for(Or, os(_, S)) :-
	array.size(S, SetSize),
	array.init(SetSize, no, !.UniqueFound),
	valid_ordering_check(0, max(O), Or, 0, UniqueCount, UniqueFound, _),
	UniqueCount = SetSize. % Ordering is only valid if all are found
		
%valid_ordering_check(Index, Last, Or, !UniqueCount, !UniqueFound)
% Count every element in the Ordered array that has not yet been found to be
% in bounds of the Sorted array
:- pred valid_ordering_check(int::in, int::in, array(T)::in, int::in, 
	int::out, array(bool)::array_di, array(bool)::array_uo) is semidet.
	
valid_ordering_check(Index, Last, Or, !UniqueCount, !UniqueFound) :-
	(if Index > Last then 
		true
	else
		array.lookup(Or, Index, Is),
		%fails if out of bounds of the Sorted array
		array.semidet_lookup(!.UniqueFound, Is, Counted), 
		(if Counted = no then
			array.set(Is, yes, !UniqueFound)
			!:UniqueCount = !.UniqueCount + 1
		else
			true
		),
		valid_ordering_check(Index + 1, Last, Or, !UniqueCount, !UniqueFound)
	).
	
current_ordering(os(O, S)) = array.generate(size(O), generate_ordering(O, S)).

:- func generate_ordering(array(T), array(T), int) = int.

generate_ordering(O, S, Io) = Is :-
	array.lookup(Io, O, T),
	(if binary_search(S, T, Found) then
		Is = Found
	else
		report_lookup_error(
"ordered_set.current_ordering: Value in ordered set not found in sorted set", 
		T, S)
	).

current_ordering(OS, current_ordering(OS)).


