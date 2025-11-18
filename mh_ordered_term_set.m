%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_ordered_term_set.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_ordered_term_set.

:- interface.

:- import_module list.
:- import_module set.

:- import_module mh_tuple.
:- import_module ordered_set.

%-----------------------------------------------------------------------------%
% Ordered set

:- type mh_ordered_term_set. % where comparison is compare_ordered_term_sets.

%-----------------------------------------------------------------------------%
% Basic operations

	% Sanity check, ensure the set map is actually congruent with the ordered
	% Array
:- pred is_valid(mh_ordered_term_set::in) is semidet.

% TODO? More detailed diagnostics for validity checks. We'll see if that's even
% needed.

:- func empty_set = mh_ordered_term_set.

:- pred empty_set(mh_ordered_term_set::out) is det.

:- pred is_empty(ordered_term_set::in) is semidet.

:- func singleton(T) = mh_ordered_term_set.

:- pred singleton(T::in, mh_ordered_term_set::out) is det.

:- pred is_singleton(mh_ordered_term_set::in) is semidet.

% True if the order of the elements are sorted, even with duplicates.
:- pred is_sorted(mh_ordered_term_set::in) is semidet.

% Sort a given set using the standard ordering without removing elements.
:- func sort(mh_ordered_term_set) = mh_ordered_term_set.
:- pred sort(mh_ordered_term_set::in, mh_ordered_term_set::out) is det.

% Sort using the standard ordering and remove duplicaates
:- func sort_and_remove_dups(mh_ordered_term_set) = mh_ordered_term_set.
:- pred sort_and_remove_dups(mh_ordered_term_set::in, mh_ordered_term_set::out)
	is det.
	
:- func remove_dups(mh_ordered_term_set) = mh_ordered_term_set.
:- pred remove_dups(mh_ordered_term_set::in, mh_ordered_term_set::out)
	is det.

% Return the number of elements in the ordered portion of the set
:- func size(mh_ordered_term_set) = int.
:- pred size(mh_ordered_term_set::in, int::out) is det.


% Return the number of unique elements in the set
:- func unique_elements(mh_ordered_term_set) = int.
:- pred unique_elements(mh_ordered_term_set::in, int::out) is det.

% Compares the ordered values in the set, including duplicates, succeeds if
% the set contains the same values in the same order. 
:- pred equal(mh_ordered_term_set::in, mh_ordered_term_set::in) is semidet.

% Succeeds if the sets have the same values, with duplicates removed
% Equivalent to compare_ordered_term_sets((=)).
:- pred equivalent(mh_ordered_term_set::in, mh_ordered_term_set::in) 
	is semidet.

% Sorts and removes duplicates from the sets and compares them according to
% the standard ordering. Used to implement mercury equality and comparison.
% This is neccecary in order to efficiently implement maps and other data
% structures for this type as *sets* ... if you need to map the values in
% the set by ordering, convert the set to an ordered list or array first.
:- pred compare_ordered_term_sets(comparison_result::uo, 
	mh_ordered_term_set::in, mh_ordered_term_set::in) is det.

%-----------------------------------------------------------------------------%
% Conversion

:- func from_tuple(mh_tuple) = mh_ordered_term_set.
:- func to_tuple(mh_ordered_term_set) = mh_tuple.

:- func from_ordered_set(ordered_set(mh_term)) = mh_ordered_term_set.
:- func to_ordered_set(mh_ordered_term_set) = ordered_set(mh_term).

:- func from_list(list(mh_term)) = mh_ordered_term_set.
:- func to_list(mh_ordered_term_set) = list(mh_term).

:- func from_array(array(mh_term)) = mh_ordered_term_set.
:- func to_array(mh_ordered_term_set) = array(mh_term).

% Sorts and removes duplicates (if any) from the set, the assigned ordering
% is ignored

:- func to_sorted_list(mh_ordered_term_set) = list(mh_term).
:- func to_sorted_array(mh_ordered_term_set) = array(mh_term).

:- func to_term_set(mh_ordered_term_set) = mh_term_set.

% More efficient than to_term_set/1
:- some [T] to_term_map(mh_ordered_term_set) = mh_term_map(T).

	
%-----------------------------------------------------------------------------%
% Lookup

% Unlike arrays, these operations are 1 indexed, the 'set_' variants of each
% call are equivalent, but operate on the sorted set version, not the ordered

% Return the maximum and minimum valid indexes for the order, return -1 for 
% both values if empty set
:- pred bounds(mh_ordered_term_set::in, int::out, int::out) is det.

% As above, but fail on emtpy set
:- pred semidet_bounds(mh_ordered_term_set::in, int::out, int::out) is semidet.

% Succeed if the given index is in bounds for the given ordered_term_set
:- pred in_bounds(mh_ordered_term_set::in, int::in) is semidet.

% Return the first index (should always be 1) unless the set is empty then -1
:- func min(mh_ordered_term_set) = int is det.
:- pred min(mh_ordered_term_set::in, int::out) is det.

:- func semidet_min(mh_ordered_term_set) = int is semidet.
:- pred semidet_min(mh_ordered_term_set::in, int::out) is semidet.

% Return the last index
:- func max(ordered_term_set) = int is det.
:- pred max(ordered_term_set::in, int::out) is det.

:- func semidet_max(ordered_term_set) = int is semidet.
:- pred semidet_max(ordered_term_set::in, int::out) is semidet.

:- pred contains(mh_ordered_term_set::in, mh_term::in) is semidet.


% Lookup the ordered value at the given index (starting at 1), throws an
% exception if index is out of bound
:- func lookup(mh_ordered_term_set, int) = mh_term is det.
:- pred lookup(mh_ordered_term_set::in, int::in, mh_term::out) is det.

% Search for the value and return the index in the ordered array (linear)
:- func search(mh_ordered_term_set, mh_term) = int is semidet.
:- pred search(mh_ordered_term_set::in, mh_term::in, int::out) is semidet.
.



%-----------------------------------------------------------------------------%
% Ordering

% Creates a new  ordered set by sorting the members of the sorted set using the
% provided comparison function. The original ordering is discarded, and
% duplicates according to the standard ordering are removed.  
% Stable/predictable ordering where the comparison func returns equality is not
% garunteed.
:- pred order_by(comparison_func(mh_term)::in(comparison_func), 
	mh_ordered_term_set::in, mh_ordered_term_set::out) is det.
	
:- func order_by(comparison_func(mh_term)::in(comparison_func), 
	mh_ordered_term_set::in) = (mh_ordered_term_set::out) is det.

% Create a new orddered set using the
% provided comparison function that preserves the original order when the
% comparison function returns equality. Does not remove duplicates.
:- pred reorder_by(comparison_func(mh_term)::in(comparison_func),
	mh_ordered_term_set::in,	mh_ordered_term_set::out) is det.
	
:- func reorder_by(comparison_func(mh_term)::in(comparison_func), 
	mh_ordered_term_set::in)	= (mh_ordered_term_set::out) is det.

% TODO: Move ordering into it's own module?

% in order for an ordering to be valid for a given set, it must contain at
% least one index for every unique member of it's sorted set, and no indexes
% that are out of the bounds of the sorted set.

:- pred valid_ordering_for(ordering::in, mh_ordered_term_set::in) is semidet.

% Return the current ordering of the given set, returns an empty array if
% The input set is empty.

:- func current_ordering(mh_ordered_term_set) = ordering.
:- pred current_ordering(mh_ordered_term_set::in, ordering::out) is det.

% Attempt to create a new ordered set from the sorted set of the provided set
% The original ordering of the input set will be ignored, fails if the ordering
% is not valid for the given set. 
% (Should be) More efficient than calling valid_ordering_for/2 first.

:- func apply_ordering(mh_ordered_term_set, ordering) = mh_ordered_term_set
	is semidet.
:- pred apply_ordering(ordering::in, mh_ordered_term_set::in, 
	mh_ordered_term_set::out) is semidet.
	
% As above, but throws an exception if the ordering is not valid.

:- func det_apply_ordering(mh_ordered_term_set, ordering) = mh_ordered_term_set.
:- pred det_apply_ordering(ordering::in, mh_ordered_term_set::in,  
	mh_ordered_term_set::out) is  det. 


%-----------------------------------------------------------------------------%
% Set operations


% The union of two sets sorted and without duplicates, order is not preserved
:- pred union(mh_ordered_term_set::in, mh_ordered_term_set::in,
	mh_ordered_term_set::out) is det.
:- func union(mh_ordered_term_set, mh_ordered_term_set) = mh_ordered_term_set.


% The intersection of two sets sorted and without duplicates, order is not 
% preserved
:- pred intersect(mh_ordered_term_set::in, mh_ordered_term_set::in,
	mh_ordered_term_set::out) is det.
:- func intersect(mh_ordered_term_set, mh_ordered_term_set)
	= mh_ordered_term_set.

% The difference of two sets sorted and without duplicates, 
% order is not preserved
:- pred difference(mh_ordered_term_set::in, mh_ordered_term_set::in, 
	mh_ordered_term_set::out) is det.
:- func difference(mh_ordered_term_set, mh_ordered_term_set)
	= mh_ordered_term_set.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module int.
:- import_module unit.
:- import_module bool.
:- import_module require.

:- import_module util.

:- import_module mh_term_map.


% Variable naming
% A == Array
% O == Ordered Array
% S == Set
% OS == Ordered Set (not deconstructed)
% CMP == Comparison Function
% R == Comparison Result
% T == Term
% I == Index
% Or == Ordering


%-----------------------------------------------------------------------------%
% Ordered set

:- type term_array == array(mh_term).

:- type term_map == mh_term_map(set(int)).

:- type mh_ordered_term_set 
	--->	ordered_term_set(
				order::term_array, 
				map::term_map
			)
		where comparison is compare_ordered_term_sets.

% Deterministic constructor
:- func os(array(mh_term), term_map) = mh_ordered_term_set.
:- mode os(in, in) = out is det.
:- mode os(out, out) = in is det.

:- pragma promise_equivalent_clauses(os/2).

os(O::in, S::in) = (ordered_term_set(O, S)::out).

% Deconstructions on types with user defined equality are cc_multi
os(O::out, S::out) = (OS::in) :- 
	promise_equivalent_solutions [O, S] ordered_term_set(O, S) = OS. 

:- pragma inline(os/2).
	

%-----------------------------------------------------------------------------%
% Basic operations

:- pred add_mapping(int::in, mh_term::in, term_map::in, term_map::out) is det.

add_mapping(Index, Term, !Map) :-
	(if search(!.Map, Term, Indexes)
	then set(Term, insert(Indexes, Index), !Map)
	else det_insert(Term, make_singleton(Index), !Map)
	).

:- func add_mapping(int, mh_term, term_map) = term_map.
	
add_mapping(Index, Term, !.Map) = !:Map :- add_mapping(Index, Term, !Map).

:- func new_term_map(term_array) = term_map.

new_term_map(A) = index_fold(add_mapping, A, init).

:- pred valid_element(term_map::in, mh_term:in) is semidet.

valid_element(Map, Term) :- search(Map, Term, _).

:- func valid_mapping(term_array, mh_term, set(int), unit) = unit is semidet.

valid_mapping(O, Term, Indexes, _) = unit :- 
	all_true(valid_mapping_index(O, Term), Indexes).
	
:- pred valid_mapping_index(term_array::in, mh_term::in, int::in) is semidet.

valid_mapping_index(Array, Term, Index) :- semidet_lookup(Array, Index, Term).

is_valid(os(O, S)) :- 
	all_true(valid_element(S), O), 
	fold(valid_mapping(O), S, unit, _).
	

empty_set = os(A, init) :- make_empty_array(A).

empty_set(empty_set).


is_empty(os(A, _)) :- size(A, 0).

singleton(T) = os(A, singleton(T, make_singleton_set(min(A)))) :- 
	init(1, T, A).

singleton(T, singleton(T)).

is_singleton(os(A, _)) :- size(A, 1).

is_sorted(os(A, _)) :- is_sorted(A).
	
sort(os(O, M)) = os(sort(copy(O))@Sorted, new_term_map(Sorted)).

sort_and_remove_dups(os(O, M)) = 
	os(sort_and_remove_dups(O)@Sorted, new_term_map(Sorted)).

size(os(A, _)) = size(A).
size(OS, size(OS)).
	
unique_elements(os(_, M)) = count(M).
unique_elements(OS, unique_elements(OS)).

equal(os(A, _), os(A, _)).

equivalent(os(_, M1), os(_, M2)) :- to_term_set(M1) = to_term_set(M2). 

compare_ordered_term_sets(compare(to_term_set(M1), to_term_set(M2)), 
	os(_, M1), 	os(_, M2)). 


%-----------------------------------------------------------------------------%
% Conversion

from_list(L) = from_array(array.from_list(L)).

to_list(os(O, _)) = array.to_list(O). 

from_array(A) = os(A, new_term_map(A)).

to_array(os(O, _)) = O.

:- func set_insert_fold(mh_term, T, set(mh_term)) = set(mh_term).
set_insert_fold(T, _, S) = insert(S, T).

:- func to_set(mh_ordered_term_set) = set(mh_term).
to_set(os(_, M)) = fold(set_insert_fold, M, init).

to_sorted_list(os(_, M)) = to_sorted_list(to_set(M)).

to_sorted_array(OT) = to_array(to_sorted_list(OT)).

:- func to_unit(T, U) = unit.
to_unit(_, _) = unit.

to_term_set(os(_, M)) = map(to_unit, M).



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

in_bounds(os(O, _), I) :- array.in_bounds(O, I - 1).
in_set_bounds(os(_, S), I) :- array.in_bounds(S, I - 1).

	
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
	
set_max(OS, max(OS)).

semidet_set_max(os(_, A)) = array.max(A) :- size(A) > 0.

semidet_set_max(OS, semidet_max(OS)).

contains(OS, T) :- search(OS, T, _).

% index and set_index implementations here

lookup(os(A, _), Index) = array.lookup(A, Index - 1).
lookup(OS, Index, lookup(OS, Index)).
set_lookup(os(_, A), Index) = array.lookup(A, Index - 1).
set_lookup(OS, Index, set_lookup(OS, Index)).

search(os(O, _), T) = array_search(O, T) + 1.
search(OS, T, search(OS, T)).

set_search(os(_, S), T) = I + 1 :- binary_search(S, T, I). 
set_search(OS, T, set_search(OS, T)).

%-----------------------------------------------------------------------------%
% Ordering

order_by(CMP, OS, order_by(CMP, OS)).

order_by(CMP, os(_, S)) = os(O@samsort(CMP, copy(S)), sort_and_remove_dups(O)).


reorder_by(CMP, OS, reorder_by(CMP, OS)).

reorder_by(CMP, os(O0, _)) = 
	os(O@mergesort(CMP, copy(O0)), sort_and_remove_dups(O)). 
	
ordering_to_list(Or) = array.to_list(Or).
ordering_from_list(L) = array.array(L).

valid_ordering_for(Or, os(_, S)) :-
	array.size(S, SetSize),
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
	
current_ordering(os(O, S)) = array.generate(size(O), generate_ordering(O, S)).

:- func generate_ordering(array(T), array(T), int) = int.

generate_ordering(O, S, Io) = Is :-
	array.lookup(O, Io, T),
	(if binary_search(S, T, Found) then
		Is = Found + 1
	else
		unexpected($module, $pred,
"ordered_term_set.current_ordering: Value in ordered set not found in sorted set")
	).

current_ordering(OS, current_ordering(OS)).

apply_ordering(os(_, S), Or) = OS :-
	size(S, SortedSize),
	size(Or, OrderedSize),
	(if SortedSize = 0	then
		 OrderedSize = 0,
		OS = empty_set
	else 
		% Find first element
		semidet_lookup(Or, 0, For),
		semidet_lookup(S, Fs@For - 1, First),
		array.init(SortedSize, no, UniqueFound0),
		array.set(Fs, yes, UniqueFound0, UniqueFound), 
		(if OrderedSize = 1 then
			SortedSize = 1,
			singleton(First, OS)
		else
			array.init(OrderedSize, First, NewOrder),
			generate_order_from_ordering(Or, S, 1, max(Or), 1, SortedSize,
				UniqueFound, _, NewOrder, O),
			OS = os(O, S)
		
		)
	).
	
apply_ordering(Or, OS, apply_ordering(OS, Or)).
	
	
:- pred generate_order_from_ordering(array(int)::in, array(T)::in, int::in, 
	int::in, int::in, int::out,  array(bool)::array_di, array(bool)::array_uo,
	array(T)::array_di, array(T)::array_uo) is semidet.

generate_order_from_ordering(Or, S, I, Last, !UniqueCount, !UniqueFound, !O) :-
	(if I > Last then
		true
	else
		array.lookup(Or, I, Ior),
		array.semidet_lookup(S, Is@(Ior - 1), T),
		array.set(I, T, !O),
		array.semidet_lookup(!.UniqueFound, Is, Counted), 
		(if Counted = no then
			array.set(Is, yes, !UniqueFound),
			!:UniqueCount = !.UniqueCount + 1
		else
			true
		),
		generate_order_from_ordering(Or, S, I + 1, Last, !UniqueCount, 
			!UniqueFound, !O)
	).
	
det_apply_ordering(OS0, Or) =  
	(if apply_ordering(OS0, Or) = OS
	then
		OS
	else
		unexpected($module, $pred, "Invalid ordering.")
	).
	
det_apply_ordering(Or, OS, det_apply_ordering(OS, Or)).
	
%-----------------------------------------------------------------------------%
% Set operations

union(OS1, OS2, union(OS1, OS2)).

union(os(_, S1), os(_, S2)) = os(S3, S3) :-
	Unsorted = array.append(S1, array(difference_list(S1, S2))),
	S3 = array.sort(Unsorted).

intersect(OS1, OS2, intersect(OS1, OS2)).
	
intersect(os(_, S1), os(_, S2)) = os(S3, S3) :- 
	S3 = array(intersect_list(S1, S2)).


difference(OS1, OS2, difference(OS1, OS2)).
	
difference(os(_, S1), os(_, S2)) = os(S3, S3) :- 
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
