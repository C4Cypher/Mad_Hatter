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

	% Unlike arrays, these operations are 1 indexed
	%TODO: unsafe versions that skip bounds checks?

	% Return the maximum and minimum valid indexes for the order, return -1 for 
	% both values if empty set
:- pred bounds(mh_ordered_term_set::in, int::out, int::out) is det.

	% As above, but fail on emtpy set
:- pred semidet_bounds(mh_ordered_term_set::in, int::out, int::out) is semidet.

	% Succeed if the given index is in bounds for the given ordered_term_set
:- pred in_bounds(mh_ordered_term_set::in, int::in) is semidet.

	% Return the first index (should always be 1) unless the set is empty 
	% If empty return -1
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

	% Lookup the ordered value at the given index (starting at 1), throws an	% exception if index is out of bounds
:- func lookup(mh_ordered_term_set, int) = mh_term is det.
:- pred lookup(mh_ordered_term_set::in, int::in, mh_term::out) is det.

	% Search for the value and return the lowest index in the ordered array 
:- func search(mh_ordered_term_set, mh_term) = int is semidet.
:- pred search(mh_ordered_term_set::in, mh_term::in, int::out) is semidet.

	% Search for the value and return a set of indexes in the ordered array 
:- func search_all(mh_ordered_term_set, mh_term) = set(int) is semidet.
:- pred search_all(mh_ordered_term_set::in, mh_term::in, set(int)::out) 
	is semidet.
	
	% As above, but return an empty set if the value is not present. 
:- func det_search_all(mh_ordered_term_set, mh_term) = set(int) is det.
:- pred det_search_all(mh_ordered_term_set::in, mh_term::in, set(int)::out) 
	is det.
	
	% return the lowest (leftmost) element, fails if the set is empty
:- func first(mh_ordered_term_set) = mh_term is semidet.
:- pred first(mh_ordered_term_set::in, mh_term::out) is semidet.

	% return the highest (rightmost) element, fails if the set is empty
:- func last(mh_ordered_term_set) = mh_term is semidet.
:- pred last(mh_ordered_term_set::in, mh_term::out) is semidet.
	

%-----------------------------------------------------------------------------%
% Insertion

	% Insert the element at the given index (starting at 1)
	% Throws an exception if the index to be inserted is invalid
:- pred insert(int::in, mh_term::in, mh_ordered_term_set::in, 
	mh_ordered_term_set::out) is det.
	
	% Inserts at the front of the list (element 1)
:- pred insert(mh_term::in, mh_ordered_term_set::in, mh_ordered_term_set::out) 
	is det.

	% Inserts at the end of the list (max + 1)
:- pred push(mh_term::in, mh_ordered_term_set::in, mh_ordered_term_set::out) 
	is det.
	
	% Change the element at the given index to the provided term
:- pred set(int::in, mh_term::in, mh_term_map::in, mh_term_map::out)
	is det.

	
%-----------------------------------------------------------------------------%
% Removal

	% Remove the element at the given index (starting from 1), shift elements
	% to the left. Throw an exception if the index is invalid.
:- pred remove(int::in, mh_term::out, mh_ordered_term_set::in,
	mh_ordered_term_set::out) is det.

	% Remove all instances of the given term from the set
:- pred remove(mh_term::in, mh_ordered_term_set::in, mh_ordered_term_set::out) is semidet.
	
:- pred det_remove(mh_term::in, T::out, mh_ordered_term_set::in, 
	mh_term_map::out) is det.
	
:- pred det_remove(mh_term::in, mh_ordered_term_set::in, mh_ordered_term_set::out) is det.
	
:- pred delete(mh_term::in,  mh_ordered_term_set::in, mh_term_map::out) is det.

:- pred delete_list(list(mh_term)::in, mh_ordered_term_set::in, 
	mh_term_map::out) is det.

%-----------------------------------------------------------------------------%
% Set operations


	% The union of two sets sorted and without duplicates, 
	% order is not preserved
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
	
semidet_bounds(os(A, _), Min, Max) :-
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

contains(OS, T) :- search(OS, T, _).

% index and set_index implementations here

lookup(os(A, _), Index) = array.lookup(A, Index - 1).
lookup(OS, Index, lookup(OS, Index)).

:- func shift_indexes(set(int), int) = set(int).
shift_indexes(Indexes, Shift) = map('+'(Shift), Indexes).
	
search(os(_, M), Value) = Index + 1 :- 
	search(M, Value, Set),  
	to_sorted_list(Set) = [ Index | _ ].

search(OS, Value, search(OS, Value)).

search_all(os(_, M), Value) = shift_indexes(search(M, Value), 1).

search_all(OS, Value, search_all(OS, Value)).

det_search_all(OS, Value) = (if search_all(OS, Value, Found)
	then Found
	else init.
	).
	
det_search_all(OS, Value, det_search_all(OS, Value)).

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
