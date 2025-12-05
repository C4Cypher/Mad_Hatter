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
:- import_module array.

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
:- func length(mh_ordered_term_set) = int.
:- pred length(mh_ordered_term_set::in, int::out) is det.


	% Return the number of unique elements in the set
:- func size(mh_ordered_term_set) = int.
:- pred size(mh_ordered_term_set::in, int::out) is det.

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

	% Fails if empty
:- func semidet_min(mh_ordered_term_set) = int is semidet.
:- pred semidet_min(mh_ordered_term_set::in, int::out) is semidet.

	% Return the last index, -1 if empty
:- func max(ordered_term_set) = int is det.
:- pred max(ordered_term_set::in, int::out) is det.

:- func semidet_max(ordered_term_set) = int is semidet.
:- pred semidet_max(ordered_term_set::in, int::out) is semidet.

:- pred contains(mh_ordered_term_set::in, mh_term::in) is semidet.

	% Lookup the ordered value at the given index (starting at 1), throws an	% exception if index is out of bounds
:- func lookup(mh_ordered_term_set, int) = mh_term is det.
:- pred lookup(mh_ordered_term_set::in, int::in, mh_term::out) is det.

	% Skips bounds checks, behavior is undefined if index is out of bounds.
:- func unsafe_lookup(mh_ordered_term_set, int) = mh_term is det.
:- pred unsafe_lookup(mh_ordered_term_set::in, int::in, mh_term::out) is det.

	% Search for the value and return the lowest index in the ordered array 
:- func search(mh_ordered_term_set, mh_term) = int is semidet.
:- pred search(mh_ordered_term_set::in, mh_term::in, int::out) is semidet.

	% Search for the value and return a list of indexes in the ordered array 
	% The 
:- func search_all(mh_ordered_term_set::in, mh_term::in) = 
	(list(int)::out(non_empty_list)) is semidet.
:- pred search_all(mh_ordered_term_set::in, mh_term::in, 
	list(int)::out(non_empty_list))	is semidet.
	
	% As above, but return an empty list if the value is not present. 
:- func det_search_all(mh_ordered_term_set, mh_term) = list(int) is det.
:- pred det_search_all(mh_ordered_term_set::in, mh_term::in, set(int)::out) 
	is det.
	
	% return the lowest (leftmost) element, fails if the set is empty
:- func first(mh_ordered_term_set) = mh_term is semidet.
:- pred first(mh_ordered_term_set::in, mh_term::out) is semidet.

	% throw an exception if the set is empty
:- func det_first(mh_ordered_term_set) = mh_term is semidet.
:- pred det_first(mh_ordered_term_set::in, mh_term::out) is semidet.

	% return the highest (rightmost) element, fails if the set is empty
:- func last(mh_ordered_term_set) = mh_term is semidet.
:- pred last(mh_ordered_term_set::in, mh_term::out) is semidet.
	
:- func det_last(mh_ordered_term_set) = mh_term is semidet.
:- pred det_last(mh_ordered_term_set::in, mh_term::out) is semidet.

	
%-----------------------------------------------------------------------------%
% Ordering

	% Create a new ordered set using the provided comparison function that
	% preserves the original order when the comparison function returns
	% equality. Stability is not garunteed when comparisons are equal.
	% Does not remove duplicates.
:- pred order_by(comparison_func(mh_term)::in(comparison_func), 
	mh_ordered_term_set::in, mh_ordered_term_set::out) is det.
	
:- func order_by(comparison_func(mh_term)::in(comparison_func), 
	mh_ordered_term_set::in) = (mh_ordered_term_set::out) is det.
	
	% Given any term map (including mh_term_set), create an ordering
:- func to_ordered_term_set(comparison_func(T)::in(comparison_func),
	mh_term_map(_)::in) = (mh_ordered_term_set::out) is det.


%-----------------------------------------------------------------------------%
% Set operations

% These operations are less efficient than first converting them to 
% mh_term_set before performing them, however, they should preserve the
% leftmost unique ordering of the terms in the sets, both in the case of union
% the ordering of the left operand is preserved in the case of intersection
% and difference

	% The union of two sets
	% appends terms in the second set not present in the first set to the end 
	% of the order in in the first set, does *not* remove duplicates already
	% present in the first set's order, but *does* remove duplicates from the 
	% second
:- pred union(mh_ordered_term_set::in, mh_ordered_term_set::in,
	mh_ordered_term_set::out) is det.
:- func union(mh_ordered_term_set, mh_ordered_term_set) = mh_ordered_term_set.

	% As above, but removes duplicates from both operands while preserving
	% leftmost ordering of the unique elements
:- pred remove_dups_union(mh_ordered_term_set::in, mh_ordered_term_set::in,
	mh_ordered_term_set::out) is det.
:- func remove_dups_union(mh_ordered_term_set, mh_ordered_term_set) 
	= mh_ordered_term_set.


	% The intersection of two sets, preserves the leftmost order of the first
	% set, removes duplicates
:- pred intersect(mh_ordered_term_set::in, mh_ordered_term_set::in,
	mh_ordered_term_set::out) is det.
:- func intersect(mh_ordered_term_set, mh_ordered_term_set)
	= mh_ordered_term_set.

	% The difference of two sets, preserves the leftmost order of the first
	% set, removes duplicates
:- pred difference(mh_ordered_term_set::in, mh_ordered_term_set::in, 
	mh_ordered_term_set::out) is det.
:- func difference(mh_ordered_term_set, mh_ordered_term_set)
	= mh_ordered_term_set.
	
%-----------------------------------------------------------------------------%
% Set operations

%-----------------------------------------------------------------------------%
% Higher order 

	% Left fold over items in an array, providing the lookup index
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


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module unit.
:- import_module map.
:- import_module require.

:- import_module util.
:- import_module array_util.

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

:- type mh_ordered_term_set 
	--->	ordered_term_set(
				order::term_array, 
				set::mh_term_set
			)
		where comparison is compare_ordered_term_sets.

% Deterministic constructor
:- func os(array(mh_term), mh_term_set) = mh_ordered_term_set.
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


:- func new_term_set(term_array) = mh_term_set.

new_term_set(A) = foldl(set, A, init).

:- pred valid_element(mh_term_set::in, mh_term:in) is semidet.

valid_element(Set, Term) :- contains(Set, Term).

is_valid(os(O, S)) :- all_true(valid_element(S), O).
	
empty_set = os(A, init) :- make_empty_array(A).

empty_set(empty_set).

is_empty(os(A, _)) :- size(A, 0).

singleton(T) = os(A, singleton(T)) :- 
	init(1, T, A).

singleton(T, singleton(T)).

is_singleton(os(A, _)) :- size(A, 1).

is_sorted(os(A, _)) :- is_sorted(A).
	
sort(os(O, S)) = os(sort(copy(O)), S).

sort_and_remove_dups(os(O, S)) = os(sort_and_remove_dups(O), S).
	
remove_dups(os(Order, Set)@OS) =  
	(if Order = NewOrder
	then OS
	else os(NewOrder, Set)
	) :- remove_dups_stable(Order, NewOrder).

length(os(A, _)) = size(A).
length(OS, length(OS)).
	
size(os(_, S)) = count(S).
size(OS, size(OS)).

equal(os(A, _), os(A, _)).

equivalent(os(_, M), os(_, M)).

compare_ordered_term_sets(compare(M1, M2)),	os(_, M1), 	os(_, M2)). 


%-----------------------------------------------------------------------------%
% Conversion

from_list(L) = from_array(array.from_list(L)).

to_list(os(O, _)) = array.to_list(O). 

from_array(A) = os(A, new_term_set(A)).

to_array(os(O, _)) = O.

to_sorted_list(OS) = array.to_list(to_sorted_array(OS)).

to_sorted_array(os(O, _)) = sort_and_remove_dups(O).

to_term_set(os(_, S)) = S.

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
	(if array.max(A)@Amax < 0
	then
		-1
	else
		Amax + 1
	).
	
max(OS, max(OS)).

semidet_max(os(A, _)) = array.max(A) :- size(A) > 0.

semidet_max(OS, semidet_max(OS)).

contains(os(_, S), T) :- contains(OS, T).

lookup(os(A, _), Index) = array.lookup(A, Index - 1).
lookup(OS, Index, lookup(OS, Index)).

unsafe_lookup(os(A, _), Index) = array.unsafe_lookup(A, Index - 1).
unsafe_lookup(OS, Index, unsafe_lookup(OS, Index)).

search(os(O, _), Value) = Index + 1 :- search(O, Value, Index).
search(OS, Value, search(OS, Value)).

search_all(OS, Value) = det_search_all(OS, Value)@List :- is_not_empty(List).
search_all(OS, Value, search_all(OS, Value)).

:- func search_fold(mh_term, int, mh_term, list(int)) = list(int).

search_fold(Search, Index, Found, List) =
	(if Search = Found
	then append(List, [ Index ])
	else List
	).

det_search_all(os(O, _), Value) = index_fold(search_fold, O, []).
	
	
det_search_all(OS, Value, det_search_all(OS, Value)).

first(OS) = lookup(OS, 1) :- size(OS) > 0.

det_first(OS) = (if First = first(OS) then First 
	else error($pred, "Empty ordered set has no first element.")).
	
last(OS) = lookup(OS, max(Os)@Max) :- Max > 0.

det_last(OS) = (if Last = last(OS) then Last 
	else error($pred, "Empty ordered set has no last element.")).

%-----------------------------------------------------------------------------%
% Ordering

 order_by(CMP, OS, order_by(CMP, OS)).

order_by(CMP, os(O, S)) = os(O@samsort(CMP, copy(O)), S).

:- type unsorted_accumulator ---> usacc(int, term_array).
:- inst unsorted_accumulator = usacc(ground, array_uniq).

:- func to_unsorted_array(mh_term_set::in) = (term_array::array_uo) is det.

:- func unsorted_insert(mh_term, unsorted_accumulator) = 
	un_sorted_accumulator.

unsorted_insert(Term, usacc(Index, !.Array)) = usacc(Index + 1, !:Array) :-
	unsafe_set(Index, !Array).
	
to_unsorted_array(Set) = Array :- %probably get a mode error,  
	fold(unsorted_insert, Set, usacc(0, init(count(Set), nil_term))) =
		usacc(_, Array).
		
:- func to_unit_map(mh_term_map(_)) = mh_term_set.

:- func unit_map(mh_term, _) = unit.
unit_map(_, _) = unit.


to_unit_map(M) = S :-
	(if dynamic_cast(M, S0)
	then S0 = S
	else map(unit_map, M, S)
	).
		
to_ordered_term_set(CMP, Map) = 
	os(samsort(CMP, to_unsorted_array(Map))), to_unit_map(Map)).
 
%-----------------------------------------------------------------------------%
% Set operations

%- pred union_and_contains(Element, !Union, !RevList).		
:- pred union_and_contains(mh_term::in, mh_term_set::in, mh_term_set::out,
	list(mh_term)::in, list(mh_term)::out) is det.
	
union_and_contains(Term, !Union, !Rev) :-
	(if insert(Term, !Union) % fails if already present 
	then 
		!:Rev = [ Term | !.Rev], 
		!:Union = !.Union % Jank if semantics requires then clause to bind var
	else true
	).
	
union(OS1, OS2, union(OS1, OS2)).

union(os(O1, S1), os(O2, _)) = os(O3, S3) :-
	fold2(union_and_contains, O2, S1, S3, [], RevAppend),
	O3 = append(O1, from_reverse_list(RevAppend)).
	
remove_dups_union(OS1, OS2, remove_dups_union(OS1, OS2)).

remove_dups_union(OS1, OS2) = union(remove_dups(OS1), OS2).

%- pred intersect_loop(Current, Last, O1, S2, !S3, !O3, !Size). 
:- pred intersect_loop(int::in, int::in, term_array::in, mh_term_set::in, 
	mh_term_set::in, mh_term_set::out, 
	term_array::array_di, term_array::array_uo, int::in, int::out) is det.
	
intersect_loop(Current, Last, O1, S2, !S3, !O3, !Size) :-
	(if Current > Last
	then true
	else
		unsafe_lookup(O1, Current, Element),
		(if contains(Element, S2), insert(Element, !S3)
		then
			unsafe_set(!.Size, Element, !O3),
			!:Size = !.Size + 1
		else true
		),
		intersect_loop(Current + 1, Last, O1, S2, !S3, !O3, !Size)
	).

intersect(OS1, OS2, intersect(OS1, OS2)).
	
intersect(os(O1, S1)@OS1, os(O2, S2)@OS2) = OS3 :- 
	Size = size(O1),
	(if Size = 1, unsafe_lookup(O1, 0, E), contains(S2, E) 
	then OS3 = OS1 
	else if Size = 0 ; size(O2) = 0
	then OS3 = empty_set
	else 
		unsafe_lookup(O1, 0, First),
		A0 = init(Size, First),
		intersect_loop(1, max(O1), O1, S2, init, S3, A0, A1, 1, NewSize),
		shrink(NewSize, A1, O3),
		OS3 = os(O3, S3)
	).

%- pred difference_loop(Current, Last, O1, S2, !S3, !O3, !Size). 
:- pred difference_loop(int::in, int::in, term_array::in, mh_term_set::in, 
	mh_term_set::in, mh_term_set::out, 
	term_array::array_di, term_array::array_uo, int::in, int::out) is det.
	
difference_loop(Current, Last, O1, S2, !S3, !O3, !Size) :-
	(if Current > Last
	then true
	else
		unsafe_lookup(O1, Current, Element),
		(if contains(Element, S2) 
		then true
		else if insert(Element, !S3)
		then
			unsafe_set(!.Size, Element, !O3),
			!:Size = !.Size + 1
		else true		
		),
		difference_loop(Current + 1, Last, O1, S2, !S3, !O3, !Size)
	).

difference(OS1, OS2, difference(OS1, OS2)).
	
difference(os(O1, S1)@OS1, os(O2, S2)@OS2) = OS3 :- 
	Size = size(O1),
	(if Size = 0
	then OS3 = empty_set
	else 
		unsafe_lookup(O1, 0, First),
		(if Size = 1
		then
			(if contains(S2, First)
			then OS3 = empty_set
			else OS3 = OS1
			)
		else 
			A0 = init(Size, First),
			difference_loop(1, max(O1), O1, S2, init, S3, A0, A1, 1, NewSize),
			shrink(NewSize, A1, O3),
			OS3 = os(O3, S3)
		)
	).
		

