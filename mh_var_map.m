%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_var_map(.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_var_map.

:- interface.

:- import_module array.
:- import_module list.
:- import_module assoc_list.
:- import_module maybe.

:- import_module mh_term.
:- import_module mh_var_id.
:- import_module mh_var_set.

%-----------------------------------------------------------------------------%
% Variable map


:- type mh_var_map(T).
	
:- pred init(mh_var_map(T)::out) is det.
:- func init = mh_var_map(T).
	
:- pred empty_var_map(mh_var_map(T)::out) is det.
:- func empty_var_map = mh_var_map(T).

:- func singleton_id(var_id, T) = mh_var_map(T).
:- pred singleton_id(var_id::in, T::in, mh_var_map(T)::out) is det.

:- func singleton(mh_var, T) = mh_var_map(T).
:- pred singleton(mh_var::in, T::in, mh_var_map(T)::out) is det.

% var_map_bounds(var_mapsitution, Min, Max)
% Return the minimum and maximum var_id's indexed by var_map
:- pred var_map_bounds(mh_var_map(T)::in, var_id_offset::out, 
	var_id_set::out) is det.

%-----------------------------------------------------------------------------%
% Conversion

:- pred from_array(array(T)::in, mh_var_map(T)::out) is det.
	
:- func from_array(array(T)) = mh_var_map(T).

:- pred from_offset_array(array(T)::in, var_id_offset::in, 
	mh_var_map(T)::out) is det.
	
:- func from_offset_array(array(T), var_id_offset) = mh_var_map(T).
	
%-----------------------------------------------------------------------------%
% Looking up variable id's in var_maps


	% Succeed if the var_map can index the provided ID
:- pred contains_id(mh_var_map(T)::in, var_id::in) is semidet.

	% Succeed if the var_map can index the provided mh_var
:- pred contains(mh_var_map(T)::in, mh_var::in) is semidet.

% Find a given variable ID in the var_map, fail if the id is not found
% If the var_map is a renaming, return the indexed var_id as a variable

:- pred id_search(mh_var_map(T)::in, var_id::in, T::out) 
	is semidet.
:- func id_search(mh_var_map(T), var_id) = T is semidet.

:- pred search(mh_var_map(T)::in, mh_var::in, T::out) is semidet.
:- func search(mh_var_map(T), mh_var) = T is semidet.

% Find a given variable ID in the var_map, throw an exception if the id is not
% found 
:- pred id_lookup(mh_var_map(T)::in, var_id::in, T::out) is det.
:- func id_lookup(mh_var_map(T), var_id) = T.


:- pred lookup(mh_var_map(T)::in, mh_var::in, T::out) is det.
:- func lookup(mh_var_map(T), mh_var) = T.

%-----------------------------------------------------------------------------%
% Iterator 

:- type var_map_iterator.

% Retreive  first element of a var map (left traversal) and an iterator, 
% fails if var map is empty
:- pred first(mh_var_map(T)::in, T::out, var_map_iterator::out) 
	is semidet.

% Also produce the corresponding var_id
:- pred first(mh_var_map(T)::in, var_id::out, T::out, var_map_iterator::out) 
	is semidet.
% Throws an exception if var_map is empty
:- pred det_first(mh_var_map(T)::in, var_id::out, T::out, 
	var_map_iterator::out) is det.

:- pred det_first(mh_var_map(T)::in, T::out, var_map_iterator::out) is det.
	
% next(Map, Elem, Last, Current)
% Retreive the next element using the iterator, fail if no more elements
:- pred next(mh_var_map(T)::in, T::out, var_map_iterator::in, 
	var_map_iterator::out) is semidet.

:- pred next(mh_var_map(T)::in, var_id::out, T::out, var_map_iterator::in, 
	var_map_iterator::out) is semidet.
	
	
% For generating new var_map arrays
:- pred init_first(mh_var_set::in, var_id::out, var_map_iterator::out) is det.

% init_next(Set, ID, Last, Current)
% Fails if there should be no next element
% Be sure to index off of the Current iterator, not the Last!
:- pred init_next(mh_var_set::in, var_id::out, var_map_iterator::in,
	var_map_iterator::out) is semidet.
	
% Array index from iterator
:- func iterator_index(var_map_iterator) = int.



%-----------------------------------------------------------------------------%
% Insertion

	% Insert an element into a var map, fails if the element already exists 
:- pred id_insert(var_id::in, T::in, mh_var_map(T)::in, mh_var_map(T)::out) 
	is semidet.
	
:- pred det_id_insert(var_id::in, T::in, mh_var_map(T)::in, mh_var_map(T)::out) 
	is det.

:- pred insert(mh_var::in, T::in, mh_var_map(T)::in, mh_var_map(T)::out) 
	is semidet.

:- pred det_insert(mh_var::in, T::in, mh_var_map(T)::in, mh_var_map(T)::out) 
	is det.


:- pred det_insert_from_corresponding_lists(list(mh_var)::in,
    list(T)::in, mh_var_map(T)::in, mh_var_map(T)::out) is det.
	

:- pred det_insert_from_assoc_list(assoc_list(mh_var, T)::in,
    mh_var_map(T)::in, mh_var_map(T)::out) is det.
	
:- pred search_insert(mh_var::in, T::in, maybe(T)::out,
    mh_var_map(T)::in, mh_var_map(T)::out) is det.

	% Inserts an element into a var map, overwriting element if it already 
	% exists
:- pred id_set(var_id::in, T::in, mh_var_map(T)::in, mh_var_map(T)::out) 
	is det.
	
:- pred set(mh_var::in, T::in, mh_var_map(T)::in, mh_var_map(T)::out) is det.


:- pred set_from_corresponding_lists(list(mh_var)::in, list(T)::in,
    mh_var_map(T)::in, mh_var_map(T)::out) is det.
	

:- pred set_from_assoc_list(assoc_list(mh_var, T)::in,
    mh_var_map(T)::in, mh_var_map(T)::out) is det.

	% Overwrite an already existing element in a hashmap, fail if key not found
:- pred id_update(var_id::in, T::in, mh_var_map(T)::in, mh_var_map(T)::out) 
	is semidet.
	
:- pred det_id_update(var_id::in, T::in, mh_var_map(T)::in, mh_var_map(T)::out) 
	is det.
	
:- pred update(mh_var::in, T::in, mh_var_map(T)::in, mh_var_map(T)::out) 
	is semidet.
	
:- pred det_update(mh_var::in, T::in, mh_var_map(T)::in, mh_var_map(T)::out) 
	is det.
	
%-----------------------------------------------------------------------------%
% Removal

	% Remove a key-value pair from a map and return the value.
	% Fail if the key is not present.
:- pred id_remove(var_id::in, T::out, mh_var_map(T)::in, mh_var_map(T)::out) 
	is semidet.
:- pred remove(mh_var::in, T::out, mh_var_map(T)::in, mh_var_map(T)::out) 
	is semidet.

:- pred det_remove(mh_var::in, T::out, mh_var_map(T)::in, mh_var_map(T)::out) 
	is det.

	% Delete a key-value pair from a map.
	% If the key is not present, leave the map unchanged.	
:- pred id_delete(var_id::in, mh_var_map(T)::in, mh_var_map(T)::out) is det.
:- pred delete(mh_var::in, mh_var_map(T)::in, mh_var_map(T)::out) is det.

:- pred id_delete_list(list(var_id)::in, mh_var_map(T)::in, 
	mh_var_map(T)::out)	is det.
:- pred delete_list(list(mh_var)::in, mh_var_map(T)::in, mh_var_map(T)::out)
	is det.	
	
%-----------------------------------------------------------------------------%
% Set operations

:- func union(func(T, T) = T, mh_var_map(T), mh_var_map(T)) = mh_var_map(T).

:- pred union(func(T, T) = T, mh_var_map(T), mh_var_map(T), mh_var_map(T)).
:- mode union(in(func(in, in) = out is det), in, in, out) is det.
:- mode union(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func intersect(func(T, T) = T, mh_var_map(T), mh_var_map(T)) = 
	mh_var_map(T).
	
:- pred intersect(func(T, T) = T, mh_var_map(T), mh_var_map(T), mh_var_map(T)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.
:- mode intersect(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func difference(mh_var_map(T), mh_var_map(_)) = mh_var_map(T).

:- pred difference(mh_var_map(T)::in, mh_var_map(_)::in, mh_var_map(T)::out)
	is det.

%-----------------------------------------------------------------------------%
% Higher order

:- pred fold_id(pred(var_id, T, A, A), mh_var_map(T), A, A).
:- mode fold_id(in(pred(in, in, in, out) is det), in, in, out) is det.
:- mode fold_id(in(pred(in, in, mdi, muo) is det), in, mdi, muo) is det.
:- mode fold_id(in(pred(in, in, di, uo) is det), in, di, uo) is det.
/*:- mode fold_id(in(pred(in, in, array_di, array_uo) is det), in, 
	array_di, array_uo) is det. */
:- mode fold_id(in(pred(in, in, in, out) is semidet), in, in, out) is semidet.
:- mode fold_id(in(pred(in, in, mdi, muo) is semidet), in, mdi, muo) 
	is semidet.

:- pred fold(pred(mh_var, T, A, A), mh_var_map(T), A, A).
:- mode fold(in(pred(in, in, in, out) is det), in, in, out) is det.
:- mode fold(in(pred(in, in, mdi, muo) is det), in, mdi, muo) is det.
:- mode fold(in(pred(in, in, di, uo) is det), in, di, uo) is det.
/*:- mode fold(in(pred(in, in, array_di, array_uo) is det), in, 
	array_di, array_uo) is det.*/
:- mode fold(in(pred(in, in, in, out) is semidet), in, in, out) is semidet.
:- mode fold(in(pred(in, in, mdi, muo) is semidet), in, mdi, muo) 
	is semidet.
	
:- pred map_id(pred(var_id, T, U), mh_var_map(T), mh_var_map(U)).
:- mode map_id(in(pred(in, in, out) is det), in, out) is det.
:- mode map_id(in(pred(in, in, out) is semidet), in, out) is semidet.


:- pred map(pred(mh_var, T, U), mh_var_map(T), mh_var_map(U)).
:- mode map(in(pred(in, in, out) is det), in, out) is det.
:- mode map(in(pred(in, in, out) is semidet), in, out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module string.
:- import_module pair.

:- import_module util.

%-----------------------------------------------------------------------------%
%  Var Maps

% Var maps are represented by a sparse array indexed by a var_set

:- type mh_var_map(T)
	--->	var_map_empty
	;		var_map(set::mh_var_set, array::array(T)).

init(init).
init = var_map_empty.


empty_var_map(empty_var_map).
empty_var_map = init.

singleton_id(ID, T) = var_map(singleton_var_set(ID), array.init(1, T)).
singleton_id(ID, T, singleton_id(ID, T)).

singleton(var(ID), T) = singleton_id(ID, T).
singleton(Var, T, singleton(Var, T)).



var_map_bounds(var_map_empty, Offset, ID_Set) :- 
	var_set_bounds(empty_var_set, Offset, ID_Set).
	
var_map_bounds(var_map(Set, _), Offset, ID_Set) :- 
	var_set_bounds(Set, Offset, ID_Set).

%-----------------------------------------------------------------------------%
% Conversion

from_array(Array, Map) :- from_offset_array(Array, null_var_id_offset, Map).

from_array(Array) = Map :- from_array(Array, Map).

from_offset_array(Array, Offset, Map) :- 
	from_offset_array(Array, Offset, first_var_id, var_map_empty, Map).

from_offset_array(Array, Offset) = Map :- from_offset_array(Array, Offset, Map).

:- pred from_offset_array(array(T)::in, var_id_offset::in, var_id::in, 
	mh_var_map(T)::in, mh_var_map(T)::out) is det.
	
from_offset_array(Array, Offset, Current, !Map) :-
	(if var_id_array_in_bounds(Array, Current) then
		var_id_array_lookup(Array, Current, T),
		var_id_offset(OffsetID, Current, Offset),
		det_id_insert(OffsetID, T, !Map),
		from_offset_array(Array, Offset, next_var_id(Current), !Map)
	else
		true
	).

%-----------------------------------------------------------------------------%
% Looking up variables in var_maps

contains_id(var_map_empty, _) :- fail.
contains_id(var_map(Set, _), ID) :- var_set_contains_id(Set, ID).

contains(Map, var(ID)) :- contains_id(Map, ID).

%-----------------------------------------------------------------------------%

id_search(var_map(Set, Array), ID, T) :- 
	id_sparse_index(ID, Set, Index),
	array.lookup(Array, Index, T).

id_search(Map, ID) = T :- id_search(Map, ID, T).

search(Map, var(ID), id_search(Map, ID)).

search(Map, Var) = T :- search(Map, Var, T).

%-----------------------------------------------------------------------------%

id_lookup(Map, ID, Term) :-
	( if id_search(Map, ID, Found)
	then Term = Found
	 else
        report_lookup_error("mh_var_map.id_lookup: Var ID not found.", ID)
	).
	
id_lookup(Map, ID) = Term :- id_lookup(Map, ID, Term).

%-----------------------------------------------------------------------------%

lookup(Map, var(ID), Term) :- 
	( if id_search(Map, ID, Found)
	then Term = Found
	 else
        report_lookup_error("mh_var_map.lookup: Var ID not found.", ID)
	).

lookup(Map, Var) = Term :- lookup(Map, Var, Term).


%-----------------------------------------------------------------------------%
% Iterator 

:- type var_map_iterator == int.

first(var_map(_, A), array.lookup(A, Index), Index@0).
first(var_map(S, A), id_reverse_sparse_index(Index, S), array.lookup(A, Index), 
	Index@0).

det_first(Map, T, I) :-
	( if first(Map, T0, I0)
	then 
		T = T0,
		I = I0
	else
		error($pred,
		"mh_var_map.det_first: Cannot produce iterator on empty map.")
	).
	
det_first(Map, ID, T, I) :-
	( if first(Map, ID0, T0, I0)
	then 
		ID = ID0,
		T = T0,
		I = I0
	else
	error($pred,
		"mh_var_map.det_first: Cannot produce iterator on empty map.")
	).
	
next(var_map(_, A), array.lookup(A, Current), Last, Current) :-
	Last < max(A),
	Current = Last + 1.
	
next(var_map(S, A), id_reverse_sparse_index(Current, S),
	array.lookup(A, Current), Last, Current) :-
	Last < max(A),
	Current = Last + 1.	
	
init_first(S, ID, Index@0) :-
	(if id_reverse_sparse_index(Index, S) = FirstID
	then ID = FirstID
	else unexpected($pred, "Unable to produce ID for new var map iterator")
	).

init_next(S, id_reverse_sparse_index(Index, S), Last, Index@(Last + 1) ).

iterator_index(I) = I.
	
%-----------------------------------------------------------------------------%
% Insertion

id_insert(ID, T, var_map_empty, singleton_id(ID, T)).

id_insert(ID, T, var_map(!.Set, !.Array), var_map(!:Set, !:Array)) :-
	var_set_insert_id(ID, !Set),
	id_sparse_index(ID, !.Set, Index),
	array_insert(Index, T, !Array).
	
det_id_insert(ID, T, !Map) :-
	(if id_insert(ID, T, !Map)
	then true
	else report_lookup_error(
		"mh_var_map.det_id_insert: Var ID aleady present in map", ID, !.Map)
	).
	
insert(var(ID), T, !Map) :- id_insert(ID, T, !Map).

det_insert(Var, T, !Map) :-
	(if insert(Var, T, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_var_map.det_insert: Var ID aleady present in map", Var, !.Map)
	).

det_insert_from_corresponding_lists([], [], !Map).
det_insert_from_corresponding_lists([], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
det_insert_from_corresponding_lists([_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
det_insert_from_corresponding_lists([K | Ks], [V | Vs], !Map) :-
    det_insert(K, V, !Map),
    det_insert_from_corresponding_lists(Ks, Vs, !Map).

det_insert_from_assoc_list([], !Map).
det_insert_from_assoc_list([K - V | KVs], !Map) :-
    det_insert(K, V, !Map),
    det_insert_from_assoc_list(KVs, !Map).	
	
search_insert(var(ID), T, Result, !Map) :-
	( if id_search(!.Map, ID, Old)
	then
		Result = yes(Old)
	else
		Result = no
	),
	id_set(ID, T, !Map).
	
id_set(ID, T, !Map) :-
	(if id_insert(ID, T, !Map)
	then !:Map = !.Map
	else 
		( if
			!.Map = var_map(Set, MapArray),
			id_sparse_index(ID, Set, Index)
		then
			slow_set(Index, T, MapArray, NewMapArray),
			!:Map = var_map(Set, NewMapArray)
		else
			unexpected($pred, 
			"empty var_map or sparse index failure, this should be impossible")
		)
	).
			
set(var(ID), T, !Map) :- id_set(ID, T, !Map).


set_from_corresponding_lists([], [], !Map).
set_from_corresponding_lists([], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
set_from_corresponding_lists([_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
set_from_corresponding_lists([K | Ks], [V | Vs], !Map) :-
    set(K, V, !Map),
    set_from_corresponding_lists(Ks, Vs, !Map).

set_from_assoc_list([], !Map).
set_from_assoc_list([K - V | KVs], !Map) :-
    set(K, V, !Map),
    set_from_assoc_list(KVs, !Map).
	
	
id_update(ID, T, var_map(Set, !.Array), var_map(Set, !:Array)) :-
	id_sparse_index(ID, Set, Index),
	slow_set(Index, T, !Array).
	
det_id_update(Var, T, !Map) :-	
	(if id_update(Var, T, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_var_map.det_id_update: Var ID not present in map", Var, !.Map)
	).

update(var(ID), T, !Map) :- id_update(ID, T, !Map).

det_update(Var, T, !Map) :-	
	(if update(Var, T, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_var_map.det_update: Var ID not present in map", Var, !.Map)
	).

%-----------------------------------------------------------------------------%
% Removal

id_remove(ID, T, var_map(Set0, Array0), Map) :-
	id_sparse_index(ID, Set0, Index),
	var_set_remove_id(ID, Set0, Set),
	lookup(Array0, Index, T),
	(if empty_var_set(Set)
	then
		Map = var_map_empty
	else
		array_delete(Index, Array0, Array),
		Map = var_map(Set, Array)
	).
	
remove(var(ID), T, !Map) :- id_remove(ID, T, !Map).

det_remove(Var, T, !Map) :-	
	(if remove(Var, Removed, !Map)
	then !:Map = !.Map, T = Removed
	else report_lookup_error(
		"mh_var_map.det_remove: Var ID not present in map", Var, !.Map)
	).

id_delete(ID, !Map) :-
	!:Map =
		(if
			!.Map = var_map(Set, Array),
			id_sparse_index(ID, Set, Index),
			var_set_remove_id(ID, Set, NewSet)		
		then
			(if 
				not empty_var_set(NewSet),
				array_delete(Index, Array, NewArray)	
			then var_map(NewSet, NewArray)
			else var_map_empty
			)
		else
			!.Map
		).

delete(var(ID), !Map) :- id_delete(ID, !Map).

id_delete_list([], !Map).
id_delete_list([ID | IDs], !Map) :- 
	id_delete(ID, !Map),
	id_delete_list(IDs, !Map).
	
delete_list([], !Map).
delete_list([ID | IDs], !Map) :- 
	delete(ID, !Map),
	delete_list(IDs, !Map).
	
%-----------------------------------------------------------------------------%
% Set operations	

union(F, M1, M2) = M :-
    union(F, M1, M2, M).

union(F, M1, M2, M) :-
	require_complete_switch [M1] (
		M1 = var_map_empty, 
		M = M2
	;
		M1 = var_map(S1, _),
		require_complete_switch [M2] (
			M2 = var_map_empty, 
			M = M2
		;
			M2 = var_map(S2, _),		
			var_set_union(S1, S2, S),
			var_set_count(S, ArraySize),
			init_first(S, FirstID, Iter),
			array.init(ArraySize, element_union(F, M1, M2, FirstID), NewArray),
			union_loop(F, M1, M2, S, Iter, NewArray, FinalArray),
			M = var_map(S, FinalArray)
		)
	).
	
:- func element_union(func(T, T) = T, mh_var_map(T), mh_var_map(T), var_id) 
	= T.
:- mode element_union(in(func(in, in) = out is det), in, in, in) = out is det.
:- mode element_union(in(func(in, in) = out is semidet), in, in, in) = out
	is semidet.
	
element_union(F, M1, M2, ID) = 
	(if id_search(M1, ID) = T1
	then
		(if id_search(M2, ID) = T2
		then
			F(T1, T2)
		else
			T1
		)
	else id_lookup(M2, ID)
	).
	
:- pred union_loop(func(T, T) = T, mh_var_map(T), mh_var_map(T), mh_var_set, 
	var_map_iterator, array(T), array(T)).
:- mode union_loop(in(func(in, in) = out is det), in, in, in, in,
	array_di, array_uo)	is det.
:- mode union_loop(in(func(in, in) = out is semidet), in, in, in, in, 
	array_di, array_uo) is semidet.
	
union_loop(F, M1, M2, S, Last, !Array) :-
	(if init_next(S, ID, Last, Current)
	then
		array.set(iterator_index(Current), element_union(F, M1, M2, ID), !Array),
		union_loop(F, M1, M2, S, Current, !Array)
	else
		true
	).
	
intersect(F, M1, M2) = M :- 
	intersect(F, M1, M2, M).

intersect(F, M1, M2, M) :- 
    require_complete_switch [M1] (
		M1 = var_map_empty,
		M = var_map_empty
	;
		M1 = var_map(S1, _),
		require_complete_switch [M2] (
			M2 = var_map_empty,
			M = var_map_empty
		;
			M2 = var_map(S2, _),
			var_set_intersection(S1, S2, S),
			(if empty_var_set(S)
			then
				M = var_map_empty
			else
				init_first(S, FirstID, Iter),
				var_set_count(S, ArraySize),
				array.init(ArraySize, element_intersect(F, M1, M2, FirstID), NewArray),
				intersect_loop(F, M1, M2, S, Iter, NewArray, FinalArray),
				M = var_map(S, FinalArray)
			)
		)
	).

	
:- func element_intersect(func(T, T) = T, mh_var_map(T), mh_var_map(T), var_id) = T.
:- mode element_intersect(in(func(in, in) = out is det), in, in, in) = out 
	is det.
:- mode element_intersect(in(func(in, in) = out is semidet), in, in, in) = out
	is semidet.
	
element_intersect(F, M1, M2, ID) = F(id_lookup(M1, ID), id_lookup(M2, ID)).
	
:- pred intersect_loop(func(T, T) = T, mh_var_map(T), mh_var_map(T), mh_var_set, 
	var_map_iterator, array(T), array(T)).
:- mode intersect_loop(in(func(in, in) = out is det), in, in, in, in,
	array_di, array_uo)	is det.
:- mode intersect_loop(in(func(in, in) = out is semidet), in, in, in, in, 
	array_di, array_uo) is semidet.
	
intersect_loop(F, M1, M2, S, Last, !Array) :-
	(if init_next(S, ID, Last, Current)
	then
		array.set(iterator_index(Current), element_intersect(F, M1, M2, ID), 
			!Array),
		intersect_loop(F, M1, M2, S, Current, !Array)
	else
		true
	).
	
difference(M1, M2) = M :-
	difference(M1, M2, M).
	
difference(M1, M2, M) :- 
	require_complete_switch [M1] (
		M1 = var_map_empty,
		M = M1
	;
		M1 = var_map(S1, _),
		require_complete_switch [M2] (
			M2 = var_map_empty,
			M = M1
		;
			M2 = var_map(S2, _),
			var_set_difference(S1, S2, S),
			(if empty_var_set(S)
			then
				M = var_map_empty
			else
				var_set_count(S, ArraySize),
				init_first(S, FirstID, Iter),
				array.init(ArraySize, id_lookup(M1, FirstID), NewArray),
				difference_loop(M1, S, Iter, NewArray, FinalArray),
				M = var_map(S, FinalArray)
			)
		)
	).

:- pred difference_loop(mh_var_map(T)::in, mh_var_set::in,
	var_map_iterator::in, array(T)::array_di, array(T)::array_uo) is det.
	
difference_loop(M, S, Last, !Array) :-
	(if init_next(S, ID, Last, Current)
	then
		array.set(iterator_index(Current), id_lookup(M, ID), !Array),
		difference_loop(M, S, Current, !Array)
	else
		true
	).

%-----------------------------------------------------------------------------%
% Higher order

fold_id(P, Map, !A) :-
	if first(Map, ID, T, Iterator) then
		P(ID, T, !A),
		fold_id(P, Map, !A, Iterator)
	else true.
	
	
:- pred fold_id(pred(var_id, T, A, A), mh_var_map(T), A, A, var_map_iterator).
:- mode fold_id(in(pred(in, in, in, out) is det), in, in, out, in) is det.
:- mode fold_id(in(pred(in, in, mdi, muo) is det), in, mdi, muo, in) is det.
:- mode fold_id(in(pred(in, in, di, uo) is det), in, di, uo, in) is det.
:- mode fold_id(in(pred(in, in, array_di, array_uo) is det), in, 
	array_di, array_uo, in) is det.
:- mode fold_id(in(pred(in, in, in, out) is semidet), in, in, out, in) is semidet.
:- mode fold_id(in(pred(in, in, mdi, muo) is semidet), in, mdi, muo, in) 
	is semidet.
	
fold_id(P, Map, !A, Iterator0) :-
	if next(Map, ID, T, Iterator0, Iterator) then
		P(ID, T, !A),
		fold_id(P, Map, !A, Iterator)
	else true.
	
:- pred curry_fold(pred(mh_var, T, A, A), var_id, T, A, A).
:- mode curry_fold(in(pred(in, in, in, out) is det), in, in, in, out) is det.
:- mode curry_fold(in(pred(in, in, mdi, muo) is det), in, in, mdi, muo) is det.
:- mode curry_fold(in(pred(in, in, di, uo) is det), in, in, di, uo) is det.
/*:- mode curry_fold(in(pred(in, in, array_di, array_uo) is det), in, in, 
	array_di, array_uo) is det. */
:- mode curry_fold(in(pred(in, in, in, out) is semidet), in, in, in, out)
	is semidet.
:- mode curry_fold(in(pred(in, in, mdi, muo) is semidet), in, in, mdi, muo) 
	is semidet.
	
curry_fold(P, ID, T, !A) :- P(var(ID), T, !A).

fold(P, Map, !A) :-
	CurriedP = curry_fold(P),
	fold_id(CurriedP, Map, !A).

map_id(P, MapT, MapU) :-
	if first(MapT, ID, T, Iterator) then
		P(ID, T, U),
		map_id(P, MapT, singleton_id(ID, U), MapU, Iterator)
	else 
		MapU = var_map_empty.

:- pred map_id(pred(var_id, T, U), mh_var_map(T), mh_var_map(U), mh_var_map(U),
	var_map_iterator).
:- mode map_id(in(pred(in, in, out) is det), in, in, out, in) is det.
:- mode map_id(in(pred(in, in, out) is semidet), in, in, out, in) is semidet.

map_id(P, MapT, !MapU, Iterator0) :-
	if next(MapT, ID, T, Iterator0, Iterator) then
		P(ID, T, U),
		det_id_insert(ID, U, !MapU),
		map_id(P, MapT, !MapU, Iterator)
	else true.
	
:- pred curry_map(pred(mh_var, T, U), var_id, T, U).
:- mode curry_map(in(pred(in, in, out) is det), in, in, out) is det.
:- mode curry_map(in(pred(in, in, out) is semidet), in, in, out) is semidet.

curry_map(P, ID, T, U) :- P(var(ID), T, U).

map(P, !Map) :- map_id(curry_map(P), !Map).
