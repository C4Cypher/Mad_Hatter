%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_tuple_exact_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_tuple_exact_map.

:- interface.

:- use_module map.
:- import_module array.
:- import_module pair.
:- import_module list.
:- import_module assoc_list.
:- import_module unit.

:- import_module mh_term.
:- import_module mh_tuple.

% TODO: get general hashing finished and replace map.map with hashmap.hashmap

%-----------------------------------------------------------------------------%
% Exact Tuple map - tuple map optimized for exact tuple lookup

:- type tuple_exact_map(T) == map.map(array(mh_term), pair(mh_tuple,T)).
:- type tuple_exact_set == tuple_exact_map(unit).

:- func init = (tuple_exact_map(T)::out) is det.
:- pred init(tuple_exact_map(_)::out) is det.

:- func singleton(mh_tuple, T) = tuple_exact_map(T).

:- pred is_empty(tuple_exact_map(_)::in) is semidet.

%-----------------------------------------------------------------------------%
% Search

	% Succeeds if the map contains the given key
:- pred contains(tuple_exact_map(T)::in, mh_tuple::in) is semidet.

	% Fails if the key is not found
:- pred search(tuple_exact_map(T)::in, mh_tuple::in, 
T::out) is semidet.
:- func search(tuple_exact_map(T), mh_tuple) = T is semidet.

	% Throws an exception if the key is not found
:- pred lookup(tuple_exact_map(T)::in, mh_tuple::in, T::out) is det.
:- func lookup(tuple_exact_map(T), mh_tuple) = T is det.

%-----------------------------------------------------------------------------%
% Insertion

:- pred insert(mh_tuple::in, T::in, tuple_exact_map(T)::in, 
	tuple_exact_map(T)::out) is semidet.
	
:- pred det_insert(mh_tuple::in, T::in, tuple_exact_map(T)::in, 
	tuple_exact_map(T)::out) is det.
	
:- pred det_insert_from_corresponding_lists(list(mh_tuple)::in, list(T)::in,
	tuple_exact_map(T)::in, tuple_exact_map(T)::out) is det.
	
:- pred det_insert_from_assoc_list(assoc_list(mh_tuple, T)::in,
	tuple_exact_map(T)::in, tuple_exact_map(T)::out) is det.
	
:- pred unsafe_array_insert(mh_tuple::in, array(mh_term)::in, T::in, 
	tuple_exact_map(T)::in,	tuple_exact_map(T)::out) is semidet.
	
:- pred det_unsafe_array_insert(mh_tuple::in, array(mh_term)::in, T::in, 
	tuple_exact_map(T)::in,	tuple_exact_map(T)::out) is det.
	
:- pred det_unsafe_array_insert_from_corresponding_lists(list(mh_tuple)::in,
	list(array(mh_term)), list(T)::in,	tuple_exact_map(T)::in, 
	tuple_exact_map(T)::out) is det.	

:- pred set(mh_tuple::in, T::in, tuple_exact_map::in, tuple_exact_map::out)
	is det.
	
:- pred set_from_corresponding_lists(list(mh_tuple)::in, list(T)::in,
	tuple_exact_map(T)::in, tuple_exact_map(T)::out) is det.
	
:- pred set_from_assoc_list(assoc_list(mh_tuple, T)::in,
	tuple_exact_map(T)::in, tuple_exact_map(T)::out) is det.
	
- pred unsafe_array_set(mh_tuple::in, array(mh_term)::in, T::in, 
	tuple_exact_map(T)::in,	tuple_exact_map(T)::out) is det.
	
:- pred unsafe_array_set_from_corresponding_lists(list(mh_tuple)::in,
	list(array(mh_term)), list(T)::in,	tuple_exact_map(T)::in, 
	tuple_exact_map(T)::out) is det.	

:- pred update(mh_tuple::in, T::in, tuple_exact_map(T)::in, 
	tuple_exact_map::out) is semidet.
	
:- pred det_update(mh_tuple::in, T::in, tuple_exact_map(T)::in, 
	tuple_exact_map::out) is det.
	
:- pred unsafe_array_update(mh_tuple::in, array(mh_term)::in, T::in, 
	tuple_exact_map(T)::in,	tuple_exact_map(T)::out) is semidet.
	
:- pred det_unsafe_array_update(mh_tuple::in, array(mh_term)::in, T::in, 
	tuple_exact_map(T)::in,	tuple_exact_map(T)::out) is det.
	
%-----------------------------------------------------------------------------%
% Removal

:- pred remove(mh_tuple::in, T::out, tuple_exact_map(T)::in, 
	tuple_exact_map::out) is semidet.
	
:- pred det_remove(mh_tuple::in, T::out, tuple_exact_map(T)::in, 
	tuple_exact_map::out) is det.
	
:- pred unsafe_array_remove(array(mh_term)::in, mh_tuple::out, T::out,
	tuple_exact_map(T)::in, tuple_exact_map::out) is semidet.
	
:- pred det_unsafe_array_remove(array(mh_term)::in, mh_tuple::out, T::out,
	tuple_exact_map(T)::in, tuple_exact_map::out) is det.
	
:- pred delete(mh_tuple::in,  tuple_exact_map(T)::in, 
	tuple_exact_map::out) is det.

:- pred delete_list(list(mh_tuple)::in, tuple_exact_map(T)::in, 
	tuple_exact_map::out) is det.
	
:- pred array_delete(array(mh_term)::in,  tuple_exact_map(T)::in, 
	tuple_exact_map::out) is det.

:- pred array_delete_list(list(array(mh_term))::in, tuple_exact_map(T)::in, 
	tuple_exact_map::out) is det.
	
%-----------------------------------------------------------------------------%
% Set operations


:- func union(func(T, T) = T, tuple_exact_map(T), tuple_exact_map(T)) =
	tuple_exact_map(T).

:- pred union(func(T, T) = T, tuple_exact_map(T), tuple_exact_map(T), tuple_exact_map(T)).
:- mode union(in(func(in, in) = out is det), in, in, out) is det.
:- mode union(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func set_union(tuple_exact_map(_), tuple_exact_map(_)) = tuple_exact_set.
:- pred set_union(tuple_exact_map(_)::in, tuple_exact_map(_)::in, 
	tuple_exact_set::out)	is det.

:- func intersect(func(T, T) = T, tuple_exact_map(T), tuple_exact_map(T))
	= var_map(T).
	
:- pred intersect(func(T, T) = T, tuple_exact_map(T), tuple_exact_map(T),
	tuple_exact_map(T)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.
:- mode intersect(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func set_intersect(tuple_exact_map(_), tuple_exact_map(_)) = 
	tuple_exact_set.
:- pred set_intersect(tuple_exact_map(_)::in, tuple_exact_map(_)::in, 
	tuple_exact_set::out) is det.

:- func difference(tuple_exact_map(T), tuple_exact_map(_)) =
	tuple_exact_map(T).

:- pred difference(var_map(T)::in, tuple_exact_map(_)::in, 
	tuple_exact_map(T)::out) is det.
	
:- func set_difference(tuple_exact_map(_), tuple_exact_map(_)) = 
	tuple_exact_set.
:- pred set_difference(tuple_exact_map(_)::in, tuple_exact_map(_)::in, 
	tuple_exact_set::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module maybe.

:- import_module mh_term_map.
:- import_module util.

%-----------------------------------------------------------------------------%
% Basic operations

init = map.init.
init(init).

singleton(Tuple, T) = 

is_empty(init).

%-----------------------------------------------------------------------------%
% Search

contains(Map, Tuple) :- map.contains(Map, to_array(Tuple)).

search(Map, Tuple, search(Map, Tuple)). 
search(Map, Tuple) = T :- map.search(Map, to_array(Tuple), ( _ - T)).

lookup(Map, Tuple, lookup(Map, Tuple)).

det_lookup(Map, Tuple) = 
	(if search(Map, Tuple) = Found
	then 
		Found
	else
		report_lookup_error("mh_tuple_exact_map.lookup: key not found", K)
	).

%-----------------------------------------------------------------------------%
% Insertion

insert(Tuple, T, !Map) :- 
	unsafe_array_insert(Tuple, to_array(Tuple), T, !Map).

:- pragma inline(insert/4).

det_insert(Tuple, T, !Map) :-
	(if insert(Tuple, T, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_tuple_exact_map.det_insert: tuple aleady present in map", 
		Tuple, !.Map)
	).
	
:- pragma inline(det_insert/4).

det_insert_from_corresponding_lists([], [], !Map).
det_insert_from_corresponding_lists([], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
det_insert_from_corresponding_lists([_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
det_insert_from_corresponding_lists([K | Ks], [V | Vs], !Map) :-
    det_insert(K, V, !Map),
    det_insert_from_corresponding_lists(Ks, Vs, !Map).
	
:- pragma inline(det_insert_from_corresponding_lists/4).

det_insert_from_assoc_list([], !Map).
det_insert_from_assoc_list([K - V | KVs], !Map) :-
    det_insert(K, V, !Map),
    det_insert_from_assoc_list(KVs, !Map).
	
:- pragma inline(det_insert_from_assoc_list/3).

unsafe_array_insert(Tuple, Array, T, !Map) :- 
	map.insert(Array, Tuple - T, !Map).
	
:- pragma inline(unsafe_array_insert/5).
	
det_unsafe_array_insert(Tuple, Array, T, !Map) :-
	(if unsafe_array_insert(Tuple, Array, T, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_tuple_exact_map.det_unsafe_array_insert: array aleady present in map", 
		Tuple, !.Map)
	).
	
:- pragma inline(det_unsafe_array_insert/5).	
	
det_unsafe_array_insert_from_corresponding_lists([], [], [], !Map).
det_unsafe_array_insert_from_corresponding_lists([], [_ | _], [_ | _], _, _) :-
	unexpected($pred, "list length mismatch").
det_unsafe_array_insert_from_corresponding_lists([_ | _], [], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
det_unsafe_array_insert_from_corresponding_lists([_ | _], [_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
det_unsafe_array_insert_from_corresponding_lists([K | Ks], [A, As], [V | Vs],
	!Map) :-
    det_unsafe_array_insert(K, A, V, !Map),
    det_unsafe_array_insert_from_corresponding_lists(Ks, As, Vs, !Map).
	
:- pragma inline(det_unsafe_array_insert_from_corresponding_lists/5).
	
set(Tuple, T, !Map) :- map.set(to_array(Tuple), Tuple - T, !Map).
	
:- pragma inline(set/4).


set_from_corresponding_lists([], [], !Map).
set_from_corresponding_lists([], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
set_from_corresponding_lists([_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
set_from_corresponding_lists([K | Ks], [V | Vs], !Map) 						:-
    set(K, V, !Map),
    set_from_corresponding_lists(Ks, Vs, !Map).
	
:- pragma inline(set_from_corresponding_lists/4).

set_from_assoc_list([], !Map).
set_from_assoc_list([K - V | KVs], !Map) :-
    set(K, V, !Map),
    set_from_assoc_list(KVs, !Map).
	
:- pragma inline(set_from_assoc_list/3).
	
unsafe_array_set(Tuple, Array, T, !Map) :- 
	map.set(Array, Tuple - T, !Map).
	
:- pragma inline(unsafe_array_set/5).
	
unsafe_array_set_from_corresponding_lists([], [], [], !Map).
unsafe_array_set_from_corresponding_lists([], [_ | _], [_ | _], _, _) :-
	unexpected($pred, "list length mismatch").
unsafe_array_set_from_corresponding_lists([_ | _], [], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
unsafe_array_set_from_corresponding_lists([_ | _], [_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
unsafe_array_set_from_corresponding_lists([K | Ks], [A, As], [V | Vs],
	!Map) :-
    unsafe_array_set(K, A, V, !Map),
    unsafe_array_set_from_corresponding_lists(Ks, As, Vs, !Map).
	
:- pragma inline(unsafe_array_set_from_corresponding_lists/5).
	
update(Tuple, T, !Map) :- 
	unsafe_array_update(Tuple, to_array(Tuple), Tuple - T, !Map).
	
:- pragma inline(update/4).

det_update(Var, T, !Map) :-	
	(if update(Var, T, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_tuple_exact_map.det_update: tuple not present in map", Var, !.Map)
	).
	
:- pragma inline(det_update/4).
	
unsafe_array_update(Tuple, Array, T, !Map) :- 
	map.insert(Array, Tuple - T, !Map).
	
:- pragma inline(unsafe_array_update/5).

	
det_unsafe_array_update(Tuple, Array, T, !Map) :-
	(if unsafe_array_update(Tuple, Array, T, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_tuple_exact_map.det_unsafe_array_update: array not present in map", 
		Tuple, !.Map)
	).
	
:- pragma inline(det_unsafe_array_update/5).
	
%-----------------------------------------------------------------------------%
% Removal

remove(Tuple, T, !Map) :- 
	unsafe_array_remove(Tuple, from_array(Tuple), T, !Map).
	
:- pragma inline(remove/4).
	
det_remove(Tuple, T, !Map) :-	
	(if remove(Tuple, FoundT, !Map)
	then !:Map = !.Map, T = FoundT
	else report_lookup_error(
		"mh_tuple_exact_map.det_remove: tuple not present in map", Tuple, 
		!.Map)
	).
	
:- pragma inline(det_remove/4).
	
unsafe_array_remove(Array, Tuple, T, !Map) :- 
	map.remove(Array, Tuple - T, !Map).
	
:- pragma inline(unsafe_array_remove/5).
	
det_unsafe_array_remove(Array, Tuple, T, !Map) :-	
	(if remove(Array, FoundTuple, FoundT, !Map)
	then !:Map = !.Map, Tuple = FoundTuple, T = FoundT
	else report_lookup_error(
		"mh_tuple_exact_map.det_unsafe_array_remove: array not present in map", 
		Tuple, !.Map)
	).
	
:- pragma inline(det_unsafe_array_remove/5).
	
delete(Tuple, !Map) :- array_delete(to_array(Tuple), !Map).
	
:- pragma inline(delete/3).

delete_list([], !Map).
delete_list([Tuple | Tuples], !Map) :- 
	delete(Tuple, !Map),
	delete_list(Tuples, !Map).
	
:- pragma inline(delete_list/3).
	
array_delete(Array, !Map) :- map.delete(Array, !Map).
	
:- pragma inline(array_delete/3).

array_delete_list([], !Map).
array_delete_list([A | As], !Map) :- 
	array_delete(A, !Map),
	array_delete_list(As, !Map).
	
:- pragma inline(array_delete_list/3).
	
%-----------------------------------------------------------------------------%
% Set operations

%TODO: re-implement all set ops with folds, the default set operations for map
% are inefficient
	

union(F, M1, M2) = foldl(union_insert(F), M2, M1).

:- func union_insert(func(T, T) = T, array(mh_term), T,	tuple_exact_map(T)) =
	tuple_exact_map(T).
	
union_insert(F, Key, V2, !.M1) = !:M1 :-
	map.search_insert(Key, V2, MaybeV1, !M1),
	(if MaybeV1 = yes(V1),
	then
		map.set(Key, F(V1, V2), !M1)
	else
		true
	).

:- pragma inline(union/3).

union(F, M1, M2, union(F, M1, M2)).

:- pragma inline(union/4).

set_union(M1, M2) = 
	map.foldl(union_insert2, M2, map.foldl(union_insert1, M1, map.init)).

:- func union_insert1(array(mh_term), _, tuple_exact_set) = tuple_exact_set.
	
union_insert1(Key, _, !.M3) = !:M3 :-
	map.set(Key, unit, !M3),	
	
:- func union_insert2(array(mh_term), _, tuple_exact_set) = tuple_exact_set.
	
union_insert2(Key, _, !.M3) = !:M3 :-
	map.search_insert(Key, unit, _, !M3).

:- pragma inline(set_union/2).

set_union(M1, M2, set_union(M3)).

:- pragma inline(set_union/3).

intersect(F, M1, M2) = map.intersect(F, M1, M2).
	
:- pragma inline(intersect/3).

intersect(F, M1, M2, intersect(F, M1, M2)).
	
:- pragma inline(intersect/4).

difference(M1, M2) = M :-
	difference(M1, M2, M).
	
:- pragma inline(difference/2).
	
difference(M1, M2, map.delete_list(M1, map.keys(M2))).
	
:- pragma inline(difference/3).