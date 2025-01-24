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

:- import_module mh_term.
:- import_module mh_tuple.

%-----------------------------------------------------------------------------%
% Exact Tuple map - tuple map optimized for exact tuple lookup

:- type tuple_exact_map(T) == map.map(array(mh_term), pair(mh_tuple,T)).

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

:- set(mh_tuple::in, T::in, tuple_exact_map::in, tuple_exact_map::out)
	is det.
	
:- pred set_from_corresponding_lists(list(mh_tuple)::in, list(T)::in,
	tuple_exact_map(T)::in, tuple_exact_map(T)::out) is det.
	
:- pred set_from_assoc_list(assoc_list(mh_tuple, T)::in,
	tuple_exact_map(T)::in, tuple_exact_map(T)::out) is det.

:- pred update(mh_tuple::in, T::in, tuple_exact_map(T)::in, 
	tuple_exact_map::out) is semidet.
	
:- pred det_update(mh_tuple::in, T::in, tuple_exact_map(T)::in, 
	tuple_exact_map::out) is det.
	
%-----------------------------------------------------------------------------%
% Removal

:- pred remove(mh_tuple::in, T::out, tuple_exact_map(T)::in, 
	tuple_exact_map::out) is semidet.
	
:- pred det_remove(mh_tuple::in, T::out, tuple_exact_map(T)::in, 
	tuple_exact_map::out) is det.
	
:- pred delete(mh_tuple::in,  tuple_exact_map(T)::in, 
	tuple_exact_map::out) is det.

:- pred delete_list(list(mh_tuple)::in, tuple_exact_map(T)::in, 
	tuple_exact_map::out) is det.
	
%-----------------------------------------------------------------------------%
% Set operations


:- func union(func(T, T) = T, tuple_exact_map(T), tuple_exact_map(T)) =
	tuple_exact_map(T).

:- pred union(func(T, T) = T, tuple_exact_map(T), tuple_exact_map(T), tuple_exact_map(T)).
:- mode union(in(func(in, in) = out is det), in, in, out) is det.
:- mode union(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func intersect(func(T, T) = T, tuple_exact_map(T), tuple_exact_map(T))
	= var_map(T).
	
:- pred intersect(func(T, T) = T, tuple_exact_map(T), tuple_exact_map(T),
	tuple_exact_map(T)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.
:- mode intersect(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func difference(tuple_exact_map(T), tuple_exact_map(_)) =
	tuple_exact_map(T).

:- pred difference(var_map(T)::in, tuple_exact_map(_)::in, 
	tuple_exact_map(T)::out)	is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

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
		report_lookup_error("tuple_exact_map.lookup: key not found", K)
	).

%-----------------------------------------------------------------------------%
% Insertion

insert(Tuple, T, !Map) :- map.insert(to_array(Tuple), Tuple - T, !Map).

det_insert(Tuple, T, !Map) :-
	(if insert(Tuple, T, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"tuple_exact_map.det_insert: tuple aleady present in map", 
		Tuple, !.Map)
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
	
set(Tuple, T, !Map) :- map.set(to_array(Tuple), Tuple - T, !Map).


set_from_corresponding_lists([], [], !Map).
set_from_corresponding_lists([], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
set_from_corresponding_lists([_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
set_from_corresponding_lists([K | Ks], [V | Vs], !Map) 						:-
    set(K, V, !Map),
    set_from_corresponding_lists(Ks, Vs, !Map).

set_from_assoc_list([], !Map).
set_from_assoc_list([K - V | KVs], !Map) :-
    set(K, V, !Map),
    set_from_assoc_list(KVs, !Map).
	
update(Tuple, T, !Map) :- map.update(to_array(Tuple), Tuple - T, !Map).

det_update(Var, T, !Map) :-	
	(if update(Var, T, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"tuple_exact_map.det_update: tuple not present in map", Var, !.Map)
	).
	
%-----------------------------------------------------------------------------%
% Removal

remove(Tuple, T, !Map) :- map.remove(from_array(Tuple), _ - T, !Map).
	
det_remove(Tuple, T, !Map) :-	
	(if remove(Tuple, T, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"tuple_exact_map.det_remove: Tuple not present in map", Tuple, 
		!.Map)
	).
	
delete(Tuple, !Map) :- map.delete(to_array(Tuple), !Map).

delete_list([], !Map).
delete_list([Tuple | Tuples], !Map) :- 
	delete(Tuple, !Map),
	delete_list(Tuples, !Map).
	
%-----------------------------------------------------------------------------%
% Set operations

union(F, M1, M2) = map.union(F, M1, M2).

union(F, M1, M2, union(F, M1, M2)).

intersect(F, M1, M2) = map.intersect(F, M1, M2).

intersect(F, M1, M2, intersect(F, M1, M2)).

difference(M1, M2) = M :-
	difference(M1, M2, M).
	
difference(M1, M2, map.delete_list(M1, map.keys(M2))).