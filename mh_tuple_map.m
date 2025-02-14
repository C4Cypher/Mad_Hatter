%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_tuple_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_tuple_map.

:- interface.

:- import_module unit.

:- import_module mh_term.
:- import_module mh_tuple.

%-----------------------------------------------------------------------------%
% Tuple map

:- type mh_tuple_map(T).
:- type mh_tuple_set == mh_tuple_map(unit).

:- func init = (mh_tuple_map(T)::uo) is det.
:- pred init(mh_tuple_map(_)::uo) is det.

:- func lazy_init = (mh_tuple_map(T)::uo) is det.
:- pred lazy_init(mh_tuple_map(_)::uo) is det.

:- func singleton(mh_tuple, T) = mh_tuple_map(T).
:- func singleton(mh_tuple) = mh_tuple_set.

:- pred is_empty(mh_tuple_map(_)::in) is semidet.

:- impure pred force_pattern_map(mh_tuple_map(T)::in) is det.

:- pred force_pattern_map(mh_tuple_map(T)::di, mh_tuple_map(T)::uo) is det.

%-----------------------------------------------------------------------------%
% Search

	% Succeeds if the map contains the given key
:- pred contains(mh_tuple_map(T)::in, mh_tuple::in) is semidet.

	% Fails if the key is not found
:- pred search(mh_tuple_map(T)::in, mh_tuple::in, T::out) is semidet.
:- func search(mh_tuple_map(T), mh_tuple) = T is semidet.

	% Throws an exception if the key is not found
:- pred lookup(mh_tuple_map(T)::in, mh_tuple::in, T::out) is det.
:- func lookup(mh_tuple_map(T), mh_tuple) = T is det.

%-----------------------------------------------------------------------------%
% Insertion

:- pred insert(mh_tuple::in, T::in, mh_tuple_map(T)::in, 
	mh_tuple_map(T)::out) is semidet.
	
:- pred insert(mh_tuple::in, mh_tuple_set::in, mh_tuple_set::out) 
	is semidet.
	
:- pred det_insert(mh_tuple::in, T::in, mh_tuple_map(T)::in, 
	mh_tuple_map(T)::out) is det.
	
:- pred det_insert(mh_tuple::in, mh_tuple_set::in, mh_tuple_set::out)
	is det.
	
:- pred det_insert_from_corresponding_lists(list(mh_tuple)::in, list(T)::in,
	mh_tuple_map(T)::in, mh_tuple_map(T)::out) is det.
	
:- pred det_insert_from_list(list(mh_tuple)::in, mh_tuple_set::in, 
	mh_tuple_set::out) is det.
	
:- pred det_insert_from_assoc_list(assoc_list(mh_tuple, T)::in,
	mh_tuple_map(T)::in, mh_tuple_map(T)::out) is det.

:- pred set(mh_tuple::in, T::in, mh_tuple_map::in, mh_tuple_map::out)
	is det.
	
:- pred set(mh_tuple::in, mh_tuple_set::in, mh_tuple_set::out) is det.
	
:- pred set_from_corresponding_lists(list(mh_tuple)::in, list(T)::in,
	mh_tuple_map(T)::in, mh_tuple_map(T)::out) is det.
	
:- pred set_from_assoc_list(assoc_list(mh_tuple, T)::in,
	mh_tuple_map(T)::in, mh_tuple_map(T)::out) is det.
	
:- pred set_from_list(list(mh_tuple)::in, mh_tuple_set::in, 
	mh_tuple_set::out) is det.

:- pred update(mh_tuple::in, T::in, mh_tuple_map(T)::in, 
	mh_tuple_map::out) is semidet.
	
:- pred det_update(mh_tuple::in, T::in, mh_tuple_map(T)::in, 
	mh_tuple_map::out) is det.
	
%-----------------------------------------------------------------------------%
% Removal

:- pred remove(mh_tuple::in, T::out, mh_tuple_map(T)::in, 
	mh_tuple_map::out) is semidet.
	
:- pred det_remove(mh_tuple::in, T::out, mh_tuple_map(T)::in, 
	mh_tuple_map::out) is det.
	
:- pred delete(mh_tuple::in,  mh_tuple_map(T)::in, 
	mh_tuple_map::out) is det.

:- pred delete_list(list(mh_tuple)::in, mh_tuple_map(T)::in, 
	mh_tuple_map::out) is det.
	
%-----------------------------------------------------------------------------%
% Set operations


:- func union(func(T, T) = T, mh_tuple_map(T), mh_tuple_map(T)) =
	mh_tuple_map(T).

:- pred union(func(T, T) = T, mh_tuple_map(T), mh_tuple_map(T), 
	mh_tuple_map(T)).
:- mode union(in(func(in, in) = out is det), in, in, out) is det.
:- mode union(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func set_union(mh_tuple_map(_), mh_tuple_map(_)) = mh_tuple_set.
:- pred set_union(mh_tuple_map(_)::in, mh_tuple_map(_)::in, mh_tuple_set::out)
	is det.

:- func intersect(func(T, T) = T, mh_tuple_map(T), mh_tuple_map(T))
	= mh_tuple_map(T).
	
:- pred intersect(func(T, T) = T, mh_tuple_map(T), mh_tuple_map(T),
	mh_tuple_map(T)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.
:- mode intersect(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func set_intersect(mh_tuple_map(_), mh_tuple_map(_)) = mh_tuple_set.
:- pred set_intersect(mh_tuple_map(_)::in, mh_tuple_map(_)::in, 
	mh_tuple_set::out) is det.

:- func difference(mh_tuple_map(T), mh_tuple_map(_)) =
	mh_tuple_map(T).

:- pred difference(mh_tuple_map(T)::in, mh_tuple_map(_)::in, 
	mh_tuple_map(T)::out) is det.
	
:- func set_difference(mh_tuple_map(_), mh_tuple_map(_)) = mh_tuple_set.
:- pred set_difference(mh_tuple_map(_)::in, mh_tuple_map(_)::in, 
	mh_tuple_set::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module lazy.
:- import_module array.
:- import_module list.
:- use_module map.

:- use_module mh_tuple_exact_map.
:- use_module mh_tuple_pattern_map.

%-----------------------------------------------------------------------------%

:- type exact_map(T) == mh_tuple_exact_map.tuple_exact_map(T).
:- type pattern_map(T) == mh_tuple_pattern_map.tuple_pattern_map(T).
:- type lazy_pattern_map(T) == lazy(pattern_map(T)). 

:- type mh_tuple_map(T)
	--->	tuple_map(exact_map(T), lazy_pattern_map(T)).
	
init = tuple_map(mh_tuple_exact_map.init, val(mh_tuple_pattern_map.init)).

init(init).

lazy_init = tuple_map(mh_tuple_exact_map.init@Map, delay(from_exact_map(Map))).

singleton(Tuple, T) = tuple_map(mh_tuple_exact_map.singleton(Tuple, T),
	val(mh_tuple_pattern_map.singleton(Tuple, T))).
	
lazy_singleton(Tuple, T) = 
	tuple_map(mh_tuple_exact_map.singleton(Tuple, T)@Map, 
		delay(from_exact_map(Map))).
	
is_empty(tuple_map(Map, _)) :- tuple_exact_map.is_empty(Map).

:- impure pred eager(lazy_pattern)

force_pattern_map(tuple_map(_, Lazy)) :- _ = force(Lazy).

:- pragma promise_impure(force_pattern_map/1).

force_pattern_map(!Map) :-
	impure force_pattern_map(!.Map).
	
:- pragma promise_pure(force_pattern_map/2).
	
%-----------------------------------------------------------------------------%
% Search

contains(tuple_map(Map, _), Tuple) :- tuple_exact_map.contains(Map, Tuple).

search(Map, Tuple, search(Map, Tuple)).

search(tuple_map(Map, _), Tuple) = tuple_exact_map.search(Map, Tuple).

lookup(Map, Tuple, lookup(Map, Tuple)).

lookup(tuple_map(Map, _), Tuple) = tuple_exact_map.lookup(Map, Tuple).


%-----------------------------------------------------------------------------%
% Insertion

insert(Tuple, T, tuple_exact_map(!.E, !.L), tuple_exact_map(!:E, !:L)) :-
	Array = to_array(Tuple),
	mh_tuple_exact_map.unsafe_array_insert(Tuple, T, Array,	!E),
	promise_pure 
		(if impure read_if_val(!.L, P0)
		then
			mh_tuple_pattern_map.unsafe_array_insert(Tuple, Array, T, P0, P),
			!:L = val(P)
		else
			!:L = delay(from_exact_map(!.E))
		).
		
insert(Tuple, !Set) :- insert(Tuple, unit, !Set).
		
det_insert(Tuple, T, !Map) :-
	(if insert(Tuple, T, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_tuple_map.det_insert: tuple aleady present in map", 
		Tuple, !.Map)
	).
		

det_insert(Tuple, !Map) :-
	(if insert(Tuple, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_tuple_map.det_insert: tuple aleady present in map", 
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
	
det_insert_from_list([], !Set).
det_insert_from_list([ Tuple | List]) :-
	det_insert(Tuple, !Set),
	det_insert_from_list(List, !Set).
	
det_insert_from_assoc_list([], !Map).
det_insert_from_assoc_list([K - V | KVs], !Map) :-
    det_insert(K, V, !Map),
    det_insert_from_assoc_list(KVs, !Map).
	

set(Tuple, T, tuple_exact_map(!.E, !.L), tuple_exact_map(!:E, !:L)) :-
	Array = to_array(Tuple),
	mh_tuple_exact_map.unsafe_array_set(Tuple, T, Array, !E),
	promise_pure 
		(if impure read_if_val(!.L, P0)
		then
			mh_tuple_pattern_map.unsafe_array_set(Tuple, Array, T, P0, P),
			!:L = val(P)
		else
			!:L = delay(from_exact_map(!.E))
		).
		
set(Tuple, !Set) :- set(Tuple, unit, !Set).	

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
	
set_from_list([], !Set).
set_from_list([Tuple | List]) :-
	set(Tuple, !Set),
	set_from_list(List, !Set).
	
	
update(Tuple, T, tuple_exact_map(!.E, !.L), tuple_exact_map(!:E, !:L)) :-
	Array = to_array(Tuple),
	mh_tuple_exact_map.unsafe_array_update(Tuple, T, Array,	!E),
	promise_pure 
		(if impure read_if_val(!.L, P0)
		then
			mh_tuple_pattern_map.unsafe_array_set(Tuple, Array, T, P0, P),
			!:L = val(P)
		else
			!:L = delay(from_exact_map(!.E))
		).

det_update(Var, T, !Map) :-	
	(if update(Var, T, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_tuple_map.det_update: tuple not present in map", Var, !.Map)
	).
	
%-----------------------------------------------------------------------------%
% Removal


remove(Tuple, T, tuple_exact_map(!.E, !.L), tuple_exact_map(!:E, !:L)) :-
	Array = to_array(Tuple),
	mh_tuple_exact_map.unsafe_array_remove(Tuple, Array, T, !E)
	promise_pure 
		(if impure read_if_val(!.L, P0)
		then
			mh_tuple_pattern_map.array_delete(Array, P0, P),
			!:L = val(P)
		else
			!:L = delay(from_exact_map(!.E))
		).
		

det_remove(Tuple, T, !Map) :-	
	(if remove(Tuple, FoundT, !Map)
	then !:Map = !.Map, T = FoundT
	else report_lookup_error(
		"mh_tuple_map.det_remove: tuple not present in map", Tuple, 
		!.Map)
	).
	
delete(Tuple, !Map) :- array_delete(to_array(Tuple), !Map).
	Array = to_array(Tuple),
	mh_tuple_exact_map.array_delete(Array, !E)
	promise_pure 
		(if impure read_if_val(!.L, P0)
		then
			mh_tuple_pattern_map.array_delete(Array, P0, P),
			!:L = val(P)
		else
			!:L = delay(from_exact_map(!.E))
		).
		
delete_list([], !Map).
delete_list([Tuple | Tuples], !Map) :- 
	delete(Tuple, !Map),
	delete_list(Tuples, !Map).
	
%-----------------------------------------------------------------------------%
% Set operations