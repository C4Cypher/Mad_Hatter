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
:- import_module array.

:- import_module mh_term.
:- import_module mh_tuple.

%-----------------------------------------------------------------------------%
% Tuple map

:- type mh_tuple_map(T).
:- type mh_tuple_set == mh_tuple_map(unit).

:- func init = (mh_tuple_map(T)::uo) is det.
:- pred init(mh_tuple_set::uo) is det.

:- func lazy_init = (mh_tuple_map(T)::uo) is det.
:- pred lazy_init(mh_tuple_set::uo) is det.

:- func singleton(mh_tuple, T) = mh_tuple_map(T).
:- func singleton(mh_tuple) = mh_tuple_set.

:- func lazy_singleton(mh_tuple, T) = mh_tuple_map(T).
:- func lazy_singleton(mh_tuple) = mh_tuple_set.

:- pred is_empty(mh_tuple_map(_)::in) is semidet.

:- func count(mh_tuple_map(_)) = int.
:- pred count(mh_tuple_map(_)::in, int::out) is det.

:- pred equal(mh_term_map(T)::in, mh_term_map(T)::in) is semidet.

:- impure pred force_pattern_map(mh_tuple_map(T)::in) is det.

:- pred force_pattern_map(mh_tuple_map(T), mh_tuple_map(T)).
:- mode force_pattern_map(in, out) is det.
:- mode force_pattern_map(di, uo) is det.

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

:- func set_union(mh_tuple_set, mh_tuple_set) = mh_tuple_set.
:- pred set_union(mh_tuple_set::in, mh_tuple_set::in, mh_tuple_set::out)
	is det.

:- func intersect(func(T, T) = T, mh_tuple_map(T), mh_tuple_map(T))
	= mh_tuple_map(T).
	
:- pred intersect(func(T, T) = T, mh_tuple_map(T), mh_tuple_map(T),
	mh_tuple_map(T)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.

:- func set_intersect(mh_tuple_set, mh_tuple_set) = mh_tuple_set.
:- pred set_intersect(mh_tuple_set::in, mh_tuple_set::in, 
	mh_tuple_set::out) is det.

:- func difference(mh_tuple_map(T), mh_tuple_map(_)) =
	mh_tuple_map(T).

:- pred difference(mh_tuple_map(T)::in, mh_tuple_map(_)::in, 
	mh_tuple_map(T)::out) is det.
	
%-----------------------------------------------------------------------------%
% Higher Order

:- func fold(func(mh_tuple, T, A) = A, mh_tuple_map(T), A) = A.

:- pred fold(func(mh_tuple, T, A) = A, mh_tuple_map(T), A, A).
:- mode fold(in(func(in, in, in) = out is det), in, in, out) is det.

:- func map(func(mh_tuple, T) = U, mh_tuple_map(T)) = mh_tuple_map(U).
 
:- pred map(func(mh_tuple, T) = U, mh_tuple_map(T), mh_tuple_map(U)).
:- mode map(in(func(in, in) = out is det), in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module lazy.
:- import_module array.
:- import_module list.
:- use_module map.

:- use_module mh_tuple_pattern_map.

%-----------------------------------------------------------------------------%

:- type exact_map(T) == map.map(array(mh_term), T).
:- type pattern_map(T) == mh_tuple_pattern_map.tuple_pattern_map(T).
:- type lazy_pattern_map(T) == lazy(pattern_map(T)). 

:- type mh_tuple_map(T)
	--->	tuple_map(exact_map(T), lazy_pattern_map(T)).
	
init = tuple_map(map.init, val(mh_tuple_pattern_map.init)).

init(init).

lazy_init = tuple_map(map.init@Map, delay(from_exact_map(Map))).

singleton(Tuple, T) = tuple_map(map.singleton(to_array(Tuple), T),
	val(mh_tuple_pattern_map.singleton(Tuple, T))).
	
singleton(Tuple) = singleton(Tuple, unit).
	
lazy_singleton(Tuple, T) = 
	tuple_map(map.singleton(to_array(Tuple), T)@Map, 
		delay(from_exact_map(Map))).
		
lazy_singleton(Tuple) = lazy_singleton(Tuple, unit).
	
is_empty(tuple_map(Map, _)) :- map.is_empty(Map).

count(tuple_map(Map, _)) = map.count(Map).
count(Map, count(Map)).

equal(tuple_map(M1, _), tuple_map(M2, _)) :- map.equal(M1, M2).

force_pattern_map(tuple_map(_, Lazy)) :- _ = force(Lazy).

:- pragma promise_impure(force_pattern_map/1).

force_pattern_map(!Map) :-
	impure force_pattern_map(!.Map).
	
:- pragma promise_pure(force_pattern_map/2).

:- func delay_pattern(exact_map(T)) = lazy_pattern_map(T).

delay_pattern(Exact) = delay(mh_tuple_pattern_map.from_exact_map(!.E)).

:- pragma inline(delay_pattern/1).
	
%-----------------------------------------------------------------------------%
% Search

contains(tuple_map(Map, _), Tuple) :- map.contains(Map, to_array(Tuple)).

search(Map, Tuple, search(Map, Tuple)).

search(tuple_map(Map, _), Tuple) = map.search(Map, to_array(Tuple)).

lookup(Map, Tuple, lookup(Map, Tuple)).

lookup(tuple_map(Map, _), Tuple) = map.lookup(Map, to_array(Tuple)).


%-----------------------------------------------------------------------------%
% Insertion

insert(Tuple, T, tuple_exact_map(!.E, !.L), tuple_exact_map(!:E, !:L)) :-
	Array = to_array(Tuple),
	map.unsafe_array_insert(Array, T, !E),
	promise_pure 
		(if impure read_if_val(!.L, P0)
		then
			mh_tuple_pattern_map.array_insert(Array, T, P0, P),
			!:L = val(P)
		else
			!:L = delay_pattern(!.E)
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
	map.set(Array, T, !E),
	promise_pure 
		(if impure read_if_val(!.L, P0)
		then
			mh_tuple_pattern_map.array_set(Tuple, Array, T, P0, P),
			!:L = val(P)
		else
			!:L = delay_pattern(!.E)
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
	map.update(Array, T, !E),
	promise_pure 
		(if impure read_if_val(!.L, P0)
		then
			mh_tuple_pattern_map.array_set(Tuple, Array, T, P0, P),
			!:L = val(P)
		else
			!:L = delay_pattern(!.E)
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
	map.remove(Array, T, !E)
	promise_pure 
		(if impure read_if_val(!.L, P0)
		then
			mh_tuple_pattern_map.array_delete(Array, P0, P),
			!:L = val(P)
		else
			!:L = delay_pattern(!.E)
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
	map.delete(Array, !E)
	promise_pure 
		(if impure read_if_val(!.L, P0)
		then
			mh_tuple_pattern_map.array_delete(Array, P0, P),
			!:L = val(P)
		else
			!:L = delay_pattern(!.E)
		).
		
delete_list([], !Map).
delete_list([Tuple | Tuples], !Map) :- 
	delete(Tuple, !Map),
	delete_list(Tuples, !Map).
	
%-----------------------------------------------------------------------------%
% Set operations

union(F, tuple_map(E1, _), tuple_map(E2, _)) = 
	tuple_map(E3@map.union(E1, E2), delay_pattern(E3)).
	

	
union(F, M1, M2, union(F, M1, M2)).

set_union(tuple_map(E1, _), tuple_map(E2, _)) = 
	tuple_map(E3@map.overlay(E1, E2), delay_pattern(E3)).

	
set_union(M1, M2, set_union(M1, M2)).

intersect(F, tuple_map(E1, _), tuple_map(E2, _)) = 
	tuple_map(E3@map.intersect(F, E1, E2), delay_pattern(E3)).
	
intersect(F, M1, M2, intersect(F, M1, M2)).

set_intersect(tuple_map(E1, _), tuple_map(E2, _)) = 
	tuple_map(E3@map.common_subset(E1, E2), delay_pattern(E3)).
	
set_intersect(M1, M2, set_intersect(M1, M2)).
	
difference(tuple_map(E1, _), tuple_map(E2, _)) = 
	tuple_map(E3@map.foldl(difference_fold, M2, M1), delay_pattern(E3)).
	
:- func difference_fold(array(mh_term), _,	tuple_exact_map(T)) = 
	tuple_exact_map(T).
	
difference_fold(Key, _, !.Map) = !:Map :-
	map.delete(Key, !Map).
	
difference(M1, M2, difference(M1, M2)).

%-----------------------------------------------------------------------------%
% Higher Order

fold(F, tuple_map(E, _), A) = map.foldl(fold_exact(F), E, A).

:- func fold_exact(func(mh_tuple, T, A) = A, array(mh_term), T, A) = A.

fold_exact(F, K, V, A) = F(from_array(K), V).

fold(F, M A, fold(F, M, A)).

map(F, tuple_map(E0, _)) = 
	tuple_map(E@map.map_values(map_exact(F), E0)), delay_pattern(E)).

:- func map_exact(func(mh_tuple, T) = U, array(mh_term), T) = U.
	
map_exact(F, K, V) = K - F(from_array(K), V).

map(F, M, map(F, M)).

