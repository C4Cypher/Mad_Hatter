%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_relation_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_relation_map.

:- interface.

:- import_module unit.
:- import_module list.
:- import_module assoc_list.

:- import_module mh_relation.

%-----------------------------------------------------------------------------%
% Relation Map

:- type mh_relation_map(T).
:- type mh_relation_set == mh_relation_map(unit).

:- func init = (mh_relation_map(_)::uo) is det.
:- pred init(mh_relation_map(_)::out) is det.

:- func eager_init = (mh_relation_map(_)::uo) is det.
:- pred eager_init(mh_relation_set::uo) is det.

:- func singleton(mh_relation, T) = mh_relation_map(T).
:- func singleton(mh_relation) = mh_relation_set.

:- func eager_singleton(mh_relation, T) = mh_relation_map(T).
:- func eager_singleton(mh_relation) = mh_relation_set.

:- pred is_empty(mh_relation_map(_)::in) is semidet.

:- func count(mh_relation_map(_)) = int.
:- pred count(mh_relation_map(_)::in, int::out) is det.

:- pred equal(mh_relation_map(T)::in, mh_relation_map(T)::in) is semidet.

:- impure pred force_pattern_map(mh_relation_map(_)::in) is det.

:- pred force_pattern_map(mh_relation_map(T), mh_relation_map(T)).
:- mode force_pattern_map(in, out) is det.
:- mode force_pattern_map(di, uo) is det.

%-----------------------------------------------------------------------------%
% Search

	% Succeeds if the map contains the given key
:- pred contains(mh_relation_map(T)::in, mh_relation::in) is semidet.

	% Fails if the key is not found
:- pred search(mh_relation_map(T)::in, mh_relation::in, 
T::out) is semidet.
:- func search(mh_relation_map(T), mh_relation) = T is semidet.

	% Throws an exception if the key is not found
:- pred lookup(mh_relation_map(T)::in, mh_relation::in, T::out) is det.
:- func lookup(mh_relation_map(T), mh_relation) = T is det.

%-----------------------------------------------------------------------------%
% Insertion

:- pred insert(mh_relation::in, T::in, mh_relation_map(T)::in, 
	mh_relation_map(T)::out) is semidet.
	
:- pred insert(mh_relation::in, mh_relation_set::in, mh_relation_set::out) 
	is semidet.
	
:- pred det_insert(mh_relation::in, T::in, mh_relation_map(T)::in, 
	mh_relation_map(T)::out) is det.
	
:- pred det_insert(mh_relation::in, mh_relation_set::in, mh_relation_set::out)
	is det.
	
:- pred det_insert_from_corresponding_lists(list(mh_relation)::in, list(T)::in,
	mh_relation_map(T)::in, mh_relation_map(T)::out) is det.
	
:- pred det_insert_from_assoc_list(assoc_list(mh_relation, T)::in,
	mh_relation_map(T)::in, mh_relation_map(T)::out) is det.
	
:- pred det_insert_from_list(list(mh_relation)::in, mh_relation_set::in, 
	mh_relation_set::out) is det.

:- pred set(mh_relation::in, T::in, mh_relation_map(T)::in, 
	mh_relation_map(T)::out) is det.
	
:- pred set(mh_relation::in, mh_relation_set::in, mh_relation_set::out) is det.
	
:- pred set_from_corresponding_lists(list(mh_relation)::in, list(T)::in,
	mh_relation_map(T)::in, mh_relation_map(T)::out) is det.
	
:- pred set_from_assoc_list(assoc_list(mh_relation, T)::in,
	mh_relation_map(T)::in, mh_relation_map(T)::out) is det.
	
:- pred set_from_list(list(mh_relation)::in, mh_relation_set::in, 
	mh_relation_set::out) is det.

:- pred update(mh_relation::in, T::in, mh_relation_map(T)::in, 
	mh_relation_map(T)::out) is semidet.
	
:- pred det_update(mh_relation::in, T::in, mh_relation_map(T)::in, 
	mh_relation_map(T)::out) is det.
	
%-----------------------------------------------------------------------------%
% Removal

:- pred remove(mh_relation::in, T::out, mh_relation_map(T)::in, 
	mh_relation_map(T)::out) is semidet.
	
:- pred remove(mh_relation::in, mh_relation_set::in, mh_relation_set::out) is semidet.
	
:- pred det_remove(mh_relation::in, T::out, mh_relation_map(T)::in, 
	mh_relation_map(T)::out) is det.
	
:- pred det_remove(mh_relation::in,  mh_relation_set::in, mh_relation_set::out) is det.
	
:- pred delete(mh_relation::in,  mh_relation_map(T)::in, 
	mh_relation_map(T)::out) is det.

:- pred delete_list(list(mh_relation)::in, mh_relation_map(T)::in, 
	mh_relation_map(T)::out) is det.
	
%-----------------------------------------------------------------------------%
% Set operations


:- func union(func(T, T) = T, mh_relation_map(T), mh_relation_map(T)) =
	mh_relation_map(T).

:- pred union(func(T, T) = T, mh_relation_map(T), mh_relation_map(T),
	mh_relation_map(T)).
:- mode union(in(func(in, in) = out is det), in, in, out) is det.
:- mode union(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func set_union(mh_relation_set, mh_relation_set) = mh_relation_set.
:- pred set_union(mh_relation_set::in, mh_relation_set::in,
	mh_relation_set::out) is det.

:- func intersect(func(T1, T2) = T3, mh_relation_map(T1), mh_relation_map(T2))
	= mh_relation_map(T3).
	
:- pred intersect(func(T1, T2) = T3, mh_relation_map(T1), mh_relation_map(T2),
	mh_relation_map(T3)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.
:- mode intersect(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func set_intersect(mh_relation_set, mh_relation_set) = mh_relation_set.
:- pred set_intersect(mh_relation_set::in, mh_relation_set::in, mh_relation_set::out) 
	is det.

:- func difference(mh_relation_map(T), mh_relation_map(_)) =
	mh_relation_map(T).

:- pred difference(mh_relation_map(T)::in, mh_relation_map(_)::in, 
	mh_relation_map(T)::out) is det.
	
%-----------------------------------------------------------------------------%
% Higher Order

:- func fold(func(mh_relation, T, A) = A, mh_relation_map(T), A) = A.
:- mode fold(in(func(in, in, in) = out is det), in, in) = out is det.
:- mode fold(in(func(in, in, in) = out is semidet), in, in) = out 
	is semidet.
:- mode fold(in(func(in, in, di) = uo is det), in, di) = uo is det.

:- func det_fold(func(mh_relation, T, A) = A, mh_relation_map(T), A) = A.
:- mode det_fold(in(func(in, in, in) = out is det), in, in) = out is det.

:- func semidet_fold(func(mh_relation, T, A) = A, mh_relation_map(T), A) = A.
:- mode semidet_fold(in(func(in, in, in) = out is semidet), in, in) = out 
	is semidet.

:- pred fold(func(mh_relation, T, A) = A, mh_relation_map(T), A, A).
:- mode fold(in(func(in, in, in) = out is det), in, in, out) is det.
:- mode fold(in(func(in, in, in) = out is semidet), in, in, out) 
	is semidet.
:- mode fold(in(func(in, in, di) = uo is det), in, di, uo) is det.

:- func map(func(mh_relation, T) = U, mh_relation_map(T)) = mh_relation_map(U).
 
:- pred map(func(mh_relation, T) = U, mh_relation_map(T), mh_relation_map(U)).
:- mode map(in(func(in, in) = out is det), in, out) is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.
:- import_module map. % use_module? 
:- import_module lazy.
:- import_module pair.
:- import_module require.

:- use_module mh_relation_pattern_map.

%-----------------------------------------------------------------------------%
% Relation Map

:- type exact_map(T) == map.map(mh_relation, T).
:- type pattern_map(T) == mh_relation_pattern_map.relation_pattern_map(T).
:- type lazy_pattern_map(T) == lazy(pattern_map(T)).

:- type mh_relation_map(T) 
	--->	relation_map(exact_map(T), lazy_pattern_map(T)).
	
:- func delay_pattern(exact_map(T)) = lazy_pattern_map(T).
delay_pattern(Exact) = delay(mh_relation_pattern_map.from_exact_map(!.E)).
:- pragma inline(delay_pattern/1).
				
init = relation_map(map.init@Map, delay_pattern(Map)).
init(init).

eager_init = relation_map(map.init, val(mh_relation_pattern_map.init)).
eager_init(eager_init).

singleton(Rel, Value) = 
	relation_map(map.singleton(Rel, Value)@Map, 
		delay(from_exact_map(Map))).
		
singleton(Relation) = singleton(Relation, unit).

eager_singleton(Relation, Value) = 
	relation_map(map.singleton(Rel, Value),
		val(mh_relation_pattern_map.singleton(Relation, Value))).
	
eager_singleton(Relation) = eager_singleton(Relation, unit).

is_empty(relation_map(Map, _)) :- map.is_empty(Map).

count(empty_relation_map) = 0.

count(relation_map(Map, _)) = map.count(Map).
count(Map, count(Map)).

equal(relation_map(M1, _), relation_map(M2, _)) :- map.equal(M1, M2).

force_pattern_map(relation_map(_, Lazy)) :- _ = force(Lazy).

force_pattern_map(!Map) :-
	impure force_pattern_map(!.Map).
	
:- pragma promise_pure(force_pattern_map/2).

	
%-----------------------------------------------------------------------------%
% Search

contains(relation_map(Map, _), Relation) :- map.contains(Map, Relation).

search(Map, Relation, search(Map, Relation)).

search(relation_map(Map, _), Relation) = map.search(Map, Relation).

lookup(Map, Relation, lookup(Map, Relation)).

lookup(relation_map(Map, _), Relation) = map.lookup(Map, Relation).


%-----------------------------------------------------------------------------%
% Insertion

insert(Relation, Value, relation_map(!.E, !.L), relation_map(!:E, !:L)) :-
	map.insert(Relation, Value, !E),
	promise_pure 
		(if impure read_if_val(!.L, P0)
		then
			mh_relation_pattern_map.insert(Relation, Value, P0, P),
			!:L = val(P)
		else
			!:L = delay_pattern(!.E)
		).
		
insert(Relation, !Set) :- insert(Relation, unit, !Set).	
	
det_insert(Relation, Value, !Map) :-
	(if insert(Relation, Value, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_relation_map.det_insert: relation aleady present in map", 
		Relation, !.Map)
	).

det_insert(Relation, !Map) :-
	(if insert(Relation, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_relation_map.det_insert: relation aleady present in map", 
		Relation, !.Map)
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
	
det_insert_from_list([], !Set).
det_insert_from_list([Relation | List]) :-
	det_insert(Relation, !Set),
	det_insert_from_list(List, !Set).
	
set(Relation, Value, relation_map(!.E, !.L), relation_map(!:E, !:L)) :-
	map.set(Relation, Value, !E),
	!:L = delay_pattern(!.E).

set(Relation, !Set) :- set(Relation, unit, !Set).

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
set_from_list([Relation | List]) :-
	set(Relation, !Set),
	set_from_list(List, !Set).	

update(Relation, Value, relation_map(!.E, !.L), relation_map(!:E, !:L)) :-
	map.update(Relation, Value, !E),
	!:L = delay_pattern(!.E).

det_update(Relation, Value, !Map) :-	
	(if update(Relation, Value, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_relation_map.det_update: relation not present in map", 
			Relation, !.Map)
	).
	
%-----------------------------------------------------------------------------%
% Removal

remove(Relation, Value, relation_map(!.E, !.L), relation_map(!:E, !:L)) :-
	map.remove(Relation, Value, !E),
	!:L = delay_pattern(!.E).

det_remove(Relation, Value, !Map) :-	
	(if remove(Relation, FoundVal, !Map)
	then !:Map = !.Map, Value = FoundVal
	else report_lookup_error(
		"mh_relation_map.det_remove: relation not present in map", Relation, 
		!.Map)
	).

delete(Relation, relation_map(!.E, !.L), relation_map(!:E, !:L)) :-
	map.delete(Relation, !E),
	!:L = delay_pattern(!.E).
		
delete_list([], !Map).
delete_list([Relation | Relations], !Map) :- 
	delete(Relation, !Map),
	delete_list(Relations, !Map).

%-----------------------------------------------------------------------------%
% Set operations

:- func merge_units(unit, unit) = unit.
merge_units(_, _) = unit.

union(F, relation_map(E1, _), relation_map(E2, _)) = 
	relation_map(E3@func_union(F, E1, E2), delay_pattern(E3)).
	
union(F, M1, M2, union(F, M1, M2)).

set_union(M1, M2) = union(merge_units, M1, M2).

set_union(M1, M2, set_union(M1, M2)).

intersect(F, relation_map(E1, _), relation_map(E2, _)) = 
	relation_map(E3@func_intersect(F, E1, E2), delay_pattern(E3)).
	
intersect(F, M1, M2, intersect(F, M1, M2)).

set_intersect(M1, M2) = intersect(merge_units, M1, M2).
	
set_intersect(M1, M2, set_intersect(M1, M2)).
	
difference(relation_map(E1, _), relation_map(E2, _)) = 
	relation_map(E3@fold(difference_fold, M2, M1), delay_pattern(E3)).
	
:- func difference_fold(mh_relation, _,	exact_map(T)) = 
	exact_map(T).
	
difference_fold(Key, _, !.Map) = !:Map :-
	map.delete(Key, !Map).
	
difference(M1, M2, difference(M1, M2)).


%-----------------------------------------------------------------------------%
% Higher Order

fold(F, relation_map(E, _), A) = fold(F, E, A).

det_fold(F, M, A) = fold(F, M, A).
semidet_fold(F, M, A) = fold(F, M, A).

fold(F, M, A, fold(F, M, A)).

map(F, relation_map(E0, _)) = 
	relation_map(E@map.map_values(F, E0), delay_pattern(E)).

map(F, M, map(F, M)).