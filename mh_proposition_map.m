%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_proposition_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_proposition_map.

:- interface.

:- import_module unit.
:- import_module list.
:- import_module assoc_list.

:- import_module mh_proposition.

%-----------------------------------------------------------------------------%
% Proposition Map

:- type mh_proposition_map(T).
:- type mh_proposition_set == mh_proposition_map(unit).

:- func init = mh_proposition_map(_).
:- pred init(mh_proposition_map(_)::out) is det.

:- func eager_init = mh_proposition_map(_).
:- pred eager_init(mh_proposition_map(_)::out) is det.

:- func singleton(mh_proposition, T) = mh_proposition_map(T).
:- func singleton(mh_proposition) = mh_proposition_set.

:- func eager_singleton(mh_proposition, T) = mh_proposition_map(T).
:- func eager_singleton(mh_proposition) = mh_proposition_set.

:- pred is_empty(mh_proposition_map(_)::in) is semidet.

:- func count(mh_proposition_map(_)) = int.
:- pred count(mh_proposition_map(_)::in, int::out) is det.

:- pred equal(mh_proposition_map(T)::in, mh_proposition_map(T)::in) is semidet.

:- impure pred force_pattern_map(mh_proposition_map(_)::in) is det.

:- pred force_pattern_map(mh_proposition_map(T), mh_proposition_map(T)).
:- mode force_pattern_map(in, out) is det.

%-----------------------------------------------------------------------------%
% Search

	% Succeeds if the map contains the given key
:- pred contains(mh_proposition_map(T)::in, mh_proposition::in) is semidet.

	% Fails if the key is not found
:- pred search(mh_proposition_map(T)::in, mh_proposition::in, 
T::out) is semidet.
:- func search(mh_proposition_map(T), mh_proposition) = T is semidet.

	% Throws an exception if the key is not found
:- pred lookup(mh_proposition_map(T)::in, mh_proposition::in, T::out) is det.
:- func lookup(mh_proposition_map(T), mh_proposition) = T is det.

%-----------------------------------------------------------------------------%
% Insertion

:- pred insert(mh_proposition::in, T::in, mh_proposition_map(T)::in, 
	mh_proposition_map(T)::out) is semidet.
	
:- pred insert(mh_proposition::in, mh_proposition_set::in, 
	mh_proposition_set::out) is semidet.
	
:- pred det_insert(mh_proposition::in, T::in, mh_proposition_map(T)::in, 
	mh_proposition_map(T)::out) is det.
	
:- pred det_insert(mh_proposition::in, mh_proposition_set::in, mh_proposition_set::out)
	is det.
	
:- pred det_insert_from_corresponding_lists(list(mh_proposition)::in, 
	list(T)::in, mh_proposition_map(T)::in, mh_proposition_map(T)::out) is det.
	
:- pred det_insert_from_assoc_list(assoc_list(mh_proposition, T)::in,
	mh_proposition_map(T)::in, mh_proposition_map(T)::out) is det.
	
:- pred det_insert_from_list(list(mh_proposition)::in, mh_proposition_set::in, 
	mh_proposition_set::out) is det.

:- pred set(mh_proposition::in, T::in, mh_proposition_map(T)::in,
	mh_proposition_map(T)::out) is det.
	
:- pred set(mh_proposition::in, mh_proposition_set::in, 
	mh_proposition_set::out) is det.
	
:- pred set_from_corresponding_lists(list(mh_proposition)::in, list(T)::in,
	mh_proposition_map(T)::in, mh_proposition_map(T)::out) is det.
	
:- pred set_from_assoc_list(assoc_list(mh_proposition, T)::in,
	mh_proposition_map(T)::in, mh_proposition_map(T)::out) is det.
	
:- pred set_from_list(list(mh_proposition)::in, mh_proposition_set::in, 
	mh_proposition_set::out) is det.

:- pred update(mh_proposition::in, T::in, mh_proposition_map(T)::in, 
	mh_proposition_map(T)::out) is semidet.
	
:- pred det_update(mh_proposition::in, T::in, mh_proposition_map(T)::in, 
	mh_proposition_map(T)::out) is det.
	
%-----------------------------------------------------------------------------%
% Removal

:- pred remove(mh_proposition::in, T::out, mh_proposition_map(T)::in, 
	mh_proposition_map(T)::out) is semidet.
	
:- pred remove(mh_proposition::in, mh_proposition_set::in, 
	mh_proposition_set::out) is semidet.
	
:- pred det_remove(mh_proposition::in, T::out, mh_proposition_map(T)::in, 
	mh_proposition_map(T)::out) is det.
	
:- pred det_remove(mh_proposition::in,  mh_proposition_set::in,
	mh_proposition_set::out) is det.
	
:- pred delete(mh_proposition::in,  mh_proposition_map(T)::in, 
	mh_proposition_map(T)::out) is det.

:- pred delete_list(list(mh_proposition)::in, mh_proposition_map(T)::in, 
	mh_proposition_map(T)::out) is det.
	
%-----------------------------------------------------------------------------%
% Set operations


:- func union(func(T, T) = T, mh_proposition_map(T), mh_proposition_map(T)) =
	mh_proposition_map(T).
:- mode union(in(func(in, in) = out is det), in, in) = out is det.
:- mode union(in(func(in, in) = out is semidet), in, in) = out is semidet.

:- pred union(func(T, T) = T, mh_proposition_map(T), mh_proposition_map(T),
	mh_proposition_map(T)).
:- mode union(in(func(in, in) = out is det), in, in, out) is det.
:- mode union(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func set_union(mh_proposition_set, mh_proposition_set) = mh_proposition_set.
:- pred set_union(mh_proposition_set::in, mh_proposition_set::in,
	mh_proposition_set::out) is det.

:- func intersect(func(T1, T2) = T3, mh_proposition_map(T1), 
	mh_proposition_map(T2)) = mh_proposition_map(T3).
:- mode intersect(in(func(in, in) = out is det), in, in) = out is det.
:- mode intersect(in(func(in, in) = out is semidet), in, in) = out is semidet.
	
:- pred intersect(func(T1, T2) = T3, mh_proposition_map(T1), 
	mh_proposition_map(T2), mh_proposition_map(T3)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.
:- mode intersect(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func set_intersect(mh_proposition_set, mh_proposition_set)
	= mh_proposition_set.
:- pred set_intersect(mh_proposition_set::in, mh_proposition_set::in,
	mh_proposition_set::out) 
	is det.

:- func difference(mh_proposition_map(T), mh_proposition_map(_)) =
	mh_proposition_map(T).

:- pred difference(mh_proposition_map(T)::in, mh_proposition_map(_)::in, 
	mh_proposition_map(T)::out) is det.
	
%-----------------------------------------------------------------------------%
% Higher Order

:- func fold(func(mh_proposition, T, A) = A, mh_proposition_map(T), A) = A.
:- mode fold(in(func(in, in, in) = out is det), in, in) = out is det.
:- mode fold(in(func(in, in, in) = out is semidet), in, in) = out 
	is semidet.
:- mode fold(in(func(in, in, di) = uo is det), in, di) = uo is det.

:- func det_fold(func(mh_proposition, T, A) = A, mh_proposition_map(T), A) = A.
:- mode det_fold(in(func(in, in, in) = out is det), in, in) = out is det.

:- func semidet_fold(func(mh_proposition, T, A) = A, mh_proposition_map(T), A) 
	= A.
:- mode semidet_fold(in(func(in, in, in) = out is semidet), in, in) = out 
	is semidet.

:- pred fold(func(mh_proposition, T, A) = A, mh_proposition_map(T), A, A).
:- mode fold(in(func(in, in, in) = out is det), in, in, out) is det.
:- mode fold(in(func(in, in, in) = out is semidet), in, in, out) 
	is semidet.
:- mode fold(in(func(in, in, di) = uo is det), in, di, uo) is det.

:- pred fold2(pred(mh_proposition, T, A, A, B, B), mh_proposition_map(T), 
	A, A, B, B).
:- mode fold2(in(pred(in, in, in, out, in, out) is semidet), in, in, out, 
	in,	out) is semidet.
:- mode fold2(in(pred(in, in, in, out, in, out) is det), in, in, out, in, out)
	is det.

:- func map(func(mh_proposition, T) = U, mh_proposition_map(T))
	= mh_proposition_map(U).
 
:- pred map(func(mh_proposition, T) = U, mh_proposition_map(T),
	mh_proposition_map(U)).
:- mode map(in(func(in, in) = out is det), in, out) is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.
:- import_module map. % use_module? 
:- import_module lazy.
:- import_module pair.
:- import_module require.

:- import_module map_util.

:- import_module mh_proposition_pattern_map.

%-----------------------------------------------------------------------------%
% Proposition Map

:- type exact_map(T) == map.map(mh_proposition, T).
:- type pattern_map(T) == mh_proposition_pattern_map.proposition_pattern_map(T).
:- type lazy_pattern_map(T) == lazy(pattern_map(T)).

:- type mh_proposition_map(T)
	---> 	proposition_map(exact_map(T), lazy_pattern_map(T)).
	
:- func delay_pattern(exact_map(T)) = lazy_pattern_map(T).
delay_pattern(Exact) = delay(Closure) :-
	Closure = ((func) = mh_proposition_pattern_map.from_exact_map(Exact)).
:- pragma inline(delay_pattern/1).
				
init = proposition_map(map.init@Map, delay_pattern(Map)).
init(init).

eager_init = proposition_map(map.init, val(mh_proposition_pattern_map.init)).
eager_init(eager_init).

singleton(Prop, Value) = 
	proposition_map(map.singleton(Prop, Value)@Map, delay_pattern(Map)).
		
singleton(Prop) = singleton(Prop, unit).

eager_singleton(Prop, Value) = 
	proposition_map(map.singleton(Prop, Value),
		val(mh_proposition_pattern_map.singleton(Prop, Value))).
	
eager_singleton(Prop) = eager_singleton(Prop, unit).

is_empty(proposition_map(Map, _)) :- map.is_empty(Map).

count(proposition_map(Map, _)) = map.count(Map).
count(Map, count(Map)).

equal(proposition_map(M1, _), proposition_map(M2, _)) :- map.equal(M1, M2).

force_pattern_map(proposition_map(_, Lazy)) :-  
	_ = force(Lazy), 
	impure private_builtin.imp.

force_pattern_map(!Map) :-
	impure force_pattern_map(!.Map).
	
:- pragma promise_pure(force_pattern_map/2).

	
%-----------------------------------------------------------------------------%
% Search

contains(proposition_map(Map, _), Prop) :- map.contains(Map, Prop).

search(Map, Prop, search(Map, Prop)).

search(proposition_map(Map, _), Prop) = map.search(Map, Prop).

lookup(Map, Prop, lookup(Map, Prop)).

lookup(proposition_map(Map, _), Prop) = map.lookup(Map, Prop).


%-----------------------------------------------------------------------------%
% Insertion

insert(Prop, Value, proposition_map(!.E, !.L), proposition_map(!:E, !:L)) :-
	map.insert(Prop, Value, !E),
	promise_pure 
		(if impure read_if_val(!.L, P0)
		then
			mh_proposition_pattern_map.insert(Prop, Value, P0, P),
			!:L = val(P)
		else
			!:L = delay_pattern(!.E)
		).
		
insert(Prop, !Set) :- insert(Prop, unit, !Set).	
	
det_insert(Prop, Value, !Map) :-
	(if insert(Prop, Value, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_proposition_map.det_insert: proposition aleady present in map", 
		Prop, !.Map)
	).

det_insert(Prop, !Map) :-
	(if insert(Prop, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_proposition_map.det_insert: proposition aleady present in map", 
		Prop, !.Map)
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
det_insert_from_list([Prop | List], !Set) :-
	det_insert(Prop, !Set),
	det_insert_from_list(List, !Set).
	
set(Prop, Value, proposition_map(!.E, _), proposition_map(!:E, L)) :-
	map.set(Prop, Value, !E), L = delay_pattern(!.E).

set(Prop, !Set) :- set(Prop, unit, !Set).

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
set_from_list([Prop | List], !Set) :-
	set(Prop, !Set),
	set_from_list(List, !Set).	

update(Prop, Value, proposition_map(!.E, _), proposition_map(!:E, L)) :-
	map.update(Prop, Value, !E), L = delay_pattern(!.E).

det_update(Prop, Value, !Map) :-	
	(if update(Prop, Value, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_proposition_map.det_update: proposition not present in map", 
			Prop, !.Map)
	).
	
%-----------------------------------------------------------------------------%
% Removal

remove(Prop, Value, proposition_map(!.E, _), proposition_map(!:E, L)) :-
	map.remove(Prop, Value, !E), L = delay_pattern(!.E).
	
remove(E, !Map) :- remove(E, _, !Map).

det_remove(Prop, Value, !Map) :-	
	(if remove(Prop, FoundVal, !Map)
	then !:Map = !.Map, Value = FoundVal
	else report_lookup_error(
		"mh_proposition_map.det_remove: proposition not present in map", Prop, 
		!.Map)
	).
	
det_remove(E, !Map) :- det_remove(E, _, !Map).

delete(Prop, proposition_map(!.E, _), proposition_map(!:E, L)) :-
	map.delete(Prop, !E), L = delay_pattern(!.E).
		
delete_list([], !Map).
delete_list([Prop | Props], !Map) :- 
	delete(Prop, !Map),
	delete_list(Props, !Map).

%-----------------------------------------------------------------------------%
% Set operations

:- func merge_units(unit, unit) = unit.
merge_units(_, _) = unit.

union(F, proposition_map(E1, _), proposition_map(E2, _)) = 
	proposition_map(E3@func_union(F, E1, E2), delay_pattern(E3)).
	
union(F, M1, M2, union(F, M1, M2)).

set_union(M1, M2) = union(merge_units, M1, M2).

set_union(M1, M2, set_union(M1, M2)).

intersect(F, proposition_map(E1, _), proposition_map(E2, _)) = 
	proposition_map(E3@func_intersect(F, E1, E2), delay_pattern(E3)).
	
intersect(F, M1, M2, intersect(F, M1, M2)).

set_intersect(M1, M2) = intersect(merge_units, M1, M2).
	
set_intersect(M1, M2, set_intersect(M1, M2)).
	
difference(proposition_map(E1, _), proposition_map(E2, _)) = 
	proposition_map(E3@fold(difference_fold, E2, E1), delay_pattern(E3)).
	
:- func difference_fold(mh_proposition, _,	exact_map(T)) = 
	exact_map(T).
	
difference_fold(Key, _, !.Map) = !:Map :-
	map.delete(Key, !Map).
	
difference(M1, M2, difference(M1, M2)).


%-----------------------------------------------------------------------------%
% Higher Order

fold(F, proposition_map(E, _), A) = fold(F, E, A).

det_fold(F, M, A) = fold(F, M, A).
semidet_fold(F, M, A) = fold(F, M, A).

fold(F, M, A, fold(F, M, A)).

fold2(P, proposition_map(E, _), !A, !B) :- foldl2(P, E, !A, !B).

map(F, proposition_map(E0, _)) = 
	proposition_map(E@map.map_values(F, E0), delay_pattern(E)).

map(F, M, map(F, M)).