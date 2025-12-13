%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_substitution_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_substitution_map.

:- interface.

:- import_module unit.
:- import_module list.
:- import_module assoc_list.

:- import_module mh_substitution.

%-----------------------------------------------------------------------------%
% Substitution map

:- type mh_substitution_map(T).
:- type mh_substitution_set == mh_substitution_map(unit).

:- func init = mh_substitution_map(_).
:- pred init(mh_substitution_map(_)::out) is det.

:- func eager_init = mh_substitution_map(_).
:- pred eager_init(mh_substitution_map(_)::out) is det.

:- func singleton(mh_substitution, T) = mh_substitution_map(T).
:- func singleton(mh_substitution) = mh_substitution_set.

:- func eager_singleton(mh_substitution, T) = mh_substitution_map(T).
:- func eager_singleton(mh_substitution) = mh_substitution_set.

:- pred is_empty(mh_substitution_map(_)::in) is semidet.

:- func count(mh_substitution_map(_)) = int.
:- pred count(mh_substitution_map(_)::in, int::out) is det.

:- pred equal(mh_substitution_map(T)::in, mh_substitution_map(T)::in)
	is semidet.

:- impure pred force_pattern_map(mh_substitution_map(_)::in) is det.

:- pred force_pattern_map(mh_substitution_map(T), mh_substitution_map(T)).
:- mode force_pattern_map(in, out) is det.

%-----------------------------------------------------------------------------%
% Search

	% Succeeds if the map contains the given key
:- pred contains(mh_substitution_map(T)::in, mh_substitution::in) is semidet.

	% Fails if the key is not found
:- pred search(mh_substitution_map(T)::in, mh_substitution::in, 
T::out) is semidet.
:- func search(mh_substitution_map(T), mh_substitution) = T is semidet.

	% Throws an exception if the key is not found
:- pred lookup(mh_substitution_map(T)::in, mh_substitution::in, T::out) is det.
:- func lookup(mh_substitution_map(T), mh_substitution) = T is det.

%-----------------------------------------------------------------------------%
% Insertion

:- pred insert(mh_substitution::in, T::in, mh_substitution_map(T)::in, 
	mh_substitution_map(T)::out) is semidet.
	
:- pred insert(mh_substitution::in, mh_substitution_set::in,
	mh_substitution_set::out) is semidet.
	
:- pred det_insert(mh_substitution::in, T::in, mh_substitution_map(T)::in, 
	mh_substitution_map(T)::out) is det.
	
:- pred det_insert(mh_substitution::in, mh_substitution_set::in,
	mh_substitution_set::out) is det.
	
:- pred det_insert_from_corresponding_lists(list(mh_substitution)::in, 
	list(T)::in, mh_substitution_map(T)::in, mh_substitution_map(T)::out)
	is det.
	
:- pred det_insert_from_assoc_list(assoc_list(mh_substitution, T)::in,
	mh_substitution_map(T)::in, mh_substitution_map(T)::out) is det.
	
:- pred det_insert_from_list(list(mh_substitution)::in, 
	mh_substitution_set::in, mh_substitution_set::out) is det.

:- pred set(mh_substitution::in, T::in, mh_substitution_map(T)::in,
	mh_substitution_map(T)::out) is det.
	
:- pred set(mh_substitution::in, mh_substitution_set::in, 
	mh_substitution_set::out) is det.
	
:- pred set_from_corresponding_lists(list(mh_substitution)::in, list(T)::in,
	mh_substitution_map(T)::in, mh_substitution_map(T)::out) is det.
	
:- pred set_from_assoc_list(assoc_list(mh_substitution, T)::in,
	mh_substitution_map(T)::in, mh_substitution_map(T)::out) is det.
	
:- pred set_from_list(list(mh_substitution)::in, mh_substitution_set::in, 
	mh_substitution_set::out) is det.

:- pred update(mh_substitution::in, T::in, mh_substitution_map(T)::in, 
	mh_substitution_map(T)::out) is semidet.
	
:- pred det_update(mh_substitution::in, T::in, mh_substitution_map(T)::in, 
	mh_substitution_map(T)::out) is det.
	
%-----------------------------------------------------------------------------%
% Removal

:- pred remove(mh_substitution::in, T::out, mh_substitution_map(T)::in, 
	mh_substitution_map(T)::out) is semidet.
	
:- pred remove(mh_substitution::in, mh_substitution_set::in,
	mh_substitution_set::out) is semidet.
	
:- pred det_remove(mh_substitution::in, T::out, mh_substitution_map(T)::in, 
	mh_substitution_map(T)::out) is det.
	
:- pred det_remove(mh_substitution::in,  mh_substitution_set::in,
	mh_substitution_set::out) is det.
	
:- pred delete(mh_substitution::in,  mh_substitution_map(T)::in, 
	mh_substitution_map(T)::out) is det.

:- pred delete_list(list(mh_substitution)::in, mh_substitution_map(T)::in, 
	mh_substitution_map(T)::out) is det.
	
%-----------------------------------------------------------------------------%
% Set operations


:- func union(func(T, T) = T, mh_substitution_map(T), mh_substitution_map(T))
	= mh_substitution_map(T).
:- mode union(in(func(in, in) = out is det), in, in) = out is det.
:- mode union(in(func(in, in) = out is semidet), in, in) = out is semidet.

:- pred union(func(T, T) = T, mh_substitution_map(T), mh_substitution_map(T),
	mh_substitution_map(T)).
:- mode union(in(func(in, in) = out is det), in, in, out) is det.
:- mode union(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func set_union(mh_substitution_set, mh_substitution_set) 
	= mh_substitution_set.
:- pred set_union(mh_substitution_set::in, mh_substitution_set::in,
	mh_substitution_set::out) is det.

:- func intersect(func(T1, T2) = T3, mh_substitution_map(T1),
	mh_substitution_map(T2)) = mh_substitution_map(T3).
:- mode intersect(in(func(in, in) = out is det), in, in) = out is det.
:- mode intersect(in(func(in, in) = out is semidet), in, in) = out is semidet.
	
:- pred intersect(func(T1, T2) = T3, mh_substitution_map(T1),
	mh_substitution_map(T2), mh_substitution_map(T3)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.
:- mode intersect(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func set_intersect(mh_substitution_set, mh_substitution_set)
	= mh_substitution_set.
:- pred set_intersect(mh_substitution_set::in, mh_substitution_set::in,
	mh_substitution_set::out) is det.

:- func difference(mh_substitution_map(T), mh_substitution_map(_)) =
	mh_substitution_map(T).

:- pred difference(mh_substitution_map(T)::in, mh_substitution_map(_)::in, 
	mh_substitution_map(T)::out) is det.
	
%-----------------------------------------------------------------------------%
% Higher Order

:- func fold(func(mh_substitution, T, A) = A, mh_substitution_map(T), A) = A.
:- mode fold(in(func(in, in, in) = out is det), in, in) = out is det.
:- mode fold(in(func(in, in, in) = out is semidet), in, in) = out 
	is semidet.
:- mode fold(in(func(in, in, di) = uo is det), in, di) = uo is det.

:- func det_fold(func(mh_substitution, T, A) = A, mh_substitution_map(T), A)
	= A.
:- mode det_fold(in(func(in, in, in) = out is det), in, in) = out is det.

:- func semidet_fold(func(mh_substitution, T, A) = A, mh_substitution_map(T),
	A) = A.
:- mode semidet_fold(in(func(in, in, in) = out is semidet), in, in) = out 
	is semidet.

:- pred fold(func(mh_substitution, T, A) = A, mh_substitution_map(T), A, A).
:- mode fold(in(func(in, in, in) = out is det), in, in, out) is det.
:- mode fold(in(func(in, in, in) = out is semidet), in, in, out) 
	is semidet.
:- mode fold(in(func(in, in, di) = uo is det), in, di, uo) is det.

:- pred fold2(pred(mh_substitution, T, A, A, B, B), mh_substitution_map(T), 
	A, A, B, B).
:- mode fold2(in(pred(in, in, in, out, in, out) is semidet), in, in, out, 
	in,	out) is semidet.
:- mode fold2(in(pred(in, in, in, out, in, out) is det), in, in, out, in, out)
	is det.

:- func map(func(mh_substitution, T) = U, mh_substitution_map(T))
	= mh_substitution_map(U).
 
:- pred map(func(mh_substitution, T) = U, mh_substitution_map(T),
	mh_substitution_map(U)).
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

:- import_module mh_var_map.
:- import_module mh_term.

:- use_module mh_substitution_pattern_map.

%-----------------------------------------------------------------------------%
% Substitution Map

:- type exact_map(T) == map.map(mh_var_map(mh_term), T).
:- type pattern_map(T) 
	== mh_substitution_pattern_map.substitution_pattern_map(T).
:- type lazy_pattern_map(T) == lazy(pattern_map(T)).

:- type mh_substitution_map(T)
	--->	substitution_map(exact_map(T), lazy_pattern_map(T)).
	
:- func delay_pattern(exact_map(T)) = lazy_pattern_map(T).
delay_pattern(Exact) = delay(Closure) :-
	Closure = ((func) = mh_substitution_pattern_map.from_exact_map(Exact)).
:- pragma inline(delay_pattern/1).
				
init = substitution_map(map.init@Map, delay_pattern(Map)).
init(init).

eager_init = substitution_map(map.init, val(mh_substitution_pattern_map.init)).
eager_init(eager_init).

singleton(Sub, Value) = 
	substitution_map(map.singleton(Sub, Value)@Map, delay_pattern(Map)).
		
singleton(Sub) = singleton(Sub, unit).

eager_singleton(Sub, Value) = 
	substitution_map(map.singleton(Sub, Value),
		val(mh_substitution_pattern_map.singleton(Sub, Value))).
	
eager_singleton(Sub) = eager_singleton(Sub, unit).

is_empty(substitution_map(Map, _)) :- map.is_empty(Map).

count(substitution_map(Map, _)) = map.count(Map).
count(Map, count(Map)).

equal(substitution_map(M1, _), substitution_map(M2, _)) :- map.equal(M1, M2).

force_pattern_map(substitution_map(_, Lazy)) :-  
	_ = force(Lazy), 
	impure private_builtin.imp.

force_pattern_map(!Map) :-
	impure force_pattern_map(!.Map).
	
:- pragma promise_pure(force_pattern_map/2).

	
%-----------------------------------------------------------------------------%
% Search

contains(substitution_map(Map, _), Sub) :- map.contains(Map, to_var_map(Sub)).

search(Map, Sub, search(Map, Sub)).

search(substitution_map(Map, _), Sub) = map.search(Map, to_var_map(Sub)).

lookup(Map, Sub, lookup(Map, Sub)).

lookup(substitution_map(Map, _), Sub) = map.lookup(Map, to_var_map(Sub)).


%-----------------------------------------------------------------------------%
% Insertion

insert(Sub, Value, substitution_map(!.E, !.L), substitution_map(!:E, !:L)) :-
	map.insert(to_var_map(Sub), Value, !E),
	promise_pure 
		(if impure read_if_val(!.L, P0)
		then
			mh_substitution_pattern_map.insert(Sub, Value, P0, P),
			!:L = val(P)
		else
			!:L = delay_pattern(!.E)
		).
		
insert(Sub, !Set) :- insert(Sub, unit, !Set).	
	
det_insert(Sub, Value, !Map) :-
	(if insert(Sub, Value, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_substitution_map.det_insert: substitution aleady present in map", 
		Sub, !.Map)
	).

det_insert(Sub, !Map) :-
	(if insert(Sub, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_substitution_map.det_insert: substitution aleady present in map", 
		Sub, !.Map)
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
det_insert_from_list([Sub | List], !Set) :-
	det_insert(Sub, !Set),
	det_insert_from_list(List, !Set).
	
set(Sub, Value, substitution_map(!.E, _), substitution_map(!:E, L)) :-
	map.set(to_var_map(Sub), Value, !E), L = delay_pattern(!.E).

set(Sub, !Set) :- set(Sub, unit, !Set).

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
set_from_list([Sub | List], !Set) :-
	set(Sub, !Set),
	set_from_list(List, !Set).	

update(Sub, Value, substitution_map(!.E, _), substitution_map(!:E, L)) :-
	map.update(to_var_map(Sub), Value, !E),
	L = delay_pattern(!.E).

det_update(Sub, Value, !Map) :-	
	(if update(Sub, Value, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_substitution_map.det_update: substitution not present in map", 
			Sub, !.Map)
	).
	
%-----------------------------------------------------------------------------%
% Removal

remove(Sub, Value, substitution_map(!.E, _), substitution_map(!:E, L)) :-
	map.remove(to_var_map(Sub), Value, !E),
	L = delay_pattern(!.E).
	
remove(E, !Map) :- remove(E, _, !Map).

det_remove(Sub, Value, !Map) :-	
	(if remove(Sub, FoundVal, !Map)
	then !:Map = !.Map, Value = FoundVal
	else report_lookup_error(
		"mh_substitution_map.det_remove: substitution not present in map", 
		Sub, !.Map)
	).
	
det_remove(E, !Map) :- det_remove(E, _, !Map).

delete(Sub, substitution_map(!.E, _), substitution_map(!:E, L)) :-
	map.delete(to_var_map(Sub), !E),
	L = delay_pattern(!.E).
		
delete_list([], !Map).
delete_list([Sub | Subs], !Map) :- 
	delete(Sub, !Map),
	delete_list(Subs, !Map).

%-----------------------------------------------------------------------------%
% Set operations

:- func merge_units(unit, unit) = unit.
merge_units(_, _) = unit.

union(F, substitution_map(E1, _), substitution_map(E2, _)) = 
	substitution_map(E3@func_union(F, E1, E2), delay_pattern(E3)).
	
union(F, M1, M2, union(F, M1, M2)).

set_union(M1, M2) = union(merge_units, M1, M2).

set_union(M1, M2, set_union(M1, M2)).

intersect(F, substitution_map(E1, _), substitution_map(E2, _)) = 
	substitution_map(E3@func_intersect(F, E1, E2), delay_pattern(E3)).
	
intersect(F, M1, M2, intersect(F, M1, M2)).

set_intersect(M1, M2) = intersect(merge_units, M1, M2).
	
set_intersect(M1, M2, set_intersect(M1, M2)).
	
difference(substitution_map(E1, _), substitution_map(E2, _)) = 
	substitution_map(E3@fold(difference_fold, E2, E1), delay_pattern(E3)).
	
:- func difference_fold(mh_substitution, _,	exact_map(T)) = 
	exact_map(T).
	
difference_fold(Key, _, !.Map) = !:Map :-
	map.delete(Key, !Map).
	
difference(M1, M2, difference(M1, M2)).


%-----------------------------------------------------------------------------%
% Higher Order

fold(F, substitution_map(E, _), A) = fold(fold_sub(F), E, A).

:- func fold_sub(func(mh_substitution, T, A) = A, mh_var_map(mh_term), T, A) 
	= A.
:- mode fold_sub(in(func(in, in, in) = out is det), in, in, in) = out is det.
:- mode fold_sub(in(func(in, in, in) = out is semidet), in, in, in) = out 
	is semidet.
	
fold_sub(F, K, V, A) = F(sub_map(K), V, A).
	
det_fold(F, M, A) = fold(F, M, A).
semidet_fold(F, M, A) = fold(F, M, A).

fold(F, M, A, fold(F, M, A)).

fold2(P, substitution_map(E, _), !A, !B) :- foldl2(P, E, !A, !B).

map(F, substitution_map(E0, _)) = 
	substitution_map(E@map.map_values(F, E0), delay_pattern(E)).

map(F, M, map(F, M)).