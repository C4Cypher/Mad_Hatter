%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_scope_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_scope_map.

:- interface.

:- import_module list.
:- import_module assoc_list.

:- import_module mh_scope.
:- import_module mh_context.
:- import_module mh_var_set.

%-----------------------------------------------------------------------------%
% Scope map

:- type mh_scope_map(T).
:- type mh_scope_set == mh_scope_map(unit).

:- func init = mh_scope_map(_). 
:- pred init(mh_scope_map(_)::out) is det.

:- func singleton(mh_scope, T) = mh_scope_map(T).
:- func singleton(mh_scope) = mh_scope_set.

:- pred is_empty(mh_scope_map(_)::in) is semidet.

:- func count(mh_scope_map(_)) = int.
:- pred count(mh_scope_map(_)::in, int::out) is det.

:- pred equal(mh_scope_map(T)::in, mh_scope_map(T)::in) is semidet.



%-----------------------------------------------------------------------------%
% Search

	% Succeeds if the map contains the given key
:- pred contains(mh_scope_map(T)::in, mh_scope::in) is semidet.

	% Fails if the key is not found
:- pred search(mh_scope_map(T)::in, mh_scope::in, T::out) is semidet.
:- func search(mh_scope_map(T), mh_scope) = T is semidet.

	% Throws an exception if the key is not found
:- pred lookup(mh_scope_map(T)::in, mh_scope::in, T::out) is det.
:- func lookup(mh_scope_map(T), mh_scope) = T is det.

%-----------------------------------------------------------------------------%
% Insertion

:- pred insert(mh_scope::in, T::in, mh_scope_map(T)::in, mh_scope_map(T)::out) 
	is semidet.
	
:- pred insert(mh_scope::in, mh_scope_set::in, mh_scope_set::out) 
	is semidet.
	
:- pred det_insert(mh_scope::in, T::in, mh_scope_map(T)::in, 
	mh_scope_map(T)::out) is det.
	
:- pred det_insert(mh_scope::in, mh_scope_set::in, mh_scope_set::out)
	is det.
	
:- pred det_insert_from_corresponding_lists(list(mh_scope)::in, list(T)::in,
	mh_scope_map(T)::in, mh_scope_map(T)::out) is det.
	
:- pred det_insert_from_assoc_list(assoc_list(mh_scope, T)::in,
	mh_scope_map(T)::in, mh_scope_map(T)::out) is det.
	
:- pred det_insert_from_list(list(mh_scope)::in, mh_scope_set::in, 
	mh_scope_set::out) is det.

:- pred set(mh_scope::in, T::in, mh_scope_map::in, mh_scope_map::out)
	is det.
	
:- pred set(mh_scope::in, mh_scope_set::in, mh_scope_set::out) is det.
	
:- pred set_from_corresponding_lists(list(mh_scope)::in, list(T)::in,
	mh_scope_map(T)::in, mh_scope_map(T)::out) is det.
	
:- pred set_from_assoc_list(assoc_list(mh_scope, T)::in,
	mh_scope_map(T)::in, mh_scope_map(T)::out) is det.
	
:- pred set_from_list(list(mh_scope)::in, mh_scope_set::in, 
	mh_scope_set::out) is det.

:- pred update(mh_scope::in, T::in, mh_scope_map(T)::in, 
	mh_scope_map::out) is semidet.
	
:- pred det_update(mh_scope::in, T::in, mh_scope_map(T)::in, 
	mh_scope_map::out) is det.
	
%-----------------------------------------------------------------------------%
% Removal

:- pred remove(mh_scope::in, T::out, mh_scope_map(T)::in, 
	mh_scope_map::out) is semidet.
	
:- pred remove(mh_scope::in, mh_scope_set::in, mh_scope_set::out) is semidet.
	
:- pred det_remove(mh_scope::in, T::out, mh_scope_map(T)::in, 
	mh_scope_map::out) is det.
	
:- pred det_remove(mh_scope::in, mh_scope_set::in, mh_scope_set::out) is det.
	
:- pred delete(mh_scope::in,  mh_scope_map(T)::in, mh_scope_map::out) is det.

:- pred delete_list(list(mh_scope)::in, mh_scope_map(T)::in, 
	mh_scope_map::out) is det.
	
%-----------------------------------------------------------------------------%
% Set operations


:- func union(func(T, T) = T, mh_scope_map(T), mh_scope_map(T)) =
	mh_scope_map(T).

:- pred union(func(T, T) = T, mh_scope_map(T), mh_scope_map(T),
	mh_scope_map(T)).
:- mode union(in(func(in, in) = out is det), in, in, out) is det.

:- func set_union(mh_scope_set, mh_scope_set) = mh_scope_set.
:- pred set_union(mh_scope_set::in, mh_scope_set::in, mh_scope_set::out)
	is det.

:- func intersect(func(T1, T2) = T3, mh_scope_map(T1), mh_scope_map(T2))
	= mh_scope_map(T3).
	
:- pred intersect(func(T1, T2) = T3, mh_scope_map(T1), mh_scope_map(T2),
	mh_scope_map(T3)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.

:- func set_intersect(mh_scope_set, mh_scope_set) = mh_scope_set.
:- pred set_intersect(mh_scope_set::in, mh_scope_set::in, mh_scope_set::out) 
	is det.

:- func difference(mh_scope_map(T), mh_scope_map(_)) =
	mh_scope_map(T).

:- pred difference(mh_scope_map(T)::in, mh_scope_map(_)::in, 
	mh_scope_map(T)::out) is det.

%-----------------------------------------------------------------------------%
% Higher Order

:- func fold(func(mh_context, mh_var_set, T, A) = A, mh_scope_map(T), A) = A.

:- pred fold(func(mh_context, mh_var_set, T, A) = A, mh_scope_map(T), A, A).
:- mode fold(in(func(in, in, in, in) = out is det), in, in, out) is det.

:- func map(func(mh_context, mh_var_set, T) = U, mh_scope_map(T)) 
	= mh_scope_map(U).
 
:- pred map(func(mh_context, mh_var_set, T) = U, mh_scope_map(T), 
	mh_scope_map(U)).
:- mode map(in(func(in, in, in) = out is det), in, out) is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module require.
:- import_module pair.



%-----------------------------------------------------------------------------%
% Scope Map

:- type set_map(T) == map(mh_var_set, T).

:- type context_map(T) == map(mh_context, set_map(T)).

:- type mh_scope_map(T)
	--->	empty_scope_map
	;		scope_map(context_map(T)).
	
init = empty_scope_map.
init(init).

singleton(Scope, Value) = scope_map(
	map.singleton(root_context(Scope), 
		map.singleton(scope_vars(Scope), Value)
	)
).

singleton(Scope) = singleton(Scope, unit).

is_empty(empty_scope_map).

count(empty_scope_map) = 0.
count(scope_map(Map)) = Count :- map.foldl_values(count_fold, Map, 0, Count).

:- pred count_fold(set_map(T)::in, int::in, int::out) is det.
count_fold(Map, Count, Count + map.count(Map)).

equal(empty_scope_map, empty_scope_map).
equal(scope_map(Map1), scope_map(Map2)) :- map.equal(Map1, Map2).


%-----------------------------------------------------------------------------%
% Search

contains(scope_map(CtxMap), Scope) :- 
	map.search(CtxMap, root_context(Scope), SetMap), 
	map.contains(SetMap, scope_vars(Scope)).

search(Map, Scope, search(Map, Scope)).
search(scope_map(CtxMap), Scope) = 
	map.search( map.search(CtxMap, root_context(Scope)) , scope_vars(Scope)). 

lookup(Map, Scope, lookup(Map, Scope)).
lookup(scope_map(CtxMap), Scope) = 
	map.lookup(map.lookup(CtxMap, root_context(Scope)), scope_vars(Scope)). 
	
%-----------------------------------------------------------------------------%
% Insertion

insert(Scope, Value, emtpy_map, singleton(Scope, Value)).

insert(Scope, Value, scope_map(!.CtxMap), scope_map(!:CtxMap)) :-
	Ctx = root_context(Scope),
	Set = scope_vars(Scope),
	some [!SetMap] 
		(if map.search(!.CtxMap, Ctx, !:SetMap)
		then
			map.insert(Set, Value, !SetMap)
			map.update(Ctx, !.SetMap, !CtxMap)
		else
			map.insert(Ctx, map.singleton(Set, Value), !CtxMap)
		).
	
insert(Scope, !Map) :- insert(Scope, unit, !Map).


det_insert(Scope, Value, !Map) :-
	(if insert(Scope, Value, !.Map, NewMap) 
	then !:Map = NewMap
	else report_lookup_error(
		"mh_scope_map.det_insert: key already present", Scope, Value)
	).

det_insert(Scope, !Map) :- det_insert(Scope, unit, !Map).

% Copied wholesale from mh_term_map ------------------------------------------%

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
det_insert_from_list([ Scope | List]) :-
	det_insert(Scope, !Set),
	det_insert_from_list(List, !Set).
	
%-----------------------------------------------------------------------------%

set(Scope, Value, emtpy_map, singleton(Scope, Value)).

set(Scope, Value, scope_map(!.CtxMap), scope_map(!:CtxMap)) :-
	Ctx = root_context(Scope),
	Set = scope_vars(Scope),
	some [!SetMap] 
		(if map.search(!.CtxMap, Ctx, !:SetMap)
		then
			map.set(Set, Value, !SetMap)
			map.update(Ctx, !.SetMap, !CtxMap)
		else
			map.set(Ctx, map.singleton(Set, Value))
		).
	
set(Scope, !Map) :- set(Scope, unit, !Map).

% Again, copied from mh_term_map ---------------------------------------------%
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
set_from_list([Scope | List]) :-
	set(Scope, !Set),
	set_from_list(List, !Set).
%-----------------------------------------------------------------------------%


update(Scope, Value, scope_map(!.CtxMap), scope_map(!:CtxMap)) :-
	Ctx = root_context(Scope),
	Set = scope_vars(Scope),
	some [!SetMap] (
		map.search(!.CtxMap, Ctx, !:SetMap),
		map.update(Set, Value, !SetMap)
		map.update(Ctx, !.SetMap, !CtxMap)
	).
	
det_update(Scope, Value, !Map) :-
	( if update(Scope, Value, !.Map, NewMap) then
        !:Map = NewMap
    else
        report_lookup_error(
			"mh_scope_map.det_update: scope not present in map", Scope, Value)
    ).
	
%-----------------------------------------------------------------------------%
% Removal

:- func check_empty(scope_map(T)) = scope_map(T).

check_empty(empty_scope_map) = empty_scope_map.

check_empty(scope_map(CtxMap)@ScopeMap) =
	(if map.is_empty(CtxMap) then empty_scope_map else ScopeMap ).

:- pred check_empty(scope_map(T)::in, scope_map(T)::out) is det.

check_empty(Map, check_empty(Map)).
	
remove(Scope, Value, scope_map(!.CtxMap), scope_map(!:CtxMap)) :-
	Ctx = root_context(Scope), Set = scope_vars(Scope), 
	some [!SetMap] (
		map.search(CtxMap, Ctx, !:SetMap),
		map.remove(Set, Value, !SetMap),
		(if map.is_empty(!.SetMap)
		then
			map.delete(Ctx, !CtxMap),
			check_empty(!CtxMap)
		else
			map.update(Ctx, !.SetMap, !CtxMap)		
		)
	).
	
remove(Scope, !Set) :- remove(Scope, unit, !Set).

det_remove(Term, Value, !Map) :-	
	(if remove(Term, FoundV, !Map)
	then !:Map = !.Map, Value = FoundV
	else report_lookup_error(
		"mh_scope_map.det_remove: scope not present in map", Term, 
		!.Map)
	).

delete(Scope, scope_map(!.CtxMap), scope_map(!:CtxMap)) :-
	Ctx = root_context(Scope), Set = scope_vars(Scope), 
	some [!SetMap] 
	(if map.search(CtxMap, Ctx, !:SetMap),
	then
		map.delete(Set, !SetMap),
		(if map.is_empty(!.SetMap)
		then
			map.delete(Ctx, !CtxMap),
			check_empty(!CtxMap)
		else
			map.update(Ctx, !.SetMap, !CtxMap)		
		)
	else
		!:CtxMap = !.CtxMap
	).
	
delete_list([], !Map).
delete_list([Scope | Scopes], !Map) :- 
	delete(Scope, !Map),
	delete_list(Scopes, !Map).
	
%-----------------------------------------------------------------------------%
% Set operations

union(_, empty_scope_map, Map) = Map.

union(F, scope_map(CtxMap1)@Map1, Map2) = 
	(if Map2 = scope_map(CtxMap2)
	then
		scope_map( map.foldl(union_ctx_fold(F), CtxMap1, CtxMap2) )
	else
		Map1	
	).
	
:- func union_ctx_fold(func(T, T) = T, mh_context, set_map(T),
	context_map(T)) = context_map(T).
	
union_ctx_fold(F, Ctx, SetMap1, CtxMap2) = CtxUnion :-
	(if map.insert(Ctx, SetMap1, CtxMap2, NewMap)
	then
		CtxUnion = NewMap
	else
		map.lookup(CtxMap2, Ctx, SetMap2),
		CtxUnion = map.foldl(union_set_fold(F), SetMap1, SetMap2)
	).
	
:- func union_set_fold(func(T, T) = T, mh_var_set, T, set_map(T)) = set_map(T).

union_set_fold(F, Set, Value1, SetMap2) = SetUnion
	(if map.insert(Set, Value1, SetMap2, NewMap)
	then
		SetUnion = NewMap
	else
		map.lookup(SetMap2, Set, Value2),
		map.set(Set, F(Value1, Value2), SetMap2, SetUnion)
	).
	
union(F, Map1, Map2, union(F, Map1, Map2)).

:- func dummy_merge(unit, unit) = unit is det.

dummy_merge(_, _) = unit.

set_union(Set1, Set2) = union(dummy_merge, Set1, Set2).

intersect(_, empty_scope_map, _) = empty_scope_map.

intersect(F, scope_map(CtxMap1), Map2) = 
	(if Map2 = scope_map(CtxMap2)
	then
		check_empty(
			scope_map( 
				map.foldl(intersect_ctx_fold(F, CtxMap2), CtxMap1, map.init)
			)
		)
	else
		empty_scope_map
	).
	
:- func intersect_ctx_fold(func(T, T) = T, context_map(T), mh_context, 
	set_map(T),	context_map(T)) = context_map(T).
	
intersect_ctx_fold(F, CtxMap2, Ctx, SetMap1, !.IntCtx) = !:IntCtx :-
	(if map.search(CtxMap2, Ctx, SetMap2)
	then
		IntSet = map.foldl(interset_set_fold(F, SetMap2), SetMap1, map.init),
		(if is_empty(IntSet)
		then true
		else
			map.det_insert(Ctx, IntSet, !IntCtx)
		)
	else true
	).
	
:- func intersect_ctx_fold(func(T, T) = T, set_map(T), mh_var_set, T,
	set_map(T)) = set_map(T).
	
interset_set_fold(F, SetMap2, Set, Value1, !.IntSet) = !:IntSet :-
	(if map.search(SetMap2, Set, Value2)
	then
		map.det_insert(Set, F(Value1, Value2), !IntSet)
	else true
	).
	
intersect(F, Map1, Map2, intersect(F, Map1, Map2)).

set_intersect(Map1, Map2) = intersect(dummy_merge, Map1, Map2).
set_insersect(Map1, Map2, set_intersect(Map1, Map2)).

difference(empty_scope_map, _) = empty_scope_map.

difference(scope_map(CtxMap1), Map2) = 
	(if Map2 = scope_map(CtxMap2)
		check_empty(
			scope_map(
				map.foldl(difference_ctx_fold, CtxMap2, CtxMap1)
			)
		)
	else 
		empty_scope_map
	).
	
:- func difference_ctx_fold(mh_context, set_map(T), context_map(T)) = 
	context_map(T).
	
difference_ctx_fold(Ctx, SetMap2, CtxMap1) = 
	(if 
		map.search(CtxMap1, Ctx, SetMap1),
		SetDiff = map.foldl(difference_set_fold, SetMap2, SetMap1),
		map.set(Ctx, SetDiff, CtxMap1, CtxDiff)
	then
		CtxDiff
	else
		CtxMap1
	).
	
:- func difference_set_fold(mh_var_set, T, set_map(T)) = set_map(T).

difference_set_fold(Set, _Value, SetMap1) = map.delete(SetMap1, Set).
	

%-----------------------------------------------------------------------------%
% Higher Order

fold(_, empty_scope_map, A) = A.
fold(F, scope_map(CtxMap), A) = map.foldl(fold_ctx(F), CtxMap, A).

:- func fold_ctx(func(mh_context, mh_var_set, T, A) = A), mh_context, 
	set_map(T), A) = A.
	
fold_ctx(F, Ctx, SetMap, A) = map.foldl(fold_set(F, Ctx), SetMap, A). 
	
:- func fold_set(func(mh_context, mh_var_set, T, A) = A), mh_context,
	mh_var_set, T, A) = A.
	
fold_set(F, Ctx, Set, Value, A) = F(Ctx, Set, T, A).

fold(F, Map, A, fold(F, Map, A)).

map(_, empty_scope_map, empty_scope_map).

map(F, scope_map(CtxMap)) =  scope_map(map.map_values(map_ctx(F), CtxMap)).

:- func map_ctx(func(mh_context, mh_var_set, T) = U, mh_context, set_map(T)) 
	= set_map(U).

map_ctx(F, Ctx, SetMap) = map.map_values(F(Ctx), SetMap).

map(F, Map, map(F, Map)).

