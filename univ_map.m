%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: univ_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module univ_map.

:- interface.

:- import_module univ.
:- import_module type_desc.

%-----------------------------------------------------------------------------%
% Univ map

:- type univ_map(T).

:- func init = (univ_map(T)::uo) is det.
:- pred init(univ_map(_)::uo) is det.

:- func singleton(K, V) = univ_map(V).
:- func singleton_univ(univ, T) = univ_map(T).

:- func count(univ_map(_)) = int.
:- pred count(univ_map(_)::in, int::out) is det.

:- pred equal(univ_map(T)::in, univ_map(T)::in) is semidet.

:- pred is_empty(univ_map(_)::in) is semidet.

%-----------------------------------------------------------------------------%
% Search and lookup

:- pred contains(univ_map(_T)::in, K::in) is semidet.
:- pred contains_univ(univ_map(_T)::in, univ::in) is semidet.
:- pred contains_type(univ_map(_T)::in, type_desc::in) is semidet.

% TODO: Function versions of calls?
% TODO: Det versions of semidet calls?

:- pred search(univ_map(V)::in, K::in, V::out) is semidet.
:- pred search_univ(univ_map(T)::in, univ::in, T::out) is semidet.

:- pred lookup(univ_map(V)::in, K::in, V::out) is det.
:- pred lookup_univ(univ_map(T)::in, univ::in, T::out) is det.

%TODO: Bound search and min max keys?

%-----------------------------------------------------------------------------%
% Insertions

:- pred insert(K::in, V::in, univ_map(V)::in, univ_map(V)::out) 
	is semidet.

:- pred insert_univ(univ::in, T::in, univ_map(T)::in, univ_map(T)::out) 
	is semidet.

:- pred update(K::in, V::in, univ_map(V)::in, univ_map(V)::out) 
	is semidet.

:- pred update_univ(univ::in, T::in, univ_map(T)::in, univ_map(T)::out) 
	is semidet.
	
:- pred set(K::in, V::in, univ_map(V)::in, univ_map(V)::out) is det.
:- pred set_univ(univ::in, T::in, univ_map(T)::in, univ_map(T)::out)
	is det.

%-----------------------------------------------------------------------------%
% Deletions

:- pred delete(K::in, univ_map(_V)::in, univ_map(_V)::out) is det.
:- pred delete_univ(univ::in, univ_map(_V)::in, univ_map(_V)::out) 
	is det.

:- pred remove(K::in, V::out,  univ_map(V)::in, univ_map(V)::out) is semidet.
:- pred remove_univ(univ::in, V::out, univ_map(V)::in, univ_map(V)::out)
	is semidet.
	
	% Remove the smallest value of a given type, fail if there are no
	% values of the given type
:- pred remove_smallest_typed(K::out, V::out, 
	univ_map(V)::in, univ_map(V)::out) 	is semidet.
	
	% Remove values from the map, starting frorm the smallest type in the 
	% standard ordering, removing the smallest value of that type, fail if
	% the map is empty, this should align with the standard ordering
:- some [K] pred remove_smallest(K::out, V::out, 
	univ_map(V)::in, univ_map(V)::out) 	is semidet.
	
%-----------------------------------------------------------------------------%
% Nondeterminsitic lookup

:- some [K] pred member(univ_map(V)::in, K::out, V::out) is nondet.

:- pred member_univ(univ_map(V)::in, univ::out, V::out) is nondet.

%-----------------------------------------------------------------------------%
% Set Operations

:- func union(func(T, T) = T, univ_map(T), univ_map(T)) = 
	univ_map(T).

:- pred union(func(T, T) = T, univ_map(T), univ_map(T), 
	univ_map(T)).
:- mode union(in(func(in, in) = out is det), in, in, out) is det.

:- func intersect(func(T, T) = T, univ_map(T), univ_map(T)) = 
	univ_map(T).
	
:- pred intersect(func(T, T) = T, univ_map(T), univ_map(T),
	univ_map(T)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.

:- func difference(univ_map(T), univ_map(_)) = univ_map(T).

:- pred difference(univ_map(T)::in, univ_map(_)::in, 
	univ_map(T)::out) is det.
	
	
%-----------------------------------------------------------------------------%
% Higher Order

:- func fold(func(univ, T, A) = A, univ_map(T), A) = A.

:- pred fold(func(univ, T, A) = A, univ_map(T), A, A).
:- mode fold(in(func(in, in, in) = out is det), in, in, out) is det.

:- func map(func(univ, T) = U, univ_map(T)) = univ_map(U).
 
:- pred map(func(univ, T) = U, univ_map(T), univ_map(U)).
:- mode map(in(func(in, in) = out is det), in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module list.
:- import_module require.
:- import_module maybe.

:- import_module util.

%-----------------------------------------------------------------------------%
% Value maps

% TODO: make univ hashable and replace this implementation with a hashmap

:- type type_map(T) == map(type_desc, value_map(T)).

:- type univ_map(T) 
	--->	univ_map(type_map(T)).
	
:- type value_map(T)
	--->	some [U] value_map(map(U, T)).
		
init = univ_map(map.init).

init(init).

singleton(K, V) = univ_map(map.singleton(Ktype, TypeMap)) :-
	Ktype = type_of(K),
	TypeMap = 'new value_map'(map.singleton(K, V)).
	
singleton_univ(U, V) = univ_map(map.singleton(Ktype, TypeMap)) :-
	Ktype = univ_type(U),
	K = univ_value(U),
	TypeMap = 'new value_map'(map.singleton(K, V)).
	
count(univ_map(Map)) = map.count(Map).
count(Map, count(Map)).
	
equal(univ_map(Map1), univ_map(Map2)) :- map.equal(Map1, Map2).

is_empty(univ_map(M)) :- map.is_empty(M).

%-----------------------------------------------------------------------------%
% Search and lookup

contains(univ_map(VM), K) :-
	Ktype = type_of(K),
	map.search(VM, Ktype, value_map(TM)),
	map.contains(TM, det_dynamic_cast(K)).
	
contains_univ(univ_map(VM), U) :-
	Ktype = univ_type(U),
	map.search(VM, Ktype, value_map(TM)),
	det_univ_to_type(U, K),
	map.contains(TM, K).
	
contains_type(univ_map(M), T) :- map.contains(M, T).

search(univ_map(VM), K, V) :-
	Ktype = type_of(K),
	map.search(VM, Ktype, value_map(TM)),
	map.search(TM, det_dynamic_cast(K), V).
	
search_univ(univ_map(VM), U, V) :-
	Ktype = univ_type(U),
	map.search(VM, Ktype, value_map(TM)),
	det_univ_to_type(U, K),
	map.search(TM, K, V).

lookup(univ_map(VM), K, V) :-
	Ktype = type_of(K),
	map.lookup(VM, Ktype, value_map(TM)),
	map.lookup(TM, det_dynamic_cast(K), V).
	
lookup_univ(univ_map(VM), U, V) :-
	Ktype = univ_type(U),
	map.lookup(VM, Ktype, value_map(TM)),
	det_univ_to_type(U, K),
	map.lookup(TM, K, V).

%-----------------------------------------------------------------------------%
% Insertions 

insert(K, V, univ_map(!.VM), univ_map(!:VM)) :-
	Ktype = type_of(K),
	( if map.search(!.VM, Ktype, value_map(TM0))
	then
		map.insert(det_dynamic_cast(K), V, TM0, TM),
		map.det_update(Ktype, 'new value_map'(TM), !VM)
	else
		map.det_insert(Ktype, 'new value_map'(map.singleton(K, V)), !VM)
	).
	
insert_univ(U, V, !M) :-
	insert(univ_value(U), V, !M).
	
update(K, V, univ_map(!.VM), univ_map(!:VM)) :-
	Ktype = type_of(K),
	map.search(!.VM, Ktype, value_map(TM0)),
	map.update(det_dynamic_cast(K), V, TM0, TM),
	map.det_update(Ktype, 'new value_map'(TM), !VM).
	
update_univ(U, V, !M) :-
	update(univ_value(U), V, !M).
	
set(K, V, univ_map(!.VM), univ_map(!:VM)) :-
	Ktype = type_of(K),
	( if map.search(!.VM, Ktype, value_map(TM0))
	then
		map.set(det_dynamic_cast(K), V, TM0, TM),
		map.det_update(Ktype, 'new value_map'(TM), !VM)
	else
		map.det_insert(Ktype, 'new value_map'(map.singleton(K, V)), !VM)
	).
	
set_univ(U, V, !M) :-
	set(univ_value(U), V, !M).

%-----------------------------------------------------------------------------%
% Deletions

delete(K, univ_map(!.VM), univ_map(!:VM)) :-
	Ktype = type_of(K),
	( if 
		map.search(!.VM, Ktype, value_map(TM0)), 
		map.remove(det_dynamic_cast(K), _, TM0, TM)
	then 
		( if map.is_empty(TM)
		then map.delete(Ktype, !VM)
		else map.det_update(Ktype, 'new value_map'(TM), !VM)
		)
	else
		!:VM = !.VM
	).
	
delete_univ(U, !M) :-
	delete(univ_value(U), !M).
	
remove(K, V, univ_map(!.VM), univ_map(!:VM)) :-
	Ktype = type_of(K),
	map.search(!.VM, Ktype, value_map(TM0)),
	map.remove(det_dynamic_cast(K), V, TM0, TM),
	( if map.is_empty(TM)
	then map.delete(Ktype, !VM)
	else map.det_update(Ktype, 'new value_map'(TM), !VM)
	).
	
remove_univ(U, V, !M) :-
	remove(univ_value(U), V,  !M).
	
remove_smallest_typed(K, V, univ_map(!.VM), univ_map(!:VM)) :-
	Ktype = type_of(K),
	map.search(!.VM, Ktype, value_map(TM0)),
	map.remove_smallest(U, V, TM0, TM),
	det_dynamic_cast(U, K),
	( if map.is_empty(TM)
	then map.delete(Ktype, !VM)
	else map.det_update(Ktype, 'new value_map'(TM), !VM)
	).
	
remove_smallest(K, V, univ_map(!.VM), univ_map(!:VM)) :-
	Ktype = min_key(!.VM),
	map.search(!.VM, Ktype, value_map(TM0)),
	( if remove_smallest_empty_value_map_check, is_empty(TM0)
	then unexpected($module, $pred, 
		"Empty type map found in value map when attempting ordered removal.")
	else true
	),
	map.remove_smallest(K, V, TM0, TM),	
	( if map.is_empty(TM)
	then map.delete(Ktype, !VM)
	else map.det_update(Ktype, 'new value_map'(TM), !VM)
	).

:- pred remove_smallest_empty_value_map_check is semidet.

:- pragma no_determinism_warning(remove_smallest_empty_value_map_check/0).

remove_smallest_empty_value_map_check :- true.
%-----------------------------------------------------------------------------%
% Nondeterminsitic lookup
	
member(univ_map(VM), K, V) :-
	map.member(VM, _, value_map(TM)),
	map.member(TM, K, V).
	
member_univ(VM, U, V) :- member(VM, K,V), type_to_univ(K, U).

%-----------------------------------------------------------------------------%
% Set Operations

union(F, Map1, Map2) = fold(union_fold(F), Map2, Map1).

:- func union_fold(func(T, T) = T, univ, T, univ_map(T)) = 
	univ_map(T).
	
union_fold(F, Univ, T2, !.Map1) = !:Map1 :-
	(if search_univ(!.Map1, Univ, T1) then
		set_univ(Univ, F(T1, T2), !Map1)
	else
		set_univ(Univ, T2, !Map1)
	).
	
union(F, Map1, Map2, union(F, Map1, Map2)).

intersect(F, Map1, Map2) = fold(intersect_fold(F, Map1), Map2, init).

:- func intersect_fold(func(T, T) = T, univ_map(T), univ, T, univ_map(T)) = 
	univ_map(T).
	
intersect_fold(F, Map1, Univ, T2, !.Map3) = !:Map3 :-
	(if search_univ(Map1, Univ, T1) then
		set_univ(Univ, F(T1, T2), !Map3)
	else true
	).
	
intersect(F, Map1, Map2, intersect(F, Map1, Map2)).

difference(Map1, Map2) = fold(difference_fold, Map2, Map1).

:- func difference_fold(univ, _, univ_map(T)) = 
	univ_map(T).
	
difference_fold(Univ, _, !.Map1) = !:Map1 :-
	delete_univ(Univ, !Map1).	
	
difference(Map1, Map2, difference(Map1, Map2)).

	
%-----------------------------------------------------------------------------%
% Higher Order

fold(F, univ_map(Map), A) = map.foldl(outer_fold(F), Map, A).

:- func outer_fold(func(univ, T, A) = A, type_desc, value_map(T), A)	= A.

outer_fold(F, _TypeDesc, value_map(TypeMap), A) = 
	map.foldl(inner_fold(F), TypeMap, A).

:- func inner_fold(func(univ, T, A) = A, K, T, A) = A.

inner_fold(F, K, T, A) = F(univ(K), T, A).

fold(F, Map, A, fold(F, Map, A)).


map(F, univ_map(Map)) = 
	univ_map( map.foldl(map_fold(F), Map, map.init) ).

:- func map_fold(func(univ, T) = U, type_desc, value_map(T), type_map(U))
	= type_map(U).
	
map_fold(F, Type, value_map(ValMap), !.TypeMap) = !:TypeMap :-
	map.det_insert(Type, 'new value_map'(map.map_values(map_univ(F), ValMap)), !TypeMap).

:- func map_univ(func(univ, T) = U, K, T) = U.

map_univ(F, K, T) = F(univ(K), T).

map(F, Map, map(F, Map)).


	
