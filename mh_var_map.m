%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_var_map(T).m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_var_map(T).

:- interface.

:- import_module array.
:- import_module list.
:- import_module assoc_list.
:- import_module maybe.

:- import_module mh_term.
:- import_module mh_var_id.
:- import_module var_set.

%-----------------------------------------------------------------------------%
% Variable map


:- type mh_var_map(T).
	
:- pred init(mh_var_map(T)::out) is det.
:- func init = mh_var_map(T).
	
:- pred empty_var_map(mh_var_map(T)::out) is det.

:- func singleton_id(var_id, T) = mh_var_map(T).
:- pred singleton_id(var_id::in, T::in, mh_var_map(T)::out) is det.

:- func singleton(mh_var, T) = mh_var_map(T).
:- pred singleton(mh_var::in, T::in, mh_var_map(T)::out) is det

% var_map_bounds(var_mapsitution, Min, Max)
% Return the minimum and maximum var_id's indexed by var_map
:- pred var_map_bounds(mh_var_map(T)::in, var_id_offset::out, 
	var_id_set::out) is det.

	
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
:- func id_search(mh_var_map(T), var_id) = mh_term is semidet.

:- pred search(mh_var_map(T)::in, mh_var::in, T::out) is semidet.
:- func search(mh_var_map, mh_var) = T is semidet.

% Find a given variable ID in the var_map, throw an exception if the id is not
% found 
:- pred id_lookup(mh_var_map(T)::in, var_id::in, T::out) is det.
:- func id_lookup(mh_var_map(T), var_id) = T.


:- pred lookup(mh_var_map(T)::in, mh_var::in, mh_term::out) is det.
:- func lookup(mh_var_map(T), mh_var) = mh_term.

%-----------------------------------------------------------------------------%
% Insertion

	% Insert an element into a hashmap, fails if the element already exists 
:- pred id_insert(var_id::in, T::in, var_map(T)::in, var_map(T)::out) 
	is semidet.

:- pred insert(mh_var::in, T::in, var_map(T)::in, var_map(T)::out) is semidet.

:- pred det_insert(mh_var::in, T::in, var_map(T)::in, var_map(T)::out) is det.


:- pred det_insert_from_corresponding_lists(list(var_id)::in,
    list(T)::in, var_map(T)::in, var_map(T)::out) is det.
	

:- pred det_insert_from_assoc_list(assoc_list(mh_var, T)::in,
    var_map(T)::in, var_map(T)::out) is det.
	
:- pred search_insert(mh_var::in, T::in, maybe(T)::out,
    var_map(T)::in, var_map(T)::out) is det.

	% Inserts an element into a hashmap, overwriting element if it already 
	% exists
:- pred id_set(var_id::in, T::in, var_map(T)::in, var_map(T)::out) is det.
	
:- pred set(mh_var::in, T::in, var_map(T)::in, var_map(T)::out) is det.


:- pred set_from_corresponding_lists(list(var_id)::in, list(T)::in,
    var_map(T)::in, var_map(T)::out) is det.
	

:- pred set_from_assoc_list(assoc_list(mh_var, T)::in,
    var_map(T)::in, var_map(T)::out) is det.

	% Overwrite an already existing element in a hashmap, fail if key not found
:- pred id_update(var_id::in, T::in, var_map(T)::in, var_map(T)::out) 
	is semidet.
	
:- pred update(mh_var::in, T::in, var_map(T)::in, var_map(T)::out) 
	is semidet.
	
:- pred det_update(mh_var::in, T::in, var_map(T)::in, var_map(T)::out) 
	is det.
	
%-----------------------------------------------------------------------------%
% Removal

	% Remove a key-value pair from a map and return the value.
	% Fail if the key is not present.
:- pred remove(mh_var::in, T::out, var_map(T)::in, var_map(T)::out) 
	is semidet.

:- pred det_remove(mh_var::in, T::out, var_map(T)::in, var_map(T)::out) 
	is det.

	% Delete a key-value pair from a map.
	% If the key is not present, leave the map unchanged.	
:- pred delete(mh_var::in, var_map(T)::in, var_map(T)::out) is det
	<= hashable(K).
:- func delete(var_map(T), K) = var_map(T).

:- func delete_list(var_map(T), list(K)) = var_map(T).
:- pred delete_list(list(K)::in, var_map(T)::in, var_map(T)::out)
	is det.	
	

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%
%  Var Maps

% Var maps are represented by a sparse array indexed by a var_set

:- type mh_var_map(T).
	--->	var_map_empty
	;		var_map(set::mh_var_set, array::array(T)).

init(init_var_map).
init = var_map_empty.


empty_var_map(init).

singleton_id(ID, T) = var_map(singleton_var_set(ID), array.init(1, T)).
singleton_id(ID, T, singleton_id(ID, T)).

singleton(var(ID), T) = singleton(ID, T).
singleton(Var, T, singleton(Var, T)).



var_map_bounds(var_map_empty, Offset, ID_Set) :- 
	var_set_bounds(empty_var_set, Offset, ID_Set).
	
var_map_bounds(var_map(Set, _), Offset, ID_Set) :- 
	var_set_bounds(Set, Offset, ID_Set).


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
        report_lookup_error("mh_var_map.id_lookup: Var ID not found.")
	).
	
id_lookup(Map, ID) = Term :- id_lookup(Map, ID, Term).

%-----------------------------------------------------------------------------%

lookup(Map, var(ID), Term) :- 
	( if id_search(var_map, ID, Found)
	then Term = Found
	 else
        report_lookup_error("mh_var_map.lookup: Var ID not found.")
	).

lookup(var_map, Var) = Term :- lookup(var_map, Var, Term).

%-----------------------------------------------------------------------------%
% Insertion

id_insert(ID, T, var_map_empty, singleton_id(ID, T)).

id_insert(ID, T, var_map(!.Set, !.Array), var_map(!:Set, !:Array)) :-
	var_set_insert_id(ID, !Set),
	id_sparse_index(ID, !.Set, Index),
	array_insert(Index, T, !Array).
	
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

det_insert_from_assoc_list(M0, AL) = M :-
    det_insert_from_assoc_list(AL, M0, M).

det_insert_from_assoc_list([], !Map).
det_insert_from_assoc_list([K - V | KVs], !Map) :-
    det_insert(K, V, !Map),
    det_insert_from_assoc_list(KVs, !Map).	
	
search_insert(var(ID), T, Result, !Map) :-
	( if search(!.Map, ID, Old)
	then
		Result = yes(Old),
	else
		Result = no.
	),
	id_set(ID, T, !Map).
	
id_set(ID, T, !Map) :-
	(if id_insert(ID, T, !Map)
	then !:Map = !.Map
	else 
		some [!Array] (	
			!.Map = var_map(Set, !.Array),
			id_sparse_index(ID, Set, Index),
			slow_set(Index, T, !Array),
			!:Map = var_map(Set, !:Array)
		;
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

update(var(ID), T, !Map) :- id_update(ID, T, !Map).

det_update(Var, T, !Map) :-	
	(if update(Var, T, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_var_map.det_update: Var ID not present in map", Var, !.Map)
	).
