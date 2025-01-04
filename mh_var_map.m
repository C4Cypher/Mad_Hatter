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
% :- import_module enum. Is this needed? At all?  Make mh_var enumerable?

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

singleton_id(Id, T) = var_map(singleton_var_set(Id), array.init(1, T)).
singleton_id(Id, T, singleton_id(Id, T)).

singleton(var(Id), T) = singleton(Id, T).
singleton(Var, T, singleton(Var, T)).



var_map_bounds(var_map_empty, Offset, Id_Set) :- 
	var_set_bounds(empty_var_set, Offset, Id_Set).
	
var_map_bounds(var_map(Set, _), Offset, Id_Set) :- 
	var_set_bounds(Set, Offset, Id_Set).


%-----------------------------------------------------------------------------%
% Looking up variables in var_maps

contains_id(var_map_empty, _) :- fail.
contains_id(var_map(Set, _), Id) :- var_set_contains_id(Set, Id).

contains(Map, var(Id)) :- contains_id(Map, Id).

%-----------------------------------------------------------------------------%

id_search(var_map(Set, Array), Id, T) :- 
	id_sparse_index(Id, Set, Index),
	array.lookup(Array, Index, T).

id_search(Map, Id) = T :- id_search(Map, Id, T).

search(Map, var(Id), id_search(Map, Id)).

search(Map, Var) = T :- search(Map, Var, T).

%-----------------------------------------------------------------------------%

id_lookup(Map, ID, Term) :-
	( if id_search(Map, ID, Found)
	then Term = Found
	 else
        report_lookup_error("mh_var_map.id_lookup: Var Id not found.")
	).
	
id_lookup(Map, ID) = Term :- id_lookup(Map, ID, Term).

%-----------------------------------------------------------------------------%

lookup(Map, var(ID), Term) :- 
	( if id_search(var_map, ID, Found)
	then Term = Found
	 else
        report_lookup_error("mh_var_map.lookup: Var Id not found.")
	).

lookup(var_map, Var) = Term :- lookup(var_map, Var, Term).

