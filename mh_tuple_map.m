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

:- func singleton(mh_tuple, T) = mh_tuple_map(T).
:- func singleton(mh_tuple) = mh_tuple_set.

:- pred is_empty(mh_tuple_map(_)::in) is semidet.

%-----------------------------------------------------------------------------%
% Search

	% Succeeds if the map contains the given key
:- pred contains(mh_tuple_map(T)::in, mh_tuple::in) is semidet.

	% Fails if the key is not found
:- pred search(mh_tuple_map(T)::in, mh_tuple::in, 
T::out) is semidet.
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
	
:- pred remove(mh_tuple::in, mh_tuple_set::in, mh_tuple_set::out) is semidet.
	
:- pred det_remove(mh_tuple::in, T::out, mh_tuple_map(T)::in, 
	mh_tuple_map::out) is det.
	
:- pred det_remove(mh_tuple::in,  mh_tuple_set::in, mh_tuple_set::out) is det.
	
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

:- func union(mh_tuple_map(_), mh_tuple_map(_)) = mh_tuple_set.
:- pred union(mh_tuple_map(_)::in, mh_tuple_map(_)::in, mh_tuple_set::out) is det.

:- func intersect(func(T, T) = T, mh_tuple_map(T), mh_tuple_map(T))
	= mh_tuple_map(T).
	
:- pred intersect(func(T, T) = T, mh_tuple_map(T), mh_tuple_map(T),
	mh_tuple_map(T)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.
:- mode intersect(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func intersect(mh_tuple_map(_), mh_tuple_map(_)) = mh_tuple_set.
:- pred intersect(mh_tuple_map(_)::in, mh_tuple_map(_)::in, mh_tuple_set::out) 

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

:- import_module array.
:- import_module list.
:- use_module map.

:- import_module mh_tuple_map.

%-----------------------------------------------------------------------------%

:- type term_list_map(T) == map.map(list(mh_term), T).

:- type mh_tuple_map(T)
	--->	tuple_map(term_list_map(T), ).
	
init = 
