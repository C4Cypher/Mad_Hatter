%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_term_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_term_map.

:- interface.


:- import_module mh_term.
:- import_module mh_var_id.

%-----------------------------------------------------------------------------%
% Term maps

:- type mh_term_map(T).
:- type mh_term_set == mh_term_map(unit).

:- type key_term_func(T) == (func(T) = mh_term).

:- func init = (mh_term_map(T)::uo) is det.
:- pred init(mh_term_map(_)::uo) is det.

:- func singleton(mh_term, T) = mh_term_map(T).
:- func singleton(mh_term) = mh_term_set.

:- pred is_empty(mh_term_map(_)::in) is semidet.



%-----------------------------------------------------------------------------%
% Search

	% Succeeds if the map contains the given key
:- pred contains(mh_term_map(T)::in, mh_term::in) is semidet.

	% Fails if the key is not found
:- pred search(mh_term_map(T)::in, mh_term::in, 
T::out) is semidet.
:- func search(mh_term_map(T), mh_term) = T is semidet.

	% Throws an exception if the key is not found
:- pred lookup(mh_term_map(T)::in, mh_term::in, T::out) is det.
:- func lookup(mh_term_map(T), mh_term) = T is det.

%-----------------------------------------------------------------------------%
% Insertion

:- pred insert(mh_term::in, T::in, mh_term_map(T)::in, 
	mh_term_map(T)::out) is semidet.
	
:- pred insert(mh_term::in, mh_term_set::in, mh_term_set::out) 
	is semidet.
	
:- pred det_insert(mh_term::in, T::in, mh_term_map(T)::in, 
	mh_term_map(T)::out) is det.
	
:- pred det_insert(mh_term::in, mh_term_set::in, mh_term_set::out)
	is det.
	
:- pred det_insert_from_corresponding_lists(list(mh_term)::in, list(T)::in,
	mh_term_map(T)::in, mh_term_map(T)::out) is det.
	
:- pred det_insert_from_assoc_list(assoc_list(mh_term, T)::in,
	mh_term_map(T)::in, mh_term_map(T)::out) is det.
	
:- pred det_insert_from_list(list(mh_term)::in, mh_term_set::in, 
	mh_term_set::out) is det.

:- pred set(mh_term::in, T::in, mh_term_map::in, mh_term_map::out)
	is det.
	
:- pred set(mh_term::in, mh_term_set::in, mh_term_set::out) is det.
	
:- pred set_from_corresponding_lists(list(mh_term)::in, list(T)::in,
	mh_term_map(T)::in, mh_term_map(T)::out) is det.
	
:- pred set_from_assoc_list(assoc_list(mh_term, T)::in,
	mh_term_map(T)::in, mh_term_map(T)::out) is det.
	
:- pred set_from_list(list(mh_term)::in, mh_term_set::in, 
	mh_term_set::out) is det.

:- pred update(mh_term::in, T::in, mh_term_map(T)::in, 
	mh_term_map::out) is semidet.
	
:- pred det_update(mh_term::in, T::in, mh_term_map(T)::in, 
	mh_term_map::out) is det.
	
%-----------------------------------------------------------------------------%
% Removal

:- pred remove(mh_term::in, T::out, mh_term_map(T)::in, 
	mh_term_map::out) is semidet.
	
:- pred remove(mh_term::in, mh_term_set::in, mh_term_set::out) is semidet.
	
:- pred det_remove(mh_term::in, T::out, mh_term_map(T)::in, 
	mh_term_map::out) is det.
	
:- pred det_remove(mh_term::in,  mh_term_set::in, mh_term_set::out) is det.
	
:- pred delete(mh_term::in,  mh_term_map(T)::in, 
	mh_term_map::out) is det.

:- pred delete_list(list(mh_term)::in, mh_term_map(T)::in, 
	mh_term_map::out) is det.
	
%-----------------------------------------------------------------------------%
% Set operations


:- func union(func(T, T) = T, mh_term_map(T), mh_term_map(T)) =
	mh_term_map(T).

:- pred union(func(T, T) = T, mh_term_map(T), mh_term_map(T), mh_term_map(T)).
:- mode union(in(func(in, in) = out is det), in, in, out) is det.
:- mode union(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func union(mh_term_map(_), mh_term_map(_)) = mh_term_set.
:- pred union(mh_term_map(_)::in, mh_term_map(_)::in, mh_term_set::out) is det.

:- func intersect(func(T, T) = T, mh_term_map(T), mh_term_map(T))
	= mh_term_map(T).
	
:- pred intersect(func(T, T) = T, mh_term_map(T), mh_term_map(T),
	mh_term_map(T)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.
:- mode intersect(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func intersect(mh_term_map(_), mh_term_map(_)) = mh_term_set.
:- pred intersect(mh_term_map(_)::in, mh_term_map(_)::in, mh_term_set::out) 
	is det.

:- func difference(mh_term_map(T), mh_term_map(_)) =
	mh_term_map(T).

:- pred difference(mh_term_map(T)::in, mh_term_map(_)::in, 
	mh_term_map(T)::out) is det.
	
:- func set_difference(mh_term_map(_), mh_term_map(_)) = mh_term_set.
:- pred set_difference(mh_term_map(_)::in, mh_term_map(_)::in, 
	mh_term_set::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.
:- import_module array.
:- import_module hash_table.
:- import_module map.
:- import_module type_desc.
:- import_module univ.

:- import_module mh_term.
:- import_module mh_symbol.
:- import_module mh_value_map.
:- import_module mh_var_map.
:- import_module mh_term_map.
:- import_module hashmap.

%-----------------------------------------------------------------------------%
% Term maps

:- type mh_term_map(T)
	--->	empty_term_map
	;		term_map(
			nil :: maybe(T),
			atom :: symbol_map(T),
			var :: mh_var_map(T),
			mr_value :: mr_value_map(T),
			cons :: mh_term_map(mh_term_map(T)), 
			tuple :: tuple_map(T),
			lazy :: mh_term_map(T),
			predicate ::
			relation ::
			function ::
	).

:- type symbol_map(T) == hashmap(mh_symbol, T).
:- type tuple_map(T) == map()

