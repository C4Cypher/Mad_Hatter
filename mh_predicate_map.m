%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_predicate_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_predicate_map.

:- interface.


:- import_module mh_term.
:- import_module mh_var_id.

%-----------------------------------------------------------------------------%
% Term maps

:- type mh_predicate_map(T).
:- type mh_predicate_set == mh_predicate_map(unit).

:- func init = (mh_predicate_map(T)::uo) is det.
:- pred init(mh_predicate_map(_)::uo) is det.

:- func singleton(mh_term, T) = mh_predicate_map(T).
:- func singleton(mh_term) = mh_term_set.

:- pred is_empty(mh_predicate_map(_)::in) is semidet.

:- pred equal(mh_predicate_map(T)::in, mh_predicate_map(T)::in) is semidet.



%-----------------------------------------------------------------------------%
% Search

	% Succeeds if the map contains the given key
:- pred contains(mh_predicate_map(T)::in, mh_term::in) is semidet.

	% Fails if the key is not found
:- pred search(mh_predicate_map(T)::in, mh_term::in, 
T::out) is semidet.
:- func search(mh_predicate_map(T), mh_term) = T is semidet.

	% Throws an exception if the key is not found
:- pred lookup(mh_predicate_map(T)::in, mh_term::in, T::out) is det.
:- func lookup(mh_predicate_map(T), mh_term) = T is det.

%-----------------------------------------------------------------------------%
% Insertion

:- pred insert(mh_term::in, T::in, mh_predicate_map(T)::in, 
	mh_predicate_map(T)::out) is semidet.
	
:- pred insert(mh_term::in, mh_term_set::in, mh_term_set::out) 
	is semidet.
	
:- pred det_insert(mh_term::in, T::in, mh_predicate_map(T)::in, 
	mh_predicate_map(T)::out) is det.
	
:- pred det_insert(mh_term::in, mh_term_set::in, mh_term_set::out)
	is det.
	
:- pred det_insert_from_corresponding_lists(list(mh_term)::in, list(T)::in,
	mh_predicate_map(T)::in, mh_predicate_map(T)::out) is det.
	
:- pred det_insert_from_assoc_list(assoc_list(mh_term, T)::in,
	mh_predicate_map(T)::in, mh_predicate_map(T)::out) is det.
	
:- pred det_insert_from_list(list(mh_term)::in, mh_term_set::in, 
	mh_term_set::out) is det.

:- pred set(mh_term::in, T::in, mh_predicate_map::in, mh_predicate_map::out)
	is det.
	
:- pred set(mh_term::in, mh_term_set::in, mh_term_set::out) is det.
	
:- pred set_from_corresponding_lists(list(mh_term)::in, list(T)::in,
	mh_predicate_map(T)::in, mh_predicate_map(T)::out) is det.
	
:- pred set_from_assoc_list(assoc_list(mh_term, T)::in,
	mh_predicate_map(T)::in, mh_predicate_map(T)::out) is det.
	
:- pred set_from_list(list(mh_term)::in, mh_term_set::in, 
	mh_term_set::out) is det.

:- pred update(mh_term::in, T::in, mh_predicate_map(T)::in, 
	mh_predicate_map::out) is semidet.
	
:- pred det_update(mh_term::in, T::in, mh_predicate_map(T)::in, 
	mh_predicate_map::out) is det.
	
%-----------------------------------------------------------------------------%
% Removal

:- pred remove(mh_term::in, T::out, mh_predicate_map(T)::in, 
	mh_predicate_map::out) is semidet.
	
:- pred remove(mh_term::in, mh_term_set::in, mh_term_set::out) is semidet.
	
:- pred det_remove(mh_term::in, T::out, mh_predicate_map(T)::in, 
	mh_predicate_map::out) is det.
	
:- pred det_remove(mh_term::in,  mh_term_set::in, mh_term_set::out) is det.
	
:- pred delete(mh_term::in,  mh_predicate_map(T)::in, 
	mh_predicate_map::out) is det.

:- pred delete_list(list(mh_term)::in, mh_predicate_map(T)::in, 
	mh_predicate_map::out) is det.
	
%-----------------------------------------------------------------------------%
% Set operations


:- func union(func(T, T) = T, mh_predicate_map(T), mh_predicate_map(T)) =
	mh_predicate_map(T).

:- pred union(func(T, T) = T, mh_predicate_map(T), mh_predicate_map(T), mh_predicate_map(T)).
:- mode union(in(func(in, in) = out is det), in, in, out) is det.
:- mode union(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func set_union(mh_term_set, mh_term_set) = mh_term_set.
:- pred set_union(mh_term_set::in, mh_term_set::in, mh_term_set::out) is det.

:- func intersect(func(T, T) = T, mh_predicate_map(T), mh_predicate_map(T))
	= mh_predicate_map(T).
	
:- pred intersect(func(T, T) = T, mh_predicate_map(T), mh_predicate_map(T),
	mh_predicate_map(T)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.
:- mode intersect(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func set_intersect(mh_term_set, mh_term_set) = mh_term_set.
:- pred set_intersect(mh_term_set::in, mh_term_set::in, mh_term_set::out) 
	is det.

:- func difference(mh_predicate_map(T), mh_predicate_map(_)) =
	mh_predicate_map(T).

:- pred difference(mh_predicate_map(T)::in, mh_predicate_map(_)::in, 
	mh_predicate_map(T)::out) is det.


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
:- import_module hashmap.
:- import_module mh_tuple_map.

%-----------------------------------------------------------------------------%
% Term maps

:- type predicate_tuple == array(predicate_term). 

:- type mh_predicate_map(T)
	--->	empty_predicate_map
	;		predicate_map(
				success :: maybe(T),
				failure :: maybe(T),
				disjunction :: mh_tuple_map(T),
				conjunction :: mh_tuple_map(T),
				negation :: mh_predicate_map(T),
				unification :: mh_term_map(T),
				some [P] mr_predicate :: map(P, T) => predicate(P) 
			).

:- type symbol_map(T) == hashmap(mh_symbol, T).

