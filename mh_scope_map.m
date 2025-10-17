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


:- import_module mh_scope.

%-----------------------------------------------------------------------------%
% Term maps

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
:- pred search(mh_scope_map(T)::in, mh_scope::in, 
T::out) is semidet.
:- func search(mh_scope_map(T), mh_scope) = T is semidet.

	% Throws an exception if the key is not found
:- pred lookup(mh_scope_map(T)::in, mh_scope::in, T::out) is det.
:- func lookup(mh_scope_map(T), mh_scope) = T is det.

%-----------------------------------------------------------------------------%
% Insertion

:- pred insert(mh_scope::in, T::in, mh_scope_map(T)::in, 
	mh_scope_map(T)::out) is semidet.
	
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
	
:- pred det_remove(mh_scope::in,  mh_scope_set::in, mh_scope_set::out) is det.
	
:- pred delete(mh_scope::in,  mh_scope_map(T)::in, 
	mh_scope_map::out) is det.

:- pred delete_list(list(mh_scope)::in, mh_scope_map(T)::in, 
	mh_scope_map::out) is det.
	
%-----------------------------------------------------------------------------%
% Set operations


:- func union(func(T, T) = T, mh_scope_map(T), mh_scope_map(T)) =
	mh_scope_map(T).

:- pred union(func(T, T) = T, mh_scope_map(T), mh_scope_map(T), 
	mh_scope_map(T)).
	
:- mode union(in(func(in, in) = out is det), in, in, out) is det.
:- mode union(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func set_union(mh_scope_set, mh_scope_set) = mh_scope_set.
:- pred set_union(mh_scope_set::in, mh_scope_set::in, mh_scope_set::out)
	is det.

:- func intersect(func(T, T) = T, mh_scope_map(T), mh_scope_map(T))
	= mh_scope_map(T).
	
:- pred intersect(func(T, T) = T, mh_scope_map(T), mh_scope_map(T),
	mh_scope_map(T)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.
:- mode intersect(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func set_intersect(mh_scope_set, mh_scope_set) = mh_scope_set.
:- pred set_intersect(mh_scope_set::in, mh_scope_set::in, mh_scope_set::out) 
	is det.

:- func difference(mh_scope_map(T), mh_scope_map(_)) =
	mh_scope_map(T).

:- pred difference(mh_scope_map(T)::in, mh_scope_map(_)::in, 
	mh_scope_map(T)::out) is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.

:- import_module mh_context.
:- import_module mh_var_set.


%-----------------------------------------------------------------------------%
% Scope Map

:- type set_map(T) == map(mh_var_set, T).

:- type context_map(T) == map(mh_context, set_map(T)).

:- type mh_scope_map(T)
	--->	empty_scope_map
	;		scope_map(context_map(T)).


