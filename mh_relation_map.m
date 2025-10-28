%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_relation_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_relation_map.

:- interface.

:- import_module unit.
:- import_module list.
:- import_module assoc_list.

:- import_module mh_relation.

%-----------------------------------------------------------------------------%
% Term maps

:- type mh_relation_map(T).
:- type mh_relation_set == mh_relation_map(unit).

:- func init = mh_relation_map(_). 
:- pred init(mh_relation_map(_)::out) is det.

:- func singleton(mh_relation, T) = mh_relation_map(T).
:- func singleton(mh_relation) = mh_relation_set.

:- pred is_empty(mh_relation_map(_)::in) is semidet.

:- func count(mh_relation_map(_)) = int.
:- pred count(mh_relation_map(_)::in, int::out) is det.

:- pred equal(mh_relation_map(T)::in, mh_relation_map(T)::in) is semidet.



%-----------------------------------------------------------------------------%
% Search

	% Succeeds if the map contains the given key
:- pred contains(mh_relation_map(T)::in, mh_relation::in) is semidet.

	% Fails if the key is not found
:- pred search(mh_relation_map(T)::in, mh_relation::in, 
T::out) is semidet.
:- func search(mh_relation_map(T), mh_relation) = T is semidet.

	% Throws an exception if the key is not found
:- pred lookup(mh_relation_map(T)::in, mh_relation::in, T::out) is det.
:- func lookup(mh_relation_map(T), mh_relation) = T is det.

%-----------------------------------------------------------------------------%
% Insertion

:- pred insert(mh_relation::in, T::in, mh_relation_map(T)::in, 
	mh_relation_map(T)::out) is semidet.
	
:- pred insert(mh_relation::in, mh_relation_set::in, mh_relation_set::out) 
	is semidet.
	
:- pred det_insert(mh_relation::in, T::in, mh_relation_map(T)::in, 
	mh_relation_map(T)::out) is det.
	
:- pred det_insert(mh_relation::in, mh_relation_set::in, mh_relation_set::out)
	is det.
	
:- pred det_insert_from_corresponding_lists(list(mh_relation)::in, list(T)::in,
	mh_relation_map(T)::in, mh_relation_map(T)::out) is det.
	
:- pred det_insert_from_assoc_list(assoc_list(mh_relation, T)::in,
	mh_relation_map(T)::in, mh_relation_map(T)::out) is det.
	
:- pred det_insert_from_list(list(mh_relation)::in, mh_relation_set::in, 
	mh_relation_set::out) is det.

:- pred set(mh_relation::in, T::in, mh_relation_map::in, mh_relation_map::out)
	is det.
	
:- pred set(mh_relation::in, mh_relation_set::in, mh_relation_set::out) is det.
	
:- pred set_from_corresponding_lists(list(mh_relation)::in, list(T)::in,
	mh_relation_map(T)::in, mh_relation_map(T)::out) is det.
	
:- pred set_from_assoc_list(assoc_list(mh_relation, T)::in,
	mh_relation_map(T)::in, mh_relation_map(T)::out) is det.
	
:- pred set_from_list(list(mh_relation)::in, mh_relation_set::in, 
	mh_relation_set::out) is det.

:- pred update(mh_relation::in, T::in, mh_relation_map(T)::in, 
	mh_relation_map::out) is semidet.
	
:- pred det_update(mh_relation::in, T::in, mh_relation_map(T)::in, 
	mh_relation_map::out) is det.
	
%-----------------------------------------------------------------------------%
% Removal

:- pred remove(mh_relation::in, T::out, mh_relation_map(T)::in, 
	mh_relation_map::out) is semidet.
	
:- pred remove(mh_relation::in, mh_relation_set::in, mh_relation_set::out) is semidet.
	
:- pred det_remove(mh_relation::in, T::out, mh_relation_map(T)::in, 
	mh_relation_map::out) is det.
	
:- pred det_remove(mh_relation::in,  mh_relation_set::in, mh_relation_set::out) is det.
	
:- pred delete(mh_relation::in,  mh_relation_map(T)::in, 
	mh_relation_map::out) is det.

:- pred delete_list(list(mh_relation)::in, mh_relation_map(T)::in, 
	mh_relation_map::out) is det.
	
%-----------------------------------------------------------------------------%
% Set operations


:- func union(func(T, T) = T, mh_relation_map(T), mh_relation_map(T)) =
	mh_relation_map(T).

:- pred union(func(T, T) = T, mh_relation_map(T), mh_relation_map(T), mh_relation_map(T)).
:- mode union(in(func(in, in) = out is det), in, in, out) is det.
:- mode union(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func set_union(mh_relation_set, mh_relation_set) = mh_relation_set.
:- pred set_union(mh_relation_set::in, mh_relation_set::in, mh_relation_set::out) is det.

:- func intersect(func(T, T) = T, mh_relation_map(T), mh_relation_map(T))
	= mh_relation_map(T).
	
:- pred intersect(func(T, T) = T, mh_relation_map(T), mh_relation_map(T),
	mh_relation_map(T)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.
:- mode intersect(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func set_intersect(mh_relation_set, mh_relation_set) = mh_relation_set.
:- pred set_intersect(mh_relation_set::in, mh_relation_set::in, mh_relation_set::out) 
	is det.

:- func difference(mh_relation_map(T), mh_relation_map(_)) =
	mh_relation_map(T).

:- pred difference(mh_relation_map(T)::in, mh_relation_map(_)::in, 
	mh_relation_map(T)::out) is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module set.
:- import_module maybe.
:- import_module map.
:- import_module pair.

:- import_module mh_scope_map.
:- import_module mh_scope.
:- import_module mh_tuple_map.
:- import_module mh_term_map.
:- import_module mh_substutition.


%-----------------------------------------------------------------------------%
% Relation Map

:- type mh_relation_map(T)
	--->	empty_relation_map
	;		relation_map(maybe(T), mh_scope_map(relation_tree(T)).
	
:- type relation_pair(T) == pair(mh_relation, T).
				

:- type relation_tree(T)
	--->	relation_tree(
				tree_conjunction_map :: mh_tuple_map(relation_pair(T)),
				tree_disjunction_map :: mh_tuple_map(relation_pair(T)),
				tree_negation_map :: mh_term_map(relation_pair(T)),
				tree_lambda_equivalence_map :: mh_tuple_map(relation_pair(T)),
				tree_lambda_application_map :: mh_tuple_map(relation_pair(T)),
				tree_lambda_unification_map :: mh_tuple_map(relation_pair(T)),
				tree_lazy_map :: mh_term_map(relation_pair(T)),
				tree_proposition_map ::	mh_proposition_map(relation_pair(T)),
				tree_closure_map :: mh_term_map(map(mh_substituiton,
					relation_pair(T)))
			).
			
:- func init_tree = relation_Tree(_).

init_tree = relation_tree(
		mh_tuple_map.init,
		mh_tuple_map.init,
		mh_term_map.init,
		mh_tuple_map.init,
		mh_tuple_map.init,
		mh_tuple_map.init,
		mh_term_map.init,
		mh_proposition_map.init,
		mh_term_map.init
		
	).

init = empty_relation_map.
init(init).

singleton(Rel, Value) = Map :- det_insert(Rel, Value, init_tree, Map).
singleton(Rel) = singleton(Rel, unit).



