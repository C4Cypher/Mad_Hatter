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

:- import_module unit.
:- import_module map.

:- import_module mh_term.
%:- import_module mh_var_id.


%-----------------------------------------------------------------------------%
% Term maps

:- type mh_term_map(T).

:- type mh_term_set == mh_term_map(unit).

% :- type key_term_func(T) == (func(T) = mh_term).

:- func init = mh_term_map(T).
:- pred init(mh_term_map(_)::out) is det.

:- func singleton(mh_term, T) = mh_term_map(T).
:- func singleton(mh_term) = mh_term_set.

:- pred is_empty(mh_term_map(_)::in) is semidet.

:- func count(mh_term_map(_)) = int.
:- pred count(mh_term_map(_)::in, int::out) is det.

:- pred equal(mh_term_map(T)::in, mh_term_map(T)::in) is semidet.




%-----------------------------------------------------------------------------%
% Search

	% Succeeds if the map contains the given key
:- pred contains(mh_term_map(_)::in, mh_term::in) is semidet.

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
	is semidet. %!
	
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
	mh_term_map(T)::out) is det.
	
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

:- func set_union(mh_term_set, mh_term_set) = mh_term_set.
:- pred set_union(mh_term_set::in, mh_term_set::in, mh_term_set::out) is det.

:- func intersect(func(T, T) = T, mh_term_map(T), mh_term_map(T))
	= mh_term_map(T).
	
:- pred intersect(func(T, T) = T, mh_term_map(T), mh_term_map(T),
	mh_term_map(T)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.
:- mode intersect(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func set_intersect(mh_term_set, mh_term_set) = mh_term_set.
:- pred set_intersect(mh_term_set::in, mh_term_set::in, mh_term_set::out) 
	is det.

:- func difference(mh_term_map(T), mh_term_map(_)) =
	mh_term_map(T).

:- pred difference(mh_term_map(T)::in, mh_term_map(_)::in, 
	mh_term_map(T)::out) is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- import_module mh_symbol_map.
:- import_module mh_var_map.
:- import_module mh_value_map.
:- import_module mh_tuple_map.
:- import_module mh_relation_map.

%-----------------------------------------------------------------------------%
% Term maps


:- type mh_term_map(T)
	--->	empty_term_map
	;		term_map(
				atom :: mh_symbol_map(T),
				var :: mh_var_map(T),
				value :: mh_value_map(T),
				cons :: mh_tuple_map(T), 
				relation :: mh_relation_map(T)
			).


:- func prototype_map = mh_term_map(_).

prototype_map = term_map(
	mh_symbol_map.init,
	mh_var_map.init,
	mh_value_map.init,
	mh_tuple_map.init,
	mh_relation_map.init
).

init = empty_term_map.
init(init).

singleton(Term, Value) = Map :- det_insert(Term, Value, prototype_map, Map).
singleton(Term) = singleton(Term, unit). 

is_empty(empty_term_map).

count(empty_term_map) = 0.

count( term_map( Atoms, Vars, Vals, Cons, Relations) ) = 
	count(Atoms) + count(Vars) + count(Vals) + count(Cons) + count(Relations).

equal(empty_term_map, empty_term_map).

% mh_var_map should be able to unify directly, due to lack of usage of
% mercury's map type or the hashmap type

equal(
	term_map(Atoms1, Vars, Vals1, Cons1, Rels1),
	term_map(Atoms2, Vars, Vals2, Cons2, Rels2)
) :-
	mh_symbol_map.equal(Atoms1, Atoms2),
	mh_value_map.equal(Vals1, Vals2),
	mh_tuple_map.equal(Cons1, Cons2),
	mh_relation_map.equal(Rels1, Rels2).
	
%-----------------------------------------------------------------------------%
% Search
	
contains(term_map(Map, _, _, _, _), atom(Symbol)) :- contains(Map, Symbol).

contains(term_map(_, Map, _, _, _), var(ID)) :- contains_id(Map, ID).

contains(term_map(_, _, Map, _, _), value(mr_value(Univ)) ) :- 
	contains_univ(Map, Univ).
	
contains(term_map(_, _, _, Map, _), cons(Functor, Args)) :- 
	contains(Map, tuple_cons(Functor, Args)).
	
contains(term_map(_, _, _, _, Map), relation(Rel)) :- search(Map, Rel).

search(term_map(Map, _, _, _, _), atom(Symbol), T) :- search(Map, Symbol, T).

search(term_map(_, Map, _, _, _), var(ID), T) :- id_search(Map, ID, T).

search(term_map(_, _, Map, _, _), value(mr_value(Univ)), T) :- 
	search_univ(Map, Univ, T).
	
search(term_map(_, _, _, Map, _), cons(Functor, Args), T) :- 
	search(Map, tuple_cons(Functor, Args), T).
	
search(term_map(_, _, _, _, Map), relation(Rel), T) :- search(Map, Rel, T).

search(Map, Term) = T :- search(Map, Term, T).

lookup(Map, Term, T) :-
    ( if search(Map, Term, T0) then
        T = T0
    else
        report_lookup_error("mh_term_map.lookup: key not found", Term, T)
    ).
	
lookup(Map, Term) = T :- lookup(Map, Term, T).

%-----------------------------------------------------------------------------%
% Insertion

/*
:- pred empty_to_prototype(mh_term_map(T)::in, mh_term_map(T)::out) is det.

empty_to_prototype(!Map) :-
	( if !.Map = empty_map then
		!:Map = prototype_map
	else true ).
*/	
	
:- pred deconstruct_term_map(mh_term_map(T)::in, mh_symbol_map(T)::out,  
	mh_var_map(T)::out, mh_tuple_map(T)::out, mh_relation_map::out) is det.

deconstruct_term_map(empty_map, mh_symbol_map.init, mh_var_map.init,
	mh_value_map.init, mh_tuple_map.init, mh_relation_map.init).

deconstruct_term_map(term_map(Atoms, Vars, Vals, Cons, Rels), Atoms, Vars, 
	Vals, Cons,	Rels).

insert(atom(Symbol), T, !Map) :-
	deconstruct_term_map(!.Map, Atoms0, Vars, Vals, Cons, Rels),
	mh_symbol_map.insert(Symbol, T, Atoms0, Atoms),
	!:Map = term_map(Atoms, Vars, Vals, Cons, Rels).
	
insert(var(ID), T, !Map) :-
	deconstruct_term_map(!.Map, Atoms, Vars0, Vals, Cons, Rels),
	mh_var_map.id_insert(ID, T, Vars0, Vars),
	!:Map = term_map(Atoms, Vars, Vals, Cons, Rels).

insert(mh_value(mr_value(Univ)), T, !Map) :-
	deconstruct_term_map(!.Map, Atoms, Vars, Vals0, Cons, Rels),
	mh_value_map.insert_univ(Univ, T, Vals0, Vals),
	!:Map = term_map(Atoms, Vars, Vals, Cons, Rels).

insert(cons(Functor, Args), T, !Map) :-
	deconstruct_term_map(!.Map, Atoms, Vars, Vals, Cons0, Rels),
	mh_symbol_map.insert(tuple_cons(Functor, Args), T, Cons0, Cons),
	!:Map = term_map(Atoms, Vars, Vals, Cons, Rels).
	
insert(relation(Rel), T, !Map) :-
	deconstruct_term_map(!.Map, Atoms, Vars, Vals, Cons, Rels0),
	mh_relation_map.insert(Symbol, T, Rels0, Rels),
	!:Map = term_map(Atoms, Vars, Vals, Cons, Rels).
	
insert(Term, !Map) :- insert(Term, unit, !Map).
	
det_insert(Term, T, !M) :-
	( if insert(Term, T, !.M, NewMap) then
        !:M = NewMap
    else
        report_lookup_error("mh_term_map.det_insert: term already present", 
		Term, T)
    ).
	
det_insert(Term, !Map) :-
	(if insert(Term, !Map)
	then !:Map = !.Map
	else report_lookup_error(
		"mh_term_map.det_insert: term already present", 
		Term, !.Map)
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
det_insert_from_list([ Tuple | List]) :-
	det_insert(Tuple, !Set),
	det_insert_from_list(List, !Set).