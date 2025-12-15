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

:- import_module map.
:- import_module unit.
:- import_module list.
:- import_module assoc_list.

:- import_module mh_term.


%-----------------------------------------------------------------------------%
% Term maps

:- type mh_term_map(T).

:- type mh_term_set == mh_term_map(unit).

:- func init = mh_term_map(T).
:- pred init(mh_term_map(_)::out) is det.

:- func singleton(mh_term, T) = mh_term_map(T).
:- func singleton(mh_term) = mh_term_set.

:- pred is_empty(mh_term_map(_)::in) is semidet.

:- func count(mh_term_map(_)) = int.
:- pred count(mh_term_map(_)::in, int::out) is det.

:- pred equal(mh_term_map(T)::in, mh_term_map(T)::in) is semidet.

%-----------------------------------------------------------------------------%
% Conversion

:- func to_map(mh_term_map(T)) = map(mh_term, T).

:- func from_map(map(mh_term, T)) = mh_term_map(T).

%-----------------------------------------------------------------------------%
% Search

	% Succeeds if the map contains the given key
:- pred contains(mh_term_map(_)::in, mh_term::in) is semidet.

	% Fails if the key is not found
:- pred search(mh_term_map(T)::in, mh_term::in, T::out) is semidet.
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

:- pred set(mh_term::in, T::in, mh_term_map(T)::in, mh_term_map(T)::out)
	is det.
	
:- pred set(mh_term::in, mh_term_set::in, mh_term_set::out) is det.
	
:- pred set_from_corresponding_lists(list(mh_term)::in, list(T)::in,
	mh_term_map(T)::in, mh_term_map(T)::out) is det.
	
:- pred set_from_assoc_list(assoc_list(mh_term, T)::in,
	mh_term_map(T)::in, mh_term_map(T)::out) is det.
	
:- pred set_from_list(list(mh_term)::in, mh_term_set::in, 
	mh_term_set::out) is det.

:- pred update(mh_term::in, T::in, mh_term_map(T)::in, 
	mh_term_map(T)::out) is semidet.
	
:- pred det_update(mh_term::in, T::in, mh_term_map(T)::in, 
	mh_term_map(T)::out) is det.
	
%-----------------------------------------------------------------------------%
% Removal

:- pred remove(mh_term::in, T::out, mh_term_map(T)::in, mh_term_map(T)::out)
	is semidet.
	
:- pred remove(mh_term::in, mh_term_set::in, mh_term_set::out) is semidet.
	
:- pred det_remove(mh_term::in, T::out, mh_term_map(T)::in, 
	mh_term_map(T)::out) is det.
	
:- pred det_remove(mh_term::in, mh_term_set::in, mh_term_set::out) is det.
	
:- pred delete(mh_term::in,  mh_term_map(T)::in, mh_term_map(T)::out) is det.

:- pred delete_list(list(mh_term)::in, mh_term_map(T)::in, 
	mh_term_map(T)::out) is det.
	
%-----------------------------------------------------------------------------%
% Set operations


:- func union(func(T, T) = T, mh_term_map(T), mh_term_map(T)) = mh_term_map(T).

:- pred union(func(T, T) = T, mh_term_map(T), mh_term_map(T), mh_term_map(T)).
:- mode union(in(func(in, in) = out is det), in, in, out) is det.

:- func set_union(mh_term_set, mh_term_set) = mh_term_set.
:- pred set_union(mh_term_set::in, mh_term_set::in, mh_term_set::out) is det.

:- func intersect(func(T1, T2) = T3, mh_term_map(T1), mh_term_map(T2))
	= mh_term_map(T3).
	
:- pred intersect(func(T1, T2) = T3, mh_term_map(T1), mh_term_map(T2),
	mh_term_map(T3)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.

:- func set_intersect(mh_term_set, mh_term_set) = mh_term_set.
:- pred set_intersect(mh_term_set::in, mh_term_set::in, mh_term_set::out) 
	is det.

:- func difference(mh_term_map(T), mh_term_map(_)) =
	mh_term_map(T).

:- pred difference(mh_term_map(T)::in, mh_term_map(_)::in, 
	mh_term_map(T)::out) is det.
	
	
%-----------------------------------------------------------------------------%
% Higher Order

:- func fold(func(mh_term, T, A) = A, mh_term_map(T), A) = A.
:- mode fold(in(func(in, in, in) = out is det), in, in) = out is det.
:- mode fold(in(func(in, in, in) = out is semidet), in, in) = out 
	is semidet.

:- func det_fold(func(mh_term, T, A) = A, mh_term_map(T), A)
	= A.
:- mode det_fold(in(func(in, in, in) = out is det), in, in) = out is det.

:- func semidet_fold(func(mh_term, T, A) = A, mh_term_map(T),
	A) = A.
:- mode semidet_fold(in(func(in, in, in) = out is semidet), in, in) = out 
	is semidet.

:- pred fold(func(mh_term, T, A) = A, mh_term_map(T), A, A).
:- mode fold(in(func(in, in, in) = out is det), in, in, out) is det.
:- mode fold(in(func(in, in, in) = out is semidet), in, in, out) is semidet.

:- pred fold2(pred(mh_term, T, A, A, B, B), mh_term_map(T), A, A, B, B).
:- mode fold2(in(pred(in, in, in, out, in, out) is semidet), in, in, out, 
	in,	out) is semidet.
:- mode fold2(in(pred(in, in, in, out, in, out) is det), in, in, out, in, out)
	is det.

:- func map(func(mh_term, T) = U, mh_term_map(T)) = mh_term_map(U).
 
:- pred map(func(mh_term, T) = U, mh_term_map(T), mh_term_map(U)).
:- mode map(in(func(in, in) = out is det), in, out) is det.




%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module pair.
:- import_module require.

:- import_module map_util.

:- import_module mh_symbol_map.
:- import_module mh_var_map.
:- import_module mh_value_map.
:- import_module mh_tuple_map.
:- import_module mh_relation_map.
:- import_module mh_tuple.
:- import_module mh_symbol.
:- import_module mh_var_id.
:- import_module mh_value.
:- import_module mh_relation.

%-----------------------------------------------------------------------------%
% Term map


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

count(Tuple, count(Tuple)).

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
% Conversion

to_map(TM) = fold(map_insert, TM, init).

:- func map_insert(mh_term, T, map(mh_term, T)) = map(mh_term, T).
map_insert(K, V, !.M) = !:M :- det_insert(K, V, !M).


from_map(M) = fold(tm_map_insert, M, init).

:- func tm_map_insert(mh_term, T, mh_term_map(T)) = mh_term_map(T).
tm_map_insert(K, V, !.Map) = !:Map :- det_insert(K, V, !Map).
	
%-----------------------------------------------------------------------------%
% Search
	
contains(term_map(Map, _, _, _, _), atom(Symbol)) :- contains(Map, Symbol).

contains(term_map(_, Map, _, _, _), var(ID)) :- contains_id(Map, ID).

contains(term_map(_, _, Map, _, _), value(Value) ) :- 
	contains(Map, Value).
	
contains(term_map(_, _, _, Map, _), cons(Functor, Args)) :- 
	contains(Map, tuple_cons(Functor, Args)).
	
contains(term_map(_, _, _, _, Map), relation(Rel)) :- contains(Map, Rel).

search(term_map(Map, _, _, _, _), atom(Symbol), T) :- search(Map, Symbol, T).

search(term_map(_, Map, _, _, _), var(ID), T) :- id_search(Map, ID, T).

search(term_map(_, _, Map, _, _), value(Value), T) :- 
	search(Map, Value, T).
	
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
	( if !.Map = empty_term_map then
		!:Map = prototype_map
	else true ).
*/	
	
:- pred deconstruct_term_map(mh_term_map(T)::in, mh_symbol_map(T)::out,
	mh_var_map(T)::out, mh_value_map(T)::out, mh_tuple_map(T)::out,
	mh_relation_map(T)::out) is det.

deconstruct_term_map(empty_term_map, mh_symbol_map.init, mh_var_map.init,
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

insert(value(Value), T, !Map) :-
	deconstruct_term_map(!.Map, Atoms, Vars, Vals0, Cons, Rels),
	mh_value_map.insert(Value, T, Vals0, Vals),
	!:Map = term_map(Atoms, Vars, Vals, Cons, Rels).

insert(cons(Functor, Args), T, !Map) :-
	deconstruct_term_map(!.Map, Atoms, Vars, Vals, Cons0, Rels),
	mh_tuple_map.insert(tuple_cons(Functor, Args), T, Cons0, Cons),
	!:Map = term_map(Atoms, Vars, Vals, Cons, Rels).
	
insert(relation(Rel), T, !Map) :-
	deconstruct_term_map(!.Map, Atoms, Vars, Vals, Cons, Rels0),
	mh_relation_map.insert(Rel, T, Rels0, Rels),
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
det_insert_from_list([ Term | List], !Set) :-
	det_insert(Term, !Set),
	det_insert_from_list(List, !Set).
	
set(atom(Symbol), T, !Map) :-
	deconstruct_term_map(!.Map, Atoms0, Vars, Vals, Cons, Rels),
	mh_symbol_map.set(Symbol, T, Atoms0, Atoms),
	!:Map = term_map(Atoms, Vars, Vals, Cons, Rels).
	
set(var(ID), T, !Map) :-
	deconstruct_term_map(!.Map, Atoms, Vars0, Vals, Cons, Rels),
	mh_var_map.id_set(ID, T, Vars0, Vars),
	!:Map = term_map(Atoms, Vars, Vals, Cons, Rels).

set(value(Value), T, !Map) :-
	deconstruct_term_map(!.Map, Atoms, Vars, Vals0, Cons, Rels),
	mh_value_map.set(Value, T, Vals0, Vals),
	!:Map = term_map(Atoms, Vars, Vals, Cons, Rels).

set(cons(Functor, Args), T, !Map) :-
	deconstruct_term_map(!.Map, Atoms, Vars, Vals, Cons0, Rels),
	mh_tuple_map.set(tuple_cons(Functor, Args), T, Cons0, Cons),
	!:Map = term_map(Atoms, Vars, Vals, Cons, Rels).
	
set(relation(Rel), T, !Map) :-
	deconstruct_term_map(!.Map, Atoms, Vars, Vals, Cons, Rels0),
	mh_relation_map.set(Rel, T, Rels0, Rels),
	!:Map = term_map(Atoms, Vars, Vals, Cons, Rels).
	
set(Term, !Map) :- set(Term, unit, !Map).
	
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
set_from_list([Term | List], !Set) :-
	set(Term, !Set),
	set_from_list(List, !Set).
	
update(atom(Symbol), T, !Map) :-
	!.Map = term_map(Atoms0, Vars, Vals, Cons, Rels),
	mh_symbol_map.update(Symbol, T, Atoms0, Atoms),
	!:Map = term_map(Atoms, Vars, Vals, Cons, Rels).
	
update(var(ID), T, !Map) :-
	!.Map = term_map(Atoms, Vars0, Vals, Cons, Rels),
	mh_var_map.id_update(ID, T, Vars0, Vars),
	!:Map = term_map(Atoms, Vars, Vals, Cons, Rels).

update(value(Value), T, !Map) :-
	!.Map = term_map(Atoms, Vars, Vals0, Cons, Rels),
	mh_value_map.update(Value, T, Vals0, Vals),
	!:Map = term_map(Atoms, Vars, Vals, Cons, Rels).

update(cons(Functor, Args), T, !Map) :-
	!.Map = term_map(Atoms, Vars, Vals, Cons0, Rels),
	mh_tuple_map.update(tuple_cons(Functor, Args), T, Cons0, Cons),
	!:Map = term_map(Atoms, Vars, Vals, Cons, Rels).
	
update(relation(Rel), T, !Map) :-
	!.Map = term_map(Atoms, Vars, Vals, Cons, Rels0),
	mh_relation_map.update(Rel, T, Rels0, Rels),
	!:Map = term_map(Atoms, Vars, Vals, Cons, Rels).
	
det_update(Term, T, !Map) :-
	( if update(Term, T, !.Map, NewMap) then
        !:Map = NewMap
    else
        report_lookup_error("mh_term_map.det_update: term not present in map", 
		Term, T)
    ).
	
%-----------------------------------------------------------------------------%
% Removal

:- func construct_term_map(mh_symbol_map(T), mh_var_map(T),	mh_value_map(T),
	mh_tuple_map(T), mh_relation_map(T)) = mh_term_map(T).
	
construct_term_map(Atoms, Vars, Vals, Cons, Rels) =
	(if 
		is_empty(Atoms), is_empty(Vars), is_empty(Vals), is_empty(Cons),
		is_empty(Rels)
	then
		empty_term_map
	else
		term_map(Atoms, Vars, Vals, Cons, Rels)
	).

remove(atom(Symbol), T, !Map) :-
	!.Map = term_map(Atoms0, Vars, Vals, Cons, Rels),
	mh_symbol_map.remove(Symbol, T, Atoms0, Atoms),
	!:Map = construct_term_map(Atoms, Vars, Vals, Cons, Rels).
	
remove(var(ID), T, !Map) :-
	!.Map = term_map(Atoms, Vars0, Vals, Cons, Rels),
	mh_var_map.id_remove(ID, T, Vars0, Vars),
	!:Map = construct_term_map(Atoms, Vars, Vals, Cons, Rels).

remove(value(Value), T, !Map) :-
	!.Map = term_map(Atoms, Vars, Vals0, Cons, Rels),
	mh_value_map.remove(Value, T, Vals0, Vals),
	!:Map = construct_term_map(Atoms, Vars, Vals, Cons, Rels).

remove(cons(Functor, Args), T, !Map) :-
	!.Map = term_map(Atoms, Vars, Vals, Cons0, Rels),
	mh_tuple_map.remove(tuple_cons(Functor, Args), T, Cons0, Cons),
	!:Map = construct_term_map(Atoms, Vars, Vals, Cons, Rels).
	
remove(relation(Rel), T, !Map) :-
	!.Map = term_map(Atoms, Vars, Vals, Cons, Rels0),
	mh_relation_map.remove(Rel, T, Rels0, Rels),
	!:Map = construct_term_map(Atoms, Vars, Vals, Cons, Rels).
	
remove(E, !Map) :- remove(E, _, !Map).
	
det_remove(Term, T, !Map) :-	
	(if remove(Term, FoundT, !Map)
	then !:Map = !.Map, T = FoundT
	else report_lookup_error(
		"mh_term_map.det_remove: term not present in map", Term, 
		!.Map)
	).
	
det_remove(E, !Map) :- det_remove(E, _, !Map).

delete(_, empty_term_map, empty_term_map).
	
delete(atom(Symbol), !Map) :-
	!.Map = term_map(Atoms0, Vars, Vals, Cons, Rels),
	mh_symbol_map.delete(Symbol, Atoms0, Atoms),
	!:Map = construct_term_map(Atoms, Vars, Vals, Cons, Rels).
	
delete(var(ID), !Map) :-
	!.Map = term_map(Atoms, Vars0, Vals, Cons, Rels),
	mh_var_map.id_delete(ID, Vars0, Vars),
	!:Map = construct_term_map(Atoms, Vars, Vals, Cons, Rels).

delete(value(Value), !Map) :-
	!.Map = term_map(Atoms, Vars, Vals0, Cons, Rels),
	mh_value_map.delete(Value, Vals0, Vals),
	!:Map = construct_term_map(Atoms, Vars, Vals, Cons, Rels).

delete(cons(Functor, Args), !Map) :-
	!.Map = term_map(Atoms, Vars, Vals, Cons0, Rels),
	mh_tuple_map.delete(tuple_cons(Functor, Args), Cons0, Cons),
	!:Map = construct_term_map(Atoms, Vars, Vals, Cons, Rels).
	
delete(relation(Rel), !Map) :-
	!.Map = term_map(Atoms, Vars, Vals, Cons, Rels0),
	mh_relation_map.delete(Rel, Rels0, Rels),
	!:Map = construct_term_map(Atoms, Vars, Vals, Cons, Rels).
	
delete_list([], !Map).
delete_list([Term | Terms], !Map) :- 
	delete(Term, !Map),
	delete_list(Terms, !Map).

	
%-----------------------------------------------------------------------------%
% Set operations

union(_, empty_term_map, empty_term_map) = empty_term_map.
union(_, Map@term_map(_, _, _, _, _), empty_term_map) = Map.
union(_, empty_term_map, Map@term_map(_, _, _, _, _)) = Map.

union(
	F,
	term_map(Symbols1, Vars1, Vals1, Cons1, Rels1),
	term_map(Symbols2, Vars2, Vals2, Cons2, Rels2)
) = term_map(
	mh_symbol_map.union(F, Symbols1, Symbols2),
	mh_var_map.union(F, Vars1, Vars2),
	mh_value_map.union(F, Vals1, Vals2),
	mh_tuple_map.union(F, Cons1, Cons2),
	mh_relation_map.union(F, Rels1, Rels2)
).

union(F, M1, M2, union(F, M1, M2)).

set_union(Set1, Set2) = union(merge_units, Set1, Set2).

:- func merge_units(unit, unit) = unit.

merge_units(_, _) = unit.

set_union(Set1, Set2, set_union(Set1, Set2)).



intersect(_, empty_term_map, empty_term_map) = empty_term_map.
intersect(_, term_map(_, _, _, _, _), empty_term_map) = empty_term_map.
intersect(_, empty_term_map, term_map(_, _, _, _, _)) = empty_term_map.

intersect(
	F,
	term_map(Symbols1, Vars1, Vals1, Cons1, Rels1),
	term_map(Symbols2, Vars2, Vals2, Cons2, Rels2)
) = construct_term_map(
	mh_symbol_map.intersect(F, Symbols1, Symbols2),
	mh_var_map.intersect(F, Vars1, Vars2),
	mh_value_map.intersect(F, Vals1, Vals2),
	mh_tuple_map.intersect(F, Cons1, Cons2),
	mh_relation_map.intersect(F, Rels1, Rels2)
).

intersect(F, M1, M2, intersect(F, M1, M2)).

set_intersect(Set1, Set2) = intersect(merge_units, Set1, Set2).
set_intersect(Set1, Set2, set_intersect(Set1, Set2)).

difference(empty_term_map, empty_term_map) = empty_term_map.
difference(Map@term_map(_, _, _, _, _), empty_term_map) = Map.
difference(empty_term_map, term_map(_, _, _, _, _)) = empty_term_map.

difference(
	term_map(Symbols1, Vars1, Vals1, Cons1, Rels1),
	term_map(Symbols2, Vars2, Vals2, Cons2, Rels2)
) = construct_term_map(
	mh_symbol_map.difference(Symbols1, Symbols2),
	mh_var_map.difference(Vars1, Vars2),
	mh_value_map.difference(Vals1, Vals2),
	mh_tuple_map.difference(Cons1, Cons2),
	mh_relation_map.difference(Rels1, Rels2)
).

difference(M1, M2, difference(M1, M2)).

%-----------------------------------------------------------------------------%
% Higher Order

fold(_, empty_term_map, A) = A.

fold(F, term_map(Symbols, Vars, Vals, Cons, Rels), !.A) = !:A :-
	!:A = mh_symbol_map.foldl(symbol_fold(F), Symbols, !.A),
	mh_var_map.fold_id(var_id_fold(F), Vars, !A),
	!:A = mh_value_map.fold(value_fold(F), Vals, !.A),
	!:A = mh_tuple_map.fold(cons_fold(F), Cons, !.A),
	!:A = mh_relation_map.fold(relation_fold(F), Rels, !.A).

:- func symbol_fold(func(mh_term, T, A) = A, mh_symbol, T, A) = A.
:- mode symbol_fold(in(func(in, in, in) = out is det), in, in, in) = out is det.
:- mode symbol_fold(in(func(in, in, in) = out is semidet), in, in, in) = out 
	is semidet.

symbol_fold(F, S, T, A) = F(atom(S), T, A).

:- pred var_id_fold(func(mh_term, T, A) = A, var_id, T, A, A).
:- mode var_id_fold(in(func(in, in, in) = out is det), in, in, in, out) is det.
:- mode var_id_fold(in(func(in, in, in) = out is semidet), in, in, in, out) 
	is semidet.

var_id_fold(F, ID, T, A, F(var(ID), T, A)). 

:- func value_fold(func(mh_term, T, A) = A, mh_value, T, A) = A.
:- mode value_fold(in(func(in, in, in) = out is det), in, in, in) = out is det.
:- mode value_fold(in(func(in, in, in) = out is semidet), in, in, in) = out 
	is semidet.
	
value_fold(F, V, T, A) = F(value(V), T, A).

:- func cons_fold(func(mh_term, T, A) = A, mh_tuple, T, A) = A.
:- mode cons_fold(in(func(in, in, in) = out is det), in, in, in) = out is det.
:- mode cons_fold(in(func(in, in, in) = out is semidet), in, in, in) = out 
	is semidet.
	
cons_fold(F, Tuple, T, A) = 
	(if Cons = cons(tuple_car(Tuple), tuple_cdr(Tuple)) then
		F(Cons, T, A)
	else
		%Introspection for more informative error message
		(if tuple_size(Tuple) = 0 then
			unexpected($module, $pred, "Empty cons tuple in term map")
		else 
			unexpected($module, $pred, 
				"Cons tuple with only one element in term map")
		)
		% If the tuple has more than one element, then the above test will
		% never fail.
	).
	
:- func relation_fold(func(mh_term, T, A) = A, mh_relation, T, A) = A.
:- mode relation_fold(in(func(in, in, in) = out is det), in, in, in) = out
	is det.
:- mode relation_fold(in(func(in, in, in) = out is semidet), in, in, in) = out 
	is semidet.
	
relation_fold(F, R, T, A) = F(relation(R), T, A).

det_fold(F, M, A) = fold(F, M, A).
semidet_fold(F, M, A) = fold(F, M, A).

fold(F, M, A, fold(F, M, A)).

%---------------------%

fold2(_, empty_term_map, A, A, B, B).

fold2(P, term_map(Symbols, Vars, Vals, Cons, Rels), !A, !B) :-
	mh_symbol_map.foldl2(symbol_fold2(P), Symbols, !A, !B),
	mh_var_map.fold2_id(var_id_fold2(P), Vars, !A, !B),
	mh_value_map.fold2(value_fold2(P), Vals, !A, !B),
	mh_tuple_map.fold2(cons_fold2(P), Cons, !A, !B),
	mh_relation_map.fold2(relation_fold2(P), Rels, !A, !B).

:- pred symbol_fold2(pred(mh_term, T, A, A, B, B), mh_symbol, T, A, A, B, B).
:- mode symbol_fold2(in(pred(in, in, in, out, in, out) is det), in, in,
	in, out, in, out) is det.
:- mode symbol_fold2(in(pred(in, in, in, out, in, out) is semidet), in, in,
	in,	out, in, out) is semidet.

symbol_fold2(P, S, T, !A, !B) :-  P(atom(S), T, !A, !B).

:- pred var_id_fold2(pred(mh_term, T, A, A, B, B), var_id, T, A, A, B, B).
:- mode var_id_fold2(in(pred(in, in, in, out, in, out) is det), in, in, 
	in, out, in, out) is det.
:- mode var_id_fold2(in(pred(in, in, in, out, in, out) is semidet), in, in, 
	in, out, in, out) is semidet.

var_id_fold2(P, ID, T, !A, !B) :- P(var(ID), T, !A, !B). 

:- pred value_fold2(pred(mh_term, T, A, A, B, B), mh_value, T, A, A, B, B).
:- mode value_fold2(in(pred(in, in, in, out, in, out) is det), in, in,
	in, out, in, out) is det.
:- mode value_fold2(in(pred(in, in, in, out, in, out) is semidet), in, in,
	in, out, in, out) is semidet.
	
value_fold2(P, V, T, !A, !B) :-  P(value(V), T, !A, !B).

:- pred cons_fold2(pred(mh_term, T, A, A, B, B), mh_tuple, T, A, A, B, B).
:- mode cons_fold2(in(pred(in, in, in, out, in, out) is det), in, in,
	in, out, in, out) is det.
:- mode cons_fold2(in(pred(in, in, in, out, in, out) is semidet), in, in,
	in, out, in,	out) is semidet.
	
cons_fold2(P, Tuple, T, !A, !B) :-  
	(if Cons = cons(tuple_car(Tuple), tuple_cdr(Tuple)) then
		P(Cons, T, !A, !B)
	else
		%Introspection for more informative error message
		(if tuple_size(Tuple) = 0 then
			unexpected($module, $pred, "Empty cons tuple in term map")
		else 
			unexpected($module, $pred, 
				"Cons tuple with only one element in term map")
		)
		% If the tuple has more than one element, then the above test will
		% never fail.
	).
	
:- pred relation_fold2(pred(mh_term, T, A, A, B, B), mh_relation, T, A, A, 
	B, B).
:- mode relation_fold2(in(pred(in, in, in, out, in, out) is det), in, in,
	in, out, in, out) is det.
:- mode relation_fold2(in(pred(in, in, in, out, in, out) is semidet), in, in,
	in, out, in, out) is semidet.
	
relation_fold2(P, R, T, !A, !B) :- P(relation(R), T, !A, !B).

%---------------------%

map(_, empty_term_map) = empty_term_map.

map(F, term_map(Symbols, Vars0, Vals, Cons, Rels)) =
	term_map(
		mh_symbol_map.map_values(map_symbol(F), Symbols),
		Vars1,
		mh_value_map.map(map_value(F), Vals),
		mh_tuple_map.map(map_cons(F), Cons),
		mh_relation_map.map(map_relation(F), Rels)
	) :- mh_var_map.map_id(map_var_id(F), Vars0, Vars1).

:- func map_symbol(func(mh_term, T) = U, mh_symbol, T) = U.
map_symbol(F, S, T) = F(atom(S), T).

:- pred map_var_id(func(mh_term, T) = U, var_id, T, U).
:- mode map_var_id(in, in, in, out) is det.

map_var_id(F, ID, T, F(var(ID), T)). 

:- func map_value(func(mh_term, T) = U, mh_value, T) = U.
map_value(F, V, T) = F(value(V), T).

:- func map_cons(func(mh_term, T) = U, mh_tuple, T) = U.
map_cons(F, Tuple, T) = 
	(if F(cons(tuple_car(Tuple), tuple_cdr(Tuple)), T) = Result then
		Result
	else
		%Introspection for more informative error message
		(if tuple_size(Tuple) = 0 then
			unexpected($module, $pred, "Empty cons tuple in term map")
		else 
			unexpected($module, $pred, 
				"Cons tuple with only one element in term map")
		)
		% If the tuple has more than one element, then the above test will
		% never fail.
	).
	
:- func map_relation(func(mh_term, T) = U, mh_relation, T) = U.
map_relation(F, R, T) = F(relation(R), T).

map(F, M, map(F, M)).
