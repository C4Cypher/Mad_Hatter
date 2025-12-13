%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_tuple_pattern_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_tuple_pattern_map.

:- interface.

:- use_module map.
:- import_module array.
:- import_module list.
:- import_module assoc_list.

:- import_module mh_term_map.
:- import_module mh_term.
:- import_module mh_tuple.

%-----------------------------------------------------------------------------%
% Tuple Pattern map

:- type tuple_pattern_map(T) == map.map(int, pattern_array(T)).

:- type pattern_array(T) == array(element_map(T)).
:- type element_map(T) == mh_term_map(exact_map(T)).
:- type exact_map(T) == map.map(array(mh_term), T).


:- func init = (tuple_pattern_map(T)::out) is det.
:- pred init(tuple_pattern_map(_)::out) is det.

:- func singleton(mh_tuple, T) = tuple_pattern_map(T).
:- func array_singleton(mh_tuple, array(mh_term), T) = 
	tuple_pattern_map(T).

:- pred is_empty(tuple_pattern_map(_)::in) is semidet.

:- func from_exact_map(exact_map(T)) = tuple_pattern_map(T).


%-----------------------------------------------------------------------------%
% Insertion

% Throw an exception if tuple already exists in map.

:- pred insert(mh_tuple::in, T::in, tuple_pattern_map(T)::in, 
	tuple_pattern_map(T)::out) is det.

:- pred insert_from_corresponding_lists(list(mh_tuple)::in, list(T)::in,
	tuple_pattern_map(T)::in, tuple_pattern_map(T)::out) is det.
	
:- pred insert_from_assoc_list(assoc_list(mh_tuple, T)::in,
	tuple_pattern_map(T)::in, tuple_pattern_map(T)::out) is det.
	
:- pred array_insert(array(mh_term)::in, T::in, 
	tuple_pattern_map(T)::in, tuple_pattern_map(T)::out) is det.

:- pred array_insert_from_corresponding_lists(list(mh_tuple)::in, 
	list(array(mh_term))::in, list(array(mh_term))::in, list(T)::in, 
	tuple_pattern_map(T)::in, tuple_pattern_map(T)::out) is det.

:- pred set(mh_tuple::in, T::in, tuple_pattern_map(T)::in, 
	tuple_pattern_map(T)::out) is det.
	
:- pred set_from_corresponding_lists(list(mh_tuple)::in, list(T)::in,
	tuple_pattern_map(T)::in, tuple_pattern_map(T)::out) is det.
	
:- pred set_from_assoc_list(assoc_list(mh_tuple, T)::in,
	tuple_pattern_map(T)::in, tuple_pattern_map(T)::out) is det.
	
:- pred array_set(array(mh_term)::in, T::in, 
	tuple_pattern_map(T)::in, tuple_pattern_map(T)::out) is det.
	
:- pred array_set_from_corresponding_lists(list(mh_tuple)::in, 
	list(array(mh_term))::in, list(array(mh_term))::in, list(T)::in, 
	tuple_pattern_map(T)::in, tuple_pattern_map(T)::out) is det.	
	
%-----------------------------------------------------------------------------%
% Removal

	
:- pred delete(mh_tuple::in,  tuple_pattern_map(T)::in, 
	tuple_pattern_map(T)::out) is det.

:- pred delete_list(list(mh_tuple)::in, tuple_pattern_map(T)::in, 
	tuple_pattern_map(T)::out) is det.
	
:- pred array_delete(array(mh_term)::in,  tuple_pattern_map(T)::in, 
	tuple_pattern_map(T)::out) is det.

:- pred array_delete_list(list(array(mh_term))::in, tuple_pattern_map(T)::in, 
	tuple_pattern_map(T)::out) is det.
	

%-----------------------------------------------------------------------------%
% Pattern Map Operations

	% Retreive the array of term maps associated with the tuple's arity
	% If none is found, create one.

:- func get_pattern_array(tuple_pattern_map(T), int) = pattern_array(T).

:- pred get_pattern_array(tuple_pattern_map(T)::in, int::in, 
	pattern_array(T)::out) is det.
	
:- func find_pattern_array(tuple_pattern_map(T), int) = pattern_array(T)
	is semidet.
	
:- pred find_pattern_array(tuple_pattern_map(T)::in, int::in, 
	pattern_array(T)::out) is semidet.
	
:- pred set_pattern_array(int::in, pattern_array(T)::in, 
	tuple_pattern_map(T)::in, tuple_pattern_map(T)::out) is det.


%-----------------------------------------------------------------------------%
% Pattern Array Operations	


% retreives the element map for the given (1 based) index in the array, throws 
% an exception if index is out of bounds of array

:- func get_element_map(pattern_array(T), int) = element_map(T).
:- pred get_element_map(pattern_array(T)::in, int::in, element_map(T)::in) is 
	det.
	
% Succeeds if pattern array contains only empty maps.
:- pred pattern_array_is_empty(pattern_array(_)::in) is semidet.

% Apply a function to every element in pattern array, returning a new array
:- pred pattern_array_map(func(element_map(T), int) = element_map(T), 
	pattern_array(T), pattern_array(T)).
	
:- mode pattern_array_map(in(func(in, in) = out is det), in, array_uo)
	is det.
	
:- pred pattern_array_fold(func(element_map(T), int, A) = A, pattern_array(T),
	A, A).
	
:- mode pattern_array_fold(in(func(in, in, in) = out is det), in, in, out)
	is det.
:- mode pattern_array_fold(in(func(in, in, in) = out is semidet), in, in, out) 
	is semidet.
:- mode pattern_array_fold(in(func(in, in, mui) = muo is det), in, mui, muo)
	is det.
:- mode pattern_array_fold(in(func(in, in, mui) = muo is semidet), in, 
	mui, muo) is semidet.
:- mode pattern_array_fold(in(func(in, in, di) = uo is det), in, di, uo)
	is det.
:- mode pattern_array_fold(in(func(in, in, array_di) = array_uo is det), in, 
	array_di, array_uo) is det.
:- mode pattern_array_fold(in(func(in, in, in) = array_uo is det), in, in, 
	array_uo) is det.
	
%-----------------------------------------------------------------------------%
% Element Map Operations

:- func get_exact_map(element_map(T), mh_term) = exact_map(T).

:- pred get_exact_map(element_map(T)::in, mh_term::in, exact_map(T)::out)
	is det.
	

:- func find_exact_map(element_map(T), mh_term) = exact_map(T) is semidet.

:- pred find_exact_map(element_map(T)::in, mh_term::in, exact_map(T)::out)
	is semidet.
	
:- pred set_exact_map(mh_term::in, exact_map(T)::in, 
	element_map(T)::in, element_map(T)::out) is det.
	
:- pred det_element_map_insert(mh_term::in, array(mh_term)::in, 
	T::in, element_map(T)::in, element_map(T)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- implementation.

:- import_module int.
:- import_module pair.

%-----------------------------------------------------------------------------%
% Tuple Pattern map

init = map.init.
init(init).

singleton(Tuple, T) = Map :- det_insert(Tuple, T, init, Map).
array_singleton(Tuple, Array, T) = Map :-
	det_array_insert(Tuple, Array, T, init, Map).

is_empty(init).

:- func insert_pattern(mh_tuple, T, tuple_pattern_map(T)) 
	= tuple_pattern_map(T).
insert_pattern(P, V, !.Map) = !:Map :- array_insert(P, V, !Map).

from_exact_map(Exact) = map.foldl(insert_pattern, Exact, map.init).

%-----------------------------------------------------------------------------%
% Insertion
	
insert(Tuple, T, !Map) :- 
	array_insert(Tuple, to_array(Tuple), T, !Map).
	
:- pragma inline(insert/4).
		
insert_from_corresponding_lists([], [], !Map).
insert_from_corresponding_lists([], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
insert_from_corresponding_lists([_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
insert_from_corresponding_lists([K | Ks], [V | Vs], !Map) :-
    insert(K, V, !Map),
    insert_from_corresponding_lists(Ks, Vs, !Map).
	
:- pragma inline(insert_from_corresponding_lists/4).

insert_from_assoc_list([], !Map).
insert_from_assoc_list([K - V | KVs], !Map) :-
    insert(K, V, !Map),
    insert_from_assoc_list(KVs, !Map).
	
:- pragma inline(insert_from_assoc_list/3).

array_insert(TupleArray, T, !Map) :-
	array.size(TupleArray, Arity),
	get_pattern_array(!.Map, Arity, OldPatternArray),
	pattern_array_map(pattern_array_map_insert(TupleArray, T), 
		OldPatternArray, NewPatternArray),
	set_pattern_array(Arity, NewPatternArray, !Map).

:- func pattern_array_map_insert(array(mh_term), mh_tuple, T, element_map(T), int) 
	= element_map(T).

pattern_array_map_insert(TupleArray, T, !.Map, Index) = !:Map :-
	array.unsafe_lookup(TupleArray, Index, Term),
	get_exact_map(!.Map, Term, OldExactMap),
	(if map.insert(TupleArray, T, OldExactMap, NewExactMap)
	then 
		set_exact_map(Term, NewExactMap, !Map)
	else 
		report_lookup_error(
"mh_tuple_pattern_map.array_insert: array aleady present in map", 
		TupleArray, !.Map)
	).
	
:- pragma inline(array_insert/5).

	
array_insert_from_corresponding_lists([], [], [], !Map).
array_insert_from_corresponding_lists([], [_ | _], [_ | _], _, _) :-
	unexpected($pred, "list length mismatch").
array_insert_from_corresponding_lists([_ | _], [], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
array_insert_from_corresponding_lists([_ | _], [_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
array_insert_from_corresponding_lists([K | Ks], [A, As], [V | Vs],
	!Map) :-
    array_insert(K, A, V, !Map),
    array_insert_from_corresponding_lists(Ks, As, Vs, !Map).
	
:- pragma inline(det_array_insert_from_corresponding_lists/5).
	

set(Tuple, T, !Map) :- 
	array_set(to_array(Tuple), T, !Map).
	
:- pragma inline(set/4).

set_from_corresponding_lists([], [], !Map).
set_from_corresponding_lists([], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
set_from_corresponding_lists([_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
set_from_corresponding_lists([K | Ks], [V | Vs], !Map) 						:-
    set(K, V, !Map),
    set_from_corresponding_lists(Ks, Vs, !Map).
	
:- pragma inline(set_from_corresponding_lists/4).

set_from_assoc_list([], !Map).
set_from_assoc_list([K - V | KVs], !Map) :-
    set(K, V, !Map),
    set_from_assoc_list(KVs, !Map).
	
:- pragma inline(set_from_assoc_list/3).

array_set(TupleArray, T, !Map) :-
	array.size(TupleArray, Arity),
	get_pattern_array(!.Map, Arity, OldPatternArray),
	pattern_array_map(pattern_array_set(TupleArray, Tuple, T), 
		OldPatternArray, NewPatternArray),
	set_pattern_array(Arity, NewPatternArray, !Map).

:- func pattern_array_set(array(mh_term), T, element_map(T), int) 
	= element_map(T).

pattern_array_set(TupleArray, T, !.Map, Index) = !:Map :-
	array.unsafe_lookup(TupleArray, Index, Term),
	get_exact_map(!.Map, Term, OldExactMap),
	mh_tuple_exact_map.array_set(TupleArray, T, OldExactMap,
		NewExactMap),
	set_exact_map(TupleArray, NewExactMap, !Map).
	
:- pragma inline(array_set/5).

array_set_from_corresponding_lists([], [], [], !Map).
array_set_from_corresponding_lists([], [_ | _], [_ | _], _, _) :-
	unexpected($pred, "list length mismatch").
array_set_from_corresponding_lists([_ | _], [], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
array_set_from_corresponding_lists([_ | _], [_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
array_set_from_corresponding_lists([K | Ks], [A, As], [V | Vs],
	!Map) :-
    array_set(K, A, V, !Map),
    array_set_from_corresponding_lists(Ks, As, Vs, !Map).
	
:- pragma inline(array_set_from_corresponding_lists/5).

%-----------------------------------------------------------------------------%
% Removal

delete(Tuple, !Map) :- array_delete(to_array(Tuple), !Map).
	
:- pragma inline(delete/3).

delete_list([], !Map).
delete_list([Tuple | Tuples], !Map) :- 
	delete(Tuple, !Map),
	delete_list(Tuples, !Map).
	
:- pragma inline(delete_list/3).
	
array_delete(TupleArray, !Map) :- 
	array.size(TupleArray, Arity),
	get_pattern_array(!.Map, Arity, OldPatternArray),
	pattern_array_map(pattern_array_delete(TupleArray), 
		OldPatternArray, NewPatternArray),
	(if pattern_array_is_empty(NewPatternArray)
	then
		map.delete(Arity, !Map)
	else
		set_pattern_array(Arity, NewPatternArray, !Map)
	).

:- func pattern_array_set(array(mh_term), mh_tuple, T, element_map(T), int) 
	= element_map(T).

pattern_array_delete(TupleArray, !.Map, Index) = !:Map :-
	array.unsafe_lookup(TupleArray, Index, Term),
	get_exact_map(!.Map, Term, OldExactMap),
	mh_tuple_exact_map.array_delete(TupleArray, T, OldExactMap,	NewExactMap),
	(if mh_tuple_exact_map.is_empty(NewExactMap)
	then
		map.delete(Term, !Map)
	else
		set_exact_map(Term, NewExactMap, !Map)
	).
	
:- pragma inline(array_delete/3).

array_delete_list([], !Map).
array_delete_list([A | As], !Map) :- 
	array_delete(A, !Map),
	array_delete_list(As, !Map).
	
:- pragma inline(array_delete_list/3).	

%-----------------------------------------------------------------------------%
% Pattern Map Operations

get_pattern_array(Map, Arity) =
	(if map.search(Map, Arity, Existing)
	then
		Existing
	else
		array.init(Arity, init)	
	).
	
:- pragma inline(get_pattern_array/2).

get_pattern_array(Map, Arity, get_pattern_array(Map, Arity)).

:- pragma inline(get_pattern_array/3).

find_pattern_array(Map, Arity) = map.search(!.Map, Arity).

:- pragma inline(find_pattern_array/2).

find_pattern_array(Map, Arity, find_pattern_array(Map, Arity)).

:- pragma inline(find_pattern_array/3).

set_pattern_array(Arity, Array, !Map) :- map.set(Arity, Array, !Map).

:- pragma inline(set_pattern_array/4).


%-----------------------------------------------------------------------------%
% Pattern Array Operations	

get_element_map(Array, Index) = array.lookup(Array, Index - 1).

:- pragma inline(get_element_map/2).

get_element_map(Array, Index, get_element_map(Aray, Index)).

:- pragma inline(get_element_map/3).

pattern_array_is_empty(Array) :- array.all_true(map.is_empty, Array).

:- pragma inline(pattern_array_is_empty/1).

pattern_array_map(F, Array, 
	generate(
		size(Array), 
		do_pattern_array_map(F, Array)
	)
). 

:- pragma inline(pattern_array_map/3).
	
:- func do_pattern_array_map(
	func(element_map(T), int) = element_map(T),
	pattern_array(T),
	int) = element_map(T).
	
:- mode do_pattern_array_map(in(func(in, in) = out is det), in, in) = out
	is det.
	
do_pattern_array_map(F, Array, Index) = F(ElementMap, Index) :-
	array.unsafe_lookup(Array, Index, ElementMap).
	
	
pattern_array_fold(F, Array, !A) :- 
	do_pattern_array_fold(F, Array, A, min(Array), max(Array)).	
	
	
:- pred do_pattern_array_fold(func(element_map(T), int, A) = A, 
	pattern_array(T), A, A, int, int).
	
:- mode pattern_array_fold(in(func(in, in, in) = out is det), in, in, out, 
	in, in) is det.
:- mode pattern_array_fold(in(func(in, in, in) = out is semidet), in, in, out,
	in, in) is semidet.
:- mode pattern_array_fold(in(func(in, in, mui) = muo is det), in, mui, muo, 
	in, in) is det.
:- mode pattern_array_fold(in(func(in, in, mui) = muo is semidet), in, 
	mui, muo, in, in) is semidet.
:- mode pattern_array_fold(in(func(in, in, di) = uo is det), in, di, uo,
	in, in) is det.
:- mode pattern_array_fold(in(func(in, in, array_di) = array_uo is det), in, 
	array_di, array_uo, in, in) is det.
:- mode pattern_array_fold(in(func(in, in, array_di) = array_uo is det), in, 
	in,	array_uo, in, in) is det.
	
do_pattern_array_fold(F, Array, !A, I, Max) :-
	(if I > Max 
	then
		true
	else
		array.unsafe_lookup(Array, I, ElementMap),
		!:A = F(ElementMap, I, !.A),
		do_pattern_array_fold(F, Array, !A, I + 1, Max)
	).
	
:- pragma inline(pattern_array_fold/4).

%-----------------------------------------------------------------------------%
% Element Map Operations

get_exact_map(Element, Term) =
	(if mh_term_map.search(Element, Term, Found)
	then
		Found
	else
		mh_term_map.init
	).
	
:- pragma inline(get_exact_map/2).
	
get_exact_map(Element, Term, get_exact_map(Element, Term)).

:- pragma inline(get_exact_map/3).

find_exact_map(Element, Term) = mh_term_map.search(Element, Term).

:- pragma inline(find_exact_map/2).

find_exact_map(Element, Term, find_exact_map(Element, Term)).

:- pragma inline(find_exact_map/3).

set_exact_map(Term, Exact, !Element) :- mh_term_map.set(Term, Exact, !Element).

:- pragma inline(set_exact_map/4).

det_element_map_insert(Term, TupleArray, T, !Map) :-
	get_exact_map(!.Map, Term, ExMap0),
	map.det_insert(TupleArray, T, ExMap0, ExMap1),
	set_exact_map(Term, ExMap1, !Map).
	
:- pragma inline(det_element_map_insert/6).