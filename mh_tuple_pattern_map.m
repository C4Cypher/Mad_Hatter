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
:- import_module pair.
:- import_module list.

:- import_module mh_term_map.
:- import_module mh_term.
:- import_module mh_tuple.
:- import_module mh_tuple_map.

%-----------------------------------------------------------------------------%
% Pattern Tuple map

:- type tuple_pattern_map(T) == 
	map.map(int, array(mh_term_map(pair(mh_tuple, T)))).

:- type pattern_array(T) == array(mh_term_map(pair(mh_tuple, T))).
:- type element_map(T) == mh_term_map(pair(mh_tuple, T)).
:- type tpm_pair(T) == pair(mh_tuple, T).


:- func init = (tuple_pattern_map(T)::out) is det.
:- pred init(tuple_pattern_map(_)::out) is det.

:- func singleton(mh_tuple, T) = tuple_pattern_map(T).

:- pred is_empty(tuple_pattern_map(_)::in) is semidet.

%-----------------------------------------------------------------------------%
% Search

:- pred select()

:- pred match()

%-----------------------------------------------------------------------------%
% Insertion
	
% throws exception if item is already present
	
:- pred insert(mh_tuple::in, T::in, tuple_pattern_map(T)::in, 
	tuple_pattern_map(T)::out) is det.

:- pred insert_from_corresponding_lists(list(mh_tuple)::in, list(T)::in,
	tuple_pattern_map(T)::in, tuple_pattern_map(T)::out) is det.
	
:- pred insert_from_assoc_list(assoc_list(mh_tuple, T)::in,
	tuple_pattern_map(T)::in, tuple_pattern_map(T)::out) is det.

:- set(mh_tuple::in, T::in, tuple_pattern_map::in, tuple_pattern_map::out)
	is det.
	
:- pred set_from_corresponding_lists(list(mh_tuple)::in, list(T)::in,
	tuple_pattern_map(T)::in, tuple_pattern_map(T)::out) is det.
	
:- pred set_from_assoc_list(assoc_list(mh_tuple, T)::in,
	tuple_pattern_map(T)::in, tuple_pattern_map(T)::out) is det.
	
%-----------------------------------------------------------------------------%
% Removal

	
:- pred delete(mh_tuple::in,  tuple_pattern_map(T)::in, 
	tuple_pattern_map::out) is det.

:- pred delete_list(list(mh_tuple)::in, tuple_pattern_map(T)::in, 
	tuple_pattern_map::out) is det.
	
%-----------------------------------------------------------------------------%
% Set operations


:- func union(func(T, T) = T, tuple_pattern_map(T), tuple_pattern_map(T)) =
	tuple_pattern_map(T).

:- pred union(func(T, T) = T, tuple_pattern_map(T), tuple_pattern_map(T),
	tuple_pattern_map(T)).
:- mode union(in(func(in, in) = out is det), in, in, out) is det.
:- mode union(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func intersect(func(T, T) = T, tuple_pattern_map(T), tuple_pattern_map(T))
	= var_map(T).
	
:- pred intersect(func(T, T) = T, tuple_pattern_map(T), tuple_pattern_map(T),
	tuple_pattern_map(T)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.
:- mode intersect(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- func difference(tuple_pattern_map(T), tuple_pattern_map(_)) =
	tuple_pattern_map(T).

:- pred difference(var_map(T)::in, tuple_pattern_map(_)::in, 
	tuple_pattern_map(T)::out)	is det.
	

%-----------------------------------------------------------------------------%
% Utility


% Apply a function to every element in pattern array, returning a new array
:- pred pattern_array_map(func(element_map(T)) = element_map(T), 
	pattern_array(T), pattern_array(T)).
	
:- mode pattern_array_map(in(func(in) = out is det), in, array_uo)
	is det.
	
	
:- pred pattern_array_fold(func(element_map(T), A) = A, pattern_array(T),
	A, A).
	
:- mode pattern_array_fold(in(func(in, in) = out is det), in, in, out) is det.
:- mode pattern_array_fold(in(func(in, in) = out is semidet), in, in, out) 
	is semidet.
:- mode pattern_array_fold(in(func(in, mui) = muo is det), in, mui, muo)
	is det.
:- mode pattern_array_fold(in(func(in, mui) = muo is semidet), in, mui, muo) 
	is semidet.
:- mode pattern_array_fold(in(func(in, di) = uo is det), in, di, uo) is det.
:- mode pattern_array_fold(in(func(in, array_di) = array_uo is det), in, 
	array_di, array_uo) is det.
:- mode pattern_array_fold(in(func(in, in) = array_uo is det), in, in, 
	array_uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- implementation.

:- import_module int.

:- import_module mh_term_map.

%-----------------------------------------------------------------------------%
% Pattern Tuple map



init = map.init.
init(init).

singleton(Tuple, T) = det_insert(Tuple, T, init).

is_empty(init).

	
	
%-----------------------------------------------------------------------------%
% Search

%-----------------------------------------------------------------------------%
% Insertion
	
	% Retreive the array of term maps associated with the tuple's arity
	% If none is found, create one.

:- func get_pattern_array(tuple_pattern_map(T), int) = pattern_array(T)).
	
get_pattern_array(Map, Arity) =
	(if map.search(!.Map, Size, Existing)
	then
		Existing
	else
		array.init(Size, init)	
	).
	
:- pragma inline(get_pattern_array/2).

:- pred get_arity_array(tuple_pattern_map(T)::in, int::in, 
	array(mh_term_map(pair(mh_tuple, T))::out) is det.

get_pattern_array(Map, Arity, get_pattern_array(Map, Arity)).

:- pragma inline(get_pattern_array/3).


insert(Tuple, T, !Map) :- 
	tuple_size(Tuple, Size),
	some [!Array] (
		get_pattern_array(!.Map, Size, !.Array),
		(if insert())
		

%-----------------------------------------------------------------------------%
% Utility

	
pattern_array_map(F, Array, 
	generate(
		size(Array), 
		do_pattern_array_map(F, Array)
	)
). 
	
:- func do_pattern_array_map(
	func(element_map(T)) = element_map(T),
	pattern_array(T),
	int) = element_map(T).
	
:- mode do_pattern_array_map(in(func(in) = out is det), in, in) = out
	is det.
	
do_pattern_array_map(F, Array, Index) = F(ElementMap) :-
	array.unsafe_lookup(Array, Index, ElementMap).
	
pattern_array_fold(F, Array, !A) :- 
	do_pattern_array_fold(F, Array, A, min(Array), max(Array)).	
	
:- pred do_pattern_array_fold(func(element_map(T), A) = A, pattern_array(T),
	A, A, int, int).
	
:- mode pattern_array_fold(in(func(in, in) = out is det), in, in, out, 
	in, in) is det.
:- mode pattern_array_fold(in(func(in, in) = out is semidet), in, in, out,
	in, in) is semidet.
:- mode pattern_array_fold(in(func(in, mui) = muo is det), in, mui, muo, 
	in, in) is det.
:- mode pattern_array_fold(in(func(in, mui) = muo is semidet), in, mui, muo, 
	in, in) is semidet.
:- mode pattern_array_fold(in(func(in, di) = uo is det), in, di, uo,
	in, in) is det.
:- mode pattern_array_fold(in(func(in, array_di) = array_uo is det), in, 
	array_di, array_uo, in, in) is det.
:- mode pattern_array_fold(in(func(in, in) = array_uo is det), in, in, 
	array_uo, in, in) is det.
	
do_pattern_array_fold(F, Array, !A, I, Max) :-
	(if I > Max 
	then
		true
	else
		array.unsafe_lookup(Array, I, ElementMap),
		!:A = F(ElementMap, !.A),
		do_pattern_array_fold(F, Array, !A, I + 1, Max)
	).
