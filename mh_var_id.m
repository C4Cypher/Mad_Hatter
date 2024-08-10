%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_var.m
% Main author: C4Cypher.
% Stability: low.

:- module mh_var_id.

:- interface.

:- import_module list.
:- import_module array
:- import_module sparse_bitset.

%-----------------------------------------------------------------------------%
% Variable ID's

:- type var_id.

:- type var_ids == list(var_id).

% Breaks encapsulation, code smell
% :- func construct_var_id(int) = var_id.

% :- func deconstruct_var_id(var_id) = int.


:- func var_id_offset(var_id, int) = var_id.

:- pred valid_var_id(var_id::in) is semidet.

:- pred require_valid_var_id(var_id::in) is det.

:- pred expect_valid_var_id(var_id::in, string::in, string::in) is det.

%-----------------------------------------------------------------------------%
% Var ID Offsets

:- type var_id_offset.

:- pred var_id_offset(var_id, var_id, var_id_offset).
:- mode var_id_offset(in, in, out) is det.
:- mode var_id_offset(out, in, in) is det.
:- mode var_id_offset(in, out, in) is det.

:- func var_id_offset(var_id, var_id) = var_id_offset.
:- mode var_id_offset(in, in) = out is det.
:- mode var_id_offset(in, out) = in is det.
:- mode var_id_offset(out, in) = in is det.

:- func apply_var_id_offset(var_id, var_id_offset) = var_id.
:- mode apply_var_id_offset(in, in) = out is det.
:- mode apply_var_id_offset(in, out) = in is det.
:- mode apply_var_id_offset(out, in) = in is det.

%-----------------------------------------------------------------------------%
% Variable sets

:- type var_id_set.

:- func init_var_id_set = var_id_set.

:- pred init_var_id_set(var_id_set::out) is det.

:- pred var_id_count(var_id_set::in, int::out) is det.

:- func var_id_count(var_id_set) = int.

:- pred valid_var_id_set(var_id_set::in) is semidet.

:- pred require_valid_var_id_set(var_id_set::in) is det.

:- pred expect_valid_var_id_set(var_id_set::in, string::in, string::in) is det.

%-----------------------------------------------------------------------------%
% Operations on var_ids and var_id_sets

:- pred new_var_id(var_id::out, var_id_set::in, var_id_set::out) is det.

:- pred new_var_ids(var_ids::out, var_id_set::in, var_id_set::out) is cc_multi.

:- pred new_var_ids(int::in, var_ids::out, var_id_set::in, var_id_set::out) 
	is det.

:- pred contains_var_id(var_id_set, var_id).
:- mode contains_var_id(in, in) is semidet.
:- mode contains_var_id(in, out) is nondet.


% complete_var_id_set(Generator, Set)
% Is true if the set produced by Generator produces all members of Set from
% 1 to var_id_count(Set) irrespective of order or duplicates

:- pred complete_var_id_set(pred(var_id), var_id_set).
:- mode complete_var_id_set(pred(out) is multi, in) is semidet.
:- mode complete_var_id_set(pred(out) is multi, out) is semidet.

%-----------------------------------------------------------------------------%
% Indexing Arrays by var_id 

:- pred var_id_in_bounds(array(_T)::in, var_id::in) is semidet.

:- pred var_id_lookup(array(T)::in, var_id::in, T::out) is det.

:- func var_id_lookup(array(T), var_id) = T.

:- pred var_id_semidet_lookup(array(T)::in, var_id::in, T::out) is semidet.

:- func var_id_semidet_lookup(array(T), var_id) = t.

:- func var_id_elem(var_id, array(T)) = T.

:- pred var_id_set(var_id::in, T::in, array(T)::array_di, array(T)::array_uo) 
	is det.

% slow set
:- pred var_id_slow_set(var_id::in, T::in, array(T)::in, array(T)::array_uo)
	is det.
	
:- pred var_id_set_init_array(var_id_set::in, T::in, array(T)::array_uo) 
	is det.

:- func var_id_set_init_array(var_id_set::in, T::in) = (array(T)::array_uo)
	is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module string.
:- import_module bitmap.
:- import_module bool.
:- import_module solutions.

:- import_module mh_index.

%-----------------------------------------------------------------------------%
% Variable ID's

:- type var_id == int.

% construct_var_id(ID) = ID.

% :- pragma inline(construct_var_id/1).

% deconstruct_var_id(ID) = ID.

% :- pragma inline(deconstruct_var_id/1).

var_id_offset(Offset, !ID) :- !:ID = !.ID + Offset.

var_id_offset(!.ID, Offset) = !:ID :- var_id_offset(Offset, !ID). 

valid_var_id(I) :- I > 0.

require_valid_var_id(I) :- require(valid_var_id(I), "Invalid var_id:" ++
	string(I) ++ " less than zero.").
	
expect_valid_var_id(I, Module, Proc) :- expect(valid_var_id(I), Module, Proc,
	"Invalid var_id:" ++ string(I) ++ " less than zero.").

%-----------------------------------------------------------------------------%
% Variable sets

:- type var_id_set == int.

init_var_id_set = 0.

init_var_id_set(init_var_id_set).

var_id_count(Set, var_id_count(Set)).

var_id_count(Count) = Count.

valid_var_id_set(Set) :- Set >= 0.

require_valid_var_id_set(Set) :- 
	require(valid_var_id_set(Set), "Invalid var_id_set. Last var_id: " ++ string(Set) 
	++	" was less than zero.").

expect_valid_var_id_set(Set, Module, Proc) :- 
	expect(valid_var_id_set(Set), Module, Proc, "Invalid var_id_set. Last var_id: " 
	++ string(Set) ++ " was less than zero.").

%-----------------------------------------------------------------------------%
% Operations on var_ids and var_id_sets



new_var_id(ID, ID - 1, ID).

new_var_ids([], !Set).

new_var_ids([ ID | IDs ], !Set) :-
	new_var_id(ID, !Set), 
	(
		IDs = [], !:Set = !.Set
	;
		new_var_ids(IDs, !Set)
	).

new_var_ids(Number, List, !Set) :-
	compare(NumCmp, Number, 0), require_complete_switch [NumCmp] (
		NumCmp = (<), unexpected($module, $pred, 
			"Cannot generate negative sized list of variable ids.") ;
		NumCmp = (=), List = [] ;
		NumCmp = (>), ( 
			new_var_id(ID, !Set),
			new_var_ids(Number - 1, IDs, !Set),
			List = [ ID | IDs ]
		)
	).

:- pragma promise_equivalent_clauses(contains_var_id/2).

contains_var_id(Last::in, ID::in) :- 
	expect_valid_var_id(ID, $module, $pred),
	ID =< Last.
	
contains_var_id(Last::in, ID::out) :-
	expect_valid_var_id(ID, $module, $pred),
	Last > 0,
	all_ids_to(1, Last, ID).
	

:- pred all_ids_to(var_id::in, var_id::in, var_id::out) is multi.

all_ids_to(First, Last, This) :-
	( if First = Last
		then This = Last
		else (
			This = First ;
			all_ids_to(First + 1, Last, This)
		)
	).

valid_var_id_set(Last) :- Last >= 0.

:- pragma promise_equivalent_clauses(complete_var_id_set/2).

complete_var_id_set(Generator::pred(out) is multi, Set::in) :-  
		( promise_equivalent_solutions [Unique]
			do_while(
				Generator, 
				accumulate_id_set_bounded(Set), 
				acc(init(Set), 0),
				acc(_, Unique)
			)
		),
		Unique = Set.
		
		
complete_var_id_set(Generator::pred(out) is multi, Set::out) :-
	( promise_equivalent_solutions [Found, Unique]
		unsorted_aggregate(
			Generator, 
			accumulate_id_set_unbounded,
			acc(init(0), 0), 
			acc(Found, Unique)
		)		
	),
	Max = num_bits(Found),
	Max = Unique,
	Set = Max.


:- type id_set_acc	---> acc(ids::bitmap, unique_count::int).
	
:- inst id_set_acc ---> acc(uniq_bitmap, ground).
	
:- pred accumulate_id_set_bounded(var_id_set::in, var_id::in, bool::out, 
	id_set_acc::in, id_set_acc::out) is det.
	
accumulate_id_set_bounded(Max, ID, InBounds, 
	acc(!.Found, Unique0), acc(!:Found, Unique)) :-
	( if (ID > Max ; ID < 1) 
		then
			!:Found = !.Found,
			InBounds = no,
			Unique = -1
		else 
			InBounds = yes,
			ID_index =id_index(ID),
			AlreadyFound = unsafe_get_bit(!.Found, ID_index),
			(
				AlreadyFound = yes,
				!:Found = !.Found,
				Unique = Unique0
			;
				AlreadyFound = no,
				unsafe_set_bit(ID_index, yes, !Found),
				Unique = Unique0 + 1
			)
	).
	
:- pred accumulate_id_set_unbounded(var_id::in, 
	id_set_acc::in, id_set_acc::out) is det.
	
accumulate_id_set_unbounded(ID, acc(!.Found, Unique0), acc(!:Found, Unique)) :-
	require_valid_var_id(ID), % prevent resizing bitmap to negativee value
	ID_index = id_index(ID),
	( if in_range(!.Found, ID_index)
		then
			AlreadyFound = unsafe_get_bit(!.Found, ID_index),
			( 
				AlreadyFound = yes,
				!:Found = !.Found,
				Unique = Unique0
			;
				AlreadyFound = no,
				unsafe_set_bit(ID_index, yes, !Found),
				Unique = Unique0 + 1
			)
		else
		% If ID_index is larger than the bitmap, then ID will be the size of
		% the new bitmap size
			!:Found = resize(!.Found, ID, no),  
			unsafe_set_bit(ID_index, yes, !Found),
			Unique = Unique0 + 1
	).

%-----------------------------------------------------------------------------%
% Indexing Arrays by var_id

:- func id_index(var_id) = int.

id_index(ID) = ID - 1.

var_id_in_bounds(Array, ID) :- in_bounds(Array, id_index(ID)).

var_id_lookup(Array, ID, T) :- lookup(Array, id_index(ID), T).

var_id_lookup(Array, ID) = lookup(Array, id_index(ID)).

var_id_semidet_lookup(Array, ID, T) :- lookup(Array, id_index(ID), T).

var_id_semidet_lookup(Array, ID) = semidet_lookup(Array, ID).

var_id_elem(ID, Array) = elem(id_index(ID), Array).

var_id_set(ID, T, !Array) :- set(id_index(ID), T, !Array).

var_id_slow_set(ID, T, !Array) :- slow_set(id_index(ID), T, !Array).

var_id_set_init_array(Last, T, A) :- array.init(Last, T, A).

var_id_set_init_array(Set, T) = A :- var_id_set_init_array(Set, T, A).