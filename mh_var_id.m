%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_var_id.m
% Main author: C4Cypher.
% Stability: low.

:- module mh_var_id.

:- interface.

:- import_module list.
:- import_module array.
% :- import_module sparse_bitset.

%-----------------------------------------------------------------------------%
% Variable ID's

:- type var_id.

:- type var_ids == list(var_id).

% Breaks encapsulation, code smell
% :- func construct_var_id(int) = var_id.

% :- func deconstruct_var_id(var_id) = int.


:- pred valid_var_id(var_id::in) is semidet.

:- pred require_valid_var_id(var_id::in) is det.

:- pred expect_valid_var_id(var_id::in, string::in, string::in) is det.

:- func null_var_id = var_id is det.

:- pred var_id_lt(var_id::in, var_id::in) is semidet.
:- pred var_id_le(var_id::in, var_id::in) is semidet.
:- pred var_id_gt(var_id::in, var_id::in) is semidet.
:- pred var_id_ge(var_id::in, var_id::in) is semidet.

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

:- pred var_id_set_offset(var_id_set, var_id_offset, var_id_set).
:- mode var_id_set_offset(in, in, out) is det.
:- mode var_id_set_offset(out, in, in) is det.
:- mode var_id_set_offset(in, out, in) is det.

:- func var_id_set_offset(var_id_set, var_id_offset) = var_id_set.
:- mode var_id_set_offset(in, in) = out is det.
:- mode var_id_set_offset(out, in) = in is det.
:- mode var_id_set_offset(in, out) = in is det.

:- func null_var_id_offset = var_id_offset.

:- pred offset_lt(var_id_offset::in, var_id_offset::in) is semidet.
:- pred offset_le(var_id_offset::in, var_id_offset::in) is semidet.
:- pred offset_gt(var_id_offset::in, var_id_offset::in) is semidet.
:- pred offset_ge(var_id_offset::in, var_id_offset::in) is semidet.



%-----------------------------------------------------------------------------%
% Variable sets

:- type var_id_set.

:- func init_var_id_set = var_id_set.

:- pred init_var_id_set(var_id_set::out) is det.

:- pred var_id_count(var_id_set::in, int::out) is det.

:- func var_id_count(var_id_set) = int.

:- pred valid_var_id_set(var_id_set::in) is semidet.

:- pred empty_var_id_set(var_id_set::in) is semidet.

:- pred var_id_set_lt(var_id_set::in, var_id_set::in) is semidet.
:- pred var_id_set_le(var_id_set::in, var_id_set::in) is semidet.
:- pred var_id_set_gt(var_id_set::in, var_id_set::in) is semidet.
:- pred var_id_set_ge(var_id_set::in, var_id_set::in) is semidet.

:- pred require_valid_var_id_set(var_id_set::in) is det.

:- pred expect_valid_var_id_set(var_id_set::in, string::in, string::in) is det.

%-----------------------------------------------------------------------------%
% Operations on var_ids and var_id_sets

:- pred new_var_id(var_id::out, var_id_set::in, var_id_set::out) is det.

:- pred new_var_ids(var_ids::out, var_id_set::in, var_id_set::out) is cc_multi.

:- pred new_var_ids(int::in, var_ids::out, var_id_set::in, var_id_set::out) 
	is det.
	
:- func first_var_id = var_id.

:- func first_var_id(var_id_offset) = var_id.
:- mode first_var_id(in) = out is det.
:- mode first_var_id(out) = in is det.

:- func last_var_id(var_id_set) = var_id. 
:- mode last_var_id(in) = out is det.
:- mode last_var_id(out) = in is det.


:- func next_var_id(var_id) = var_id.
:- mode next_var_id(in) = out is det.
:- mode next_var_id(out) = in is det.

:- func previous_var_id(var_id) = var_id.
:- mode previous_var_id(in) = out is det.
:- mode previous_var_id(out) = in is det.


	
% contains(Set, ID) 
% Succeeds with any ID between 1 and the last var_id in Set

:- pred contains_var_id(var_id_set, var_id).
:- mode contains_var_id(in, in) is semidet.
:- mode contains_var_id(in, out) is nondet.

% contains(Set, Offset, ID)
% Succeeds with any ID between Offset and the last var_id of Set
:- pred contains_var_id(var_id_set, var_id_offset, var_id).
:- mode contains_var_id(in, in, in) is semidet.
:- mode contains_var_id(in, in, out) is nondet.


% complete_var_id_set(Generator, Set)
% Is true if the set produced by Generator produces all members of Set from
% 1 to var_id_count(Set) irrespective of order or duplicates

:- pred complete_var_id_set(pred(var_id), var_id_set).
:- mode complete_var_id_set(pred(out) is multi, in) is semidet.
:- mode complete_var_id_set(pred(out) is multi, out) is semidet.

%-----------------------------------------------------------------------------%
% Indexing Arrays by var_id 

:- func array_var_id_set(array(_T)) = var_id_set. 

:- func offset_array_var_id_set(array(_T), var_id_offset) = var_id_set.

:- pred var_id_in_bounds(array(_T)::in, var_id::in) is semidet.
:- pred var_id_in_bounds(array(_T)::in, var_id_offset::in, var_id::in) 
	is semidet.

:- pred var_id_lookup(array(T)::in, var_id::in, T::out) is det.
:- pred var_id_lookup(array(T)::in, var_id_offset::in, var_id::in, T::out) 
	is det.

:- func var_id_lookup(array(T), var_id) = T.
:- func var_id_lookup(array(T), var_id_offset, var_id) = T.

:- pred var_id_semidet_lookup(array(T)::in, var_id::in, T::out) is semidet.
:- pred var_id_semidet_lookup(array(T)::in, var_id_offset::in, 
	var_id::in, T::out) is semidet.

:- func var_id_semidet_lookup(array(T), var_id) = T is semidet.
:- func var_id_semidet_lookup(array(T), var_id_offset, var_id) = T is semidet.

:- func var_id_elem(var_id, array(T)) = T. 
:- func var_id_elem(var_id, var_id_offset, array(T)) = T. 

:- pred var_id_member(array(T), var_id, T).
:- mode var_id_member(in, in, out) is semidet.
:- mode var_id_member(in, out, out) is nondet.

:- pred var_id_member(array(T), var_id_offset, var_id, T).
:- mode var_id_member(in, in, in, out) is semidet.
:- mode var_id_member(in, in, out, out) is nondet.



:- pred var_id_set(var_id::in, T::in, array(T)::array_di, array(T)::array_uo) 
	is det.
	
:- pred var_id_set(var_id::in, T::in, var_id_offset::in, 
	array(T)::array_di, array(T)::array_uo) is det.

% slow set
:- pred var_id_slow_set(var_id::in, T::in, array(T)::in, array(T)::array_uo)
	is det.

:- pred var_id_slow_set(var_id::in, T::in, var_id_offset::in,
	array(T)::in, array(T)::array_uo) is det.

:- pred var_id_set_init_array(var_id_set::in, T::in, array(T)::array_uo) 
	is det.
	
:- pred var_id_set_init_array(var_id_offset::in, var_id_set::in, T::in, 
	array(T)::array_uo) is det.

:- func var_id_set_init_array(var_id_set::in, T::in) = (array(T)::array_uo)
	is det. 
	
:- func var_id_set_init_array(var_id_offset::in, var_id_set::in, T::in) = 
	(array(T)::array_uo) is det.


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

valid_var_id(I) :- I > 0.

require_valid_var_id(I) :- require(valid_var_id(I), "Invalid var_id:" ++
	string(I) ++ " less than zero.").
	
expect_valid_var_id(I, Module, Proc) :- expect(valid_var_id(I), Module, Proc,
	"Invalid var_id:" ++ string(I) ++ " less than zero.").
	
null_var_id = 0.

var_id_lt(ID1, ID2) :- ID1 < ID2.
var_id_le(ID1, ID2) :- ID1 =< ID2.
var_id_gt(ID1, ID2) :- ID1 > ID2.
var_id_ge(ID1, ID2) :- ID1 >= ID2.

%-----------------------------------------------------------------------------%
% Var ID Offsets

:- type var_id_offset == int.

var_id_offset(ID1, ID2, ID1 - ID2).

var_id_offset(ID1, ID2) = Offset :- var_id_offset(ID1, ID2, Offset).

apply_var_id_offset(ID1, Offset) = ID2 :- var_id_offset(ID1, ID2, Offset).

var_id_set_offset(Set, Offset, Set + Offset).

var_id_set_offset(Set, Offset) = Set + Offset.

null_var_id_offset = 0.

offset_lt(Offset1, Offset2) :- Offset1 < Offset2.
offset_le(Offset1, Offset2) :- Offset1 =< Offset2.
offset_gt(Offset1, Offset2) :- Offset1 > Offset2.
offset_ge(Offset1, Offset2) :- Offset1 >= Offset2.



%-----------------------------------------------------------------------------%
% Variable sets

:- type var_id_set == int.

init_var_id_set = 0.

init_var_id_set(init_var_id_set).

var_id_count(Set, var_id_count(Set)).

var_id_count(Count) = Count.

valid_var_id_set(Set) :- Set >= 0.

empty_var_id_set(0).

var_id_set_lt(Set1, Set2) :- Set1 < Set2.
var_id_set_le(Set1, Set2) :- Set1 =< Set2.
var_id_set_gt(Set1, Set2) :- Set1 > Set2.
var_id_set_ge(Set1, Set2) :- Set1 >= Set2.



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
	
first_var_id = 1.
first_var_id(Offset) = Offset + 1. 
	
last_var_id(Last) = Last.

next_var_id(ID) = ID + 1.

previous_var_id(ID) = ID - 1.
	

:- pragma promise_equivalent_clauses(contains_var_id/2).

contains_var_id(Last::in, ID::in) :- 
	ID > 0,
	ID =< Last.
	
contains_var_id(Last::in, ID::out) :-
	Last > 0,
	all_ids_to(1, Last, ID).
	
:- pragma promise_equivalent_clauses(contains_var_id/3).

contains_var_id(Last::in, Offset::in, ID::in) :- 
	ID > Offset,
	ID =< Last.
	
contains_var_id(Last::in, Offset::in, ID::out) :-
	Last > 0,
	all_ids_to(Offset + 1, Last, ID).	
	


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

:- pragma inline(id_index/1).

:- func id_index(var_id) = int.
:- mode id_index(in) = out is det.
:- mode id_index(out) = in is det.

id_index(ID) = ID - 1.

:- pragma inline(id_index/2).

:- func id_index(var_id, var_id_offset) = int.
:- mode id_index(in, in) = out is det.
:- mode id_index(out, in) = in is det.
:- mode id_index(in, out) = in is det.

id_index(ID, Offset) = id_index(ID - Offset).



array_var_id_set(Array) = size(Array).

offset_array_var_id_set(Array, Offset) = size(Array) + Offset.

var_id_in_bounds(Array, ID) :- in_bounds(Array, id_index(ID)).

var_id_in_bounds(Array, Offset, ID) :- in_bounds(Array, id_index(ID, Offset)).

var_id_lookup(Array, ID, T) :- lookup(Array, id_index(ID), T).
var_id_lookup(Array, Offset, ID, T) :- lookup(Array, id_index(ID, Offset), T).

var_id_lookup(Array, ID) = lookup(Array, id_index(ID)).
var_id_lookup(Array, Offset, ID) = lookup(Array, id_index(ID, Offset)).

var_id_semidet_lookup(Array, ID, T) :- semidet_lookup(Array, id_index(ID), T).
var_id_semidet_lookup(Array, Offset, ID, T) :- 
	semidet_lookup(Array, id_index(ID, Offset), T).

var_id_semidet_lookup(Array, ID) = T :- var_id_semidet_lookup(Array, ID, T).
var_id_semidet_lookup(Array, Offset, ID) = T :- 
	var_id_semidet_lookup(Array, Offset, ID, T).


var_id_elem(ID, Array) = elem(id_index(ID), Array).
var_id_elem(ID, Offset, Array) = elem(id_index(ID, Offset), Array).


:- pragma promise_equivalent_clauses(var_id_member/3).

var_id_member(Array::in, ID::in, T::out) :- 
	var_id_semidet_lookup(Array, ID, T).

var_id_member(Array::in, ID::out, T::out) :-
	nondet_int_in_range(array.min(Array), array.max(Array), Index),
	unsafe_lookup(Array, Index, T),
	id_index(ID) = Index.
	
var_id_member(Array, Offset, ID - Offset, T) :- var_id_member(Array, ID, T).


var_id_set(ID, T, !Array) :- set(id_index(ID), T, !Array).

var_id_set(ID, T, Offset, !Array) :- var_id_set(ID - Offset, T, !Array).

var_id_slow_set(ID, T, !Array) :- slow_set(id_index(ID), T, !Array).

var_id_slow_set(ID, T, Offset, !Array) :- 
	var_id_slow_set(id_index(ID, Offset), T, !Array).

var_id_set_init_array(Last, T, A) :- array.init(Last, T, A).

var_id_set_init_array(Offset, Last, T, A) :- array.init(Last - Offset, T, A).

var_id_set_init_array(Set, T) = A :- var_id_set_init_array(Set, T, A).

var_id_set_init_array(Offset, Set, T) = 
	var_id_set_init_array(Set - Offset, T).
	