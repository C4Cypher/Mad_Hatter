%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_var_id.m
% Main author: C4Cypher.
% Stability: medium?

:- module mh_var_id.

:- interface.

:- import_module list.
:- import_module array.

:- import_module mh_mercury_term.
:- import_module mh_arity.
:- import_module mh_var_set.

% While possibly overkill, I wanted to avoid handling variable ID's as naked
% integer values. Too much can go wrong, and it's hard for me to keep unmarked
% numbers straight if the roles of 'variable id', 'variable set' and 'variable
% offset' are not clearly deliniated.   Granted all of these are represented
% by simple unboxed integer values, and this should compile into inlined basic
% integer arithmetic, however it's critical for me that my code shows the
% intent of the operations on these values, rather than nested arithmetic calls
% that I will have to come back and make sense of later.

%-----------------------------------------------------------------------------%
% Variable ID's


:- type var_id.

:- type var_ids == list(var_id). 

	% Converts a mercury term.var into a valid Mad Hatter var_id
:- func mr_var_id(mr_var(_)) = var_id.


:- pred valid_var_id(var_id::in) is semidet.

:- pred require_valid_var_id(var_id::in) is det.

:- pred expect_valid_var_id(var_id::in, string::in, string::in) is det.

:- func null_var_id = var_id is det.

:- pred var_id_lt(var_id::in, var_id::in) is semidet.
:- pred var_id_le(var_id::in, var_id::in) is semidet.
:- pred var_id_gt(var_id::in, var_id::in) is semidet.
:- pred var_id_ge(var_id::in, var_id::in) is semidet.

:- pred var_id_order(var_id::in, var_id::in, var_id::out, var_id::out) is det.

%-----------------------------------------------------------------------------%
% Var ID Offsets

% A var_id_offset represents the difference between two var_id's. 
% In many cases, this is used to represent the difference a contiguous set of 
% variables and a 'complete' var_id_set from 1 to N ... usually indicating
% the first var_id of a set of variables.

:- type var_id_offset.

	% var_id_offset(A, B, A - B). 
:- pred var_id_offset(var_id, var_id, var_id_offset).
:- mode var_id_offset(in, in, out) is det.
:- mode var_id_offset(out, in, in) is det.
:- mode var_id_offset(in, out, in) is det.

	% var_id_offset(A, B) = A - B.
:- func var_id_offset(var_id, var_id) = var_id_offset.
:- mode var_id_offset(in, in) = out is det.
:- mode var_id_offset(in, out) = in is det.
:- mode var_id_offset(out, in) = in is det.


	% apply_var_id_offset(A, var_id_offset(A, B)) = B.
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

:- func offset_from_id_set(var_id_set) = var_id_offset.
:- func offset_from_arity(T) = var_id_offset <= arity(T).

:- func null_var_id_offset = var_id_offset.

:- pred offset_lt(var_id_offset::in, var_id_offset::in) is semidet.
:- pred offset_le(var_id_offset::in, var_id_offset::in) is semidet.
:- pred offset_gt(var_id_offset::in, var_id_offset::in) is semidet.
:- pred offset_ge(var_id_offset::in, var_id_offset::in) is semidet.

:- pred offset_order(var_id_offset::in, var_id_offset::in, var_id_offset::out,
	var_id_offset::out) is det.

%-----------------------------------------------------------------------------%
% Variable id sets

% A var_id_set represents a complete set of variable id's from 1 to N.
% 

:- type var_id_set.

:- func init_var_id_set = var_id_set.

:- pred init_var_id_set(var_id_set::out) is det.

:- func empty_var_id_set = var_id_set.

:- func var_id_set_from_offset(var_id_offset) = var_id_set.

:- pred var_id_count(var_id_set::in, int::out) is det.

:- func var_id_count(var_id_set) = int.

:- pred valid_var_id_set(var_id_set::in) is semidet.

:- pred empty_var_id_set(var_id_set::in) is semidet.
:- pred not_empty_var_id_set(var_id_set::in) is semidet.

:- func var_id_set_from_arity(T) = var_id_set <= arity(T).

:- pred var_id_set_lt(var_id_set::in, var_id_set::in) is semidet.
:- pred var_id_set_le(var_id_set::in, var_id_set::in) is semidet.
:- pred var_id_set_gt(var_id_set::in, var_id_set::in) is semidet.
:- pred var_id_set_ge(var_id_set::in, var_id_set::in) is semidet.

:- pred var_id_set_order(var_id_set::in, var_id_set::in, var_id_set::out,
	var_id_set::out) is det.

:- pred require_valid_var_id_set(var_id_set::in) is det.

:- pred expect_valid_var_id_set(var_id_set::in, string::in, string::in) is det.

:- pred append_var_id_sets(var_id_set, var_id_set, var_id_set).
:- mode append_var_id_sets(in, in, out) is det.
:- mode append_var_id_sets(out, in, in) is det.
:- mode append_var_id_sets(in, out, in) is det.

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
:- pred contains_var_id(var_id_offset, var_id_set, var_id).
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

	% sparse_index_weight(ID, Offset, Set) = Index
	% returns the sparse index corresponding to the ID if the id is within the
	% bounds specified by Offset, and Set, otherwise returns the index of the
	% element after the last element specified by Set
:- func sparse_index_weight(var_id, var_id_offset, var_id_set) = int.

	% var_id_sparse_index(ID, Offset, Set) = ID
	% look up the ID refrenced by the given sparse index
:- func var_id_sparse_index(var_id, var_id_offset, var_id_set) = var_id.

	% Reverse look up the ID refrenced by the given sparse index. Fails if the 
	% given index refrences an ID that is not within 
	% the given Offset, id_set bounds
	
:- func reverse_sparse_index(int, var_id_offset, var_id_set) = var_id 
	 is semidet.
	 
:- func id_set_weight(var_id_set)  = int.

:- func get_array_var_id_set(array(_T)) = var_id_set. 

:- func get_offset_array_var_id_set(array(_T), var_id_offset) = var_id_set.

:- pred var_id_array_in_bounds(array(_T)::in, var_id::in) is semidet.
:- pred var_id_array_in_bounds(array(_T)::in, var_id_offset::in, var_id::in) 
	is semidet.

:- pred var_id_array_lookup(array(T)::in, var_id::in, T::out) is det.
:- pred var_id_array_lookup(array(T)::in, var_id_offset::in, var_id::in, 
	T::out)	is det.

:- func var_id_array_lookup(array(T), var_id) = T.
:- func var_id_array_lookup(array(T), var_id_offset, var_id) = T.

:- pred var_id_array_semidet_lookup(array(T)::in, var_id::in, T::out)
	is semidet.
:- pred var_id_array_semidet_lookup(array(T)::in, var_id_offset::in, 
	var_id::in, T::out) is semidet.

:- func var_id_array_semidet_lookup(array(T), var_id) = T is semidet.
:- func var_id_array_semidet_lookup(array(T), var_id_offset, var_id) = T 
	is semidet.

:- func var_id_array_elem(var_id, array(T)) = T. 
:- func var_id_array_elem(var_id, var_id_offset, array(T)) = T. 

:- pred var_id_array_member(array(T), var_id, T).
:- mode var_id_array_member(in, in, out) is semidet.
:- mode var_id_array_member(in, out, out) is nondet.

:- pred var_id_array_member(array(T), var_id_offset, var_id, T).
:- mode var_id_array_member(in, in, in, out) is semidet.
:- mode var_id_array_member(in, in, out, out) is nondet.

:- pred var_id_array_set(var_id::in, T::in, array(T)::array_di, 
	array(T)::array_uo)	is det.
	
:- pred var_id_array_set(var_id::in, T::in, var_id_offset::in, 
	array(T)::array_di, array(T)::array_uo) is det.

% slow set
:- pred var_id_array_slow_set(var_id::in, T::in, array(T)::in,
	array(T)::array_uo)	is det.

:- pred var_id_array_slow_set(var_id::in, T::in, var_id_offset::in,
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
% Higher order Indexing by var_id

:- type container_size_pred(T) == pred(T, int).
:- inst container_size_pred == (pred(in, out) is det).

:- pred get_var_id_set(T::in, container_size_pred(T)::in(container_size_pred),
	var_id_set::out) is det.
:- func get_var_id_set(T::in, container_size_pred(T)::in(container_size_pred))
 =	(var_id_set::out) is det.

:- type get_pred(T, V) == pred(T, int, V).
:- inst get_pred_det == (pred(in, in, out) is det).
:- inst get_pred_semidet == (pred(in, in, out) is semidet).


:- pred var_id_index(T, var_id, get_pred(T, V), V).
:- mode var_id_index(in, in, in(get_pred_det), out) is det.
:- mode var_id_index(in, in, in(get_pred_semidet), out) is semidet.

:- func var_id_index(T, var_id, get_pred(T, V)) = V.
:- mode var_id_index(in, in, in(get_pred_det)) = out is det.
:- mode var_id_index(in, in, in(get_pred_semidet)) = out is semidet. 

:- type set_pred(T, V) == pred(int, V, T, T).
:- inst set_pred_det == (pred(in, in, in, out) is det).
:- inst set_pred_semidet == (pred(in, in, in, out) is semidet).
:- inst set_pred_unique == (pred(in, in, di, uo) is det).
:- inst set_pred_unique_semidet == (pred(in, in, di, uo) is semidet).

:- pred var_id_index(var_id, V, set_pred(T, V), T, T).
:- mode var_id_index(in, in, in(set_pred_det), in, out) is det.
:- mode var_id_index(in, in, in(set_pred_semidet), in, out) is semidet.
:- mode var_id_index(in, in, in(set_pred_unique), di, uo) is det.
:- mode var_id_index(in, in, in(set_pred_unique_semidet), di, uo) is semidet.

%-----------------------------------------------------------------------------%
% mh_var_set id_set conversion

:- pred generate_sparse_id_set_for_var_set(mh_var_set::in, var_id_set::out)
	is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- use_module term.
:- import_module require.
:- import_module string.
:- import_module bitmap.
:- import_module bool.
:- import_module solutions.

:- import_module mh_index.

%-----------------------------------------------------------------------------%
% Variable ID's

:- type var_id == int.

mr_var_id(V) = term.var_to_int(V).

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

var_id_order(ID1, ID2, IDL, IDG) :-
	(if ID1 > ID2
	then
		IDG = ID1,
		IDL = ID2
	else
		IDL = ID1,
		IDG = ID2
	).

%-----------------------------------------------------------------------------%
% Var ID Offsets

:- type var_id_offset == int.

var_id_offset(ID1, ID2, ID1 - ID2).

var_id_offset(ID1, ID2) = Offset :- var_id_offset(ID1, ID2, Offset).

apply_var_id_offset(ID1, Offset) = ID2 :- var_id_offset(ID1, ID2, Offset).

var_id_set_offset(Set, Offset, Set + Offset).

var_id_set_offset(Set, Offset) = Set + Offset.

null_var_id_offset = 0.

offset_from_id_set(S) = S - 1.
offset_from_arity(T) = arity(T).

offset_lt(Offset1, Offset2) :- Offset1 < Offset2.
offset_le(Offset1, Offset2) :- Offset1 =< Offset2.
offset_gt(Offset1, Offset2) :- Offset1 > Offset2.
offset_ge(Offset1, Offset2) :- Offset1 >= Offset2.

offset_order(O1, O2, OL, OG) :-
	(if O1 > O2
	then
		OG = O1,
		OL = O2
	else
		OL = O1,
		OG = O2
	).

%-----------------------------------------------------------------------------%
% Variable id sets

:- type var_id_set == int.

init_var_id_set = 0.

init_var_id_set(init_var_id_set).

empty_var_id_set = init_var_id_set.

var_id_set_from_offset(O) = O + 1.

var_id_count(Set, var_id_count(Set)).

var_id_count(Count) = Count.

valid_var_id_set(Set) :- Set >= 0.

empty_var_id_set(0).
not_empty_var_id_set(Set) :- Set > 0.

var_id_set_from_arity(T) = arity(T).

var_id_set_lt(Set1, Set2) :- Set1 < Set2.
var_id_set_le(Set1, Set2) :- Set1 =< Set2.
var_id_set_gt(Set1, Set2) :- Set1 > Set2.
var_id_set_ge(Set1, Set2) :- Set1 >= Set2.

var_id_set_order(Set1, Set2, SetL, SetG) :-
	(if Set1 > Set2
	then
		SetG = Set1,
		SetL = Set2
	else
		SetL = Set1,
		SetG = Set2
	).

require_valid_var_id_set(Set) :- 
	require(valid_var_id_set(Set), "Invalid var_id_set. Last var_id: "
	++ string(Set)	++	" was less than zero.").

expect_valid_var_id_set(Set, Module, Proc) :- 
	expect(valid_var_id_set(Set), Module, Proc, 
	"Invalid var_id_set. Last var_id: "	++ string(Set) 
	++ " was less than zero.").
	
append_var_id_sets(A, B, A + B).

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

contains_var_id( Offset::in, Last::in,ID::in) :- 
	ID > Offset,
	ID =< Last.
	
contains_var_id( Offset::in, Last::in,ID::out) :-
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
			ID_index =id_array_index(ID),
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
	ID_index = id_array_index(ID),
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

:- pragma inline(id_array_index/1).

:- func id_array_index(var_id) = int.
:- mode id_array_index(in) = out is det.
:- mode id_array_index(out) = in is det.

id_array_index(ID) = ID - 1.

:- pragma inline(id_array_index/2).

:- func id_array_index(var_id, var_id_offset) = int.
:- mode id_array_index(in, in) = out is det.
:- mode id_array_index(out, in) = in is det.
:- mode id_array_index(in, out) = in is det.

id_array_index(ID, Offset) = id_array_index(ID - Offset).

sparse_index_weight(ID, Offset, Set) = 
	( if var_id_gt(Index, Last)
	then next_var_id(Last)
	else Index
	) :- 
		Index = id_array_index(ID, Offset),
		Last = last_var_id(Set).

var_id_sparse_index(ID, Offset, Set) = sparse_index_weight(ID, Offset, Set).
	
reverse_sparse_index(Index, Offset, Set) = ID :-
	id_array_index(ID, Offset) = Index,
	contains_var_id(Offset, Set, ID).
	
id_set_weight(Set) = last_var_id(Set).
	
get_array_var_id_set(Array) = size(Array).

get_offset_array_var_id_set(Array, Offset) = size(Array) + Offset.

var_id_array_in_bounds(Array, ID) :- in_bounds(Array, id_array_index(ID)).

var_id_array_in_bounds(Array, Offset, ID) :- 
	in_bounds(Array, id_array_index(ID, Offset)).

var_id_array_lookup(Array, ID, T) :- lookup(Array, id_array_index(ID), T).
var_id_array_lookup(Array, Offset, ID, T) :- 
	lookup(Array, id_array_index(ID, Offset), T).

var_id_array_lookup(Array, ID) = lookup(Array, id_array_index(ID)).
var_id_array_lookup(Array, Offset, ID) = 
	lookup(Array, id_array_index(ID, Offset)).

var_id_array_semidet_lookup(Array, ID, T) :- 
	semidet_lookup(Array, id_array_index(ID), T).
var_id_array_semidet_lookup(Array, Offset, ID, T) :- 
	semidet_lookup(Array, id_array_index(ID, Offset), T).

var_id_array_semidet_lookup(Array, ID) = T :- 
	var_id_array_semidet_lookup(Array, ID, T).
var_id_array_semidet_lookup(Array, Offset, ID) = T :- 
	var_id_array_semidet_lookup(Array, Offset, ID, T).


var_id_array_elem(ID, Array) = elem(id_array_index(ID), Array).
var_id_array_elem(ID, Offset, Array) = elem(id_array_index(ID, Offset), Array).


:- pragma promise_equivalent_clauses(var_id_array_member/3).

var_id_array_member(Array::in, ID::in, T::out) :- 
	var_id_array_semidet_lookup(Array, ID, T).

var_id_array_member(Array::in, ID::out, T::out) :-
	nondet_int_in_range(array.min(Array), array.max(Array), Index),
	unsafe_lookup(Array, Index, T),
	id_array_index(ID) = Index.
	
var_id_array_member(Array, Offset, ID - Offset, T) :- 
	var_id_array_member(Array, ID, T).


var_id_array_set(ID, T, !Array) :- set(id_array_index(ID), T, !Array).

var_id_array_set(ID, T, Offset, !Array) :- 
	var_id_array_set(ID - Offset, T, !Array).

var_id_array_slow_set(ID, T, !Array) :- 
	slow_set(id_array_index(ID), T, !Array).

var_id_array_slow_set(ID, T, Offset, !Array) :- 
	var_id_array_slow_set(id_array_index(ID, Offset), T, !Array).

var_id_set_init_array(Last, T, A) :- array.init(Last, T, A).

var_id_set_init_array(Offset, Last, T, A) :- array.init(Last - Offset, T, A).

var_id_set_init_array(Set, T) = A :- var_id_set_init_array(Set, T, A).

var_id_set_init_array(Offset, Set, T) = 
	var_id_set_init_array(Set - Offset, T).
	

%-----------------------------------------------------------------------------%
% Higher order Indexing by var_id	

get_var_id_set(T, P, S) :- P(T, S).
get_var_id_set(T, P) = S :- get_var_id_set(T, P, S).

var_id_index(T, I, P, V) :- P(T, I, V).

var_id_index(T, I, P) = V :- var_id_index(T, I, P, V).

var_id_index(I, V, P, !T) :- P(I, V, !T).

%-----------------------------------------------------------------------------%
% mh_var_set id_set conversion

generate_sparse_id_set_for_var_set(VarSet, var_set_count(VarSet)). 