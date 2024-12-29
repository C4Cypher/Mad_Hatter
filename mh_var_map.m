%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_var_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_var_map.

:- interface.

% :- import_module map.
:- import_module array.
% :- import_module enum.

:- import_module mh_term.
:- import_module mh_var_id.
:- import_module var_set.

%-----------------------------------------------------------------------------%
% Variable map


:- type mh_var_map(T).
	
:- pred init_var_map(mh_var_map::out) is det.
:- func init_var_map = mh_var_map.
	
:- pred empty_var_map(mh_var_map::in) is semidet.

:- pred single

% var_map_bounds(var_mapsitution, Min, Max)
% Return the minimum and maximum var_id's indexed by var_map, fail if the
% var_map is empty
:- pred var_map_bounds(mh_var_map::in, var_id_offset::out, 
	var_id_set::out) is semidet.
	
:- pred var_map_bounds_det(mh_var_map::in, var_id_offset::out,
	var_id_set::out) is det.

	

%-----------------------------------------------------------------------------%
% Looking up variable id's in var_maps


% Succeed if the var_map can index the provided ID
% note that this will fail for any input with the ren_offset/1 constructor

:- pred var_map_contains_id(mh_var_map::in, var_id::in) is semidet.


% Find a given variable ID in the var_map, fail if the id is not found
% If the var_map is a renaming, return the indexed var_id as a variable

:- pred var_map_id_search(mh_var_map(T)::in, var_id::in, T::out) 
	is semidet.
:- func var_map_id_search(mh_var_map(T), var_id) = mh_term is semidet.

% Find a given variable ID in the var_map if the id is not found return
% the variable indexed by the ID
:- pred var_map_id_lookup(mh_var_map(T)::in, var_id::in, mh_term::out) is det.
:- func var_map_id_lookup(mh_var_map(T), var_id) = mh_term.

:- pred var_map_search(mh_var_map(T)::in, mh_var::in, mh_term::out) 
	is semidet.
:- func var_map_search(mh_var_map(T), mh_var) = mh_term is semidet.

:- pred var_map_lookup(mh_var_map(T)::in, mh_var::in, mh_term::out) is det.
:- func var_map_lookup(mh_var_map(T), mh_var) = mh_term.


%-----------------------------------------------------------------------------%
% var_map composition TODO remove?

% compose_var_maps(S1, S2, S3) 
% Create a var_map that, when applied, has the same effect as applying
% S1 and then S2

:- pred compose_var_maps(mh_var_map::in, 
	mh_var_map::in, mh_var_map::out) is det.

:- func compose_var_maps(mh_var_map, mh_var_map) = 
	mh_var_map.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%
%  Var Maps

% Var maps are represented by a sparse array indexed by a var_set

:- type mh_var_map(T).
	--->	var_map_empty
	;		var_map(set::mh_var_set, array::array(T)).

init_var_map(init_var_map).
init_var_map = var_map_empty.

init_array_var_map(Array, Offset, var_map) :-
	(if Offset = null_var_id_offset
	then
		var_map = var_map_array(Array)
	else
		var_map = var_map_array(Array, Offset)
	).

init_array_var_map(Array, Offset) = var_map :- init_array_var_map(Array, Offset, var_map).	

empty_var_map(var_map_empty).
empty_var_map(var_map_array(make_empty_array)).
empty_var_map(var_map_array(make_empty_array, _)).
empty_var_map(ren_empty).
empty_var_map(ren_array(make_empty_array)).
empty_var_map(ren_array(make_empty_array, _)).
empty_var_map(ren_offset(null_var_id_offset, _)).

single_var_map(var_map_single(_, _)).
single_var_map(ren_single(_, _)).

array_var_map(var_map_array(_)).
array_var_map(var_map_array(_, _)).
array_var_map(ren_array(_)).
array_var_map(ren_array(_, _)).

offset_var_map(ren_offset(_, _)).



var_map_bounds(var_map_single(ID, _), Offset, Set) :-  
	ID = first_var_id(Offset),
	ID = last_var_id(Set).

var_map_bounds(var_map_array(Array), 
	null_var_id_offset, 
	array_var_id_set(Array)
).
var_map_bounds(var_map_array(Array, Offset), 
	Offset,
	offset_array_var_id_set(Array, Offset)
).
var_map_bounds(ren_single(ID, _), Offset, Set) :-
	ID = first_var_id(Offset),
	ID = last_var_id(Set).

var_map_bounds(ren_array(Array), 
	null_var_id_offset, 
	array_var_id_set(Array)
).

var_map_bounds(ren_array(Array, Offset), 
	Offset,
	offset_array_var_id_set(Array, Offset)
).

var_map_bounds(ren_offset(Offset, Set), Min, Set) :- 
	( offset_le(Offset, null_var_id_offset) -> 
		Min = null_var_id_offset ; 
		Min = Offset ).
	
var_map_bounds_det(var_map, Offset, Set) :-
	(if var_map_bounds(var_map, Offset0, Set0)
	then
		Offset = Offset0,
		Set = Set0
	else error($pred, 
		"Empty var_maps do not have bounds, check for empty var_map"
		)
	).
	
%-----------------------------------------------------------------------------%
% Looking up variables in var_maps

var_map_contains_id(var_map_empty, _) :- fail.
var_map_contains_id(var_map_single(ID, _), ID).

var_map_contains_id(var_map_array(Array), ID) :- 
	var_id_in_bounds(Array, ID),
	not var_id_lookup(Array, ID, nil).

var_map_contains_id(var_map_array(Array, Offset), ID1) :- 
	var_id_offset(ID1, ID2, Offset),
	var_id_in_bounds(Array, ID2),
	not var_id_lookup(Array, ID2, nil).
	
var_map_contains_id(ren_empty, _) :- fail.
var_map_contains_id(ren_single(ID, _), ID).

var_map_contains_id(ren_array(Array), ID1) :- 
	var_id_semidet_lookup(Array, ID1, ID2),
	var_id_gt(ID2, null_var_id).

var_map_contains_id(ren_array(Array, Offset), ID1) :- 
	var_id_offset(ID1, ID2, Offset), 
	var_id_semidet_lookup(Array, ID2, ID3),
	var_id_gt(ID3, null_var_id).
	
var_map_contains_id(ren_offset(Offset, Set), ID) :- 
	contains_var_id(Offset, Set, ID).

var_map_offset(ren_offset(Offset, _), Offset).
var_map_offset(ren_offset(Offset, _)) = Offset.

%-----------------------------------------------------------------------------%


var_map_id_search(var_map_empty, _, nil) :- fail.

var_map_id_search(var_map_single(ID, Term), ID, Term).


var_map_id_search(var_map_array(Array), ID, Term)  :- 
	var_id_semidet_lookup(Array, ID, Term),
	Term \= nil.
	
var_map_id_search(var_map_array(Array, Offset), ID, Term) :-
	var_id_semidet_lookup(Array, Offset, ID, Term),
	Term \= nil.
	
var_map_id_search(ren_empty, _, nil) :- fail.
 
var_map_id_search(ren_single(ID1, ID2), ID1, var(ID2)).
	
var_map_id_search(ren_array(Array), ID1, var(ID2)) :- 
	var_id_semidet_lookup(Array, ID1, ID2),
	var_id_gt(ID2, null_var_id).
	
var_map_id_search(ren_array(Array, Offset), ID1, var(ID2)) :-
	var_id_semidet_lookup(Array, Offset, ID1, ID2),
	var_id_gt(ID2, null_var_id).
	
var_map_id_search(ren_offset(Offset, Set), !.ID, var(!:ID) ) :- 
	contains_var_id(Set, !.ID),
	var_id_offset(!ID, Offset).

var_map_id_search(var_map, ID) = Term :- var_map_id_search(var_map, ID, Term).

%-----------------------------------------------------------------------------%

var_map_id_lookup(var_map, ID, Term) :-
	( if var_map_id_search(var_map, ID, Found)
	then Term = Found
	else Term = var(ID)).
	
var_map_id_lookup(var_map, ID) = Term :- var_map_id_lookup(var_map, ID, Term).

%-----------------------------------------------------------------------------%

var_map_search(var_map, var(ID), Term) :- var_map_id_search(var_map, ID, Term).

var_map_search(var_map, Var) = Term :- var_map_search(var_map, Var, Term).

%-----------------------------------------------------------------------------%

var_map_lookup(var_map, var(ID), Term) :- var_map_id_lookup(var_map, ID, Term).

var_map_lookup(var_map, Var) = Term :- var_map_lookup(var_map, Var, Term).

	
%-----------------------------------------------------------------------------%
% var_map composition

compose_var_maps(var_map1, var_map2, var_map) :-
	( if promise_equivalent_solutions [var_map3]
		compose_var_map_special(var_map1, var_map2, var_map3)
	then
		var_map = var_map3
	else
		var_map_bounds_det(var_map1, Offset1, Set1),
		var_map_bounds_det(var_map2, Offset2, Set2),
		(offset_lt(Offset1, Offset2) -> Offset = Offset1 ; Offset = Offset2 ),
		(var_id_set_lt(Set1, Set2) -> Set = Set1 ; Set = Set2),
		var_id_set_init_array(Offset, Set, nil, Array0),
		compose_var_mapstiution_step(
			first_var_id(Offset), last_var_id(Set),
			var_map1, var_map2,
			Offset, Array0, Array1
		),
		init_array_var_map(Array1, Offset, var_map)
	).
	
compose_var_maps(var_map1, var_map2) = var_map :- 
	compose_var_maps(var_map1, var_map2, var_map).
		
		
:- pred compose_var_map_special(mh_var_map::in, mh_var_map::in,
	mh_var_map::out) is cc_nondet.


compose_var_map_special(S, var_map_empty, S).
compose_var_map_special(var_map_empty, S, S).

compose_var_map_special(S, ren_empty, S).
compose_var_map_special(ren_empty, S, S).



compose_var_map_special(
	var_map_single(ID1, Term1), 
	var_map_single(ID2, Term2),
	var_map
) :-
	( if Term1 = var(ID2)
	then
		var_map = var_map_single(ID1, Term2)
	else 
		compare(Comp, ID1, ID2),
		(
			Comp = (=),
			var_map = var_map_single(ID1, Term1)
		;
			(
				Comp = (>),
				first_var_id(Offset) = ID2,
				last_var_id(Set) = ID1
			;
				Comp = (<),
				first_var_id(Offset) = ID1,
				last_var_id(Set) = ID2
			),
			var_id_set_init_array(Offset, Set, nil, A0),
			var_id_set(ID1, Term1, Offset, A0, A1),
			var_id_set(ID2, Term2, Offset, A1, A2),
			init_array_var_map(A2, Offset, var_map)
		)
		
	).
	
	
compose_var_map_special(
	ren_single(ID1, Var1),
	var_map_single(ID2, Term2),
	var_map
) :- 
	( if ID2 = Var1
	then
		var_map = var_map_single(ID1, Term2)
	else 
		compare(Comp, ID1, ID2),
		(
			Comp = (=),
			var_map = ren_single(ID1, Var1)
		;
			(
				Comp = (>),
				first_var_id(Offset) = ID2,
				last_var_id(Set) = ID1
			;
				Comp = (<),
				first_var_id(Offset) = ID1,
				last_var_id(Set) = ID2
			),
			var_id_set_init_array(Offset, Set, nil, A0),
			var_id_set(ID1, var(Var1), Offset, A0, A1),
			var_id_set(ID2, Term2, Offset, A1, A2),
			init_array_var_map(A2, Offset, var_map)
		)
		
	).

compose_var_map_special(
	var_map_single(ID1, Term1),
	ren_single(ID2, Var2),
	var_map
) :-
	( if Term1 = var(ID2)
	then
		var_map = ren_single(ID1, Var2)
	else 
		compare(Comp, ID1, ID2),
		promise_equivalent_solutions [var_map]
		(
			Comp = (=),
			var_map = var_map_single(ID1, Term1)
		;
			(
				Comp = (>),
				first_var_id(Offset) = ID2,
				last_var_id(Set) = ID1
			;
				Comp = (<),
				first_var_id(Offset) = ID1,
				last_var_id(Set) = ID2
			),
			var_id_set_init_array(Offset, Set, nil, A0),
			var_id_set(ID1, Term1, Offset, A0, A1),
			var_id_set(ID2, var(Var2), Offset, A1, A2),
			init_array_var_map(A2, Offset, var_map)
		)
		
	).

	
:- pred compose_var_mapstiution_step(
	var_id, var_id,
	mh_var_map, mh_var_map,
	var_id_offset, array(mh_term), array(mh_term)
).

:- mode compose_var_mapstiution_step(
	in, in, 
	in, in, 
	in, array_di, array_uo) is det.
	
compose_var_mapstiution_step(
	Current, Last,
	var_map1, var_map2,
	Offset, !Array
) :-
	% Look up the current id in the first var_map, if the found term is a variable
	% pass it through the second var_map, if it is not a variable, 
	(if var_map_id_search(var_map1, Current, Term1)
	then
		(if	Term1 = var(Term1_ID)
		then
			(if var_map_id_search(var_map2, Term1_ID, Term2)
			then
				var_id_set(Current, Term2, Offset, !Array)
			else
				var_id_set(Current, Term1, Offset, !Array)
			)
		else 
			var_id_set(Current, Term1, Offset, !Array)
		)
	else
		(if var_map_id_search(var_map2, Current, Term)
		then
			var_id_set(Current, Term, Offset, !Array)
		else
			!:Array = !.Array
		)
	),
	
	(if var_id_ge(Current, Last)
	then true
	else
		compose_var_mapstiution_step(
			next_var_id(Current), Last, 
			var_map1, var_map2, 
			Offset, !Array)
	).
	
	
%-----------------------------------------------------------------------------%
% Renaming

is_renaming(ren_empty). 
is_renaming(ren_single(_, _) ).
is_renaming(ren_array(_) ).
is_renaming(ren_array(_, _) ).
is_renaming(ren_offset(_, _)).

init_ren(init_ren).
init_ren = ren_empty.

empty_renaming(ren_empty).
empty_renaming(ren_array(make_empty_array)).
empty_renaming(ren_array(make_empty_array, _)).
empty_renaming(ren_offset(null_var_id_offset, _)).


%-----------------------------------------------------------------------------%
% Looking up variables in renamings

ren_contains_id(ren_empty, _) :- fail.
ren_contains_id(ren_single(ID, _), ID).

ren_contains_id(ren_array(Array), ID1) :- 
	var_id_semidet_lookup(Array, ID1, ID2),
	var_id_gt(ID2, null_var_id).

ren_contains_id(ren_array(Array, Offset), ID1) :- 
	var_id_semidet_lookup(Array, Offset, ID1, ID2),
	var_id_gt(ID2, null_var_id).
	
ren_contains_id(ren_offset(Offset, Set), ID) :- 
	contains_var_id(Offset, Set, ID).


renaming_offset(ren_offset(Offset, _), Offset).

%-----------------------------------------------------------------------------%


ren_id_search(ren_empty, _, null_var_id) :- fail.

ren_id_search(ren_single(ID1, ID2), ID1, ID2).
	
ren_id_search(ren_array(Array), ID1, ID2) :- 
	var_id_semidet_lookup(Array, ID1, ID2),
	var_id_gt(ID2, null_var_id).
	

ren_id_search(ren_array(Array, Offset), ID1, ID2) :-
	var_id_semidet_lookup(Array, Offset, ID1, ID2),
	var_id_gt(ID2, null_var_id).
	
ren_id_search(ren_offset(Offset, Set), !ID) :- 
	contains_var_id(Set, !.ID),
	var_id_offset(!ID, Offset).

ren_id_search(Ren, !.ID) = !:ID :- ren_id_search(Ren, !ID).

%-----------------------------------------------------------------------------%

ren_id_lookup(Ren, !ID) :-
	( if ren_id_search(Ren, !.ID, Found)
	then !:ID = Found
	else !:ID = !.ID).
	
ren_id_lookup(Ren, !.ID) = !:ID :- ren_id_lookup(Ren, !ID).

%-----------------------------------------------------------------------------%

ren_var_search(Ren, var(ID1), ID2) :- ren_id_search(Ren, ID1, ID2).

ren_var_search(Ren, Var) = ID :- ren_var_search(Ren, Var, ID).

%-----------------------------------------------------------------------------%

ren_var_lookup(Ren, var(!.ID), var(!:ID)) :- ren_id_lookup(Ren, !ID).

ren_var_lookup(Ren, !.Var) = !:Var :- ren_var_lookup(Ren, !Var).


%-----------------------------------------------------------------------------%
% Utility

:- pred var_map_out_of_range(var_id::in, string::in) is erroneous.

var_map_out_of_range(ID, Container) :-
	error("error: var_id #" ++ string(ID) ++ 
		" not found in "++ Container).