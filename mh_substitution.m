%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_substitution.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_substitution.

:- interface.

% :- import_module map.
:- import_module array.
% :- import_module enum.

:- import_module mh_term.
:- import_module mh_var_id.
:- import_module mh_var_map.

%-----------------------------------------------------------------------------%
% Substitutions

:- type mh_substitution
	--->	sub_map(mh_var_map(mh_term))
	;		ren_map(mh_var_map(var_id)).
	

	
:- pred init_sub(mh_substitution::out) is det.
:- func init_sub = mh_substitution.
	
:- pred empty_substitution(mh_substitution::in) is semidet.

%-----------------------------------------------------------------------------%
% Conversion

:- pred sub_from_array(array(mh_term)::in, mh_substitution::out) is det.
	
:- func sub_from_array(array(mh_term)) = mh_substitution.

:- pred sub_from_offset_array(array(mh_term)::in, var_id_offset::in, 
	mh_substitution::out) is det.
	
:- func sub_from_offset_array(array(mh_term), var_id_offset) = mh_substitution.


%-----------------------------------------------------------------------------%
% Bounds

:- pred substitution_bounds(mh_substitution::in, var_id_offset::out, 
	var_id_set::out) is semidet.
	
	
:- pred substitution_bounds_det(mh_substitution::in, var_id_offset::out, 
	var_id_set::out) is det.

	

%-----------------------------------------------------------------------------%
% Looking up variables in substitutions


% Succeed if the substitution can index the provided ID
% note that this will fail for any input with the ren_offset/1 constructor

:- pred sub_contains_id(mh_substitution::in, var_id::in) is semidet.


% Find a given variable ID in the substitution, fail if the id is not found
% If the substitution is a renaming, return the indexed var_id as a variable

:- pred sub_id_search(mh_substitution::in, var_id::in, mh_term::out) 
	is semidet.
:- func sub_id_search(mh_substitution, var_id) = mh_term is semidet.

% Find a given variable ID in the substitution if the id is not found return
% the variable indexed by the ID
:- pred sub_id_lookup(mh_substitution::in, var_id::in, mh_term::out) is det.
:- func sub_id_lookup(mh_substitution, var_id) = mh_term.

:- pred sub_var_search(mh_substitution::in, mh_var::in, mh_term::out) 
	is semidet.
:- func sub_var_search(mh_substitution, mh_var) = mh_term is semidet.

:- pred sub_var_lookup(mh_substitution::in, mh_var::in, mh_term::out) is det.
:- func sub_var_lookup(mh_substitution, mh_var) = mh_term.


%-----------------------------------------------------------------------------%
% Substitution composition

% compose_substitutions(S1, S2, S3) 
% Create a substitution that, when applied, has the same effect as applying
% S1 and then S2

:- pred compose_substitutions(mh_substitution::in, 
	mh_substitution::in, mh_substitution::out) is det.

:- func compose_substitutions(mh_substitution, mh_substitution) = 
	mh_substitution.


%-----------------------------------------------------------------------------%
% Renaming

:- inst mh_renaming
	--->	ren_map(ground).
	
:- type mh_renaming =< mh_substitution
	---> 	ren_map(mh_var_map(var_id)).
	
:- mode is_renaming == ground >> mh_renaming.

:- pred is_renaming(mh_substitution::is_renaming) is semidet.

:- pred init_ren(mh_renaming::out) is det.
:- func init_ren = mh_renaming.

:- pred empty_renaming(mh_renaming::in) is semidet.

%-----------------------------------------------------------------------------%
% Conversion

:- pred ren_from_array(array(var_id)::in, mh_renaming::out) is det.
	
:- func ren_from_array(array(var_id)) = mh_renaming.

:- pred ren_from_offset_array(array(var_id)::in, var_id_offset::in, 
	mh_renaming::out) is det.
	
:- func ren_from_offset_array(array(var_id), var_id_offset) = mh_renaming.

:- pred sub_to_renaming(mh_substitution::in, mh_renaming::out) is semidet.

:- func sub_to_renaming(mh_substitution) = mh_renaming is semidet.


%-----------------------------------------------------------------------------%
% Bounds

:- pred renaming_bounds(mh_renaming::in, var_id_offset::out, 
	var_id_set::out) is semidet.
	
	
:- pred renaming_bounds_det(mh_renaming::in, var_id_offset::out, 
	var_id_set::out) is det.


%-----------------------------------------------------------------------------%
% Looking up variables in renamings

:- pred ren_contains_id(mh_renaming::in, var_id::in) is semidet.

:- pred renaming_offset(mh_renaming::in, var_id_offset::out) is semidet.

:- pred ren_id_search(mh_renaming::in, var_id::in, var_id::out) 
	is semidet.
:- func ren_id_search(mh_renaming, var_id) = var_id is semidet.


:- pred ren_id_lookup(mh_renaming::in, var_id::in, var_id::out) is det.
:- func ren_id_lookup(mh_renaming, var_id) = var_id.


:- pred ren_var_search(mh_renaming::in, mh_var::in, var_id::out) is semidet.
:- func ren_var_search(mh_renaming, mh_var) = var_id is semidet.

:- pred ren_var_lookup(mh_renaming::in, mh_var::in, mh_var::out) is det.
:- func ren_var_lookup(mh_renaming, mh_var) = mh_var.

%-----------------------------------------------------------------------------%
% Renaming composition

% compose_renamings(R1, R2, R3) 
% Create a renaming that, when applied, has the same effect as applying
% R1 and then R2

:- pred compose_renamings(mh_renaming::in, 
	mh_renaming::in, mh_renaming::out) is det.

:- func compose_renamings(mh_renaming, mh_renaming) = 
	mh_renaming.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%
% 	Substitutions

init_sub(init_sub).
init_sub = sub_map(init).

empty_substitution(sub_map(init)).
empty_substitution(ren_map(init)).

%-----------------------------------------------------------------------------%
% Conversion

sub_from_array(Array, sub_from_array(Array)).

sub_from_array(Array) = sub_map(mh_var_map.from_array(Array)).

sub_from_offset_array(Array, Offset, sub_from_offset_array(Array, Offset)).

sub_from_offset_array(Array, Offset) = 
	sub_map(mh_var_map.from_offset_array(Array, Offset)).



%-----------------------------------------------------------------------------%
% Bounds

substitution_bounds(sub_array(Map),	Offset, Set) :-
	(if empty_var_map(Map) then fail
	else var_map_bounds(Map, Offset, Set)
	).
	
substitution_bounds(ren_array(Map),	Offset, Set) :- 
	(if empty_var_map(Map) then fail
	else var_map_bounds(Map, Offset, Set)
	).
	
substitution_bounds_det(sub_array(Map),	Offset, Set) :- 
	var_map_bounds(Map, Offset, Set)).
	
substitution_bounds_det(ren_array(Map),	Offset, Set) :- 
	var_map_bounds(Map, Offset, Set)).
	

%-----------------------------------------------------------------------------%
% Looking up variables in substitutions

sub_contains_id(sub_unbound, _) :- fail.
sub_contains_id(sub_array(Map), ID) :- contains_id(Map, ID).
sub_contains_id(ren_array(Map), ID) :- contains_id(Map, ID).

substitution_offset(ren_offset(Offset, _), Offset).
substitution_offset(ren_offset(Offset, _)) = Offset.

sub_id_search(sub_map(Map), ID) = id_search(Map, ID).
sub_id_search(ren_map(Map), ID) = var(id_search(Map, ID)).


sub_id_lookup(Sub, ID, Term) :-
	( if sub_id_search(Sub, ID, Found)
	then Term = Found
	else Term = var(ID)).
	
sub_id_lookup(Sub, ID) = Term :- sub_id_lookup(Sub, ID, Term).

sub_var_search(Sub, var(ID), Term) :- sub_id_search(Sub, ID, Term).
sub_var_search(Sub, Var) = Term :- sub_var_search(Sub, Var, Term).

sub_var_lookup(Sub, var(ID), Term) :- sub_id_lookup(Sub, ID, Term).
sub_var_lookup(Sub, Var) = Term :- sub_var_lookup(Sub, Var, Term).

	
%-----------------------------------------------------------------------------%
% Substitution composition

compose_substitutions(Sub1, Sub2, Sub3) :-
promise_equivalent_solutions [Sub3] 
	(
		empty_substitution(Sub1), Sub3 = Sub2
	;	
		empty_substitution(Sub2), Sub3 = Sub1
	;
		Sub1 = sub_map(Map1), Sub2 = sub_map(Map2),
		first(Map2, First, Iterator),
		compose_sub_sub
	
)

:- pred compose_sub_sub(mh_var_map(mh_term)::in, mh_var_map(mh_term)::in,
	var_map_iterator::in, mh_substitution::out) is det.

/* Depriciated
compose_substitutions(Sub1, Sub2, Sub) :-
	( if promise_equivalent_solutions [Sub3]
		compose_sub_special(Sub1, Sub2, Sub3)
	then
		Sub = Sub3
	else
		substitution_bounds_det(Sub1, Offset1, Set1),
		substitution_bounds_det(Sub2, Offset2, Set2),
		(offset_lt(Offset1, Offset2) -> Offset = Offset1 ; Offset = Offset2 ),
		(var_id_set_lt(Set1, Set2) -> Set = Set1 ; Set = Set2),
		var_id_set_init_array(Offset, Set, nil, Array0),
		compose_substiution_step(
			first_var_id(Offset), last_var_id(Set),
			Sub1, Sub2,
			Offset, Array0, Array1
		),
		init_array_sub(Array1, Offset, Sub)
	).
	
compose_substitutions(Sub1, Sub2) = Sub :- 
	compose_substitutions(Sub1, Sub2, Sub).
		
		
:- pred compose_sub_special(mh_substitution::in, mh_substitution::in,
	mh_substitution::out) is cc_nondet.


compose_sub_special(S, sub_empty, S).
compose_sub_special(sub_empty, S, S).

compose_sub_special(S, ren_empty, S).
compose_sub_special(ren_empty, S, S).



compose_sub_special(
	sub_single(ID1, Term1), 
	sub_single(ID2, Term2),
	Sub
) :-
	( if Term1 = var(ID2)
	then
		Sub = sub_single(ID1, Term2)
	else 
		compare(Comp, ID1, ID2),
		(
			Comp = (=),
			Sub = sub_single(ID1, Term1)
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
			init_array_sub(A2, Offset, Sub)
		)
		
	).
	
	
compose_sub_special(
	ren_single(ID1, Var1),
	sub_single(ID2, Term2),
	Sub
) :- 
	( if ID2 = Var1
	then
		Sub = sub_single(ID1, Term2)
	else 
		compare(Comp, ID1, ID2),
		(
			Comp = (=),
			Sub = ren_single(ID1, Var1)
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
			init_array_sub(A2, Offset, Sub)
		)
		
	).

compose_sub_special(
	sub_single(ID1, Term1),
	ren_single(ID2, Var2),
	Sub
) :-
	( if Term1 = var(ID2)
	then
		Sub = ren_single(ID1, Var2)
	else 
		compare(Comp, ID1, ID2),
		promise_equivalent_solutions [Sub]
		(
			Comp = (=),
			Sub = sub_single(ID1, Term1)
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
			init_array_sub(A2, Offset, Sub)
		)
		
	).

	
:- pred compose_substiution_step(
	var_id, var_id,
	mh_substitution, mh_substitution,
	var_id_offset, array(mh_term), array(mh_term)
).

:- mode compose_substiution_step(
	in, in, 
	in, in, 
	in, array_di, array_uo) is det.
	
compose_substiution_step(
	Current, Last,
	Sub1, Sub2,
	Offset, !Array
) :-
	% Look up the current id in the first sub, if the found term is a variable
	% pass it through the second sub, if it is not a variable, 
	(if sub_id_search(Sub1, Current, Term1)
	then
		(if	Term1 = var(Term1_ID)
		then
			(if sub_id_search(Sub2, Term1_ID, Term2)
			then
				var_id_set(Current, Term2, Offset, !Array)
			else
				var_id_set(Current, Term1, Offset, !Array)
			)
		else 
			var_id_set(Current, Term1, Offset, !Array)
		)
	else
		(if sub_id_search(Sub2, Current, Term)
		then
			var_id_set(Current, Term, Offset, !Array)
		else
			!:Array = !.Array
		)
	),
	
	(if var_id_ge(Current, Last)
	then true
	else
		compose_substiution_step(
			next_var_id(Current), Last, 
			Sub1, Sub2, 
			Offset, !Array)
	).

*/ 	
	
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
% Conversion
	
ren_from_array(Array, ren_from_array(Array)).

ren_from_array(Array) = ren_map(mh_var_map.from_array(Array)).

ren_from_offset_array(Array, Offset, ren_from_offset_array(Array, Offset)).

ren_from_offset_array(Array, Offset) = 
	ren_map(mh_var_map.from_offset_array(Array, Offset)).
	
%-----------------------------------------------------------------------------%
% Bounds


renaming_bounds(ren_array(Map),	Offset, Set) :- 
	(if empty_var_map(Map) then fail
	else var_map_bounds(Map, Offset, Set)
	).

	
renaming_bounds_det(ren_array(Map),	Offset, Set) :- 
	var_map_bounds(Map, Offset, Set)).
	
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
% Renaming composition

compose_renamings(Ren1, Ren2, Ren3) :-
promise_equivalent_solutions [Ren3] 
	(
		empty_renaming(Ren1), Ren3 = Ren2
	;	
		empty_renaming(Ren2), Ren3 = Ren1
	;
		Ren1 = Ren_map(Map1), Ren2 = Ren_map(Map2),
	
)

:- pred compose((func(T) = var_id), mh_var_map(T), var_id, T, var_map_iterator, 
	mh_var_map(T), mh_var_map(T)).
:- mode compose(in(func(in) = out is semidet), in, in, in, in, in, out) is det.
:- mode compose(in(func(in) = out is det), in, in, in, in, in, out) is det.

compose(ToVar, Map2, ID, T,  Iter, !Map) :-
	(if )

:- func identity(X) = X.

identity(X) = X.

%-----------------------------------------------------------------------------%
% Utility

:- pred sub_out_of_range(var_id::in, string::in) is erroneous.

sub_out_of_range(ID, Container) :-
	error("error: var_id #" ++ string(ID) ++ 
		" not found in "++ Container).