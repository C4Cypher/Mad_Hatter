%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_substitution.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_substitution.

:- interface.

:- import_module map.
:- import_module array.
% :- import_module enum.

:- import_module mh_term.
:- import_module mh_var_id.

%-----------------------------------------------------------------------------%
% Substitutions

:- type mh_substitution
	--->	sub_empty
	;		sub_single(var_id, mh_term)
	;		sub_array(array(mh_term))
	;		sub_array(array(mh_term), var_id_offset)
	;		ren_empty
	;		ren_single(var_id, var_id)
	;		ren_array(array(var_id))
	;		ren_array(array(var_id), var_id_offset)
	;		ren_offset(var_id_offset, var_id_set).
	

	
:- pred init_sub(mh_substitution::out) is det.
:- func init_sub = mh_substitution.
	
:- pred empty_substitution(mh_substitution::in) is semidet.

:- pred single_substitution(mh_substiution::in) is semidet.

:- pred array_substitution(mh_substituion::in) is semidet.

:- pred offset_substituion(mh_substition::in) is semidet.

% substitution_bounds(Subsitution, Min, Max)
% Return the minimum and maximum var_id's indexed by Substitution, fail if the
% Substituion is empty
:- pred substitution_bounds(mh_substitution::in, var_id_offset::out, 
	var_id_set::out) is semidet.
	

%-----------------------------------------------------------------------------%
% Looking up variables in substitutions


% Succeed if the substitution can index the provided ID
% note that this will fail for any input with the ren_offset/1 constructor

:- pred sub_contains_id(mh_substitution::in, var_id::in) is semidet.

% Get the offset of a substitution, if it has one

:- pred substitution_offset(mh_substiution::in, var_id_offset::out) is semidet.


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

:- pred sub_quantified_search(mh_substitution::in, quantified_var::in, 
	mh_term::out) is semidet.
:- func sub_quantified_search(mh_substitution, quantified_var) = mh_term
	is semidet.
	
:- pred sub_quantified_lookup(mh_substitution::in, quantified_var::in, 
	mh_term::out) is det.
:- func sub_quantified_lookup(mh_substitution, quantified_var) = mh_term.


%-----------------------------------------------------------------------------%
% Substitution composition

% compose_substitutions(S2, S1, S3) 
% Create a substitution that, when applied, has the same effect as applying
% S1 and then S2

:- pred compose_substitutions(mh_substitution::in, 
	mh_substitution::in, mh_substitution::out) is det.

:- func compose_substitutions(mh_substitution, mh_substitition) = 
	mh_substitution.


%-----------------------------------------------------------------------------%
% Renaming

:- inst mh_renaming
	--->	ren_empty
	;		ren_single(ground, ground)
	;		ren_array(ground)
	;		ren_offset(ground)
	;		ren_offset(ground, ground).
	
:- type mh_renaming =< mh_substitution
	---> 	ren_empty
	;		ren_single(var_id, var_id)
	;		ren_array(array(var_id))
	;		ren_offset(var_id_offset)
	;		ren_offset(array(var_id), var_id_offset).
	
:- mode is_renaming == ground >> mh_renaming.

:- pred is_renaming(mh_substitution::is_renaming) is semidet.

:- pred init_ren(mh_renaming::out) is det.
:- func init_ren = mh_renaming.

:- pred empty_renaming(mh_renaming::in) is semidet.
:- mode empty_renaming(in) is semidet.
:- mode empty_renaming(out) is cc_multi.

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

:- pred ren_quantified_search(mh_renaming::in, quantified_var::in, 
	var_id::out) is semidet.
:- func ren_quantified_search(mh_renaming, quantified_var) = var_id is semidet.

:- pred ren_quantified_lookup(mh_renaming::in, quantified_var::in, 
	quantified_var::out) is det.
:- func ren_quanitifed_lookup(mh_renaming, quantified_var) = quantified_var.

%-----------------------------------------------------------------------------%
% Substitutible typeclass

:- typeclass substitutable(T) where [
	pred apply_substitution(mh_substiution::in, T::in, T::out) is det
].	

:- func apply_substitution(mh_substitution, T) = T <= substitutable(T).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%
% 	Substitutions

init_sub(init_sub).
init_sub = sub_empty.

empty_substitution(sub_empty).
empty_substitution(sub_array(make_empty_array)).
empty_substitution(sub_array(make_empty_array, _)).
empty_substitution(ren_empty).
empty_substitution(ren_array(make_empty_array)).
empty_substitution(ren_array(make_empty_array, _)).
empty_substitution(ren_offset(null_var_id_offset, _)).

single_substitution(sub_single(_, _)).
single_substitution(ren_single(_, _)).

array_substitution(sub_array(_)).
array_substitution(sub_array(_, _)).
array_substitution(ren_array(_)).
array_substitution(ren_array(_, _)).

offset_substituion(ren_offset(_, _)).



substitution_bounds(sub_single(ID, _), Offset, Set) :-  
	ID = first_var_id(Offset),
	ID = last_var_id(Set).

substitution_bounds(sub_array(Array), 
	null_var_id_offset, 
	array_var_id_set(Array)
).
substitution_bounds(sub_array(Array, Offset), 
	Offset,
	offset_array_var_id_set(Array, Offset)
).
substitution_bounds(ren_single(ID, _), Offset, Set) :-
	ID = first_var_id(Offset),
	ID = last_var_id(Set).

substitution_bounds(ren_array(Array), 
	null_var_id_offset, 
	array_var_id_set(Array)
).

substitution_bounds(ren_array(Array, Offset), 
	Offset,
	offset_array_var_id_set(Array, Offset)
).

substitution_bounds(ren_offset(Offset, Set), Min, Set) :- 
	( Offset =< null_var_id_offset -> 
		Min = null_var_id_offset ; 
		Min = Offset ).
	
	
	
%-----------------------------------------------------------------------------%
% Looking up variables in substitutions

sub_contains_id(sub_empty, _) :- fail.
sub_contains_id(sub_single(ID, _), ID).

sub_contains_id(sub_array(Array), ID) :- 
	var_id_in_bounds(Array, ID),
	not var_id_lookup(Array, ID, nil).

sub_contains_id(sub_array(Array, Offset), ID1) :- 
	var_id_offset(ID1, ID2, Offset),
	var_id_in_bounds(Array, ID2),
	not var_id_lookup(Array, ID2, nil).
	
sub_contains_id(ren_empty, _) :- fail.
sub_contains_id(ren_single(ID, _), ID).

sub_contains_id(ren_array(Array), ID1) :- 
	var_id_semidet_lookup(Array, ID1, ID2),
	ID2 > 0.

sub_contains_id(ren_array(Array, Offset), ID1) :- 
	var_id_offset(ID1, ID2, Offset), 
	var_id_semidet_lookup(Array, ID2, ID3),
	ID3 > 0.
	
sub_contains_id(ren_offset(Offset, Set), ID) :- 
	contains_var_id(Set, Offset, ID).

substitution_offset(ren_offset(Offset), Offset).

%-----------------------------------------------------------------------------%


sub_id_search(sub_empty, _, nil) :- fail.

sub_id_search(sub_single(ID, Term), ID, Term).


sub_id_search(sub_array(Array), ID, Term)  :- 
	var_id_semidet_lookup(Array, ID, Term),
	Term \= nil.
	
sub_id_search(sub_array(Array, Offset), ID1, Term) :-
	var_id_semidet_lookup(Array, Offset, ID2, Term),
	Term \= nil.
	
sub_id_search(ren_empty, _, nil) :- fail.
 
sub_id_search(ren_single(ID1, ID2), ID1, var(ID2)).
	
sub_id_search(ren_array(Array), !.ID, var(!:ID)) :- 
	var_id_semidet_lookup(Array, !ID),
	!:ID > 0.
	
sub_id_search(ren_array(Array, Offset), !.ID, var(!:ID)) :-
	var_id_semidet_lookup(Array, Offset, ID2, Term),
	!:ID > 0.
	
sub_id_search(ren_offset(Offset, Set), !.ID, var(!:ID) ) :- 
	contains_var_id(Set, !.ID),
	var_id_offset(!ID, Offset).

sub_id_search(Sub, ID) = Term :- sub_id_search(Sub, ID, Term).

%-----------------------------------------------------------------------------%

sub_id_lookup(Sub, ID, Term) :-
	( if sub_id_search(Sub, ID, Found)
	then Term = Found
	else Term = var(ID)).
	
sub_id_lookup(Sub, ID) = Term :- sub_id_lookup(Sub, ID, Term).

%-----------------------------------------------------------------------------%

sub_var_search(Sub, var(ID), Term) :- sub_id_search(Sub, ID, Term).

sub_var_search(Sub, Var) = Term :- sub_var_search(Sub, Var, Term).

%-----------------------------------------------------------------------------%

sub_var_lookup(_, anonymous, anonymous).

sub_var_lookup(Sub, var(ID), Term) :- sub_id_lookup(Sub, ID, Term).

sub_var_lookup(Sub, Var) = Term :- sub_var_lookup(Sub, Var, Term).

%-----------------------------------------------------------------------------%

sub_quantified_search(Sub, var(ID), Term) :- sub_id_search(Sub, ID, Term).

sub_quantified_search(Sub, Var) = Term :- 
	sub_quantified_search(Sub, Var, Term).
	
%-----------------------------------------------------------------------------%

sub_quantified_lookup(Sub, var(ID), Term) :- sub_id_lookup(Sub, ID, Term).

sub_quantified_lookup(Sub, Var) = Term :- 
	sub_quantified_lookup(Sub, Var, Term).


	
%-----------------------------------------------------------------------------%
% Substitution composition

compose_substitutions(Sub2, Sub1, Sub) :-
	( if compose_sub_special(Sub2, Sub1, Sub3)
	then
		Sub = Sub3
	else
		substitution_bounds(Sub1, Offset1, Set1),
		substitution_bounds(Sub2, Offset2, Set2),
		(Offset1 < Offset2 -> Offset = Offset1 ; Offset = Offset2 ),
		(Set1 < Set2 -> Set = Set1 ; Set = Set2),
		var_id_set_init_array(Offset, Set, nil, !.Array),
		compose_substiution_step(
			first_var_id(Offset), last_var_id(Set)
			Sub2, Sub1,
			Offset, !Array
		),
		(If Offset = null_var_id_offset
		then
			Sub = sub_array(!:Array)
		else
			Sub = sub_array(!:Array, Offset)
		)
	).
		
		
:- pred compose_sub_special(mh_substition::in, mh_substiution::in,
	mh_substiutiton::out) is semidet.

:- pragma promise_equivalent_clauses(compose_sub_special/3).


compose_sub_special(S, sub_empty, S).
compose_sub_special(sub_empty, S, S).

compose_sub_special(S, ren_empty, S).
compose_sub_special(ren_empty, S, S).



compose_sub_special(
	sub_single(ID2, Term2), 
	sub_single(ID1, Term1),
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
			var_id_set_init_array(Offset, Set, nil, !.Array),
			var_id_set(ID1, Term1, Offset, !Array),
			var_id_set(ID2, Term2, Offset, !Array),
			( if Offset = null_var_id_offset
			then Sub = sub_array(!:Array)
			else Sub = sub_array(!:Array, Offset)
			)
		)
		
	).
	
compose_sub_special(
	sub_single(ID2, Term2),
	ren_single(ID1, Var1),
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
			var_id_set_init_array(Offset, Set, nil, !.Array),
			var_id_set(ID1, var(Var1), Offset, !Array),
			var_id_set(ID2, Term2, Offset, !Array),
			( if Offset = null_var_id_offset
			then Sub = sub_array(!:Array)
			else Sub = sub_array(!:Array, Offset)
			)
		)
		
	).

compose_sub_special(
	ren_single(ID2, Var2),
	sub_single(ID1, Term1),
	Sub
) :-
	( if Term1 = var(ID2)
	then
		Sub = ren_single(ID1, Var2)
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
			var_id_set_init_array(Offset, Set, nil, !.Array),
			var_id_set(ID1, Term1, Offset, !Array),
			var_id_set(ID2, var(Var2), Offset, !Array),
			( if Offset = null_var_id_offset
			then Sub = sub_array(!:Array)
			else Sub = sub_array(!:Array, Offset)
			)
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
	Current, Last
	Sub1, Sub2
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
	
	(if Current => Last
	then true
	else
		compose_substiution_step(
			next_var_id(Current), Last, 
			Sub2, Sub1, 
			!Array)
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




%-----------------------------------------------------------------------------%

ren_contains_id(ren_empty, _) :- fail.
ren_contains_id(ren_single(ID, _), ID).

ren_contains_id(ren_array(Array), ID1) :- 
	var_id_semidet_lookup(Array, ID1, ID2).
	ID2 > 0..

ren_contains_id(ren_array(Array, Offset), ID1) :- 
	var_id_semidet_lookup(Array, Offset, ID1, ID2),
	ID2 > 0.
	
ren_contains_id(ren_offset(Offset, Set), ID) :- 
	contains_var_id(Set, Offset, ID).


renaming_offset(ren_offset(Offset, _), Offset).

%-----------------------------------------------------------------------------%


ren_id_search(ren_empty, _, null_var_id) :- fail.

ren_id_search(ren_single(ID1, ID2), ID1, ID2).
	
ren_id_search(ren_array(Array), !ID) :- 
	var_id_semidet_lookup(Array, !ID),
	!:ID > 0.
	

ren_id_search(ren_array(Array, Offset), !ID) :-
	var_id_semidet_lookup(Array, Offset, ID2, Term),
	!:ID > 0.
	
ren_id_search(ren_offset(Offset), !ID) :- 
	contains_var_id(Set, !.ID),
	var_id_offset(!ID, Offset).

ren_id_search(Ren, !.ID) = !:ID :- ren_id_search(Ren, !ID).

%-----------------------------------------------------------------------------%

ren_id_lookup(Ren, !ID) :-
	( if ren_id_search(Sub, !.ID, Found)
	then !:ID = Found
	else !:ID = !.ID).
	
ren_id_lookup(Ren, !.ID) = !:ID :- ren_id_lookup(Ren, !ID).

%-----------------------------------------------------------------------------%

ren_var_search(Ren, var(ID1), ID2) :- ren_id_search(Ren, ID1, ID2).

ren_var_search(Ren, Var) = ID :- ren_var_search(Ren, Var, ID).

%-----------------------------------------------------------------------------%

ren_var_lookup(_, anonymous, anonymous).

ren_var_lookup(Ren, var(!.ID), var(!:ID)) :- ren_id_lookup(Ren, !ID).

ren_var_lookup(Ren, !.Var) = !:Var :- ren_var_lookup(Ren, !Var).

%-----------------------------------------------------------------------------%

ren_quantified_search(Ren, var(!.ID), !:ID) :- ren_id_search(Ren, !ID).

ren_quantified_search(Ren, Var) = ID :- 
	ren_quantified_search(Ren, Var, ID).

%-----------------------------------------------------------------------------%

ren_quanitified_lookup(Ren, var(!.ID), var(!:ID)) :- ren_id_lookup(Ren, !IO).

ren_quanttified_lookup(Ren, !.Var) = !:Var :- ren_quanitifed_lookup(Ren, !Var). 
	

		
%-----------------------------------------------------------------------------%
% Substitutible typeclass

apply_substitution(Sub !.T) = !:T :- apply_substitution(Sub, !T).

%-----------------------------------------------------------------------------%
% Utility

:- pred sub_out_of_range(var_id::in, string::in) is erroneous.

sub_out_of_range(ID, Container) :-
	error("error: var_id #" ++ string(ID) ++ 
		" not found in "++ Container).