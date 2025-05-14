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
	
:- inst term_substitution
	--->	sub_map(ground).
	
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


% If a substitution consists only of var mappings, convert the internal
% representation to a renaming
:- pred sub_to_ren(mh_substitution::in, mh_substitution::out(mh_renaming))
	is semidet.

:- func sub_to_ren(mh_substitution::in) = 
	(mh_substitution::out(mh_renaming)) is semidet.
	
% If a substitution is in renaming form, convert to a var mapping substitution
:- pred ren_to_sub(mh_substitution::in, 
	mh_substitution::out(term_substitution)) is det.
:- func ren_to_sub(mh_substitution::in) = 
	(mh_substitution::out(term_substitution)) is det.

%-----------------------------------------------------------------------------%
% Bounds

:- pred substitution_bounds(mh_substitution::in, var_id_offset::out, 
	var_id_set::out) is semidet.
	
	
:- pred det_substitution_bounds(mh_substitution::in, var_id_offset::out, 
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
:- import_module pair.

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
	
sub_to_ren(sub_map(!.Map), ren_map(!:Map)) :- map_id(var_to_id, !Map).
sub_to_ren(Sub, Sub) :- is_renaming(Sub).

:- pred var_to_id(_::unused, mh_term::in, var_id::out) is semidet.

var_to_id(_, var(ID), ID).

ren_to_sub(ren_map(!.Map), sub_map(!:Map)) :- map_id(id_to_var, !Map).
ren_to_sub(Sub@sub_map(_), Sub).

:- pred id_to_var(_::unused, var_id::in, mh_term::out) is det.

id_to_var(_, ID, var(ID)).

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
	
det_substitution_bounds(sub_array(Map),	Offset, Set) :-
	var_map_bounds(Map, Offset, Set).
	
det_substitution_bounds(ren_array(Map),	Offset, Set) :- 
	var_map_bounds(Map, Offset, Set).

%-----------------------------------------------------------------------------%
% Looking up variables in substitutions

sub_contains_id(sub_array(Map), ID) :- contains_id(Map, ID).
sub_contains_id(ren_array(Map), ID) :- contains_id(Map, ID).

sub_id_search(Sub, ID, sub_id_search(Sub, ID)).
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
	if promise_equivalent_solutions [SubTemp] 
		(
			empty_substitution(Sub1), SubTemp = Sub2
		;	
			empty_substitution(Sub2), SubTemp = Sub1
		)
	then
		Sub3 = SubTemp
	else if
		Sub1 = ren_map(Map1), Sub2 = ren_map(Sub2)
	then
		fold_id(remap_renaming(Map2), Map1, Map1 - Map2, Map03 - MapDiff),
		fold_id(mh_var_map.det_insert, MapDiff, Map03, Map3),
		Sub3 = ren_map(Map3)
	else
		ren_to_sub(Sub1) = sub_map(Map1), ren_to_sub(Sub2) = sub_map(Map2),
		fold_id(remap_substitution(Sub2), Map1, Map1 - Map2, Map03 - MapDiff),
		fold_id(mh_var_map.det_insert, MapDiff, Map03, Map3),
		Sub3 = ren_map(Map3).

:- pred remap_substitution(mh_var_map(mh_term)::in, var_id::in, mh_term::in, 
	pair(mh_var_map(mh_term), mh_var_map(mh_term))::in, 
	pair(mh_var_map(mh_term), mh_var_map(mh_term))::out) is det.
	
remap_substitution(Map2, From1, To1, !.Map3 - !.MapDiff, !:Map3 - !:MapDiff) 
:-
	(if 
		To1 = var(Var1),
		id_search(Map2, Var1, To2)
	then
		id_update(From1, To2, !Map3)
	else true	
	),
	id_delete(From1, !MapDiff).
	

%-----------------------------------------------------------------------------%
% Renaming

is_renaming(ren_map(_)).

init_ren(init_ren).
init_ren = ren_map(init).

empty_renaming(ren_map(init)).

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
	var_map_bounds(Map, Offset, Set).
	
%-----------------------------------------------------------------------------%
% Looking up variables in renamings

ren_contains_id(ren_array(Map), ID) :- contains_id(Map, ID).

ren_id_search(Ren, ID, ren_id_search(Ren, ID)).
ren_id_search(ren_map(Map), ID) = var(id_search(Map, ID)).

ren_id_lookup(Ren, !ID) :-
	if ren_id_search(Ren, !.ID, Found)
	then !:ID = Found
	else !:ID = !.ID.
	
ren_id_lookup(Ren, !.ID) = !:ID :- ren_id_lookup(Ren, !ID).

ren_var_search(Ren, var(ID1), ID2) :- ren_id_search(Ren, ID1, ID2).
ren_var_search(Ren, Var) = ID :- ren_var_search(Ren, Var, ID).

ren_var_lookup(Ren, var(!.ID), var(!:ID)) :- ren_id_lookup(Ren, !ID).
ren_var_lookup(Ren, !.Var) = !:Var :- ren_var_lookup(Ren, !Var).

%-----------------------------------------------------------------------------%
% Renaming composition

compose_renamings(Ren1, Ren2, Ren3) :-
	if promise_equivalent_solutions [Ren3] 
	(
		empty_renaming(Ren1), RenTemp = Ren2
	;	
		empty_renaming(Ren2), RenTemp = Ren1
	)
	then
		Ren3 = RenTemp
	else
		Ren1 = ren_map(Map1), Ren2 = ren_map(Map2),
		fold_id(remap_renaming(Map2), Map1, Map1 - Map2, Map03 - MapDiff),
		fold_id(mh_var_map.det_insert, MapDiff, Map03, Map3),
		Ren3 = ren_map(Map3).



:- pred remap_renaming(mh_var_map(var_id)::in, var_id::in, var_id::in, 
	pair(mh_var_map(var_id), mh_var_map(var_id))::in, 
	pair(mh_var_map(var_id), mh_var_map(var_id))::out) is det.
	
remap_renaming(Map2, From1, To1, !.Map3 - !.MapDiff, !:Map3 - !:MapDiff) 
:-
	(if id_search(Map2, To1, To2)
	then
		id_update(From1, To2, !Map3)
	else true	
	),
	id_delete(From1, !MapDiff).



compose_renamings(Ren1, Ren2) = Ren3 :- compose_renamings(Ren1, Ren2, Ren3).

%-----------------------------------------------------------------------------%
% Utility

:- pred sub_out_of_range(var_id::in, string::in) is erroneous.

sub_out_of_range(ID, Container) :-
	error("error: var_id #" ++ string(ID) ++ 
		" not found in "++ Container).