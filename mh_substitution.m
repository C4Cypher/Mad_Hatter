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
	;		sub_map(map(var_id, mh_term))
	;		sub_array(array(mh_term))
	;		sub_offset(mh_substitution, var_id_offset)
	;		ren_empty
	;		ren_single(var_id, var_id)
	;		ren_map(map(var_id, var_id))
	;		ren_array(array(var_id))
	;		ren_offset(var_id_offset)
	;		ren_offset(mh_renaming, var_id_offset).
	
:- pred init_sub(mh_substitution::out) is det.
:- func init_sub = mh_substitution.
	
:- pred empty_substitution(mh_substitution).
:- mode empty_substitution(in) is semidet.
:- mode empty_substitution(out) is multi.
:- mode empty_substitution(out) is cc_multi.


	
%-----------------------------------------------------------------------------%
% Substitution search

% Succeed if the substitution can index the provided ID
% note that this will fail for any input with the ren_offset/1 constructor

:- pred sub_contains_id(mh_substitution::in, var_id::in) is semidet.

% Find a given variable ID in the substitution, fail if the id is not found

:- pred sub_id_search(mh_substitution::in, var_id::in, mh_term::out) 
	is semidet.
:- func sub_id_search(mh_substitution, var_id) = mh_term is semidet.


:- pred sub_var_search(mh_substitution::in, mh_var::in, mh_term::out) 
	is semidet.
:- func sub_var_search(mh_substitution, mh_var) = mh_term is semidet.

:- pred sub_quantified_search(mh_substitution::in, quantified_var::in, 
	mh_term::out) is semidet.
:- func sub_quantified_search(mh_substitution, quantified_var) = mh_term
	is semidet.

%-----------------------------------------------------------------------------%
% Substitution composition
/*
:- pred compose_substiution(pred(var_id, mh_term), mh_substitution).
:- mode compose_substiution(pred(out, out) is nondet, out) is det.
:- mode compose_substiution(pred(out, out) is multi, out) is det.

% Add a new id term pair to a substitution 
% fail if the substitution already present

:- pred sub_id_add(var_id::in, mh_term::in, 
	mh_substitution::in, mh_substitution::out) is semidet.

:- pred sub_id_set(var_id::in, mh_term::in, 
	mh_substitution::in, mh_substitution::out) is det.
*/
%-----------------------------------------------------------------------------%
% Renaming

:- inst mh_renaming
	--->	ren_empty
	;		ren_single(ground, ground)
	;		ren_map(ground)
	;		ren_array(ground)
	;		ren_offset(ground)
	;		ren_offset(ground, ground).
	
:- type mh_renaming =< mh_substitution
	---> 	ren_empty
	;		ren_single(var_id, var_id)
	;		ren_map(map(var_id, var_id))
	;		ren_array(array(var_id))
	;		ren_offset(var_id_offset)
	;		ren_offset(mh_renaming, var_id_offset).
	
:- mode is_renaming == ground >> mh_renaming.

:- pred is_renaming(mh_substitution::is_renaming) is semidet.

%-----------------------------------------------------------------------------%
% Renaming search

:- pred ren_id_search(mh_renaming::in, var_id::in, var_id::out) 
	is semidet.
:- func ren_id_search(mh_renaming, var_id) = var_id is semidet.


:- pred ren_var_search(mh_renaming::in, mh_var::in, var_id::out) is semidet.
:- func ren_var_search(mh_renaming, mh_var) = var_id is semidet.

:- pred ren_quantified_search(mh_renaming::in, quantified_var::in, 
	var_id::out) is semidet.
:- func ren_quantified_search(mh_renaming, quantified_var) = var_id is semidet.

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

empty_substitution(sub_map(init)).
empty_substitution(sub_array(make_empty_array)).

%-----------------------------------------------------------------------------%
% Substitution search

sub_contains_id(sub_empty, _) :- fail.
sub_contains_id(sub_single(ID, _), ID).
sub_contains_id(sub_map(Map), ID) :- contains(Map, ID).
sub_contains_id(sub_array(Array), ID) :- var_id_in_bounds(Array, ID).
sub_contains_id(sub_offset(Sub, Offset, !.ID) :- 
	var_id_offset(Offset, !ID), sub_contains_id(Sub, !:ID).
sub_contains_id(ren_empty, _) :- fail.
sub_contains_id(ren_single(ID, _), ID).
sub_contains_id(ren_map(Map), ID) :- contains(Map, ID).
sub_contains_id(ren_array(Array), ID) :- var_id_in_bounds(Array, ID).
sub_contains_id(ren_offset(_), _) :- fail.
sub_contains_id(ren_offset(Sub, Offset, !.ID) :- 
	var_id_offset(Offset, !ID), sub_contains_id(Sub, !:ID).



%-----------------------------------------------------------------------------%


sub_id_search(sub_empty, _, nil) :- fail.

sub_id_search(sub_single(ID, Term), ID, Term).

sub_id_search(sub_map(Map), ID, Term) :- search(Map, ID, Term).
	
sub_id_search(sub_array(Array), ID, Term)  :- 
	var_id_semidet_lookup(Array, ID, Term).
	
sub_id_search(sub_offset(Sub, Offset), !.ID, Term) :-
	var_id_offset(!ID, Offset),
	sub_id_search(Sub, !:ID, Term).
	
sub_id_search(ren_empty, _, nil) :- fail.
 
sub_id_search(ren_single(ID1, ID2), ID1, var(ID2)).

sub_id_search(ren_map(Map), !.ID, var(!:ID)) :- search(Map, !ID).
	
sub_id_search(ren_array(Array), !.ID, var(!:ID)) :- 
	var_id_semidet_lookup(Array, !ID).
	
sub_id_search(ren_offset(Offset), !.ID, var(!:ID) ) :- 
	var_id_offset(!ID, Offset).
	
sub_id_search(ren_offset(Sub, Offset), !.ID, var(!:ID)) :-
	var_id_offset(!ID, Offset),
	ren_id_search(Ren, !ID).

sub_id_search(Sub, ID) = Term :- sub_id_search(Sub, ID, Term).

%-----------------------------------------------------------------------------%

sub_var_search(Sub, var(ID), Term) :- sub_id_search(Sub, ID, Term).

sub_var_search(Sub, Var) = Term :- sub_var_search(Sub, Var, Term).

%-----------------------------------------------------------------------------%

sub_quantified_search(Sub, var(ID), Term) :- sub_id_search(Sub, ID, Term).

sub_quantified_search(Sub, Var) = Term :- 
	sub_quantified_search(Sub, Var, Term).
	
%-----------------------------------------------------------------------------%
% Substitution composition
/*
sub_id_add(ID, Term, !Sub) :-
	not sub_contains_id(!.Sub, ID),
	sub_id_set(ID, Term, !Sub).
	
sub_id_add
*/
%-----------------------------------------------------------------------------%
% Renaming

is_renaming(ren_empty). 
is_renaming(ren_single(_, _) ).
is_renaming(ren_map(_) ).
is_renaming(ren_array(_) ).
is_renaming(ren_offset(_) ).
is_renaming(ren_offset(_, _)).


%-----------------------------------------------------------------------------%
% Renaming search

ren_id_search(ren_empty, _, null_var_id) :- fail.

ren_id_search(ren_single(!ID), !ID).

ren_id_search(ren_map(Map), !ID) :- search(Map, !ID).
	
ren_id_search(ren_array(Array), !ID) :- 
	var_id_semidet_lookup(Array, !ID).
	
ren_id_search(ren_offset(Offset), !ID) :- 
	var_id_offset(!ID, Offset).
	
ren_id_search(ren_offset(Ren, Offset), !ID) :-
	var_id_offset(!ID),
	ren_id_search(Ren, !ID).

ren_id_search(Ren, !.ID) = !:ID :- ren_id_search(Ren, !ID).

%-----------------------------------------------------------------------------%

ren_var_search(Ren, var(ID1), ID2) :- ren_id_search(Ren, ID1, ID2).

ren_var_search(Ren, Var) = ID :- ren_var_search(Ren, Var, ID).

%-----------------------------------------------------------------------------%

ren_quantified_search(Ren, var(ID1), ID2) :- ren_id_search(Ren, ID1, ID2).

ren_quantified_search(Ren, Var) = ID :- 
	ren_quantified_search(Ren, Var, ID).
	
%-----------------------------------------------------------------------------%
% Utility

:- pred sub_out_of_range(var_id::in, string::in, string::in) is erroneous.

sub_out_of_range(ID, Type, Container) :-
	error("error: " ++ Type ++ " with id of " ++ string(ID) ++ 
		" not found in ":++ Container).