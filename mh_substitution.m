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
:- import_module enum.

:- import_module mh_term.
:- import_module mh_var_id.

%-----------------------------------------------------------------------------%
% Substitutions

:- type mh_substitution
	--->	sub_empty
	;		sub_map(map(var_id, mh_term))
	;		sub_array(array(mh_term))
	;		sub_stack(mh_substitution, mh_substitution)
	;		ren_empty
	;		ren_map(map(var_id, var_id))
	;		ren_array(array(var_id))
	;		ren_offset(int)
	;		ren_stack(mh_renaming, mh_renaming).
	
:- pred init_sub(mh_substitution::out) is det.
:- func init_sub = mh_substitution.
	
:- pred empty_substitution(mh_substitution).
:- mode empty_substitution(in) is semidet.
:- mode empty_substitution(out) is multi
:- mode empty_substitution(out) is cc_multi.

%-----------------------------------------------------------------------------%
% Substitution lookup

:- pred sub_id_lookup(mh_substitution::in, var_id::in, mh_term::out) is det.
:- func sub_id_lookup(mh_substitution, var_id) = mh_term.

:- pred sub_stack_id_lookup(mh_substitution::in, mh_subst)


:- pred sub_var_lookup(mh_substitution::in, mh_var::in, mh_term::out) is det.
:- func sub_var_lookup(mh_substitution, mh_var) = mh_term.

:- pred sub_quantified_lookup(mh_substitution, quantified_var::in, 
	mh_term::out) is det.
:- func sub_quantified_lookup(mh_substitution, quantified_var) = mh_term.

%-----------------------------------------------------------------------------%
% Substitution composition



%-----------------------------------------------------------------------------%
% Renaming

:- inst mh_renaming
	--->	ren_map(ground)
	;		ren_array(ground)
	;		ren_offset(ground)
	;		ren_stack(ground, ground).
	
:- type mh_renaming =< mh_substitution
	---> 	ren_map(map(var_id, var_id))
	;		ren_array(arrray(var_id))
	;		ren_offset(int)
	;		ren_stack(mh_renaming, mh_renaming).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%-----------------------------------------------------------------------------%
% 	Substitutions

init_sub(init_sub).
init_sub = sub_empty.

empty_substitution(sub_map(init)).
empty_substitution(sub_array(make_empty_array)).
empty_substitution(sub_offset(0)).

%-----------------------------------------------------------------------------%
% Substitution lookup

sub_id_lookup(sub_empty, ID, var(ID)).

sub_id_lookup(sub_map(Map), ID, Term) :-
	( if search(Map, ID, Found)
	then 	Term = Found
	else 	Term = var(ID)
	).
	
sub_id_lookup(sub_array(Array), ID, Term)  :-
	( if var_id_semidet_lookup(Array, ID, Found)
	then Term = Found
	else Term = var(ID)
	).
	
	
sub_id_lookup(sub_stack(Sub1, Sub2), ID, Term) :-
	sub_id_lookup(Sub2, ID, Term2),
	(if Term2 = var(ID2)
	then	Term = sub_id_lookup(Sub1, ID2)
	else	Term = Term2
	).
	
sub_id_lookup(ren_empty, ID, var(ID)).

sub_id_lookup(ren_map(Map), ID0, var(ID)) :-
	( if search(Map, ID0, Found)
	then 	ID = Found
	else 	ID = ID0
	).
	
sub_id_lookup(ren_array(Array), ID0, var(ID)) :-
	( if var_id_semidet_lookup(Array, ID0, Found)
	then ID = Found
	else ID = ID0
	).
	
sub_id_lookup(ren_offset(Offset), ID0, var(ID)) :- 
	ID = construct_var_id(Offset + deconstruct_var_id(ID0)).
	
sub_id_lookup(ren_stack(Ren1, Ren2))




	

	
	
	