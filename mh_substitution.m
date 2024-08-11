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
	;		sub_single(var_id, mh_term)
	;		sub_map(map(var_id, mh_term))
	;		sub_array(array(mh_term))
	;		ren_empty
	;		ren_single(var_id, var_id)
	;		ren_map(map(var_id, var_id))
	;		ren_array(array(var_id))
	;		ren_offset(var_id_offset).
	
:- pred init_sub(mh_substitution::out) is det.
:- func init_sub = mh_substitution.
	
:- pred empty_substitution(mh_substitution).
:- mode empty_substitution(in) is semidet.
:- mode empty_substitution(out) is multi
:- mode empty_substitution(out) is cc_multi.

%-----------------------------------------------------------------------------%
% Substitution lookup

:- pred sub_id_lookup(mh_substitution::in, var_id::in, mh_term::out) 
	is semidet.
:- func sub_id_lookup(mh_substitution, var_id) = mh_term is semidet.


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
	--->	ren_empty
	;		ren_single(ground, ground)
	;		ren_map(ground)
	;		ren_array(ground)
	;		ren_offset(ground).
	
:- type mh_renaming =< mh_substitution
	---> 	ren_empty
	;		ren_single(var_id, var_id)
	;		ren_map(map(var_id, var_id))
	;		ren_array(arrray(var_id))
	;		ren_offset(sub_id_offset).
	
:- mode is_renaming == ground >> mh_renaming.

:- pred is_renaming(mh_substitution::is_renaming) is semidet.
	
%-----------------------------------------------------------------------------%
% Renaming lookup

:- pred ren_id_lookup(mh_renaming::in, var_id::in, var_id::out) 
	is semidet.
:- func ren_id_lookup(mh_renaming, var_id) = var_id is semidet.


:- pred ren_var_lookup(mh_renaming::in, mh_var::in, var_id::out) is det.
:- func ren_var_lookup(mh_renaming, mh_var) = var_id.

:- pred ren_quantified_lookup(mh_renaming, quantified_var::in, 
	var_id::out) is det.
:- func ren_quantified_lookup(mh_renaming, quantified_var) = var_id.

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

sub_id_lookup(sub_empty, _, nil) :- fail.

sub_id_lookup(sub_single(ID, Term), ID, Term).

sub_id_lookup(sub_map(Map), ID, Term) :- search(Map, ID, Found).
	
sub_id_lookup(sub_array(Array), ID, Term)  :- 
	var_id_semidet_lookup(Array, ID, Found).
	
sub_id_lookup(ren_empty, _, nil) :- fail.

sub_id_lookup(ren_single(ID1, ID2), ID1, var(ID2)).

sub_id_lookup(ren_map(Map), ID1, var(ID2)) :- search(Map, ID1, ID2).
	
sub_id_lookup(ren_array(Array), ID1, var(ID2)) :- 
	var_id_semidet_lookup(Array, ID1, ID2).
	
sub_id_lookup(ren_offset(Offset), ID1, var(ID2) ) :- 
	var_id_offset(ID1, ID2, Offset).

sub_id_lookup(Sub, ID) = Term :- sub_id_lookup(Sub, ID, Term).

%-----------------------------------------------------------------------------%

sub_var_lookup(Sub, var(ID), Term) :- sub_id_lookup(Sub, ID, Term).

sub_var_lookup(Sub, anonymous, anonymous).

sub_var_lookup(Sub, Var) = Term :- sub_var_lookup(Sub, Var, Term).

%-----------------------------------------------------------------------------%

sub_quantified_lookup(Sub, var(ID), Term) :- sub_id_lookup(Sub, ID, Term).

sub_quantified_lookup(Sub, Var) = Term :- 
	sub_quantified_lookup(Sub, Var, Term).
	
%-----------------------------------------------------------------------------%
% Substitution composition



%-----------------------------------------------------------------------------%
% Renaming

is_renaming(ren_empty). 
is_renaming(ren_single(_, _) ).
is_renaming(ren_map(_) ).
is_renaming(ren_array(_) ).
is_renaming(ren_offset(_) ).

%-----------------------------------------------------------------------------%
% Renaming lookup

% ren_id_lookup(Ren, !ID)

ren_id_lookup(ren_empty, _, null_var_id) :- fail.

ren_id_lookup(ren_single(ID1, ID2), ID1, ID2).

ren_id_lookup(ren_map(Map), ID1, ID2) :- search(Map, ID1, ID2).
	
ren_id_lookup(ren_array(Array), ID1, ID2) :- 
	var_id_semidet_lookup(Array, ID1, ID2).
	
ren_id_lookup(ren_offset(Offset), ID1, ID2) :- 
	var_id_offset(ID1, ID2, Offset).

ren_id_lookup(Ren, ID) = Term :- ren_id_lookup(Ren, ID, Term).

%-----------------------------------------------------------------------------%

ren_var_lookup(Ren, var(ID1), ID2) :- ren_id_lookup(Ren, ID1, ID2).

ren_var_lookup(Ren, Var) = ID :- ren_var_lookup(Ren, Var, ID).

%-----------------------------------------------------------------------------%

ren_quantified_lookup(Ren, var(ID1), ID2) :- ren_id_lookup(Ren, ID1, ID2).

ren_quantified_lookup(Ren, Var) = ID :- 
	ren_quantified_lookup(Ren, Var, ID).