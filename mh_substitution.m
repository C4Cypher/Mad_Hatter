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
	;		ren_empty
	;		ren_single(var_id, var_id)
	;		ren_map(map(var_id, var_id))
	;		ren_array(array(var_id))
	;		ren_offset(var_id_offset).
	
:- pred init_sub(mh_substitution::out) is det.
:- func init_sub = mh_substitution.
	
:- pred empty_substitution(mh_substitution).
:- mode empty_substitution(in) is semidet.
:- mode empty_substitution(out) is multi.
:- mode empty_substitution(out) is cc_multi.

%-----------------------------------------------------------------------------%
% Substitution lookup

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

% :- pred sub_set(var_id::in, mh_term::in, 
	% mh_substitution::in, mh_substitution::out) is det.

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
	;		ren_array(array(var_id))
	;		ren_offset(var_id_offset).
	
:- mode is_renaming == ground >> mh_renaming.

:- pred is_renaming(mh_substitution::is_renaming) is semidet.
	
%-----------------------------------------------------------------------------%
% Renaming lookup

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

%-----------------------------------------------------------------------------%
% 	Substitutions

init_sub(init_sub).
init_sub = sub_empty.

empty_substitution(sub_map(init)).
empty_substitution(sub_array(make_empty_array)).

%-----------------------------------------------------------------------------%
% Substitution lookup

sub_id_search(sub_empty, _, nil) :- fail.

sub_id_search(sub_single(ID, Term), ID, Term).

sub_id_search(sub_map(Map), ID, Term) :- search(Map, ID, Term).
	
sub_id_search(sub_array(Array), ID, Term)  :- 
	var_id_semidet_lookup(Array, ID, Term).
	
sub_id_search(ren_empty, _, nil) :- fail.

sub_id_search(ren_single(ID1, ID2), ID1, var(ID2)).

sub_id_search(ren_map(Map), ID1, var(ID2)) :- search(Map, ID1, ID2).
	
sub_id_search(ren_array(Array), ID1, var(ID2)) :- 
	var_id_semidet_lookup(Array, ID1, ID2).
	
sub_id_search(ren_offset(Offset), ID1, var(ID2) ) :- 
	var_id_offset(ID1, ID2, Offset).

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



%-----------------------------------------------------------------------------%
% Renaming

is_renaming(ren_empty). 
is_renaming(ren_single(_, _) ).
is_renaming(ren_map(_) ).
is_renaming(ren_array(_) ).
is_renaming(ren_offset(_) ).

%-----------------------------------------------------------------------------%
% Renaming lookup

% ren_id_search(Ren, !ID)

ren_id_search(ren_empty, _, null_var_id) :- fail.

ren_id_search(ren_single(ID1, ID2), ID1, ID2).

ren_id_search(ren_map(Map), ID1, ID2) :- search(Map, ID1, ID2).
	
ren_id_search(ren_array(Array), ID1, ID2) :- 
	var_id_semidet_lookup(Array, ID1, ID2).
	
ren_id_search(ren_offset(Offset), ID1, ID2) :- 
	var_id_offset(ID1, ID2, Offset).

ren_id_search(Ren, ID) = Term :- ren_id_search(Ren, ID, Term).

%-----------------------------------------------------------------------------%

ren_var_search(Ren, var(ID1), ID2) :- ren_id_search(Ren, ID1, ID2).

ren_var_search(Ren, Var) = ID :- ren_var_search(Ren, Var, ID).

%-----------------------------------------------------------------------------%

ren_quantified_search(Ren, var(ID1), ID2) :- ren_id_search(Ren, ID1, ID2).

ren_quantified_search(Ren, Var) = ID :- 
	ren_quantified_search(Ren, Var, ID).