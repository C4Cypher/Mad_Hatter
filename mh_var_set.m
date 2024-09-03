%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_var_set.m
% Main author: C4Cypher.
% Stability: low.

:- module mh_var_set.

:- interface.

:- import_module array.

:- import_module mh_var_id.
:- import_module mh_term.


%-----------------------------------------------------------------------------%
% Variable sets

:- type mh_var_set.
	
	
:- func var_set(var_id_set) = mh_var_set.
:- mode var_set(in) = out is det.
:- mode var_set(out) = in is semidet.

:- pred valid_var_set(mh_var_set::in) is semidet.
:- pred require_valid_var_set(mh_var_set::in) is det.


:- pred complete_var_set(mh_var_set::in) is semidet.
:- pred contiguous_var_set(mh_var_set::in) is semidet.

:- pred empty_var_set(mh_var_set::out) is det.
:- func empty_var_set = mh_var_set.

:- pred singleton_var_set(var_id, mh_var_set).
:- mode singleton_var_set(in, out) is det.
:- mode singleton_var_set(out, in) is semidet.

:- func singleton_var_set(var_id) = mh_var_set.
:- mode singleton_var_set(in) = out is det.
:- mode singleton_var_set(out) = in is semidet.

%-----------------------------------------------------------------------------%
% Var Set membership

:- pred var_set_contains_id(mh_var_set, var_id).
:- mode var_set_contains_id(in, in) is semidet.
:- mode var_set_contains_id(in, out) is nondet.

:- pred var_set_contains_var(mh_var_set, mh_var).
:- pred var_set_contains_var(in, in) is semidet.
:- pred var_set_contains_var(in, out) is nondet.

:- pred var_set_contains_quantified(mh_var_set, quantified_var).
:- pred var_set_contains_quantified(in, in) is semidet.
:- pred var_set_contains_quantified(in, out) is nondet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%
% Variable sets

:- type mh_var_set
	--->	var_set(var_id_offset, var_id_set)
	;		var_set(var_id_offset, var_id_set, mh_var_set).

var_set(Set) = var_set(null_var_id_offset, Set).

valid_var_set(var_set(Offset, Set)) :-
	offset_ge(Offset, null_var_id_offset),
	valid_var_id_set(Set),
	var_id_le(first_var_id(Offset), last_var_id(Set)).
	
valid_var_set(var_set(Offset, Set, Next)) :-
	offset_ge(Offset, null_var_id_offset),
	valid_var_id_set(Set),
	var_id_le(first_var_id(Offset), last_var_id(Set)),
	valid_var_set_step(Set, Next).
	
:- pred valid_var_set_step(var_id_set::in, mh_var_set::in) is semidet.

valid_var_set_step(Last, var_set(Offset, Set)) :-
	var_id_gt(first_var_id(Offset), last_var_id(Last)),
	valid_var_id_set(Set),
	var_id_le(first_var_id(Offset), last_var_id(Set)).
		
valid_var_set_step(Last, var_set(Offset, Set, Next)) :-
	var_id_gt(first_var_id(Offset), last_var_id(Last)),
	var_id_le(first_var_id(Offset), last_var_id(Set)),
	valid_var_set_step(Set, Next).

%-----------------------------------------------------------------------------%

	
require_valid_var_set(var_set(Offset, Set)) :-
	expect(offset_ge(Offset, null_var_id_offset), $module, $pred, 
		"Var set offset less than zero"),
	expect(valid_var_id_set(Set), $module, $pred,
		"Var set var_id_set less than zero"),
	expect(var_id_le(first_var_id(Offset), last_var_id(Set)), $module, $pred,
		"Var set offset not less than var_id_set").
		
require_valid_var_set(var_set(Offset, Set, Next)) :-
	expect(offset_ge(Offset, null_var_id_offset), $module, $pred, 
		"Var set offset less than zero"),
	expect(valid_var_id_set(Set), $module, $pred,
		"Var set var_id_set less than zero"),
	expect(var_id_le(first_var_id(Offset), last_var_id(Set)), $module, $pred,
		"Var set offset greater than var_id_set"),
	require_valid_var_set_step(Set, Next).


:- pred require_valid_var_set_step(var_id_set::in, mh_var_set::in) is det.

require_valid_var_set_step(Last, var_set(Offset, Set)) :-
	expect(var_id_gt(first_var_id(Offset), last_var_id(Last)), $module, $pred,
		"Offset must be greater than the last pair's var_id_set"),
	expect(var_id_le(first_var_id(Offset), last_var_id(Set)), $module, $pred,
		"Var set offset greater than var_id_set").
		
require_valid_var_set_step(Last, var_set(Offset, Set, Next)) :-
	expect(var_id_gt(first_var_id(Offset), last_var_id(Last)), $module, $pred,
		"Offset must be greater than the last pair's var_id_set"),
	expect(var_id_le(first_var_id(Offset), last_var_id(Set)), $module, $pred,
		"Var set offset greater than var_id_set"),
	require_valid_var_set_step(Set, Next).

%-----------------------------------------------------------------------------%

	
complete_var_set(var_set(null_var_id_offset, _)).

contiguous_var_set(var_set(_, _)).

%-----------------------------------------------------------------------------%


empty_var_set(empty_var_set).

empty_var_set = var_set(null_var_id_offset, init_var_id_set).

%-----------------------------------------------------------------------------%

singleton_var_set(ID, var_set(Offset, Set)) :-
	ID = first_var_id(Offset),
	ID = last_var_id(Set).

singleton_var_set(ID) = VarSet :- singleton_var_set(ID, VarSet).
%-----------------------------------------------------------------------------%
% Var Set membership

var_set_contains_id(var_set(Offset, Set), ID) :-  
	contains_var_id(Offset, Set, ID).
	
var_set_contains_id(var_set(Offset, Set, Next), ID) :-
	contains_var_id(Offset, Set, ID);
	var_set_contains_id(Next, ID).

var_set_contains_var(Set, var(ID)) :- var_set_contains_id(Set, ID).
var_set_contains_quantified(Set, var(ID)) :- var_set_contains_id(Set, ID).