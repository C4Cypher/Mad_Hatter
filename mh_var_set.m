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

:- func var_set_first_id(mh_var_set) = var_id.
:- func var_set_last_id(mh_var_set) = var_id.

:- func var_set_first(mh_var_set) = mh_var.
:- func var_set_last(mh_var_set) = mh_var.

:- func var_set_first_quantified(mh_var_set) = quantified_var.
:- func var_set_last_quantified(mh_var_set) = quantified_var.

%-----------------------------------------------------------------------------%
% Var Set membership

:- pred var_set_contains_id(mh_var_set, var_id).
:- mode var_set_contains_id(in, in) is semidet.
:- mode var_set_contains_id(in, out) is nondet.

:- pred var_set_contains(mh_var_set, mh_var).
:- mode var_set_contains(in, in) is semidet.
:- mode var_set_contains(in, out) is nondet.

:- pred var_set_contains_quantified(mh_var_set, quantified_var).
:- mode var_set_contains_quantified(in, in) is semidet.
:- mode var_set_contains_quantified(in, out) is nondet.

%-----------------------------------------------------------------------------%
% Var Set insertion and removal

% These clauses throw an exception if insertion would create 
% null or negative variables

% Add a new var at the end of a var set

:- pred var_set_append(mh_var_set, mh_var_set).
:- mode var_set_append(in, out) is det.
:- mode var_set_append(out, in) is det.

% Add a new var at the end of a var set and return it's id

:- pred var_set_append_id(mh_var_set, mh_var_set, var_id).
:- mode var_set_append_id(in, out, out) is det.
:- mode var_set_append_id(out, in, out) is det.

% Add a new var at the beginning of a var set

:- pred var_set_prepend(mh_var_set, mh_var_set).
:- mode var_set_prepend(in, out) is det.
:- mode var_set_prepend(out, in) is det.

% Add a new var at the beginning of a var set and return it's id

:- pred var_set_prepend_id(mh_var_set, mh_var_set, var_id).
:- mode var_set_prepend_id(in, out, out) is det.
:- mode var_set_prepend_id(out, in, out) is det.



% :- pred var_set_insert_id(var_id, mh_var_set, mh_var_set).
% :- mode var_set_insert_id(in, in, out) is semidet.
% :- mode var_set_insert_id(in, out, in) is semidet.
% :- mode var_set_insert_id(out, in, in) is semidet.

% :- pred var_set_merge_id(var_id, mh_var_set, mh_var_set).
% :- mode var_set_merge_id(in, in, out) is det.
% :- mode var_set_merge_id(in, out, in) is semidet.
% :- mode var_set_merge_id(out, in, in) is semidet.

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

var_set_first_id(var_set(Offset, _)) = first_var_id(Offset).
var_set_first_id(var_set(Offset, _, _)) = first_var_id(Offset).

var_set_last_id(var_set(_, Set)) = last_var_id(Set).
var_set_last_id(var_set(_,_, Next)) = var_set_last_id(Next).

var_set_first(Set) = var(var_set_first_id(Set)).
var_set_last(Set) = var(var_set_last_id(Set)).

var_set_first_quantified(Set) = var(var_set_first_id(Set)).
var_set_last_quantified(Set) = var(var_set_last_id(Set)).

%-----------------------------------------------------------------------------%
% Var Set membership

var_set_contains_id(var_set(Offset, Set), ID) :-  
	contains_var_id(Offset, Set, ID).
	
var_set_contains_id(var_set(Offset, Set, Next), ID) :-
	contains_var_id(Offset, Set, ID);
	var_set_contains_id(Next, ID).
	
var_set_contains(Set, var(ID)) :- var_set_contains_id(Set, ID).
var_set_contains_quantified(Set, var(ID)) :- var_set_contains_id(Set, ID).

%-----------------------------------------------------------------------------%
% Var Set insertion and removal


var_set_append(var_set(Offset, Set1), var_set(Offset, Set2)) :-
	Last = last_var_id(Set2),
	require(var_id_gt(Last, null_var_id), 
		"Invalid var_set, emppty var_set cannot be pre-pended."),
	next_var_id(last_var_id(Set1)) =  Last.
	
var_set_append(var_set(Offset, Set, Next1), var_set(Offset, Set, Next2)) :-
	var_set_append(Next1, Next2).
	
var_set_append_id(var_set(Offset, Set1), var_set(Offset, Set2), Last) :-
	Last = last_var_id(Set2),
	require(var_id_gt(Last, null_var_id), 
		"Invalid var_set, emppty var_set cannot be pre-pended."),
	next_var_id(last_var_id(Set1)) =  Last.
	
var_set_append_id(
	var_set(Offset, Set, !.Next),
	var_set(Offset, Set, !:Next),
	Last) :-
	var_set_append_id(!Next, Last).
	
var_set_prepend(!Set) :- var_set_append(!:Set, !.Set).
var_set_prepend_id(!Set, Last) :- var_set_append_id(!:Set, !.Set, Last).
 	
