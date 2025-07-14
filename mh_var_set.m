%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
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
	
	
:- func complete_var_set(var_id_set) = mh_var_set.
:- mode complete_var_set(in) = out is det.
:- mode complete_var_set(out) = in is semidet.

:- pred complete_var_set(var_id_set, mh_var_set).
:- mode complete_var_set(in, out) is det.
:- mode complete_var_set(out, in) is semidet.

:- func contiguous_var_set(var_id_offset, var_id_set) = mh_var_set.
:- mode contiguous_var_set(in, in) = out is det.
:- mode contiguous_var_set(out, out) = in is semidet.

:- pred valid_var_set(mh_var_set::in) is semidet.
:- pred require_valid_var_set(mh_var_set::in) is det.

:- pred is_complete_var_set(mh_var_set::in) is semidet.
:- pred is_contiguous_var_set(mh_var_set::in) is semidet.

:- pred empty_var_set(mh_var_set::out) is det.
:- func empty_var_set = mh_var_set.

:- pred is_empty(mh_var_set::in) is semidet.

:- pred expect_non_empty_var_set(mh_var_set::in) is det.

:- pred singleton_var_set(var_id, mh_var_set).
:- mode singleton_var_set(in, out) is det.
:- mode singleton_var_set(out, in) is semidet.

:- func singleton_var_set(var_id) = mh_var_set.
:- mode singleton_var_set(in) = out is det.
:- mode singleton_var_set(out) = in is semidet.

:- func var_set_first_id(mh_var_set) = var_id.
:- func var_set_last_id(mh_var_set) = var_id.

	% Extract the first id from a var_set and provide the var set with the id
	% removed. Throw an exception if the var_set is empty.
:- pred var_set_first_id(var_id::out, mh_var_set::in, mh_var_set::out) is det.

:- func var_set_first(mh_var_set) = mh_var.
:- func var_set_last(mh_var_set) = mh_var.

	% The number of vars contained in a set
:- pred var_set_count(mh_var_set::in, int::out) is det.
:- func var_set_count(mh_var_set) = int.

%-----------------------------------------------------------------------------%
% Var Set bounds

:- func var_set_id_set(mh_var_set) = var_id_set.
:- pred var_set_id_set(mh_var_set::in, var_id_set::out) is det.

:- func var_set_offset(mh_var_set) = var_id_offset.
:- pred var_set_offset(mh_var_set::in, var_id_offset::out) is det.

	
:- pred var_set_bounds(mh_var_set::in, var_id_offset::out, var_id_set::out)
	is det.

%-----------------------------------------------------------------------------%
% Var Set membership

:- pred var_set_contains_id(mh_var_set, var_id).
:- mode var_set_contains_id(in, in) is semidet.
:- mode var_set_contains_id(in, out) is nondet.

:- pred var_set_contains(mh_var_set, mh_var).
:- mode var_set_contains(in, in) is semidet.
:- mode var_set_contains(in, out) is nondet.

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

% Fails if id is already a member of the set
 
:- pred var_set_insert_id(var_id, mh_var_set, mh_var_set).
:- mode var_set_insert_id(in, in, out) is semidet.
:- mode var_set_insert_id(in, out, in) is semidet.
:- mode var_set_insert_id(out, in, in) is semidet.

:- pred var_set_insert(mh_var, mh_var_set, mh_var_set).
:- mode var_set_insert(in, in, out) is semidet.
:- mode var_set_insert(in, out, in) is semidet.
:- mode var_set_insert(out, in, in) is semidet.


% If the id is already a member of the set, the set is unchanged

:- pred var_set_merge_id(var_id, mh_var_set, mh_var_set).
:- mode var_set_merge_id(in, in, out) is det.
:- mode var_set_merge_id(in, out, in) is det.
:- mode var_set_merge_id(out, in, in) is semidet.

:- pred var_set_merge(mh_var, mh_var_set, mh_var_set).
:- mode var_set_merge(in, in, out) is det.
:- mode var_set_merge(in, out, in) is det.
:- mode var_set_merge(out, in, in) is semidet.

% Inverse of var_set_insert_id
:- pred var_set_remove_id(var_id, mh_var_set, mh_var_set).
:- mode var_set_remove_id(in, in, out) is semidet.
:- mode var_set_remove_id(in, out, in) is semidet.
:- mode var_set_remove_id(out, in, in) is semidet.

:- pred var_set_remove(mh_var, mh_var_set, mh_var_set).
:- mode var_set_remove(in, in, out) is semidet.
:- mode var_set_remove(in, out, in) is semidet.
:- mode var_set_remove(out, in, in) is semidet.

%-----------------------------------------------------------------------------%
% Var Set composition

:- pred var_set_union(mh_var_set::in, mh_var_set::in, mh_var_set::out) is det.
:- func var_set_union(mh_var_set, mh_var_set) = mh_var_set.

:- pred var_set_intersection(mh_var_set::in, mh_var_set::in, mh_var_set::out) 
	is det.
:- func var_set_intersection(mh_var_set, mh_var_set) = mh_var_set.

:- pred var_set_difference(mh_var_set::in, mh_var_set::in, mh_var_set::out) 
	is det.
:- func var_set_difference(mh_var_set, mh_var_set) = mh_var_set.

%-----------------------------------------------------------------------------%
% Var Set indexing

	% given a var set and a var_id, give the sparse index that corresponds to
	% the id's position in the  set, starting with zero. For example, for the
	% set [ 6, 7,  9 ], the sparse indexes of the members would be [ 0, 1, 2]
	% fails if the given id is not a member of the set.
:- pred id_sparse_index(var_id, mh_var_set, int).
:- mode id_sparse_index(in, in, out) is semidet.
:- mode id_sparse_index(out, in, in) is semidet.
:- mode id_sparse_index(out, in, out) is nondet.

:- func id_sparse_index(var_id, mh_var_set) = int.
:- mode id_sparse_index(in, in) = out is semidet.
:- mode id_sparse_index(out, in) = in is semidet.

:- func id_reverse_sparse_index(int, mh_var_set) = var_id.
:- mode id_reverse_sparse_index(in, in) = out is semidet.
:- mode id_reverse_sparse_index(out, in) = in is semidet.

:- pred sparse_index(mh_var, mh_var_set, int).
:- mode sparse_index(in, in, out) is semidet.
:- mode sparse_index(out, in, in) is semidet.
:- mode sparse_index(out, in, out) is nondet.

:- func sparse_index(mh_var, mh_var_set) = int.
:- mode sparse_index(in, in) = out is semidet.
:- mode sparse_index(out, in) = in is semidet.

:- func reverse_sparse_index(int, mh_var_set) = mh_var.
:- mode reverse_sparse_index(in, in) = out is semidet.
:- mode reverse_sparse_index(out, in) = in is semidet.

%-----------------------------------------------------------------------------%
% Church indexing

	% Similar to sparse indexing for arrays, church indexing is effectively
	% Sparse indexing, but for church encoding in other structures 
	% only 1 based, rather than 0 based for the above calls, given that var_ids
	% start at 1, not 0

:- pred id_church_index(var_id, mh_var_set, var_id).
:- mode id_church_index(in, in, out) is semidet.
:- mode id_church_index(out, in, in) is semidet.
:- mode id_church_index(out, in, out) is nondet.

:- func id_church_index(var_id, mh_var_set) = var_id.
:- mode id_church_index(in, in) = out is semidet.
:- mode id_church_index(out, in) = in is semidet.

:- func id_reverse_church_index(var_id, mh_var_set) = var_id.
:- mode id_reverse_church_index(in, in) = out is semidet.
:- mode id_reverse_church_index(out, in) = in is semidet.

:- pred church_index(mh_var, mh_var_set, mh_var).
:- mode church_index(in, in, out) is semidet.
:- mode church_index(out, in, in) is semidet.
:- mode church_index(out, in, out) is nondet.

:- func church_index(mh_var, mh_var_set) = mh_var.
:- mode church_index(in, in) = out is semidet.
:- mode church_index(out, in) = in is semidet.

:- func reverse_church_index(mh_var, mh_var_set) = mh_var.
:- mode reverse_church_index(in, in) = out is semidet.
:- mode reverse_church_index(out, in) = in is semidet.

%-----------------------------------------------------------------------------%
% Higher order

:- pred fold_id(pred(var_id, A, A), mh_var_set, A, A).
:- mode fold_id(in(pred(in, in, out) is det), in, in, out) is det.
:- mode fold_id(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode fold_id(in(pred(in, di, uo) is det), in, di, uo) is det.
/*:- mode fold_id(in(pred(in, in, array_di, array_uo) is det), in, 
	array_di, array_uo) is det. */
:- mode fold_id(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode fold_id(in(pred(in, mdi, muo) is semidet), in, mdi, muo) 
	is semidet.

:- pred fold(pred(mh_var, A, A), mh_var_set, A, A).
:- mode fold(in(pred(in, in, out) is det), in, in, out) is det.
:- mode fold(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode fold(in(pred(in, di, uo) is det), in, di, uo) is det.
/*:- mode fold(in(pred(in, in, array_di, array_uo) is det), in, 
	array_di, array_uo) is det.*/
:- mode fold(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode fold(in(pred(in, mdi, muo) is semidet), in, mdi, muo) 
	is semidet.
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.
:- import_module int.

%-----------------------------------------------------------------------------%
% Variable sets

:- type mh_var_set
	--->	var_set(var_id_offset, var_id_set)
	;		var_set(var_id_offset, var_id_set, mh_var_set).

complete_var_set(Set) = var_set(null_var_id_offset, Set).
complete_var_set(Set, complete_var_set(Set)).

contiguous_var_set(Offset, Set) = var_set(Offset, Set).

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

is_complete_var_set(var_set(null_var_id_offset, _)).

is_contiguous_var_set(var_set(_, _)).

%-----------------------------------------------------------------------------%

empty_var_set(empty_var_set).

empty_var_set = var_set(null_var_id_offset, init_var_id_set).

is_empty(VarSet) :- 
	require_valid_var_set(VarSet),
	VarSet = var_set(O, S),
	empty_var_id_offset_and_id_set(O, S).
	
expect_non_empty_var_set(VarSet) :-
	(if is_empty(VarSet)
	then
		unexpected($module, $pred, "Expected non-empty var_set")
	else
		true
	).

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

var_set_first_id(ID@var_set_first_id(Set), Set, NewSet) :-
	expect_non_empty_var_set(Set),
	(if var_set_remove_id(ID, Set, Removed)
	then NewSet = Removed
	else unexpected($module, $pred,
		"Removal of an mh_var_set's first ID from itself failed")
	).

var_set_first(Set) = var(var_set_first_id(Set)).
var_set_last(Set) = var(var_set_last_id(Set)).

%-----------------------------------------------------------------------------%

var_set_count(Set, var_set_count(Set)).

var_set_count(var_set(_, Set)) = var_id_count(Set).
var_set_count(var_set(_, Set, Rest)) = var_id_count(Set) + var_set_count(Rest).

%-----------------------------------------------------------------------------%
% Var Set bounds

var_set_id_set(var_set(_, Set)) = Set.
var_set_id_set(var_set(_, _, Next)) = var_set_id_set(Next).

var_set_id_set(Set, var_set_id_set(Set)).

var_set_offset(var_set(Offset, _)) = Offset.
var_set_offset(var_set(Offset, _, _)) = Offset.

var_set_offset(Set, var_set_offset(Set)).

var_set_bounds(Set, var_set_offset(Set), var_set_id_set(Set)).

%-----------------------------------------------------------------------------%
% Var Set membership

var_set_contains_id(var_set(Offset, Set), ID) :-  
	contains_var_id(Offset, Set, ID).
	
var_set_contains_id(var_set(Offset, Set, Next), ID) :-
	contains_var_id(Offset, Set, ID);
	var_set_contains_id(Next, ID).
	
var_set_contains(Set, var(ID)) :- var_set_contains_id(Set, ID).


%-----------------------------------------------------------------------------%
% Var Set insertion and removal


var_set_append(var_set(Offset, !.Set), var_set(Offset, !:Set)) :-
	new_appended_id(!Set, _).
	
var_set_append(var_set(Offset, Set, !.Next), var_set(Offset, Set, !:Next)) :-
	var_set_append(!Next).
	
var_set_append_id(var_set(Offset, !.Set), var_set(Offset, !:Set), Last) :-
	new_appended_id(!Set, Last).

var_set_append_id(
	var_set(Offset, Set, !.Next),
	var_set(Offset, Set, !:Next),
	Last) :-
	var_set_append_id(!Next, Last).	

:- pragma inline(new_appended_id/3).
	
:- pred new_appended_id(var_id_set, var_id_set, var_id).
:- mode new_appended_id(in, out, out) is det.
:- mode new_appended_id(out, in, out) is det.
	
new_appended_id(!Set, New) :-
	New = last_var_id(!:Set) @ next_var_id(last_var_id(!.Set)),
	expect(var_id_gt(New, null_var_id), $module, $pred,
		"Cannot remove var from empty var_set").	
	
	
%-----------------------------------------------------------------------------%

var_set_prepend(var_set(!.Offset, Set), var_set(!:Offset, Set)) :-
	new_prepended_id(!Offset, _).

var_set_prepend(var_set(!.Offset, Set, Next), var_set(!:Offset, Set, Next)) :-
	new_prepended_id(!Offset, _).
	
var_set_prepend_id(var_set(!.Offset, Set), var_set(!:Offset, Set), First) :-
	new_prepended_id(!Offset, First).

var_set_prepend_id(var_set(!.Offset, Set, Next), var_set(!:Offset, Set, Next), 
	First) :-
	new_prepended_id(!Offset, First).

:- pragma inline(new_prepended_id/3).

:- pred new_prepended_id(var_id_offset, var_id_offset, var_id).
:- mode new_prepended_id(in, out, out) is det.
:- mode new_prepended_id(out, in, out) is det.

new_prepended_id(!Offset, New) :-
	New = first_var_id(!:Offset) @ previous_var_id(first_var_id(!.Offset)),
	expect(var_id_ge(New, first_var_id), $module, $pred,
		"Cannot pre-pend var to var_set starting at first_var_id. " ++
		"In other words, you can't add a var to a var_set starting at id 1.").
		
%-----------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(var_set_insert_id/3).

var_set_insert_id(ID::in, VS1::in, VS2::out) :-
	not var_set_contains_id(VS1, ID),
	var_set_merge_id(ID, VS1, VS2).
	
var_set_insert_id(ID::in, VS1::out, VS2::in) :-
	var_set_contains_id(VS2, ID),
	var_set_merge_id(ID, VS1, VS2).
	
var_set_insert_id(ID::out, VS1::in, VS2::in) :- 
	var_set_merge_id(ID, VS1, VS2).
	
var_set_insert(var(ID), VS1, VS2) :- var_set_insert_id(ID, VS1, VS2).

:- pragma promise_equivalent_clauses(var_set_merge_id/3).
	
var_set_merge_id(ID::in, VS1::in, VS2::out) :-
	var_set_union(VS1, singleton_var_set(ID), VS2).
	
var_set_merge_id(ID::in, VS1::out, VS2::in) :-
	var_set_difference(VS2, singleton_var_set(ID), VS1).
	
var_set_merge_id(ID::out, VS1::in, VS2::in) :-
	var_set_difference(VS2, VS1, singleton_var_set(ID)).
	
var_set_merge(var(ID), VS1, VS2) :- var_set_merge_id(ID, VS1, VS2).
	
var_set_remove_id(ID, VS1, VS2) :- var_set_insert_id(ID, VS2, VS1).
	
var_set_remove(var(ID), VS1, VS2) :- var_set_remove_id(ID, VS1, VS2).

%-----------------------------------------------------------------------------%
% Var Set composition

var_set_union(VS1, VS2, var_set_union(VS1, VS2)).

var_set_union(var_set(O1, S1), var_set(O2, S2)) =
	var_set_union_pairs(O1, S1, O2, S2).
		
var_set_union(var_set(O1, S1), var_set(O2, S2, Next)) = 

	% if the greater pair does not overlap with the lesser pair
	 (if var_id_gt(first_var_id(OG), next_var_id(last_var_id(SL)))
	then
		var_set(OL, SL, var_set_union(var_set(OG, SG), Next))
	else
		% if the greater pair does not overlap with next pair
		(if 
			var_id_lt(
				last_var_id(SG),
				previous_var_id(var_set_first_id(Next))
			)
		then
			var_set(OL, SG, Next)
		else
			%union both pairs with the next var_set
			var_set_union(var_set(OL, SG), Next)
		)
	)
:-
	offset_order(O1, O2, OL, OG),
	var_id_set_order(S1, S2, SL, SG).
	
var_set_union(VS1 @ var_set(_, _, _), VS2 @ var_set(_, _)) = 
	var_set_union(VS2, VS1).
	
var_set_union(var_set(O1, S1, Next1), var_set(O2, S2, Next2)) = 
	var_set_union(
		var_set_union_pairs(O1, S1, O2, S2),
		var_set_union(Next1, Next2)
	).
			



:- func var_set_union_pairs(
	var_id_offset, var_id_set,
	var_id_offset, var_id_set) = mh_var_set.

var_set_union_pairs(O1, S1, O2, S2) =
	(if SL = empty_var_id_set
	then
		var_set(OG, SG)
	else if var_id_gt(first_var_id(OG), next_var_id(last_var_id(SL)))
	then
		var_set(OL, SL, var_set(OG, SG))
	else
		var_set(OL, SG)
	) 
:-
	offset_order(O1, O2, OL, OG),
	var_id_set_order(S1, S2, SL, SG).
	
%-----------------------------------------------------------------------------%

var_set_intersection(VS1, VS2, var_set_intersection(VS1, VS2)).

var_set_intersection(var_set(O1, S1), var_set(O2, S2)) =
	var_set_intersection_pairs(O1, S1, O2, S2).

var_set_intersection(VS1 @ var_set(O1, S1), var_set(O2, S2, Next)) =
	var_set_union(
		var_set_intersection_pairs(O1, S1, O2, S2),
		var_set_intersection(VS1, Next)
		).
	
var_set_intersection(VS1 @ var_set(_, _, _), VS2 @ var_set(_, _)) =
	var_set_intersection(VS2, VS1).
	
var_set_intersection(VS1 @ var_set(O1, S1, N1), VS2 @ var_set(O2, S2, N2)) =
	(if var_id_ge(first_var_id(O1), var_set_first_id(N2))
	then var_set_intersection(VS1, N2)
	
	else if var_id_ge(first_var_id(O2), var_set_first_id(N1))
	then var_set_intersection(VS2, N1)
	
	else if var_id_ge(last_var_id(S1), var_set_first_id(N2) )
	then
		var_set_union(VS3, 
			var_set_union(var_set_intersection(VS1, N2), N3)
		)
	else if var_id_ge(last_var_id(S2), var_set_first_id(N1) )
	then
		var_set_union(VS3, 
			var_set_union(var_set_intersection(VS2, N1), N3)
		)
	else
		var_set_union(VS3, N3)
	)
:- 
	var_set_intersection(N1, N2, N3),
	var_set_intersection_pairs(O1, S1, O2, S2, VS3).

:- func var_set_intersection_pairs(
	var_id_offset, var_id_set,
	var_id_offset, var_id_set) = mh_var_set.
	
var_set_intersection_pairs(O1, S1, O2, S2) = 
	(if (SL = empty_var_id_set ; var_id_lt(last_var_id(SL), first_var_id(OG)))
	then empty_var_set
	else var_set(OG, SL)
	)
:- 
	offset_order(O1, O2, _, OG),
	var_id_set_order(S1, S2, SL, _).
	
:- pred var_set_intersection_pairs(
	var_id_offset::in, var_id_set::in,
	var_id_offset::in, var_id_set::in,
	mh_var_set::out) is det.
	
var_set_intersection_pairs(O1, S1, O2, S2, 
	var_set_intersection_pairs(O1, S1, O2, S2)).
	
%-----------------------------------------------------------------------------%

var_set_difference(VS1, VS2, var_set_difference(VS1, VS2)).

var_set_difference(var_set(O1, S1), var_set(O2, S2)) =
	var_set_difference_pairs(O1, S1, O2, S2).
	
var_set_difference(VS1 @ var_set(O1, S1, N1), VS2 @ var_set(O2, S2)) =
	(if var_id_lt(last_var_id(S2), first_var_id(O1))
	then 
		VS1
	else if var_id_gt(first_var_id(O2), last_var_id(S1))
	then
		var_set_difference(N1, VS2)
	else
		var_set_union(
			var_set_difference_pairs(O1, S1, O2, S2), 
			var_set_difference(N1, VS2)
		)
	).
	
var_set_difference(VS1 @ var_set(O1, S1), var_set(O2, S2, N2)) =
	(if var_id_gt(first_var_id(O2), last_var_id(S1))
	then
		VS1
	else if var_id_lt(last_var_id(S2), first_var_id(O1))
	then
		var_set_difference(VS1, N2)
	else
		var_set_difference(var_set_difference_pairs(O1, S1, O2, S2), N2)
	).
	
var_set_difference(VS1 @ var_set(O1, S1, N1), VS2 @ var_set(O2, S2, N2)) =
	(if var_id_lt(last_var_id(S2), first_var_id(O1))
	then 
		var_set_difference(VS1, N2)
	else if var_id_gt(first_var_id(O2), last_var_id(S1))
	then
		var_set_union(
			VS1,
			var_set_difference(N1, VS2)
		)
	else
		var_set_union(
			var_set_difference(var_set_difference_pairs(O1, S1, O2, S2), N2),
			var_set_difference(N1, VS2)
		)
	).


:- func var_set_difference_pairs(
	var_id_offset, var_id_set, 
	var_id_offset, var_id_set
	) = mh_var_set.
	
	
var_set_difference_pairs(O1, S1, O2, S2) = Diff :-
	(if S1 = empty_var_id_set
	then Diff = empty_var_set
	
	else if 
		S2 = empty_var_id_set;
		var_id_lt(last_var_id(S2), first_var_id(O1));
		var_id_gt(first_var_id(O2), last_var_id(S1))
	then Diff = var_set(O1, S1)
	
	else 
	if	offset_gt(O2, O1)
	then
		last_var_id(S3) = previous_var_id(first_var_id(O2)),
		(if var_id_set_lt(S2, S1)
		then
			Diff = var_set(O1, S3, var_set(O3, S1)),
			first_var_id(O3) = next_var_id(last_var_id(S2))
		else
			Diff = var_set(O1, S3)
		)
		
	else if var_id_set_lt(S2, S1)
	then
		Diff = var_set(O3, S1),
		first_var_id(O3) = next_var_id(last_var_id(S2))
	else
		Diff = empty_var_set
	).
	
	
%-----------------------------------------------------------------------------%
% Var Set indexing	
	
:- pragma promise_equivalent_clauses(id_sparse_index/3).

id_sparse_index(ID::in, Set::in, sparse_index_fwd(ID, Set)::out).
id_sparse_index(sparse_index_rev(Index, Set)::out, Set::in, Index::in).
id_sparse_index(ID::out, Set::in, Index::out) :- 
	var_set_contains_id(Set, ID), 
	Index = sparse_index_fwd(ID, Set).


id_sparse_index(ID, Set) = Index :- id_sparse_index(ID, Set, Index).
id_reverse_sparse_index(id_sparse_index(ID, Set), Set) = ID.

:- func sparse_index_fwd(var_id, mh_var_set) = int is semidet.

sparse_index_fwd(ID, var_set(Offset, Set)) = 
	sparse_index_weight(ID, Offset, Set) :-
	var_id_ge(ID, first_var_id(Offset)),
	var_id_le(ID, last_var_id(Set)).
	
sparse_index_fwd(ID, var_set(Offset, Set, Next)) = 
	( if var_id_le(ID, last_var_id(Set)) 
	then sparse_index_weight(ID, Offset, Set)
	else sparse_index_fwd(sparse_index_weight(ID, Offset, Set), ID, Next) 
	) :-
	var_id_ge(ID, first_var_id(Offset)).
	
:- func sparse_index_fwd(int, var_id, mh_var_set) = int is semidet.

sparse_index_fwd(Sum, ID, var_set(Offset, Set)) = 
	sparse_index_weight(ID, Offset, Set) + Sum :-
	var_id_ge(ID, first_var_id(Offset)),
	var_id_le(ID, last_var_id(Set)).
	
sparse_index_fwd(Sum, ID, var_set(Offset, Set, Next)) = 
	( if var_id_le(ID, last_var_id(Set)) 
	then Sum + sparse_index_weight(ID, Offset, Set)
	else sparse_index_fwd(Sum + sparse_index_weight(ID, Offset, Set), ID, Next) 
	) :-
	var_id_ge(ID, first_var_id(Offset)).
	
:- func sparse_index_rev(int, mh_var_set) = var_id is semidet.

sparse_index_rev(Index, var_set(Offset, Set)) = 
	reverse_sparse_index(Index, Offset, Set).
	
sparse_index_rev(Index, var_set(Offset, Set, Next)) = 
	(if reverse_sparse_index(Index, Offset, Set) = Found
	then Found
	else sparse_index_rev(Index - id_set_weight(Set), Next)
	).

sparse_index(var(ID), Set, Index) :- id_sparse_index(ID, Set, Index).
sparse_index(var(ID), Set) = id_sparse_index(ID, Set).

reverse_sparse_index(sparse_index(Var, Set), Set) = Var.

%-----------------------------------------------------------------------------%
% Church indexing
	
id_church_index(ID, VarSet, Cid) :- 
	var_set_church_index(ID, VarSet, Cid).
	
id_church_index(ID, Set) = Cid :- id_church_index(ID, Set, Cid).

id_reverse_church_index(id_church_index(ID, Set), Set) = ID.

church_index(var(ID), Set, var(Cid)) :- id_church_index(ID, Set, Cid).
church_index(var(ID), Set) = var(id_church_index(ID, Set)).

reverse_church_index(church_index(Var, Set), Set) = Var.

%-----------------------------------------------------------------------------%
% Higher order

fold_id(P, Set, !A) :-
	(if empty_var_set(Set)
	then true
	else
		ID = var_set_first_id(Set),
		P(ID, !A),
		(if var_set_remove_id(ID, Set, Next)
		then
			fold_id(P, Next, !A)
		else
			unexpected($module, $pred, 
				"Failed to remove first id from set, should be impossible")
		)
	).
	
:- pred curry_fold(pred(mh_var, A, A), var_id, A, A).
:- mode curry_fold(in(pred(in, in, out) is det), in, in, out) is det.
:- mode curry_fold(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode curry_fold(in(pred(in, di, uo) is det), in, di, uo) is det.
/*:- mode curry_fold(in(pred(in, in, array_di, array_uo) is det), in, in, 
	array_di, array_uo) is det. */
:- mode curry_fold(in(pred(in, in, out) is semidet), in, in, out)
	is semidet.
:- mode curry_fold(in(pred(in, mdi, muo) is semidet), in, mdi, muo) 
	is semidet.
	
curry_fold(P, ID, !A) :- P(var(ID), !A).

fold(P, Map, !A) :-
	CurriedP = curry_fold(P),
	fold_id(CurriedP, Map, !A).
		
	
	