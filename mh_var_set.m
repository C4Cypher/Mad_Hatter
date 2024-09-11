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
	
	
:- func complete_var_set(var_id_set) = mh_var_set.
:- mode complete_var_set(in) = out is det.
:- mode complete_var_set(out) = in is semidet.

:- func contiguous_var_set(var_id_offset, var_id_set) = mh_var_set.
:- mode contiguous_var_set(in, in) = out is det.
:- mode contiguous_var_set(out, out) = in is semidet.

:- pred valid_var_set(mh_var_set::in) is semidet.
:- pred require_valid_var_set(mh_var_set::in) is det.

:- pred is_complete_var_set(mh_var_set::in) is semidet.
:- pred is_contiguous_var_set(mh_var_set::in) is semidet.

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
% Var Set composition

:- pred var_set_union(mh_var_set::in, mh_var_set::in, mh_var_set::out) is det.
:- func var_set_union(mh_var_set, mh_var_set) = mh_var_set.

:- pred var_set_intersection(mh_var_set::in, mh_var_set::in, mh_var_set::out) 
	is det.
:- func var_set_intersection(mh_var_set, mh_var_set) = mh_var_set.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%
% Variable sets

:- type mh_var_set
	--->	var_set(var_id_offset, var_id_set)
	;		var_set(var_id_offset, var_id_set, mh_var_set).

complete_var_set(Set) = var_set(null_var_id_offset, Set).

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

% var_set_insert_id(_, _, _) :- sorry($module,$pred), semidet_true.
	

/* Bad Implementation
:- pred var_set_insert_id_cc(var_id, mh_var_set, mh_var_set).
:- mode var_set_insert_id_cc(in, in, out) is cc_nondet.
:- mode var_set_insert_id_cc(in, out, in) is cc_nondet.
:- mode var_set_insert_id_cc(out, in, in) is cc_nondet.


% var_set_insert_id_cc(ID, Set1, Set2)

% If ID is greater than the element after the last element of the first pair
% of Set1, and greater than the element before the last element of the
% next element of Set1, insert ID into the next var set of Set1 		

var_set_insert_id_cc(
	ID, 
	var_set(Offset, Set, !.Next),
	var_set(Offset, Set, !:Next)
	) :-
		var_id_gt(ID, next_var_id(last_var_id(Set))),
		var_id_ge(ID, previous_var_id(var_set_first_id(!.Next))),
		var_set_insert_id_cc(ID, !Next).


% Set1 is empty and ID is the only element of Set2
var_set_insert_id_cc(
	first_var_id(Offset) @ last_var_id(Set), 
	empty_var_set, 
	var_set(Offset, Set)
	).

% ID is less than the element prior to the first element of Set1,
% Set1 is the next set after Set2, and 
% ID is the first and last element of Set2
% Basically, insert ID before Set1
var_set_insert_id_cc(ID, First, var_set(Offset, Set, First) ) :-
	var_id_lt(ID, previous_var_id(var_set_first_id(First))),
	ID = first_var_id(Offset) @ last_var_id(Set),
	expect(var_id_ge(ID, first_var_id), $module, $pred,
		"Attempted to insert invalid (less than one) var_id into var_set").

% ID is the first elment of Set2 and one prior to the first element of Set1
var_set_insert_id_cc(ID, var_set(!.Offset, Set), var_set(!:Offset, Set) ) :-
	new_prepended_id(!Offset, ID).
	
var_set_insert_id_cc(ID, var_set(!.Offset, Set, Next), 
	var_set(!:Offset, Set, Next) ) :-	
		new_prepended_id(!Offset, ID).
	

% ID is the last element of Set2 and the one after the last element of Set1		
var_set_insert_id_cc(ID, var_set(Offset, !.Set), var_set(Offset, !:Set) ) :-
	not_empty_var_id_set(!.Set),
	new_appended_id(!Set, ID).
	
% ID is more than the element after the last element of Set1
% Set2 is Set1 with a next set composed of ID
% Basically, insert ID after Set1
var_set_insert_id_cc(
	ID,
	var_set(Offset, Set), 
	var_set(Offset, Set, var_set(NextOffset, NextSet)) 
	) :-
		var_id_gt(ID, next_var_id(last_var_id(Set))),
		ID = first_var_id(NextOffset) @ last_var_id(NextSet).

% Add ID to the end of the first pair of Set1, ID is less than one element 
% before the next pair of Set1
var_set_insert_id_cc(
	ID, 
	var_set(Offset, !.Set, Next),
	var_set(Offset, !:Set, Next)
	) :-	
		new_appended_id(!Set, ID),
		var_id_lt(ID, previous_var_id(var_set_first_id(Next))).

% ID is between the first pair of Set1 and the next pair
% ID must be greater than the element after the last element of the first pair
% and the element before the first element of the next pair
var_set_insert_id_cc(
	ID, 
	var_set(Offset, Set, Next),
	var_set(Offset, Set, var_set(NewOffset, NewSet, Next))
	) :-	
		var_id_gt(ID, next_var_id(last_var_id(Set))),
		var_id_lt(ID, previous_var_id(var_set_first_id(Next))),
		ID = first_var_id(NewOffset) @ last_var_id(NewSet).
		
% ID is the missing element between the first and next pairs of Set1
% Set2 is the union of the first two pairs of Set1 and ID	
var_set_insert_id_cc(
	ID,
	var_set(Offset, Set, var_set(NextOffset, NextSet)),
	var_set(Offset, NextSet)
	) :-
	ID = next_var_id(last_var_id(Set)) @ 
		previous_var_id(first_var_id(NextOffset)).
		
var_set_insert_id_cc(
	ID,
	var_set(Offset, Set, var_set(NextOffset, NextSet, NextNext)),
	var_set(Offset, NextSet, NextNext)
	) :-
	ID = next_var_id(last_var_id(Set)) @ 
		previous_var_id(first_var_id(NextOffset)).	


		
% If ID is greater than the element after the last element of the only pair in
% Set1, insert it as the next set in Set2

var_set_insert_id_cc(
	ID, 
	var_set(Offset, Set), 
	var_set(Offset, Set, var_set(NextOffset, NextSet))
	) :-
		var_id_gt(ID, next_var_id(last_var_id(Set))),
		ID = first_var_id(NextOffset) @ last_var_id(NextSet).
	
*/	
	

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
	(if var_id_gt(first_var_id(OG), next_var_id(last_var_id(SL)))
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
		)
:-
	offset_order(O1, O2, OL, OG),
	var_id_set_order(S1, S2, SL, SG).
	
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
	offset_order(O1, O2, OL, OG),
	var_id_set_order(S1, S2, SL, SG).
	
:- pred var_set_intersection_pairs(
	var_id_offset::in, var_id_set::in,
	var_id_offset::in, var_id_set::in,
	mh_var_set::out) is det.
	
var_set_intersection_pairs(O1, S1, O2, S2, 
	var_set_intersection_pairs(O1, S1, O2, S2)).
	

:- pred var_set_intersection_pairs(
	var_id_offset::in, var_id_set::in,
	var_id_offset::in, var_id_set::in,
	var_id_offset::out, var_id_set::out) is semidet.
	
var_set_intersection_pairs(
	O1, S1,
	O2, S2,
	OG, SL) :-
	
	offset_order(O1, O2, OL, OG),
	var_id_set_order(S1, S2, SL, SG),
	var_id_ge(last_var_id(SL), first_var_id(OG)).