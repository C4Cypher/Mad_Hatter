%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_op_table.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_op_table.

:- interface.

:- import_module ops.

%-----------------------------------------------------------------------------%
% Mad Hatter Ops Table

:- type mh_op_table.


:- func init_mh_op_table = (mh_op_table::uo) is det.

:- instance op_table(mh_op_table).
%-----------------------------------------------------------------------------%



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.


%-----------------------------------------------------------------------------%
% Mad Hatter Ops Table

:- type mh_op_table ---> mh_op_table.

:- instance op_table(mh_op_table) where [
pred(lookup_infix_op/5) is          	mh_op_table_lookup_infix_op,
    pred(lookup_prefix_op/4) is         mh_op_table_lookup_prefix_op,
    pred(lookup_binary_prefix_op/5) is  mh_op_table_lookup_binary_prefix_op,
    pred(lookup_postfix_op/4) is        mh_op_table_lookup_postfix_op,
    pred(is_op/2) is                    mh_op_table_is_op,
    pred(lookup_op_infos/3) is          mh_op_table_lookup_op_infos,
    pred(lookup_operator_term/4) is     mh_op_table_lookup_operator_term,
    func(universal_priority/1) is       mh_op_table_universal_priority,
    func(loosest_op_priority/1) is      mh_op_table_loosest_op_priority,
    func(tightest_op_priority/1) is     mh_op_table_tightest_op_priority,
    func(comma_priority/1) is           mh_op_table_comma_priority,
    func(arg_priority/1) is             mh_op_table_arg_priority

].

init_mh_op_table = mh_op_table.

:- pred mh_op_table_lookup_infix_op(mh_op_table::in, string::in, priority::out,
        arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is semidet.

mh_op_table_lookup_infix_op(_OpTable, Name, OpPriority, LeftGtOrGe, 
	RightGtOrGe) :-
	mercury_op_table_search_infix_op(Name, OpPriority, LeftGtOrGe, RightGtOrGe).

:- pred mh_op_table_lookup_prefix_op(mh_op_table::in, string::in,
        priority::out, arg_prio_gt_or_ge::out) is semidet.
	
mh_op_table_lookup_prefix_op(_OpTable, Name, OpPriority, LeftGtOrGe) :-
	if (Name = "?") 
	then
		OpPriority = lazy_operator_priority, 
		LeftGtOrGe = lazy_operator_right_arg_prio_gt_or_ge
	else
		mercury_op_table_search_prefix_op(Name, OpPriority, LeftGtOrGe).

:- pred mh_op_table_lookup_binary_prefix_op(mh_op_table::in, string::in,
        priority::out, arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out)
        is semidet.

mh_op_table_lookup_binary_prefix_op(_OpTable, Name, OpPriority,
        LeftGtOrGe, RightGtOrGe) :-
    mercury_op_table_search_binary_prefix_op(Name, OpPriority,
        LeftGtOrGe, RightGtOrGe).

		
:- pred mh_op_table_lookup_postfix_op(mh_op_table::in, string::in, priority::out,
        arg_prio_gt_or_ge::out) is semidet.
		
mh_op_table_lookup_postfix_op(_OpTable, Name, OpPriority, LeftGtOrGe) :-
    mercury_op_table_search_postfix_op(Name, OpPriority, LeftGtOrGe).

:- pred mh_op_table_is_op(mh_op_table::in, string::in) is semidet.

mh_op_table_is_op(_OpTable, Name) :-
    mercury_op_table_is_op(Name) ; Name = "?".

:- pred mh_op_table_lookup_op_infos(mh_op_table::in, string::in, op_infos::out) 
	is semidet.

mh_op_table_lookup_op_infos(_OpTable, Name, OpInfos) :-
	if Name = "?" then
		OpInfos = lazy_operator_op_infos
	else
		mercury_op_table_search_op_infos(Name, OpInfos).
	
:- pred mh_op_table_lookup_operator_term(mh_op_table::in, priority::out,
        arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is det.

mh_op_table_lookup_operator_term(_OpTable, OpPriority, LeftGtOrGe, RightGtOrGe) :-
    mercury_op_table_lookup_operator_term(OpPriority, LeftGtOrGe, RightGtOrGe).

:- func mh_op_table_universal_priority(mh_op_table) = priority.

mh_op_table_universal_priority(_Table) =
    mercury_op_table_universal_priority.

:- func mh_op_table_loosest_op_priority(mh_op_table) = priority.

mh_op_table_loosest_op_priority(_Table) =
    mercury_op_table_loosest_op_priority.

:- func mh_op_table_tightest_op_priority(mh_op_table) = priority.

mh_op_table_tightest_op_priority(_Table) =
    mercury_op_table_tightest_op_priority.

:- func mh_op_table_comma_priority(mh_op_table) = priority.
	
mh_op_table_comma_priority(_Table) =
    mercury_op_table_comma_priority.

:- func mh_op_table_arg_priority(mh_op_table) = priority.

mh_op_table_arg_priority(_Table) =
    mercury_op_table_arg_priority.
	
%-----------------------------------------------------------------------------%
% Lazy Operator "?"

% Remember kiddos, constant values must only be defined ONCE

:- func lazy_operator_op_infos = op_infos.

lazy_operator_op_infos = 
	op_infos(
		no_in, 
		no_bin_pre, 
		lazy_operator_maybe_op_info_prefix,
		no_post
	).

:- func lazy_operator_maybe_op_info_prefix = maybe_op_info_prefix.

lazy_operator_maybe_op_info_prefix = 
	pre(lazy_operator_priority, lazy_operator_right_arg_prio_gt_or_ge).

:- func lazy_operator_priority = priority.

lazy_operator_priority = prio(80u).

:- func lazy_operator_right_arg_prio_gt_or_ge = arg_prio_gt_or_ge.

lazy_operator_right_arg_prio_gt_or_ge = arg_gt.