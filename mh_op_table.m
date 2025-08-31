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
:- import_module map.
:- import_module bool.

%-----------------------------------------------------------------------------%
% Mad Hatter Ops Table

:- type mh_op_table ---> mh_op_table(
	op_info_map :: map(string, op_infos),
	use_mercury_op_table :: bool	
).

:- instance op_table(mh_op_table).

:- func standard_mh_op_table = mh_op_table.

%-----------------------------------------------------------------------------%
% Operators

% Lazy Operator "?" fx 80
:- func lazy_operator_op_infos = op_infos.

% Co-function arrow "<-" xfy 1050
:- func cofunction_operator_op_infos = op_infos.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.


%-----------------------------------------------------------------------------%
% Mad Hatter Ops Table

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

:- pred mh_op_table_lookup_infix_op(mh_op_table::in, string::in, priority::out,
        arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is semidet.

mh_op_table_lookup_infix_op(mh_op_table(Map, UseMR), Name, OpPriority,
	LeftGtOrGe,	RightGtOrGe) :-
	
	(if 
		search(Map, Name, op_infos(in(P, Left, Right), _, _, _))
	then
		OpPriority = P, LeftGtOrGe = Left, RightGtOrGe = Right
	else 
		UseMR = yes,
		mercury_op_table_search_infix_op(Name, OpPriority, LeftGtOrGe,
			RightGtOrGe)
	).
	
	

:- pred mh_op_table_lookup_prefix_op(mh_op_table::in, string::in,
        priority::out, arg_prio_gt_or_ge::out) is semidet.
	
mh_op_table_lookup_prefix_op(mh_op_table(Map, UseMR), Name, OpPriority,
	RightGtOrGe) :-
	
	(if 
		search(Map, Name, op_infos(_, _, pre(P, Right), _))
	then
		OpPriority = P, 
		RightGtOrGe = Right
	else 
		UseMR = yes,
		mercury_op_table_search_prefix_op(Name, OpPriority, RightGtOrGe)
	).

:- pred mh_op_table_lookup_binary_prefix_op(mh_op_table::in, string::in,
        priority::out, arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out)
        is semidet.

mh_op_table_lookup_binary_prefix_op(mh_op_table(Map, UseMR), Name, OpPriority,
	LeftGtOrGe, RightGtOrGe) :-
	
	(if 
		search(Map, Name, op_infos(_, bin_pre(P, Left, Right), _, _))
	then
		OpPriority = P, LeftGtOrGe = Left, RightGtOrGe = Right
	else 
		UseMR = yes,
		mercury_op_table_search_binary_prefix_op(Name, OpPriority, LeftGtOrGe,
			RightGtOrGe)
	).

		
:- pred mh_op_table_lookup_postfix_op(mh_op_table::in, string::in, 
	priority::out, arg_prio_gt_or_ge::out) is semidet.
		
mh_op_table_lookup_postfix_op(mh_op_table(Map, UseMR), Name, OpPriority, 		LeftGtOrGe) :-

	(if 
		search(Map, Name, op_infos(_, _, _, post(P, Left)))
	then
		OpPriority = P, 
		LeftGtOrGe = Left
	else 
		UseMR = yes,
		mercury_op_table_search_postfix_op(Name, OpPriority, LeftGtOrGe)
	).

:- pred mh_op_table_is_op(mh_op_table::in, string::in) is semidet.

mh_op_table_is_op(mh_op_table(Map, UseMR), Name) :-
	contains(Map, Name) ; UseMR = yes, mercury_op_table_is_op(Name).

:- pred mh_op_table_lookup_op_infos(mh_op_table::in, string::in,
	op_infos::out)	is semidet.

mh_op_table_lookup_op_infos(mh_op_table(Map, UseMR), Name, OpInfos) :-
	(if 
		search(Map, Name, OI)
	then 
		OpInfos = OI
	else
		UseMR = yes,
		mercury_op_table_search_op_infos(Name, OpInfos)
	).
	
:- pred mh_op_table_lookup_operator_term(mh_op_table::in, priority::out,
        arg_prio_gt_or_ge::out, arg_prio_gt_or_ge::out) is det.

	% I don't see any reason to deviate from standard Mercury behavior for 
	% 'operator terms'  which are terms of the form "X `Op` Y"
mh_op_table_lookup_operator_term(_OpTable, OpPriority, LeftGtOrGe, RightGtOrGe) :-
    mercury_op_table_lookup_operator_term(OpPriority, LeftGtOrGe, RightGtOrGe).

	% Mercury returns 0 ... no reason to change
:- func mh_op_table_universal_priority(mh_op_table) = priority.

mh_op_table_universal_priority(_Table) =
    mercury_op_table_universal_priority.

	% Mercury returns 1 ... no reason to change
:- func mh_op_table_loosest_op_priority(mh_op_table) = priority.

mh_op_table_loosest_op_priority(_Table) =
    mercury_op_table_loosest_op_priority.

:- func mh_op_table_tightest_op_priority(mh_op_table) = priority.

mh_op_table_tightest_op_priority(mh_op_table(Map, UseMR)) = 
	foldl(fold_tightest_priority, Map,
		(if UseMR = yes 
		then 
			mercury_op_table_tightest_op_priority 
		else
			prio(0u)
		)
	).
	
:- func fold_tightest_priority(string, op_infos, priority)  = priority.


% Ugly as sin if chain, but I don't see a better way of doing this
fold_tightest_priority(_, Infos, !.Tightest ) = !:Tightest :-

	( if oi_infix(Infos) = in(InfixP, _, _), priority_gt(InfixP, !.Tightest)
	then
		!:Tightest = InfixP
	else true
	),
	
	( if 
		oi_binary_prefix(Infos) = bin_pre(BinPreP, _, _), 
		priority_gt(BinPreP, !.Tightest)
	then
		!:Tightest = BinPreP
	else true
	),
	
	( if 
		oi_prefix(Infos) = pre(PreP, _), 
		priority_gt(PreP, !.Tightest)
	then
		!:Tightest = PreP
	else true
	),
	
	( if 
		oi_postfix(Infos) = post(PostP, _), 
		priority_gt(PostP, !.Tightest)
	then
		!:Tightest = PostP
	else true
	).

% Let's not overload the priority for ',' and ';' for now, please?

:- func mh_op_table_comma_priority(mh_op_table) = priority.
	
mh_op_table_comma_priority(_Table) =
    mercury_op_table_comma_priority.

% No idea what messing with this will do to the syntax, for now will leave 
% alone

:- func mh_op_table_arg_priority(mh_op_table) = priority.

mh_op_table_arg_priority(_Table) =
    mercury_op_table_arg_priority.
	
standard_mh_op_table = mh_op_table(Map, yes) :-
	some [!M] (
		!:M = init,
		det_insert("?", lazy_operator_op_infos, !M),
		det_insert("<-", cofunction_operator_op_infos, !M),
		Map = !.M
	).
	
%-----------------------------------------------------------------------------%
% Operators

% Lazy Operator "?" fx 80
lazy_operator_op_infos = 
	op_infos(
		no_in, 
		no_bin_pre, 
		pre(prio(80u), arg_gt),
		no_post
	).

% Co-function arrow "<-" xfy 1050
cofunction_operator_op_infos = 
	op_infos(
		in(prio(1050u), arg_ge, arg_gt), 
		no_bin_pre, 
		no_pre,
		no_post
	).
