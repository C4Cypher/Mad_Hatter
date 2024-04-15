%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_relation.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_relation.

:- interface.

:- import_module mh_term.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type relation(T).

:- type relation == relation(mh_term).

:- type ground_relation == relation(mh_ground).

:- type state_relation == relation(mh_stval).


:- func relation(list(T)) = relation(T).
:- mode relation(in) = out is det.
:- mode relation(out) = in is det.

%-----------------------------------------------------------------------------%

:- type argument
--->	arg(int)
;		arg(int, argument).


%-----------------------------------------------------------------------------%

/* 	Different data structures behave differently when information in them
	changes. A new key value pair assigned to a map may remove an already
	existing pair if a value is already assigned to the given key.
	
	relation outcomes are expected to return with the logical results of 
	modifying a data structure, starting with the change that was asserted or
	retracted. */
	
:- type relation_outcome
--->	now_true(state_relation)
;		now_false(state_relation)
;		now_all(list(atomic_relation_outcome))
;		failed(string).	% Structure unmodified, reason provided in string.

:- type atomic_relation_outcome =< relation_outcome
--->	now_true(state_relation)
;		now_false(state_relation).

:- typeclass relation_state(T) where [
	
	%Create an empty data structure to be populated
	func init_relation = T, 
	mode init_relation = uo is det,

	pred assert_relation(state_relation, T, T, relation_outcome),
	mode assert_relation(in, in, out, out) is det,
	mode assert_relation(in, di, uo, out) is det,
	
	pred retract_relation(state_relation, T, T, relation_outcome),
	mode retract_relation(in, in, out, out) is det,
	mode retract_relation(in, di, uo, out) is det,
	
	pred query_relation(T, relation, relation),
	mode query_relation(in, in, out) is nondet,

	pred query_relation(T, state_relation),
	mode query_relation(in, in) is semidet,
	mode query_relation(in, out) is nondet
	].


:- implementation.


:- import_module array.

:- type relation(T) == array(T).


:- pragma promise_equivalent_clauses(relation/1).

relation(List::in) = (array(List)::out).

relation(to_list(Relation)::out) = (Relation::in).
