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
:- import_module array.

:- type relation == array(mh_term).

:- inst relation == relation(ground).

:- inst relation(I) == array(I).

:- func relation(list(mh_term)) = relation.
:- mode relation(in) = out is det.
:- mode relation(out) = in is det.


:- type ground_relation == array(mh_ground).

:- inst ground_relation == relation(mh_ground).

:- func ground_relation(list(mh_ground)) = ground_relation.
:- mode ground_relation(in) = out is det.
:- mode ground_relation(out) = in is det.

/* 	Different data structures behave differently when information in them
	changes. A new key value pair assigned to a map may remove an already
	existing pair if a value is already assigned to the given key.
	
	relation outcomes are expected to return with the logical results of 
	modifying a data structure, starting with the change that was asserted or
	retracted. */
	
:- type relation_outcome
--->	now_true(ground_relation)
;		now_false(ground_relation)
;		now_all(list(atomic_relation_outcome))
;		failed(string).	% Structure unmodified, reason provided in string.

:- type atomic_relation_outcome =< relation_outcome
--->	now_true(ground_relation)
;		now_false(ground_relation).

:- typeclass relation_state(T) where [
	
	%Create an empty data structure to be populated
	func init_relation = T, 
	mode init_relation = uo is det,

	pred assert_relation(ground_relation, T, T, relation_outcome),
	mode assert_relation(in, in, out, out) is det,
	mode assert_relation(in, di, uo, out) is det,
	
	pred retract_relation(ground_relation, T, T, relation_outcome),
	mode retract_relation(in, in, out, out) is det,
	mode retract_relation(in, di, uo, out) is det,
	
	pred query_relation(T, relation, relation),
	mode query_relation(in, in, out) is nondet ].


:- implementation.

:- pragma promise_equivalent_clauses(relation/1).

relation(List::in) = (array(List)::out).

relation(to_list(Relation)::out) = (Relation::in).

:- pragma promise_equivalent_clauses(ground_relation/1).

ground_relation(List::in) = (array(List)::out).

ground_relation(to_list(Relation)::out) = (Relation::in).