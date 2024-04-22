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

:- import_module array.
:- import_module list.

%-----------------------------------------------------------------------------%


:- type relation(T) == array(T).

:- type relation == relation(mh_term).

:- type ground_relation == relation(mh_ground).

:- type state_relation == relation(mh_stval).

:- inst relaiton == array.

:- inst uniq_relation(I) == uniq_array(I).

:- inst uniq_relation == uniq_array.

:- mode rdi == array_di.
:- mode ruo == array_uo.
:- mode rui == array_ui.



:- func relation(list(T)) = relation(T).
:- mode relation(in) = out is det.
:- mode relation(out) = in is det.

% init_relation(Size, InitVal, Relation).
% Create a new relation of specified size, filling all elements with InitVal 
:- pred init_relation(int::in, T::in, relation(T)::ruo) is det.

:- func init_relation(int::in, T::in) = (relation(T)::ruo) is det.

%-----------------------------------------------------------------------------%
% destructive update, fails if indexs is out of bounds

update_relation(int::in, T::in, relation(T)::rdi, relation(T)::ruo) is semidet.





%-----------------------------------------------------------------------------%
% Definitions for instances of the index typeclass

:- pred valid_relation_index0(relation(T), int).
:- mode valid_relation_index0(in, in) is semidet.
:- mode valid_relation_index0(in, out) is nondet. 

:- pred relation_index0(relation(T), int, T).
:- mode relation_index0(in, in, in) is semidet.
:- mode relation_index0(in, in, out) is semidet. 
:- mode relation_index0(in, out, out) is nondet. 
	
:- pred set_relation_index0(int, T, relation(T), relation(T)).
:- mode set_relation_index0(in, in, in, out) is semidet. 
:- mode set_relation_index0(out, in, in, out) is nondet.

%-----------------------------------------------------------------------------%

:- pred valid_relation_index1(relation(T), int).
:- mode valid_relation_index1(in, in) is semidet.
:- mode valid_relation_index1(in, out) is nondet. 

:- pred relation_index1(relation(T), int, T).
:- mode relation_index1(in, in, in) is semidet.
:- mode relation_index1(in, in, out) is semidet. 
:- mode relation_index1(in, out, out) is nondet. 
	
:- pred set_relation_index1(int, T, relation(T), relation(T)).
:- mode set_relation_index1(in, in, in, out) is semidet. 
:- mode set_relation_index1(out, in, in, out) is nondet.




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

:- import_module mh_index.



:- pragma promise_equivalent_clauses(relation/1).

relation(List::in) = (array(List)::out).

relation(to_list(Relation)::out) = (Relation::in).

init_relation(S, T, R) :- init(S, T, R).

init_relation(S, T) = init(S, T).

update_relation(I, T, !R) :- semidet_set(I, T, !R).

%-----------------------------------------------------------------------------%

valid_relation_index0(R, I) :- valid_array_index0(R, I).

relation_index0(R, I, T) :- array_index0(R, I, T).

set_relation_index0(I, T, !R) :- set_array_index0(I, T, !R).

%-----------------------------------------------------------------------------%

valid_relation_index1(R, I) :- valid_array_index1(R, I).

relation_index1(R, I, T) :- array_index1(R, I, T).

set_relation_index1(I, T, !R) :- set_array_index1(I, T, !R).

%-----------------------------------------------------------------------------%