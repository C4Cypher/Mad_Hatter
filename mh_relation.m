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
:- import_module mh_index.

:- import_module array.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type proc_relation 
	--->	relation(array(mh_term))
	;		function_relation(array(mh_term)).

:- type relation =< proc_relation
	---> relation(array(mh_term)).
	
:- type function_relation =< proc_relation
	---> function_relation(array(mh_term)).
	
%-----------------------------------------------------------------------------%

:- type ground_proc_relation =< proc_relation
	--->	relation(array(mh_ground))
	;		function_relation(array(mh_ground)).

:- type ground_relation =< relation
	---> relation(array(mh_ground)).
	
:- type ground_function_relation =< function_relation
	---> function_relation(array(mh_ground)).

%-----------------------------------------------------------------------------%

:- type state_proc_relation =< proc_relation
	--->	relation(array(mh_stval))
	;		function_relation(array(mh_stval)).

:- type state_relation =< relation
	---> relation(array(mh_stval)).
	
:- type state_function_relation =< function_relation
	---> function_relation(array(mh_stval)).

%-----------------------------------------------------------------------------%

:- func relation_arguments(list(mh_term)) = relation.
:- mode relation_arguments(in) = out is det.
:- mode relation_arguments(out) = in is det.

:- func function_relation_arguments(mh_term, list(mh_term)) = function_relation.
:- mode function_relation_arguments(in, in) = out is det.
:- mode function_relation_arguments(out, out) = in is semidet.

% init_relation(Size, InitVal, Relation).
% Create a new relation of specified size, filling all elements with InitVal 
:- pred init_relation(int::in, mh_term::in, relation::out) is det.

:- func init_relation(int, mh_term) = relation.

% init_function_relation(Args, InitVal, Relation) 
% creates an array of size Args+1
:- pred init_function_relation(int::in, mh_term::in, function_relation::out) 
	is det. 
	
:- func init_function_relation(int, mh_term) = function_relation.
	
%-----------------------------------------------------------------------------%
	
:- instance index(proc_relation, 	mh_term).
:- instance index(relation, 		mh_term).
:- instance index(function_relation, mh_term).

:- instance index(ground_proc_relation, mh_ground).
:- instance index(ground_relation, 		mh_ground).
:- instance index(ground_function_relation, mh_ground).

:- instance index(state_proc_relation, 	mh_stval).
:- instance index(state_relation, 		mh_stval).
:- instance index(state_function_relation, mh_stval).

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

%-----------------------------------------------------------------------------%

:- implementation.


:- pragma promise_equivalent_clauses(relation_arguments/1).

relation_arguments(List::in) = (relation(array(List))::out).

relation_arguments(to_list(Array)::out) = (relation(Array)::in).

:- pragma promise_equivalent_clauses(function_relation_arguments/2).

function_relation_arguments(T::in, List::in) = 
	(function_relation(array([T | List ]))::out).

function_relation_arguments(X::out, Xs::out) = 
	(function_relation(Array)::in) :-  to_list(Array) = [X | Xs].

init_relation(S, T, relation(A)) :- init(S, T, A).

init_relation(S, T) = relation(init(S, T)).

init_function_relation(S, T, function_relation(A)) :- init(S, T, A).

init_function_relation(S, T) = function_relation(init(S, T)).

%-----------------------------------------------------------------------------%

:- instance index(proc_relation, mh_term) where [

	valid_index(relation(A), I) :- valid_array_index1(A, I),
	valid_index(function_relation(A), I) :- valid_array_index0(A, I),
	
	index(relation(A), I, Term) :- array_index1(A, I, Term),
	index(function_relation(A), I, Term) :- array_index0(A, I, Term),
	
	set_index(I, Term, relation(!.A), relation(!:A)) :- 
		set_array_index1(I, Term, !A),
	set_index(I, Term, function_relation(!.A), function_relation(!:A)) :-
		set_array_index0(I, Term, !A)
	].
	
:- instance index(relation, mh_term) where [
	valid_index(relation(A), I) :- valid_array_index1(A, I),
	index(relation(A), I, Term) :- array_index1(A, I, Term),
	set_index(I, Term, relation(!.A), relation(!:A)) :- 
		set_array_index1(I, Term, !A) ].
	
:- instance index(function_relation, mh_term) where [
	valid_index(function_relation(A), I) :- valid_array_index0(A, I),
	index(function_relation(A), I, Term) :- array_index0(A, I, Term),
	set_index(I, Term, function_relation(!.A), function_relation(!:A)) :-
		set_array_index0(I, Term, !A) ].
	
:- instance index(ground_proc_relation, mh_ground) where [

	valid_index(relation(A), I) :- valid_array_index1(A, I),
	valid_index(function_relation(A), I) :- valid_array_index0(A, I),
	
	index(relation(A), I, Term) :- array_index1(A, I, Term),
	index(function_relation(A), I, Term) :- array_index0(A, I, Term),
	
	set_index(I, Term, relation(!.A), relation(!:A)) :- 
		set_array_index1(I, Term, !A),
	set_index(I, Term, function_relation(!.A), function_relation(!:A)) :-
		set_array_index0(I, Term, !A) ].
	
:- instance index(ground_relation, mh_ground) where [
	valid_index(relation(A), I) :- valid_array_index1(A, I),
	index(relation(A), I, Term) :- array_index1(A, I, Term),
	set_index(I, Term, relation(!.A), relation(!:A)) :- 
		set_array_index1(I, Term, !A) ].
	
:- instance index(ground_function_relation, mh_ground) where [
	valid_index(function_relation(A), I) :- valid_array_index0(A, I),
	index(function_relation(A), I, Term) :- array_index0(A, I, Term),
	set_index(I, Term, function_relation(!.A), function_relation(!:A)) :-
		set_array_index0(I, Term, !A) ].
	
:- instance index(state_proc_relation, mh_stval) where [

	valid_index(relation(A), I) :- valid_array_index1(A, I),
	valid_index(function_relation(A), I) :- valid_array_index0(A, I),
	
	index(relation(A), I, Term) :- array_index1(A, I, Term),
	index(function_relation(A), I, Term) :- array_index0(A, I, Term),
	
	set_index(I, Term, relation(!.A), relation(!:A)) :- 
		set_array_index1(I, Term, !A),
	set_index(I, Term, function_relation(!.A), function_relation(!:A)) :-
		set_array_index0(I, Term, !A) ].
	
:- instance index(state_relation, mh_stval) where [
	valid_index(relation(A), I) :- valid_array_index1(A, I),
	index(relation(A), I, Term) :- array_index1(A, I, Term),
	set_index(I, Term, relation(!.A), relation(!:A)) :- 
		set_array_index1(I, Term, !A) ].
	
:- instance index(state_function_relation, mh_stval) where [
	valid_index(function_relation(A), I) :- valid_array_index0(A, I),
	index(function_relation(A), I, Term) :- array_index0(A, I, Term),
	set_index(I, Term, function_relation(!.A), function_relation(!:A)) :-
		set_array_index0(I, Term, !A) ].
	
	

	

	
