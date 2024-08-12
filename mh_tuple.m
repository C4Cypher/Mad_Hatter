%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_tuple.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_tuple.

:- interface.

:- import_module list.
:- import_module array.

:- import_module mh_arity.
:- import_module mh_term.

%-----------------------------------------------------------------------------%
% Tuple typeclass

% Represents an indexable tuple of terms

:- typeclass tuple(T) <= arity(T) where [

	% tuple_index(Tuple, Index, Term) 
	% retreive the Term from the Tuple at Index, throw an exception if
	% index is out of range, nondet version will fail if tuple is of zero arity
	pred tuple_index(T, int, mh_term),
	mode tuple_index(in, in, out) is det,
	mode tuple_index(in, out, out) is nondet,
	
	% fold_tuple(Closure, Relation, !Accumulator)
	% perform a left fold from index 1 to arity(Relation), calling Closure on
	% each term in Relation with !Accumulator
	pred fold_tuple(pred(mh_term, A, A), T, A, A),
	mode fold_tuple(pred(in, in, out) is det, in, in, out) is det
	
].

%-----------------------------------------------------------------------------%
% Function versions and determinism casts of tuple methods

:- func tuple_index(T, int) = mh_term <= tuple(T).

:- func fold_tuple(func(mh_term, A) = A, T, A) = A <= tuple(T).

:- pred det_tuple_index(T::in, int::in, mh_term::out) is det <= tuple(T).
:- pred semidet_tuple_index(T::in, int::in, mh_term::out) is semidet 
	<= tuple(T).
:- pred nondet_tuple_index(T::in, int::out, mh_term::out) is nondet <= tuple(T).
:- pred cc_nondet_tuple_index(T::in, int::out, mh_term::out) is cc_nondet 
	<= tuple(T).
	

%-----------------------------------------------------------------------------%
% Tuple type

:- type mh_tuple
	--->	some [T] mr_tuple(T) => tuple(T)
	;		list_tuple(list(mh_term))
	;		array_tuple(array(mh_term)).

:- func tuple(T) = mh_tuple <= tuple(T).
:- mode tuple(in) = out is det.
:- mode tuple(out) = in is semidet.

:- instance arity(mh_tuple).
:- instance tuple(mh_tuple).
	

	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.


:- import_module mh_index.

%-----------------------------------------------------------------------------%
% Function versions and determinism casts of tuple methods

tuple_index(T, I) = Term :- tuple_index(T, I, Term).

fold_tuple(Func, T, !.A) = !:A :- fold_tuple(function_closure(Func), T, !A).

:- pred function_closure(func(T, A) = A, T, A, A).
:- mode function_closure(func(in, in) = out is det, in, in, out) is det.

function_closure(Func, T, !A) :- Func(T, !.A) = !:A.

det_tuple_index(T, I, U) :- tuple_index(T, I, U).
semidet_tuple_index(T, I, U) :- valid_index(T, I), tuple_index(T, I, U).
nondet_tuple_index(T, I, U) :- valid_index(T, I), tuple_index(T, I, U).
cc_nondet_tuple_index(T, I, U) :- valid_index(T, I), tuple_index(T, I, U).

%-----------------------------------------------------------------------------%
% Tuple type

:- pragma promise_equivalent_clauses(tuple/1).

tuple(T::in) = (Tuple::out) :-
	( if promise_equivalent_solutions [U] (
			dynamic_cast(T, U:mh_tuple);
			dynamic_cast(T, V:list(mh_term)), U = list_tuple(V);
			dynamic_cast(T, V:array(mh_term)), U = array_tuple(V);
			dynamic_cast(T, tuple_term(V):mh_term), U = tuple(V);
			dynamic_cast(T, tuple_term(V):compound_term), U = tuple(V)
		)
	then
		Tuple = U
	else
		Tuple = 'new mr_tuple'(T)
	).
		
tuple(T::out) = (Tuple::in) :-
	require_complete_switch [Tuple] (
		Tuple = mr_tuple(U), dynamic_cast(U, T);
		Tuple = list_tuple(U), dynamic_cast(U, T);
		Tuple = array_tuple(U), dynamic_cast(U, T)
	).


:- instance arity(mh_tuple) where [
	arity(mr_tuple(T), A) :- arity(T, A),
	arity(list_tuple(L), A) :- arity(L, A),
	arity(array_tuple(Array), A) :- arity(Array, A)
].

:- instance tuple(mh_tuple) where [
	
	tuple_index(mr_tuple(T), I, Term) :- tuple_index(T, I, Term),
	tuple_index(list_tuple(L), I, Term) :- list_index(L, I, Term),
	tuple_index(array_tuple(A), I, Term) :- array_index(A, I, Term),
	
	fold_tuple(Closure, mr_tuple(T), !A) :- fold_tuple(Closure, T, !A),
	fold_tuple(Closure, list_tuple(L), !A) :- fold_list_index(Closure, L, !A),
	fold_tuple(Closure, array_tuple(T), !A) :- fold_array_index(Closure, T, !A)
].

