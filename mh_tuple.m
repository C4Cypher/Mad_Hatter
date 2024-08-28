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
:- import_module mh_substitution.


%-----------------------------------------------------------------------------%
% Tuple type

% Represents an indexable tuple of terms

:- type mh_tuple
	--->	some [T] mr_tuple(T) => mr_tuple(T)
	;		list_tuple(list(mh_term))
	;		array_tuple(array(mh_term))
	;		tuple_sub(mh_tuple, mh_substitution).

:- func tuple(T) = mh_tuple <= mr_tuple(T).
:- mode tuple(in) = out is det.
:- mode tuple(out) = in is semidet.

:- pred tuple_size(mh_tuple::in, int::out) is det.
:- func tuple_size(mh_tuple) = int.

:- instance arity(mh_tuple).
:- instance mr_tuple(mh_tuple).

%-----------------------------------------------------------------------------%
% Tuple indexing

% :- pred tuple_contains(mr_tuple::in, int::in) is semidet.

% % Throw an exception if index is not found
% :- pred tuple_lookup(mr_tuple::in, int::in, mh_term::out) is det.

% :- pred tuple_search(mr_tuple::in, int::in, mh_term::out) is semidet.

% :- pred tuple_member(mr_tuple::in, int::out, mh_term::out) is nondet.


%-----------------------------------------------------------------------------%
% Tuple typeclass

% A mercury type that can represent a tuple

:- typeclass mr_tuple(T) <= arity(T) where [

	% tuple_index(Tuple, Index, Term) 
	% retreive the Term from the Tuple at Index, throw an exception if
	% index is out of range, nondet version will fail if tuple is of zero arity
	pred mr_tuple_index(T, int, mh_term),
	mode mr_tuple_index(in, in, out) is det,
	mode mr_tuple_index(in, out, out) is nondet,
	
	% fold_tuple(Closure, Relation, !Accumulator)
	% perform a left fold from index 1 to arity(Relation), calling Closure on
	% each term in Relation with !Accumulator
	pred fold_mr_tuple(pred(mh_term, A, A), T, A, A),
	mode fold_mr_tuple(pred(in, in, out) is det, in, in, out) is det
	
].

%-----------------------------------------------------------------------------%
% Function versions and determinism casts of tuple typeclass methods

:- func mr_tuple_index(T, int) = mh_term <= mr_tuple(T).

:- func fold_mr_tuple(func(mh_term, A) = A, T, A) = A <= mr_tuple(T).

:- pred det_mr_tuple_index(T::in, int::in, mh_term::out) is det <= mr_tuple(T).
:- pred semidet_mr_tuple_index(T::in, int::in, mh_term::out) is semidet 
	<= mr_tuple(T).
:- pred nondet_mr_tuple_index(T::in, int::out, mh_term::out) is nondet 
	<= mr_tuple(T).
:- pred cc_nondet_mr_tuple_index(T::in, int::out, mh_term::out) is cc_nondet 
	<= mr_tuple(T).
	
	

	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.


:- import_module mh_index.



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
	
tuple_size(mr_tuple(T), S) :- arity(T, S).
tuple_size(list_tuple(L), S) :- length(L, S).
tuple_size(array_tuple(Array), S) :- size(Array, S).


:- instance arity(mh_tuple) where [
	pred(arity/2) is tuple_size
].

:- instance tuple(mh_tuple) where [
	
	mr_tuple_index(mr_tuple(T), I, Term) :- tuple_index(T, I, Term),
	mr_tuple_index(list_tuple(L), I, Term) :- list_index(L, I, Term),
	mr_tuple_index(array_tuple(A), I, Term) :- array_index(A, I, Term),
	
	fold_mr_tuple(Closure, mr_tuple(T), !A) :- fold_tuple(Closure, T, !A),
	fold_mr_tuple(Closure, list_tuple(L), !A) :- fold_list_index(Closure, L, !A),
	fold_mr_tuple(Closure, array_mr_tuple(T), !A) :- 
		fold_array_index(Closure, T, !A)
].


%-----------------------------------------------------------------------------%
% Tuple indexing

% tuple_contains(Tuple, Index) :- Index > 0, Index =< tuple_size(Tuple).

% tuple_lookup(mr_tuple(T), Index, Term) :- mr_tuple_index(T, Index, Term).
% tuple_lookup(list_tuple(L), Index, Term) :- list_index(L, Index, Term).
% tuple_lookup(array_tuple(A), Index, Term) :- array_index(A, Index, Term).

% tuple_lookup(tuple_sub(Tuple, Sub), Index, Term) :-
	% tuple_lookup(Tuple, Index, Term0),
	


%-----------------------------------------------------------------------------%
% Function versions and determinism casts of tuple class methods

mr_tuple_index(T, I) = Term :- mr_tuple_index(T, I, Term).

fold_mr_tuple(Func, T, !.A) = !:A :- 
	fold_mr_tuple(function_closure(Func), T, !A).

:- pred function_closure(func(T, A) = A, T, A, A).
:- mode function_closure(func(in, in) = out is det, in, in, out) is det.

function_closure(Func, T, !A) :- Func(T, !.A) = !:A.

det_mr_tuple_index(T, I, U) :- tuple_index(T, I, U).
semidet_mr_tuple_index(T, I, U) :- valid_index(T, I), tuple_index(T, I, U).
nondet_mr_tuple_index(T, I, U) :- valid_index(T, I), tuple_index(T, I, U).
cc_nondet_mr_tuple_index(T, I, U) :- valid_index(T, I), tuple_index(T, I, U).