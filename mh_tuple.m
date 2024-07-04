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

:- import_module mh_arity.
:- import_module mh_term.

% TODO:  create a commmon interface for unification? Typeclass? 

%-----------------------------------------------------------------------------%
% Tuple

% Represents an indexable tuple of terms

:- typeclass tuple(T) <= arity(T) where [

	% tuple_index(Tuple, Index, Term) 
	% retreive the Term from the Tuple at Index, throw an exception if
	% index is out of range, nondet version will fail if tuple is of zero arity
	pred tuple_index(T, int, mh_term),
	mode tuple_index(in, in, out) is det,
	mode tuple_index(in, out, out) is nondet,
	
	% convert_tuple_one_way(Tuple, Type)
	% Accept some other Tuple and return a tuple of this Type with the
	% same terms, fail if the types are incompatable
	pred convert_tuple_one_way(U::in, T::out)  is semidet <= tuple(U),
	
	% fold_tuple(Closure, Relation, !Accumulator)
	% perform a left fold from index 1 to arity(Relation), calling Closure on
	% each term in Relation with !Accumulator
	pred fold_tuple(pred(mh_term, A, A), T, A, A),
	mode fold_tuple(pred(in, in, out) is det, in, in, out) is det
	
].

%-----------------------------------------------------------------------------%
% Function versions and determinism casts of tuple methods

:- func tuple_index(T, int) = mh_term <= tuple(T).

:- func convert_tuple_one_way(T) = U <= (tuple(T), tuple(U)).

:- func fold_tuple(func(mh_term, A) = A, T, A) = A <= tuple(T).

:- pred det_tuple_index(T::in, int::in, U::out) is det <= tuple(T).
:- pred semidet_tuple_index(T::in, int::in, U::out) is semidet <= tuple(T).
:- pred nondet_tuple_index(T::in, int::out, U::out) is nondet <= tuple(T).
:- pred cc_nondet_tuple_index(T::in, int::out, U::out) is cc_nondet 
	<= tuple(T).
	
%-----------------------------------------------------------------------------%
% Tuple conversion

:- pred convert_tuple(T, U) <= (tuple(T), tuple(U)).
:- mode convert_tuple(in, out) is semidet.
:- mode convert_tuple(out, in) is semidet.

:- func convert_tuple(T) = U <= (tuple(T), tuple(U)).
:- mode convert_tuple(in) = out is semidet.
:- mode convert_tuple(out) = in is semidet.
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%
% Function versions and determinism casts of tuple methods

tuple_index(T, I) = Term :- tuple_index(T, I, Term).

convert_tuple_one_way(T) = U :- convert_tuple_one_way(T, U).

fold_tuple(Func, T, !.A) = !:A :- fold_tuple(function_closure(Func), T, !A).

:- pred function_closure(func(T, A) = A, T, A, A).
:- mode function_closure(func(in, in) = out is det, in, in, out) is det.

function_closure(Func, T, !A) :- Func(T, !.A) = !:A.

det_tuple_index(T, I, U) :- tuple_index(T, I, U).
semidet_tuple_index(T, I, U) :- valid_index(T, I), tuple_index(T, I, U).
nondet_tuple_index(T, I, U) :- valid_index(T, I), tuple_index(T, I, U).
cc_nondet_tuple_index(T, I, U) :- valid_index(T, I), tuple_index(T, I, U)