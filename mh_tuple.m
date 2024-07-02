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

%-----------------------------------------------------------------------------%
% Tuple

% Represents an indexable tuple of terms

:- typeclass tuple(T) <= arity(T) where [

	% term_index(Tuple, Index, Term) 
	% retreive the Term from the Tuple at Index, throw an exception if
	% index is out of range
	pred term_index(T, int, mh_term),
	mode term_index(in, in, out) is det,
	mode term_index(in, out, out) is nondet,
	
	% convert_relation(Tuple, Type)
	% Accept some other Tuple and return a tuple of this Type with the
	% same terms, fail if the types are incompatable
	some [U] pred convert_tuple(Tup::in, T::out) => relaiton(Tup) is semidet,
	
	% fold_term_index(Closure, Relation, !Accumulator)
	% perform a left fold from index 1 to arity(Relation), calling Closure on
	% each term in Relation with !Accumulator
	pred fold_term_index(pred(U, A, A), T, A, A),
	mode fold_term_index(pred(in, in, out) is det, in, in, out) is det,
	
	% map_term_index(Closure, !Relation)
	pred map_term_index

].

%-----------------------------------------------------------------------------%


	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


:- implementation.

