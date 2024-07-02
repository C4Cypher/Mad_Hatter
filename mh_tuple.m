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

	% tuple_index(Tuple, Index, Term) 
	% retreive the Term from the Tuple at Index, throw an exception if
	% index is out of range, nondet version will fail if tuple is of zero arity
	pred tuple_index(T, int, mh_term),
	mode tuple_index(in, in, out) is det,
	mode tuple_index(in, out, out) is nondet,
	
	% set_tuple(Index, Term, !Tuple)
	% Make a new !:Tuple by subsituting the term at Index in !.Tuple
	% fail if Term is incompatable with the Tuple type
	pred set_tuple(int, mh_term, T, T), % set_tuple(Index, Value, !Container)
	mode set_tuple(in, in, in, out) is semidet, % exception on invalid index
	mode set_tuple(out, in, in, out) is nondet, % update any index nondet
	
	% convert_relation(Tuple, Type)
	% Accept some other Tuple and return a tuple of this Type with the
	% same terms, fail if the types are incompatable
	some [U] pred convert_tuple(U::in, T::out)  is semidet => tuple(U),
	
	% fold_tuple(Closure, Relation, !Accumulator)
	% perform a left fold from index 1 to arity(Relation), calling Closure on
	% each term in Relation with !Accumulator
	pred fold_tuple(pred(mh_term, A, A), T, A, A),
	mode fold_tuple(pred(in, in, out) is det, in, in, out) is det
	
].

%-----------------------------------------------------------------------------%


	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


:- implementation.

