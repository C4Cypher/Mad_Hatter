%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
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
	%;		trie_tuple(???)  data structure that organizes a tuple into a 
	% 						 prefix tree structure?
	;		tuple_sub(mh_tuple, mh_substitution).

:- func tuple(T) = mh_tuple <= mr_tuple(T).
:- mode tuple(in) = out is det.
:- mode tuple(out) = in is semidet.

:- pred tuple_size(mh_tuple::in, int::out) is det.
:- func tuple_size(mh_tuple) = int.

% :- pred ground_tuple(mh_tuple::in) is semidet.

% :- func ground_tuple(mh_tuple) = mh_tuple.
% :- mode ground_tuple(in) = out is semidet.
% :- mode ground_tuple(out) = in is semidet.

:- instance arity(mh_tuple).
:- instance mr_tuple(mh_tuple).

%-----------------------------------------------------------------------------%
% Tuple indexing

:- pred tuple_contains(mh_tuple::in, int::in) is semidet.

% Throw an exception if index is not found
:- pred tuple_index(mh_tuple::in, int::in, mh_term::out) is det.

:- pred tuple_search(mh_tuple::in, int::in, mh_term::out) is semidet.

:- pred tuple_member(mh_tuple::in, int::out, mh_term::out) is nondet.

%-----------------------------------------------------------------------------%
% Tuple fold

:- pred fold_tuple(pred(mh_term, A, A), mh_tuple, A, A).
:- mode fold_tuple(pred(in, in, out) is det, in, in, out) is det.
:- mode fold_tuple(pred(in, in, out) is semidet, in, in, out) is semidet.

:- func fold_tuple(pred(mh_term, A, A), mh_tuple, A) = A.
:- mode fold_tuple(pred(in, in, out) is det, in, in) = out is det.
:- mode fold_tuple(pred(in, in, out) is semidet, in, in) = out is semidet.

:- pred all_tuple(pred(mh_term), mh_tuple).
:- mode all_tuple(pred(in) is semidet, in) is semidet.


%-----------------------------------------------------------------------------%
% Tuple substitutions

:- pred apply_tuple_substiution(mh_substitution::in, mh_tuple::in,
	mh_tuple::out) is det.

%-----------------------------------------------------------------------------%
% Tuple typeclass

% A mercury type that can represent a tuple

:- typeclass mr_tuple(T) <= arity(T) where [

	% tuple_index(Tuple, Index, Term) 
	% retreive the Term from the Tuple at Index, throw an exception if
	% index is out of range, nondet version will fail if tuple is of zero arity
	pred mr_tuple_index(T, int, mh_term),
	mode mr_tuple_index(in, in, out) is det,
	
	% fold_tuple(Closure, Tuple, !Accumulator)
	% perform a left fold from index 1 to arity(Tuple), calling Closure on
	% each term in Tuple with !Accumulator
	pred fold_mr_tuple(pred(mh_term, A, A), T, A, A),
	mode fold_mr_tuple(pred(in, in, out) is det, in, in, out) is det,
	mode fold_mr_tuple(pred(in, in, out) is semidet, in, in, out) is semidet,
	
	% all_mr_tuple(Predicate, Tuple)
	% call Predicate for each term in Tuple, failing on the first term that
	% Predicate fails on
	pred all_mr_tuple(pred(mh_term), T),
	mode all_mr_tuple(pred(in) is semidet, in) is semidet
	
].

%-----------------------------------------------------------------------------%
% Function versions of tuple typeclass methods

:- func mr_tuple_index(T, int) = mh_term <= mr_tuple(T).

:- func fold_mr_tuple(func(mh_term, A) = A, T, A) = A <= mr_tuple(T).

	
	

	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.


:- import_module mh_index.
:- import_module require.
:- import_module int.
:- import_module bool.



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
		Tuple = array_tuple(U), dynamic_cast(U, T);
		Tuple = tuple_sub(Tup0, Sub), 
			apply_tuple_substiution(Sub, Tup0, Tup1),
			Tup1 = tuple(T)
	).
	
tuple_size(mr_tuple(T), S) :- arity(T, S).
tuple_size(list_tuple(L), S) :- length(L, S).
tuple_size(array_tuple(Array), S) :- size(Array, S).
tuple_size(tuple_sub(Tup, _), S) :- tuple_size(Tup, S).

tuple_size(T) = S :- tuple_size(T, S).

% ground_tuple(T) :-  all_tuple(ground_term, T).

% ground_tuple(T) = T :- ground_tuple(T).

:- instance arity(mh_tuple) where [
	pred(arity/2) is tuple_size
].


:- instance mr_tuple(mh_tuple) where [
	pred(mr_tuple_index/3) is tuple_index,
	pred(fold_mr_tuple/4) is fold_tuple,
	pred(all_mr_tuple/2) is all_tuple
].


%-----------------------------------------------------------------------------%
% Tuple indexing

tuple_contains(Tuple, Index) :- Index > 0, Index =< tuple_size(Tuple).

tuple_index(mr_tuple(T), Index, Term) :- mr_tuple_index(T, Index, Term).
tuple_index(list_tuple(L), Index, Term) :- list_index(L, Index, Term).
tuple_index(array_tuple(A), Index, Term) :- array_index(A, Index, Term).

tuple_index(tuple_sub(Tuple, Sub), Index, Term) :-
	tuple_index(Tuple, Index, Term0),
	( if 
		Term0 = var(ID),
		sub_id_search(Sub, ID, Term1)
	then
		Term = Term1
	else
		Term = Term0
	).

tuple_search(Tuple, Index, Term) :-
	tuple_contains(Tuple, Index),
	tuple_index(Tuple, Index, Term).
	
tuple_member(Tup, Index, Term) :-
	Last = tuple_size(Tup),
	Last > 0,
	nondet_int_in_range(1, Last, Index),
	tuple_index(Tup, Index, Term).
	
%-----------------------------------------------------------------------------%
% Tuple fold

fold_tuple(Closure, mr_tuple(T), !A) :- fold_mr_tuple(Closure, T, !A).

fold_tuple(Closure, list_tuple(L), !A) :- fold_list_index(Closure, L, !A).

fold_tuple(Closure, array_tuple(T), !A) :- 
		fold_array_index(Closure, T, !A).
		
fold_tuple(Closure, tuple_sub(Tup0, S), !A) :-
	apply_tuple_substiution(S, Tup0, Tup),
	fold_tuple(Closure, Tup, !A).
		
fold_tuple(Closure, Tuple, !.A) = !:A :- fold_tuple(Closure, Tuple, !A).

all_tuple(Pred, T) :- all_mr_tuple(Pred, T).
all_tuple(Pred, list_tuple(L)) :- all_list_index(Pred, L).
all_tuple(Pred, array_tuple(A)) :- all_array_index(Pred, A).

all_tuple(Pred, tuple_sub(Tup0, Sub)) :- 
	apply_tuple_substiution(Sub, Tup0, Tup),
	all_tuple(Pred, Tup).

%-----------------------------------------------------------------------------%
% Tuple substitutions

apply_tuple_substiution(_, _, _) :- sorry($module, $pred,
	"apply_tuple_substiution/3").
	
:- pragma no_determinism_warning(apply_tuple_substiution/3).

%-----------------------------------------------------------------------------%
% Function versions of tuple typeclass methods

mr_tuple_index(T, I) = Term :- mr_tuple_index(T, I, Term).

fold_mr_tuple(Func, T, !.A) = !:A :- 
	fold_mr_tuple(function_closure(Func), T, !A).

:- pred function_closure(func(T, A) = A, T, A, A).
:- mode function_closure(func(in, in) = out is det, in, in, out) is det.

function_closure(Func, T, !A) :- Func(T, !.A) = !:A.
