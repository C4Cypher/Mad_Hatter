%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
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

:- import_module ordered_set.

:- import_module mh_arity.
:- import_module mh_term.
:- import_module mh_var_set.
:- import_module mh_var_id.
:- import_module mh_substitution.


%-----------------------------------------------------------------------------%
% Tuple type

% Represents an indexable tuple of terms

% TODO: Internalize tuple structure, implement lazy caching of different forms

:- type mh_tuple.

:- instance arity(mh_tuple).

:- pred tuple_is_empty(mh_tuple::in) is semidet.

:- func tuple_compare(mh_tuple, mh_tuple) = comparison_result.
:- pred tuple_compare(comparison_result::out, mh_tuple::in, mh_tuple::in)
	is det.

:- pred tuple_equal(mh_tuple::in, mh_tuple::in) is semidet.

%-----------------------------------------------------------------------------%
% Tuple constructors and conversion

% reverse modes fail on type mismatch

:- func tuple(T) = mh_tuple.
:- mode tuple(in) = out is semidet.
:- mode tuple(out) = in is semidet. 

% manipulating tuples as if they were s-expressions
:- func tuple_cons(mh_term, mh_tuple) = mh_tuple.
:- mode tuple_cons(in, in) = out is det.
:- mode tuple_cons(out, out) = in is semidet.

:- func tuple_car(mh_tuple) = mh_term is semidet.
:- func tuple_cdr(mh_tuple) = mh_tuple is semidet.


% Direct conversions to other containers
:- func to_list(mh_tuple) = list(mh_term).
:- func from_list(list(mh_term)) = mh_tuple.

:- func to_array(mh_tuple) = array(mh_term).
:- func from_array(array(mh_term)) = mh_tuple.

:- func to_set(mh_tuple) = ordered_set(mh_term).
:- func from_set(ordered_set(mh_term)) = mh_tuple.


%-----------------------------------------------------------------------------%
% Tuple properties

:- pred tuple_size(mh_tuple::in, int::out) is det.
:- func tuple_size(mh_tuple) = int.

:- pred tuple_var_set(mh_tuple::in, mh_var_set::out) is det.
:- func tuple_var_set(mh_tuple) = mh_var_set.

:- pred tuple_var_id_set(mh_tuple::in, var_id_set::out) is det.
:- func tuple_var_id_set(mh_tuple) = var_id_set.

:- pred ground_tuple(mh_tuple::in) is semidet.

:- func ground_tuple(mh_tuple) = mh_tuple.
:- mode ground_tuple(in) = out is semidet.
:- mode ground_tuple(out) = in is semidet.


%-----------------------------------------------------------------------------%
% Tuple indexing

:- pred tuple_contains(mh_tuple::in, int::in) is semidet.

% Throw an exception if index is not found
:- pred tuple_index(mh_tuple::in, int::in, mh_term::out) is det.

:- pred tuple_search(mh_tuple::in, int::in, mh_term::out) is semidet.

:- pred tuple_member(mh_tuple::in, int::out, mh_term::out) is nondet.


%-----------------------------------------------------------------------------%
% Tuple substitutions


:- pred apply_tuple_substiution(mh_substitution::in, mh_tuple::in,
	mh_tuple::out) is det.
	
:- func apply_tuple_substiution(mh_tuple, mh_substitution) = mh_tuple.

%-----------------------------------------------------------------------------%
% Higher Order

	% This was written before I adopted the convention of single accumulators
	% being passed by function, rather than predicate, I've refactored the
	% func call with a closure, holding off on reimplementing the main call
:- pred fold_tuple(pred(mh_term, A, A), mh_tuple, A, A).
:- mode fold_tuple(pred(in, in, out) is det, in, in, out) is det.
:- mode fold_tuple(pred(in, in, out) is semidet, in, in, out) is semidet.

:- func fold_tuple(func(mh_term, A) = A, mh_tuple, A) = A.
:- mode fold_tuple(func(in, in) = out is det, in, in) = out is det.
:- mode fold_tuple(func(in, in) = out is semidet, in, in) = out is semidet.

:- func det_fold_tuple(func(mh_term, A) = A, mh_tuple, A) = A.

:- func semidet_fold_tuple(func(mh_term, A) = A, mh_tuple, A) = A.
:- mode semidet_fold_tuple(func(in, in) = out is semidet, in, in) = out 
	is semidet.

:- pred map_tuple(func(mh_term) = mh_term, mh_tuple, mh_tuple).
:- mode map_tuple(func(in) = out is det, in, out) is det.

:- func map_tuple(func(mh_term) = mh_term, mh_tuple) = mh_tuple.

:- pred all_tuple(pred(mh_term), mh_tuple).
:- mode all_tuple(pred(in) is semidet, in) is semidet.
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module int.
:- import_module bool.
:- import_module univ.

:- import_module util.

:- import_module mh_index.
:- import_module mh_value.


%-----------------------------------------------------------------------------%
% Tuple type

% TODO: rename list_tuple to mr_list_tuple, then make nested compound terms
% terminated by 'nil' unify with the mh_term constructor 'tuple_term/1'

:- type mh_tuple
	--->	list_tuple(list(mh_term))
	;		array_tuple(array(mh_term))
	;		slice_tuple(array(mh_term), int).
	
tuple_is_empty(list_tuple([])).
tuple_is_empty(array_tuple(A)) :- size(A) = 0.

tuple_compare(T1, T2) = array_compare(to_array(T1), to_array(T2)).
tuple_compare(tuple_compare(T1, T2), T1, T2).

tuple_equal(T1, T2) :- tuple_compare(=, T1, T2).

%-----------------------------------------------------------------------------%
% Tuple slices

:- func slice_index(int, int) = int.

slice_index(First, Index) = Index - First.

:- func slice_lookup(array(mh_term), int, int) = mh_term.

slice_lookup(A, F, I) = lookup(A, slice_index(F, I)).

:- func unslice_array(array(mh_term), int) = array(mh_term).

unslice_array(A, F) = generate(size(A) - F, slice_lookup(A, F)).

:- pragma inline(unslice_array/2).

%-----------------------------------------------------------------------------%
% Tuple constructors and conversion

:- pragma promise_equivalent_clauses(tuple/1).

tuple(T::in) = (Tuple::out) :-
	promise_equivalent_solutions [U] (
			dynamic_cast(T, U:mh_tuple);
			dynamic_cast(T, V:list(mh_term)), U = list_tuple(V);
			dynamic_cast(T, V:array(mh_term)), U = array_tuple(V);
			dynamic_cast(T, V:mh_term), V = value(mr_value(univ(U)))
		),
	Tuple = U.
		
tuple(T::out) = (Tuple::in) :-
	promise_equivalent_solutions [T] (
		Tuple = list_tuple(U), dynamic_cast(U, T);
		Tuple = array_tuple(U), dynamic_cast(U, T);
		Tuple = slice_tuple(Array, First), 
			dynamic_cast(unslice_array(Array, First), T);
		dynamic_cast(value(mr_value(univ(Tuple))):mh_term, T)
	).
	

:- pragma promise_equivalent_clauses(tuple_cons/2).

tuple_cons(X::in, Xs::in) = (list_tuple([X | to_list(Xs)])::out).
tuple_cons(tuple_car(T)::out, tuple_cdr(T)::out) = (T::in).

tuple_car(list_tuple([T | _])) = T.
tuple_car(array_tuple(A)) = T :- semidet_lookup(A, 0, T).
tuple_car(slice_tuple(A, F)) = T :- semidet_lookup(A, F, T).

tuple_cdr(list_tuple([_ | Ts])) = list_tuple(Ts).
tuple_cdr(array_tuple(A)) = Xs :- 
	(if size(A) > 1
	then Xs = slice_tuple(A, 1)
	else size(A) = 1, Xs = array_tuple(make_empty_array)
	).
	
tuple_cdr(slice_tuple(A, F)) = Xs :-
	(if (size(A) - F > 1)
	then 
		Xs = slice_tuple(A, F + 1)
	else 
		size(A) - F = 1, 
		Xs = array_tuple(make_empty_array)
	).


	
to_list(list_tuple(List)) = List.

to_list(array_tuple(Array)) = array.to_list(Array).

to_list(slice_tuple(Array, First)) = slice_to_list(Array, First).

:- func slice_to_list(array(mh_term), int) = list(mh_term).

slice_to_list(A, F) = 
    (if F > max(A) 
        then [] 
        else [ lookup(A, F) | slice_to_list(A, F + 1)]
    ).

from_list(List) = list_tuple(List).

to_array(list_tuple(List)) = array.from_list(List).

to_array(array_tuple(Array)) = Array.

to_array(slice_tuple(Array, First)) = unslice_array(Array, First).

from_array(Array) = array_tuple(Array).

to_set(Tuple) = ordered_set.from_array(mh_tuple.to_array(Tuple)).

from_set(Set) = array_tuple(ordered_set.to_array(Set)).
	
%-----------------------------------------------------------------------------%
% Tuple properties	
	
tuple_size(list_tuple(L), S) :- length(L, S).
tuple_size(array_tuple(Array), S) :- size(Array, S).
tuple_size(slice_tuple(A, F), size(A) - F).

tuple_size(T) = S :- tuple_size(T, S).

tuple_var_set(Tuple, complete_var_set(Set)) :- tuple_var_id_set(Tuple, Set).
tuple_var_set(Tuple) = Set :- tuple_var_set(Tuple, Set).

tuple_var_id_set(Tuple, Set) :- get_var_id_set(Tuple, tuple_size, Set).
tuple_var_id_set(Tuple) = Set :- tuple_var_id_set(Tuple, Set).

ground_tuple(T) :-  all_tuple(ground_term, T).

ground_tuple(T) = T :- ground_tuple(T).

:- instance arity(mh_tuple) where [
	pred(arity/2) is tuple_size
].

%-----------------------------------------------------------------------------%
% Tuple indexing

tuple_contains(Tuple, Index) :- Index > 0, Index =< tuple_size(Tuple).

tuple_index(list_tuple(L), Index, Term) :- list_index(L, Index, Term).
tuple_index(array_tuple(A), Index, Term) :- array_index(A, Index, Term).
tuple_index(slice_tuple(A, F), Index, Term) :- 
	array_index(A, slice_index(F, Index), Term).


tuple_search(Tuple, Index, Term) :-
	tuple_contains(Tuple, Index),
	tuple_index(Tuple, Index, Term).
	
tuple_member(Tup, Index, Term) :-
	Last = tuple_size(Tup),
	Last > 0,
	nondet_int_in_range(1, Last, Index),
	tuple_index(Tup, Index, Term).

%-----------------------------------------------------------------------------%
% Tuple substitutions

apply_tuple_substiution(_, _, _) :- sorry($module, $pred,
	"apply_tuple_substiution/3").
	
:- pragma no_determinism_warning(apply_tuple_substiution/3).

apply_tuple_substiution(!.T, S) = !:T :- apply_tuple_substiution(S, !T).
	
%-----------------------------------------------------------------------------%
% Higher Order

fold_tuple(Closure, list_tuple(L), !A) :- fold_list_index(Closure, L, !A).

fold_tuple(Closure, array_tuple(Array), !Acc) :- 
	fold_array_index(Closure, Array, !Acc).
		
fold_tuple(Closure, slice_tuple(Array, First), !Acc) :-
	fold_slice_index(Closure, Array, First, !Acc).
		
:- pred fold_slice_index(pred(T, A, A), array(T), int, A, A). 
:- mode fold_slice_index(pred(in, in, out) is det, in, in, in, out) is det.
:- mode fold_slice_index(pred(in, in, out) is semidet, in, in, in, out) 
	is semidet.
	
fold_slice_index(Closure, Array, First, !Acc) :-
	(if First > max(Array) then true
	else
		lookup(Array, First, Term),
		Closure(Term, !Acc),
		fold_slice_index(Closure, Array, First + 1, !Acc)
	).
	
:- pred func_closure(func(mh_term, A) = A, mh_term, A, A).
:- mode func_closure(func(in, in) = out is det, in, in, out) is det.
:- mode func_closure(func(in, in) = out is semidet, in, in, out) is semidet.

func_closure(F, Term, A, F(Term, A)).

fold_tuple(F, Tuple, !.A) = !:A :- fold_tuple(func_closure(F), Tuple, !A).

det_fold_tuple(F, T, A) = fold_tuple(F, T, A).
semidet_fold_tuple(F, T, A) = fold_tuple(F, T, A).

map_tuple(F, Tuple, map_tuple(F, Tuple)).

map_tuple(F, list_tuple(L)) = list_tuple(map(F, L)).
map_tuple(F, Tuple@array_tuple(_)) = 
	array_tuple(generate(tuple_size(Tuple), map_gen(F, Tuple))).
map_tuple(F, Tuple@slice_tuple(_, _)) = 
	array_tuple(generate(tuple_size(Tuple), map_gen(F, Tuple))).

:- func map_gen(func(mh_term) = mh_term, mh_tuple, int) = mh_term.

map_gen(F, Tuple, Index) = F(Term) :- tuple_index(Tuple, Index + 1, Term).


all_tuple(Pred, list_tuple(L)) :- all_list_index(Pred, L).
all_tuple(Pred, array_tuple(A)) :- all_array_index(Pred, A).
