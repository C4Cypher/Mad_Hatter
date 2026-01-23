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

:- func tuple_compare(mh_tuple::in, mh_tuple::in) = (comparison_result::uo)
	is det.
:- pred tuple_compare(comparison_result::uo, mh_tuple::in, mh_tuple::in)
	is det.

:- pred tuple_equal(mh_tuple::in, mh_tuple::in) is semidet.

%-----------------------------------------------------------------------------%
% Tuple constructors and conversion

:- func tuple(T) = mh_tuple is semidet.

% manipulating tuples as if they were s-expressions
:- func tuple_cons(mh_term, mh_tuple) = mh_tuple.
:- mode tuple_cons(in, in) = out is det.
:- mode tuple_cons(out, out) = in is semidet.

:- pred tuple_cons(mh_tuple, mh_term, mh_tuple).
:- mode tuple_cons(out, in, in) is det.
:- mode tuple_cons(in, out, out) is semidet.

:- func tuple_car(mh_tuple) = mh_term is semidet.
:- func tuple_cdr(mh_tuple) = mh_tuple is semidet.


% Direct conversions to other containers
:- func to_list(mh_tuple) = list(mh_term).
:- func from_list(list(mh_term)) = mh_tuple.

:- func to_array(mh_tuple) = array(mh_term).
:- func from_array(array(mh_term)) = mh_tuple.

:- func to_set(mh_tuple) = ordered_set(mh_term).
:- func from_set(ordered_set(mh_term)) = mh_tuple. %TODO: change this to ots


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

% Tuples are 1 indexed, 1 will always index the first element of a tuple

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
	;		slice_tuple(array(mh_term), int)
	where comparison is tuple_compare.
	
tuple_is_empty(Tuple) :- tuple_size(Tuple) = 0.

tuple_compare(Tuple1, Tuple2) = Result :- 
	tuple_compare(Result, Tuple1, Tuple2).

tuple_compare(Result, Tuple1, Tuple2) :-
    tuple_size(Tuple1, Size1),
    tuple_size(Tuple2, Size2),
    compare(SizeResult, Size1, Size2),
    require_complete_switch [SizeResult] (
        SizeResult = (=),
        compare_elements(Tuple1, Tuple2, Result)
    ;
        ( SizeResult = (<)
        ; SizeResult = (>)
        ),
        Result = SizeResult
    ).
	
:- pred compare_elements(mh_tuple::in, mh_tuple::in, comparison_result::uo)
	is det.
	
compare_elements(Tuple1, Tuple2, Result) :-
	(if 
		tuple_cons(Tuple1, X, Xs),
		tuple_cons(Tuple2, Y, Ys)
	then
		compare(TermResult, X, Y),
		(if TermResult = (=)
		then
			compare_elements(Xs, Ys, Result)
		else
			Result = TermResult
		)
	else
		%If tuple_cons/3 failed, all elements have been compared
		Result = (=)
	).

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


tuple(T) = (Tuple) :-
	promise_equivalent_solutions [U] (
			dynamic_cast(T, U:mh_tuple);
			dynamic_cast(T, V:list(mh_term)), U = list_tuple(V);
			dynamic_cast(T, V:array(mh_term)), U = array_tuple(V);
			dynamic_cast(T, V:mh_term), V = value(mr_value(univ(U)))
		),
	Tuple = U.
	
:- pragma promise_equivalent_clauses(tuple_cons/2).

tuple_cons(X::in, Xs::in) = (list_tuple([X | to_list(Xs)])::out).
tuple_cons(tuple_car(T)::out, tuple_cdr(T)::out) = (T::in).

tuple_cons(tuple_cons(X, Xs), X, Xs).

tuple_car(Tuple) = Term :- 
	tuple_size(Tuple) > 0,
	tuple_index(Tuple, 1, Term).

tuple_cdr(Tuple) = Xs :- 
	tuple_size(Tuple) > 0,
	promise_equivalent_solutions [Xs] (
		Tuple = list_tuple(List),
		det_tail(List, Ls),
		Xs = list_tuple(Ls)
	;
		Tuple = array_tuple(A),
		(if size(A) > 1
		then Xs = slice_tuple(A, 1)
		else Xs = array_tuple(make_empty_array) 
		)
	;
		Tuple = slice_tuple(A, F),
		(if (size(A) - F > 1)
		then Xs = slice_tuple(A, F + 1)
		else Xs = array_tuple(make_empty_array)
		)
	).

to_list(Tuple) = List :- promise_equivalent_solutions [List]
	(
		Tuple = list_tuple(List)
	;
		Tuple = array_tuple(Array),
		List = array.to_list(Array)
	;
		Tuple = slice_tuple(Array, First),
		List = slice_to_list(Array, First)
	).

:- func slice_to_list(array(mh_term), int) = list(mh_term).

slice_to_list(A, F) = 
    (if F > max(A) 
        then [] 
        else [ lookup(A, F) | slice_to_list(A, F + 1)]
    ).

from_list(List) = list_tuple(List).

to_array(Tuple) = Array :- promise_equivalent_solutions [Array]
	(
		Tuple = list_tuple(List),
		Array = array.from_list(List)
	;
		Tuple = array_tuple(Array)
	;
		Tuple = slice_tuple(Slice, First),
		Array = unslice_array(Slice, First)
	).

from_array(Array) = array_tuple(Array).

to_set(Tuple) = ordered_set.from_array(mh_tuple.to_array(Tuple)).

from_set(Set) = from_array(ordered_set.to_array(Set)).
	
%-----------------------------------------------------------------------------%
% Tuple properties	
	
tuple_size(Tuple, S) :- promise_equivalent_solutions [S] 
	(
		Tuple = list_tuple(L),
		length(L, S)
	;
		Tuple = array_tuple(Array),
		size(Array, S)
	;
		Tuple = slice_tuple(A, F),
		S = size(A) - F
	).

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

tuple_index(Tuple, Index, Term) :- promise_equivalent_solutions [Term]		
	(
		Tuple = list_tuple(L), list_index(L, Index, Term)
	;
		Tuple = array_tuple(A), array_index(A, Index, Term)
	;
		Tuple = slice_tuple(A, F), array_index(A, slice_index(F, Index), Term)
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
% Tuple substitutions

apply_tuple_substiution(_, _, _) :- sorry($module, $pred,
	"apply_tuple_substiution/3").
	
:- pragma no_determinism_warning(apply_tuple_substiution/3).

apply_tuple_substiution(!.T, S) = !:T :- apply_tuple_substiution(S, !T).
	
%-----------------------------------------------------------------------------%
% Higher Order

:- type deconstructed_tuple 
	--->	decon_list_tuple(list(mh_term))
	;		decon_array_tuple(array(mh_term))
	;		decon_slice_tuple(array(mh_term), int).
	
:- pred deconstruct_tuple(mh_tuple::in, deconstructed_tuple::out) is det.

deconstruct_tuple(Tuple, Decon) :- promise_equivalent_solutions [Decon]
	(
		Tuple = list_tuple(List),
		Decon = decon_list_tuple(List)
	;
		Tuple = array_tuple(Array),
		Decon = decon_array_tuple(Array)
		
	;
		Tuple = slice_tuple(Slice, First),
		Decon = decon_slice_tuple(Slice, First)
	).

fold_tuple(Closure, Tuple, !A) :- 
	deconstruct_tuple(Tuple, Decon), 
	require_complete_switch [Decon] (
		Decon = decon_list_tuple(List),
		fold_list_index(Closure, List, !A)
	;
		Decon = decon_array_tuple(Array),
		fold_array_index(Closure, Array, !A)
	;
		Decon = decon_slice_tuple(Slice, First),
		fold_slice_index(Closure, Slice, First, !A)
	).
		
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

map_tuple(F, !.Tuple) = !:Tuple :- promise_equivalent_solutions [!:Tuple]
	(
		!.Tuple = list_tuple(L),
		!:Tuple = list_tuple(map(F, L))
	;
		!.Tuple = array_tuple(_),
		!:Tuple =
			array_tuple(generate(tuple_size(!.Tuple), map_gen(F, !.Tuple)))
	;
		!.Tuple = slice_tuple(_, _),
		!:Tuple =
			array_tuple(generate(tuple_size(!.Tuple), map_gen(F, !.Tuple)))
	).

:- func map_gen(func(mh_term) = mh_term, mh_tuple, int) = mh_term.

map_gen(F, Tuple, Index) = F(Term) :- tuple_index(Tuple, Index + 1, Term).


all_tuple(Pred, Tuple) :-
	deconstruct_tuple(Tuple, Decon), 
	require_complete_switch [Decon] (
		Decon = decon_list_tuple(List),
		all_list_index(Pred, List)
	;
		Decon = decon_array_tuple(Array),
		all_array_index(Pred, Array)
	;
		Decon = decon_slice_tuple(Slice, First),
		all_array_index_from(Pred, Slice, First)
	).