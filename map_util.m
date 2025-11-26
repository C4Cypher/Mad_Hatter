%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: map_util.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module map_util.

:- interface.

:- import_module map.
:- import_module array.

%-----------------------------------------------------------------------------%
% Basic map operations

:- pred is_singleton(map(_, _)::in) is semidet.

%-----------------------------------------------------------------------------%
% Set operations
	
	% Reimplementation of union and intersect utilizing folds rather than
	% Assoc Lists, additionally, seperate pred calls passing funcs as higher
	% order merging calls are defined, and the intersection calls are typed
	
	% The intersection calls fold over one map, testing for membership of
	% the second. This call is more efficient if the smaller map is the first.

:- func func_intersect(func(V1, V2) = V3, map(K, V1), map(K, V2)) = map(K, V3).
:- mode func_intersect(in(func(in, in) = out is det), in, in) = out is det.
:- mode func_intersect(in(func(in, in) = out is semidet), in, in) = out is semidet.

:- pred func_intersect(func(V1, V2) = V3, map(K, V1), map(K, V2), map(K, V3)).
:- mode func_intersect(in(func(in, in) = out is det), in, in, out) is det.
:- mode func_intersect(in(func(in, in) = out is semidet), in, in, out)
	is semidet.
	
:- pred pred_intersect(pred(V1, V2, V3), map(K, V1), map(K, V2), map(K, V3)).
:- mode pred_intersect(in(pred(in, in, out) is det), in, in, out) is det.	
:- mode pred_intersect(in(pred(in, in, out) is semidet), in, in, out)
	is semidet.

	% These union calls fold over the first map, inserting elements into the
	% second map, as before, this is more efficient if the first map is the 
	% smaller one.

:- func func_union(func(V, V) = V, map(K, V), map(K, V)) = map(K, V).
:- mode func_union(in(func(in, in) = out is det), in, in) = out is det.
:- mode func_union(in(func(in, in) = out is semidet), in, in) = out is semidet.

:- pred func_union(func(V, V) = V, map(K, V), map(K, V), map(K, V)).
:- mode func_union(in(func(in, in) = out is det), in, in, out) is det.
:- mode func_union(in(func(in, in) = out is semidet), in, in, out) is semidet.

:- pred pred_union(pred(V, V, V), map(K, V), map(K, V), map(K, V)).
:- mode pred_union(in(pred(in, in, out) is det), in, in, out) is det.	
:- mode pred_union(in(pred(in, in, out) is semidet), in, in, out) is semidet.

%-----------------------------------------------------------------------------%
% Higher Order

	% Perform a left fold over the map, failing early if the applied function
	% fails. The standard library implementatin was det only.
:- func fold(func(K, V, A) = A, map(K, V), A) = A.
:- mode fold(in(func(in, in, in) = out is det), in, in) = out is det.
:- mode fold(in(func(in, in, in) = out is semidet), in, in) = out 
	is semidet.
:- mode fold(in(func(in, in, di) = uo is det), in, di) = uo is det.
	
	% Predicate form of fold, given that the standard libary versions
	% pass a predicate
:- pred fold(func(K, V, A) = A, map(K, V), A, A).
:- mode fold(in(func(in, in, in) = out is det), in, in, out) is det.
:- mode fold(in(func(in, in, in) = out is semidet), in, in, out) 
	is semidet.
:- mode fold(in(func(in, in, di) = uo is det), in, di, uo) is det.

	% Fold over the map, passing a mutable array as the accumulator
:- func array_fold(func(K, V, A) = A, map(K, V), A) = A.
:- mode array_fold(in(func(in, in, array_di) = array_uo is det), in, array_di)
	= array_uo is det.
	
:- pred array_fold(func(K, V, A) = A, map(K, V), A, A).
:- mode array_fold(in(func(in, in, array_di) = array_uo is det), in, array_di,
	array_uo) is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.

% :- import_module require.

%-----------------------------------------------------------------------------%
% Basic map operations

is_singleton(Map) :- keys(Map, [_]).

%-----------------------------------------------------------------------------%
% Set operations

:- func intersect_fold(func(V1, V2) = V3, map(K, V2), K, V1, map(K, V3)) = 
	map(K, V3).
:- mode intersect_fold(in(func(in, in) = out is det), in, in, in, in) = out 
	is det.
:- mode intersect_fold(in(func(in, in) = out is semidet), in, in, in, in) = out
	is semidet.
	
intersect_fold(F, M2, K, V1, !.M3) = !:M3 :-
	(if search(M2, K, V2)
	then det_insert(K, F(V1, V2), !M3)
	else true
	).
	
func_intersect(F, M1, M2) = fold(intersect_fold(F, M2), M1, init).

func_intersect(F, M1, M2, func_intersect(F, M1, M2)).

:- func curry_pred(pred(V1, V2, V3), V1, V2) = V3.
:- mode curry_pred(in(pred(in, in, out) is det), in, in) = out is det.
:- mode curry_pred(in(pred(in, in, out) is semidet), in, in) = out is semidet.

curry_pred(P, V1, V2) = V3 :- P(V1, V2, V3).

pred_intersect(P, M1, M2, func_intersect(curry_pred(P), M1, M2)).

:- func union_fold(func(V, V) = V, K, V, map(K, V)) = map(K, V).
:- mode union_fold(in(func(in, in) = out is det), in, in, in) = out is det.
:- mode union_fold(in(func(in, in) = out is semidet), in, in, in) = out
	is semidet.
	
union_fold(F, K, V1, !.M2) = !:M2 :-
	search_insert(K, V1, MaybV2, !M2),
	(if MaybV2 = yes(V2)
	then set(K, F(V1, V2), !M2)
	else true
	).
	
func_union(F, M1, M2) = fold(union_fold(F), M1, M2).

func_union(F, M1, M2, func_union(F, M1, M2)).

pred_union(P, M1, M2, func_union(curry_pred(P), M1, M2)).
	
%-----------------------------------------------------------------------------%
% Higher Order

:- pred apply_func(func(K, V, A) = A, K, V, A, A).
:- mode apply_func(in(func(in, in, in) = out is det), in, in, in, out) is det.
:- mode apply_func(in(func(in, in, in) = out is semidet), in, in, in, out)
	is semidet.
:- mode apply_func(in(func(in, in, di) = uo is det), in, in, di, uo) is det.
	
apply_func(F, K, V, A, F(K, V, A)).

fold(F, Map, !.A) = !:A :- map.foldl(apply_func(F), Map, !A).

fold(F, M, A, fold(F, M, A)).

	% Mercury cheats uniqueness for arrays, so can we.
	%
	% The compiler would *not* allow us to pull something like this for
	% actually unique insted variables, but the array library's unqiue modes
	% are a work around, aliased to the 'ground' inst, allowing us to
	% pass 'ground' insted arrays as unique without complaint from the mmc
	% ... just don't do this with any array you want to treat as immutable.
	% see also:  array.m line 58
:- func coerce_uniq_array(T::in) = (T::array_uo).

coerce_uniq_array(T) = T.

:- func wrap_array_acc(func(K, V, A) = A, K, V, A) = A.
:- mode wrap_array_acc(in(func(in, in, array_di) = array_uo is det), in, in, 
	in) = out is det.

wrap_array_acc(F, K, V, A) = F(K, V, coerce_uniq_array(A)).

array_fold(F, M, A) = coerce_uniq_array(fold(wrap_array_acc(F), M, A)).

array_fold(F, M, A, array_fold(F, M, A)).

	
