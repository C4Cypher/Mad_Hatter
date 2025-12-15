%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_value_univ_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_value_map.

:- interface.

:- import_module mh_value.

%-----------------------------------------------------------------------------%
% Value maps

:- type mh_value_map(T).

:- func init = (mh_value_map(T)::uo) is det.
:- pred init(mh_value_map(_)::uo) is det.

:- func singleton(mh_value, V) = mh_value_map(V).

:- func count(mh_value_map(_)) = int.
:- pred count(mh_value_map(_)::in, int::out) is det.

:- pred equal(mh_value_map(T)::in, mh_value_map(T)::in) is semidet.

:- pred is_empty(mh_value_map(_)::in) is semidet.

%-----------------------------------------------------------------------------%
% Search and lookup

:- pred contains(mh_value_map(_T)::in, mh_value::in) is semidet.

:- pred search(mh_value_map(V)::in, mh_value::in, V::out) is semidet.

:- pred lookup(mh_value_map(V)::in, mh_value::in, V::out) is det.

%-----------------------------------------------------------------------------%
% Insertions

:- pred insert(mh_value::in, V::in, mh_value_map(V)::in, mh_value_map(V)::out) 
	is semidet.

:- pred update(mh_value::in, V::in, mh_value_map(V)::in, mh_value_map(V)::out) 
	is semidet.
	
:- pred set(mh_value::in, V::in, mh_value_map(V)::in, mh_value_map(V)::out) 
	is det.

%-----------------------------------------------------------------------------%
% Deletions

:- pred delete(mh_value::in, mh_value_map(_V)::in, mh_value_map(_V)::out) 
	is det.

:- pred remove(mh_value::in, V::out,  mh_value_map(V)::in, 
	mh_value_map(V)::out) is semidet.
	
	% Remove values from the map, starting frorm the smallest type in the 
	% standard ordering, removing the smallest value of that type, fail if
	% the map is empty, this should align with the standard ordering
:- pred remove_smallest(mh_value::out, V::out, 
	mh_value_map(V)::in, mh_value_map(V)::out) 	is semidet.
	
%-----------------------------------------------------------------------------%
% Nondeterminsitic lookup

:- pred member(mh_value_map(V)::in, mh_value::out, V::out) is nondet.

%-----------------------------------------------------------------------------%
% Set Operations

:- func union(func(T, T) = T, mh_value_map(T), mh_value_map(T)) = 
	mh_value_map(T).

:- pred union(func(T, T) = T, mh_value_map(T), mh_value_map(T), 
	mh_value_map(T)).
:- mode union(in(func(in, in) = out is det), in, in, out) is det.

:- func intersect(func(T1, T2) = T3, mh_value_map(T1), mh_value_map(T2)) = 
	mh_value_map(T3).
	
:- pred intersect(func(T1, T2) = T3, mh_value_map(T1), mh_value_map(T2),
	mh_value_map(T3)).
:- mode intersect(in(func(in, in) = out is det), in, in, out) is det.

:- func difference(mh_value_map(T), mh_value_map(_)) = mh_value_map(T).

:- pred difference(mh_value_map(T)::in, mh_value_map(_)::in, 
	mh_value_map(T)::out) is det.
	
	
%-----------------------------------------------------------------------------%
% Higher Order

:- func fold(func(mh_value, T, A) = A, mh_value_map(T), A) = A.
:- mode fold(in(func(in, in, in) = out is det), in, in) = out is det.
:- mode fold(in(func(in, in, in) = out is semidet), in, in) = out 
	is semidet.
	
:- func det_fold(func(mh_value, T, A) = A, mh_value_map(T), A)
	= A.
:- mode det_fold(in(func(in, in, in) = out is det), in, in) = out is det.

:- func semidet_fold(func(mh_value, T, A) = A, mh_value_map(T),
	A) = A.
:- mode semidet_fold(in(func(in, in, in) = out is semidet), in, in) = out 
	is semidet.

:- pred fold(func(mh_value, T, A) = A, mh_value_map(T), A, A).
:- mode fold(in(func(in, in, in) = out is det), in, in, out) is det.
:- mode fold(in(func(in, in, in) = out is semidet), in, in, out) 
	is semidet.
	
:- pred fold2(pred(mh_value, T, A, A, B, B), mh_value_map(T), A, A, B, B).
:- mode fold2(in(pred(in, in, in, out, in, out) is semidet), in, in, out, 
	in,	out) is semidet.
:- mode fold2(in(pred(in, in, in, out, in, out) is det), in, in, out, in, out)
	is det.

:- func map(func(mh_value, T) = U, mh_value_map(T)) = mh_value_map(U).
 
:- pred map(func(mh_value, T) = U, mh_value_map(T), mh_value_map(U)).
:- mode map(in(func(in, in) = out is det), in, out) is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module univ.

:- import_module univ_map.

%-----------------------------------------------------------------------------%
% Value maps

% TODO: make univ hashable and replace this implementation with a hashmap

:- type mh_value_map(T) 
	--->	mh_value_map(univ_map(T)).
	
		
init = mh_value_map(univ_map.init).

init(init).

singleton(mr_value(U), T) = mh_value_map(univ_map.singleton(U, T)).
	
count(mh_value_map(Map)) = univ_map.count(Map).
count(Map, count(Map)).

equal(mh_value_map(Map1), mh_value_map(Map2)) :- univ_map.equal(Map1, Map2).
	
is_empty(mh_value_map(M)) :- univ_map.is_empty(M).



%-----------------------------------------------------------------------------%
% Search and lookup

contains(mh_value_map(M), mr_value(U)) :- univ_map.contains_univ(M, U).

search(mh_value_map(M), mr_value(U), T) :- univ_map.search_univ(M, U, T).

lookup(mh_value_map(M), mr_value(U), T) :- univ_map.lookup(M, U, T).

%-----------------------------------------------------------------------------%
% Insertions 

insert(mr_value(U), T, mh_value_map(!.M), mh_value_map(!:M)) :- 
	univ_map.insert_univ(U, T, !M).
	
update(mr_value(U), T, mh_value_map(!.M), mh_value_map(!:M)) :-
	univ_map.update_univ(U, T, !M).
	
set(mr_value(U), T, mh_value_map(!.M), mh_value_map(!:M)) :-
	univ_map.set_univ(U, T, !M).

%-----------------------------------------------------------------------------%
% Deletions

delete(mr_value(U), mh_value_map(!.M), mh_value_map(!:M)) :-
	univ_map.delete_univ(U, !M).
	
remove(mr_value(U), T, mh_value_map(!.M), mh_value_map(!:M)) :-
	univ_map.remove_univ(U, T, !M).
	
remove_smallest(mr_value(univ(K)), V, mh_value_map(!.M), mh_value_map(!:M)) :-
	univ_map.remove_smallest(K, V, !M).
	
%-----------------------------------------------------------------------------%
% Nondeterminsitic lookup
	
member(mh_value_map(M), mr_value(U), T) :- univ_map.member_univ(M, U, T).


%-----------------------------------------------------------------------------%
% Set Operations

union(F, mh_value_map(Map1), mh_value_map(Map2)) = 
	mh_value_map(univ_map.union(F, Map1, Map2)).
	
union(F, Map1, Map2, union(F, Map1, Map2)).

intersect(F, mh_value_map(Map1), mh_value_map(Map2)) = 
	mh_value_map(univ_map.intersect(F, Map1, Map2)).
	
intersect(F, Map1, Map2, intersect(F, Map1, Map2)).

difference(mh_value_map(Map1), mh_value_map(Map2)) = 
	mh_value_map(univ_map.difference(Map1, Map2)).
	
difference(Map1, Map2, difference(Map1, Map2)).	

%-----------------------------------------------------------------------------%
% Higher Order

:- func value_fold(func(mh_value, T, A) = A, univ, T, A) = A.
:- mode value_fold(in(func(in, in, in) = out is det), in, in, in) = out is det.
:- mode value_fold(in(func(in, in, in) = out is semidet), in, in, in) = out
	is semidet.

value_fold(F, Univ, T, A) = F(mr_value(Univ), T, A).

fold(F, mh_value_map(Map), A) = univ_map.fold(value_fold(F), Map, A).

det_fold(F, M, A) = fold(F, M, A).
semidet_fold(F, M, A) = fold(F, M, A).

fold(F, Map, A, fold(F, Map, A)).

:- pred value_fold2(pred(mh_value, T, A, A, B, B), univ, T, A, A, B, B).
:- mode value_fold2(in(pred(in, in, in, out, in, out) is det), in, in, 
	in, out, in, out) is det.
:- mode value_fold2(in(pred(in, in, in, out, in, out) is semidet), in, in, 
	in, out, in, out) is semidet.
	
value_fold2(P, Univ, T, !A, !B) :- P(mr_value(Univ), T, !A, !B). 

fold2(P, mh_value_map(Map), !A, !B) :- 
	univ_map.fold2(value_fold2(P), Map, !A, !B). 

map(F, mh_value_map(Map)) = mh_value_map(univ_map.map(value_map(F), Map)).

:- func value_map(func(mh_value, T) = U, univ, T) = U.

value_map(F, Univ, T) = F(mr_value(Univ), T).

map(F, Map, map(F, Map)).