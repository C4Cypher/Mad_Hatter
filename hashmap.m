%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: hashmap.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module hashmap.

% An attempt at implementing Haskell's implementation of a HAMT in Merrcury

:- interface.

:- use_module hash_table.

%-----------------------------------------------------------------------------%
% Hash Array Mapped Table

:- type hashmap(K, V)
	--->	hm(
				root :: hashmap_tree(K, V),
				hash :: hash_pred(K)
			).

:- type hash_pred(K) == hash_table.hash_pred(K).
:- inst hash_pred == hash_table.hash_pred.

:- type hashmap_tree(K, V).

%-----------------------------------------------------------------------------%
% Insertion

:- pred insert(K::in, V::in, hashmap(K, V)::in, hashmap(K, V)::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module uint.
:- import_module array.
:- import_module bool.

%-----------------------------------------------------------------------------%
% Hash Array Mapped Table

%-----------------------------------------------------------------------------%
% Hash map tree

:- type hash == uint.

:- type bitmap == uint.

:- func hash_size = int.

hash_size = bits_per_uint.

:- type hash_array(K, V) == array(hashmap_tree(K, V)).

:- type hashmap_tree(K, V)
	--->	empty_tree
	;		indexed_branch(bitmap, hash_array(K, V))
	;		leaf(hash, K, V)
	;		full_branch(hash_array(K, V))
	;		collision(hash, leaf_array(K, V)).
	
:- type ht(K, V) == hashmap_tree(K, V).

:- inst hashmap_leaf
	--->	leaf(ground, ground, ground).

:- type hashmap_leaf(K, V) =< hashmap_tree
	---> 	leaf(hash, K, V).
	
:- type leaf_array(K, V) == array(hashmap_leaf(K, V)).

%-----------------------------------------------------------------------------%
% Insertion

insert(K, V, hm(!.HT, HashPred), hm(!:HT, HashPred)) :- 
	insert_tree(hash(HashPred, K), K, V, 0, no, !HT).

%  pred insert_tree(Hash, Key, Value, ??, Replace, !HashTree) is semidet.
:- pred insert_tree(hash::in, K::in, V::in, int::in, bool::in
	ht(K, V)::in, ht(K, V)::out) is semidet.

insert_tree(H, K, V, _, _, empty_tree, leaf(H, K, V)).

insert_tree(H, K, V, Shift, Replace, !.HT@leaf(LH, LK, LV), !:HT) :-
	(if H = LH
	then
		(if K = LK
		then
			(if V = LV
			then !:HT = !.HT
			else Replace = yes, 
				!:HT = leaf(H, K, V)
			)
		else
		!:HT = collision(H, leaf(H, K, V), coerce(!.HT))	
		)
	else
	%TODO: two branch
	)

:- func collision(hash, hashmap_leaf(K, V), hashmap_leaf(K, V)) = ht(K, V).

collision(Hash, L1, L2) = collsision(Hash, !:Array) :-
	array.init(2, L1, !.Array),
	array.set(1, L2, !Array).
	
:- func two()

%-----------------------------------------------------------------------------%
% Utilites

:- func hash(hash_pred(T), T) = hash.

hash(P, T) = H :- P(T, H).

%-----------------------------------------------------------------------------%
% Hamming weight

:- func uint_weight(uint) = int.

uint_weight(I) = 
	(if I =< 1
	then 
		cast_to_int(I)
	else 
		weight(I /\ (I-1) ) + 1
	).
	
:- func bitmap_weight(bitmap) = int.

bitmap_weight(B) = W :-
	