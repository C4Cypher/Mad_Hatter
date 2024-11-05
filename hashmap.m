%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%
% Additionally, this file uses comments and translated snippets from the 
% Haskell Data.HashMap.Internal module by Johan Tibell under the BSD 3 licence
%
% Copyright (c) 2010, Johan Tibell
% 
% All rights reserved.
% 
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
% 
%     * Redistributions of source code must retain the above copyright
%       notice, this list of conditions and the following disclaimer.
% 
%     * Redistributions in binary form must reproduce the above
%       copyright notice, this list of conditions and the following
%       disclaimer in the documentation and/or other materials provided
%       with the distribution.
% 
%     * Neither the name of Johan Tibell nor the names of other
%      contributors may be used to endorse or promote products derived
%       from this software without specific prior written permission.
% 
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%-----------------------------------------------------------------------------%
% 
% File: hashmap.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module hashmap.

% An attempt at implementing Haskell's implementation of a HAMT in Merrcury
% Original implementation found in Data.HashMap.Internal by Johan Tibell

:- interface.

:- import_module hashable.

%-----------------------------------------------------------------------------%
% Hash Array Mapped Table

:- type hashmap(K, V).

%-----------------------------------------------------------------------------%
% Construction

:- func init = hashmap(K, _V) <= hashable(K).
:- pred init(hashmap(K, _)::out) is det <= hashable(K).

:- func singleton(K, V) = hashmap(K, V) <= hashable(K).
:- pred singleton(K::in, V::in, hashmap(K, V)::out) is det <= hashable(K).

%-----------------------------------------------------------------------------%
% Basic interface

:- pred is_empty(hashmap(_, _)::in) is semidet.

% Number of elements contained (HS size function)
:- func count(hashmap(_, _)) = int is det.
:- pred count(hashmap(_, _)::in, int::out) is det.

:- pred contains(map(K, _V)::in, K::in) is semidet.

%-----------------------------------------------------------------------------%
% Search

%-----------------------------------------------------------------------------%
% Insertion

:- pred insert(K::in, V::in, hashmap(K, V)::in, hashmap(K, V)::out) is semidet
	<= hashable(K).
:- func insert(K, V, hashmap(K, V)) = hashmap(K, V) is semidet <= hashable(K).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module uint.
:- import_module array.
:- import_module bool.
:- import_module require.

:- import_module mh_util.

%-----------------------------------------------------------------------------%
% Hash Array Mapped Table

:- type hashmap(K, V)
	--->	empty_tree
	;		indexed_branch(bitmap, hash_array(K, V))
	;		leaf(hash, K, V)
	;		full_branch(hash_array(K, V))
	;		collision(hash, leaf_array(K, V)).

:- type hash == uint.

:- type bitmap == uint.

:- type shift == int.

:- func hash_size = int.

hash_size = bits_per_uint.

:- type hash_array(K, V) == array(hashmap(K, V)).

:- type hm(K, V) == hashmap(K, V).
	
	
%-----------------------------------------------------------------------------%
% Hashmap Leaf

:- inst hashmap_leaf
	--->	leaf(ground, ground, ground).


:- type hashmap_leaf(K, V) =< hashmap(K, V)
	---> 	leaf(hash, K, V).
	
:- type leaf_array(K, V) == array(hashmap_leaf(K, V)).
	
:- mode hashmap_leaf == ground >> hashmap_leaf.

:- pred is_hashmap_leaf(hashmap(_K, _V)::hashmap_leaf) is semidet.

is_hashmap_leaf(leaf(_, _, _)).
	
:- func coerce_leaf(hashmap(K, V)) = hashmap_leaf(K, V) is semidet.

coerce_leaf(L) = coerce(L) :- is_hashmap_leaf(L).

:- func det_coerce_leaf(hashmap(K, V)) = hashmap_leaf(K, V).

det_coerce_leaf(L) = coerce(L) :- 
	(if is_hashmap_leaf(L) 
	then true
	else unexpected($module, $pred, 
		"Could not coerce hashmap tree to leaf, was not a leaf constructor.")
	).

%-----------------------------------------------------------------------------%
% Construction

init = empty_tree.
init(init).

singleton(K, V) = leaf(hash(K), K, V).
singleton(K, V, singleton(K, V)).

%-----------------------------------------------------------------------------%
% Basic interface

is_empty(empty_tree).

count(HM) = count(HM, 0).

:- func count(hashmap(_, _), int) = int.

count(empty_tree, N) 				= N.
count(leaf(_, _, _), N) 			= N + 1.
count(indexed_branch(_, Array), N) 	= array.foldl(count, Array, N).
count(full_branch(Array), N) 		= array.foldl(count, Array, N).
count(collision(Array), N) 			= N + array.size(Array).

contains(HM, K) :- search(HM, K, _).

%-----------------------------------------------------------------------------%
% Search


	
%-----------------------------------------------------------------------------%
% Insertion

insert(K, V, !HM) :- 
	insert_tree(hash(K), K, V, 0, no, !HM).
	
:- pragma inline(insert/4).
	
insert(K, V, !.HM) = !:HM :-
	insert(K, V, !HM).
	
:- pragma inline(insert/3).

%  pred insert_tree(Key, Value, Shift, Replace, !HashTree) is semidet.
:- pred insert_tree(hash::in, K::in, V::in, shift::in, bool::in,
	hm(K, V)::in, hm(K, V)::out) is semidet.

insert_tree(H, K, V, _, _, empty_tree, leaf(H, K, V)).

insert_tree(H, K, V, S, R, !.HM@leaf(LH, LK, LV), !:HM) :-
	(if H = LH
	then
		(if K = LK
		then
			(if V = LV
			then !:HM = !.HM
			else R = yes, 
				!:HM = leaf(H, K, V)
			)
		else
		!:HM = collision(H, leaf(H, K, V), det_coerce_leaf(!.HM))	
		)
	else
		!:HM = two(S, H, K, V, leaf(LH, LK, LV))
	).
	
insert_tree(H, K, V, S, R, !.HM@indexed_branch(B, !.Array), !:HM) :-
	M = mask(H, S),
	I = sparse_index(B, M),
	( if B /\ M = 0u
	then
		array_insert(I, leaf(H, K, V), !Array),
		!:HM = indexed_or_full_branch(B \/ M, !.Array)
	else 
		lookup(!.Array, I, St0),
		insert_tree(H, K, V, next_shift(S), R, St0, St1),
		(if private_builtin.pointer_equal(St1, St0)
		then
			!:HM = !.HM
		else
			slow_set(I, St1, !Array),
			!:HM = indexed_branch(B, !.Array)
		)
	).
	
insert_tree(H, K, V, S, R, !.HM@full_branch(!.Array), !:HM) :-
	I = index(H, S),
	lookup(!.Array, I, St0),
	insert_tree(H, K, V, next_shift(S), R, St0, St1),
	(if private_builtin.pointer_equal(St1, St0)
	then
		!:HM = !.HM
	else
		slow_set(I, St1, !Array),
		!:HM = full_branch(!.Array)
	).

insert_tree(H, K, V, S, R, !.HM@collision(CH, Array), !:HM) :-
	(if H = CH
	then
		!:HM = collision(H, collision_insert(R, H, K, V, Array))
	else
		array.init(1, !.HM, BArray),
		insert_tree(H, K, V, S, R, indexed_branch(mask(H, S), BArray), !:HM)
	).

%-----------------------------------------------------------------------------%
% Node creation


% Create a 'Collision' value with two 'Leaf' values.

:- func collision(hash, hashmap_leaf(K, V), hashmap_leaf(K, V)) = hm(K, V).

collision(Hash, L1, L2) = collision(Hash, Array) :-
	array.init(2, L1, Array0),
	array.set(1, L2, Array0, Array).
	
% collision_insert(Replace, Hash, Key, Value, !.Array) = !:Array.
:- func collision_insert(bool, hash, K, V, leaf_array(K,V)) = 
	leaf_array(K,V)	is semidet.
	
collision_insert(R, H, K, V, A) = 
	collision_insert(R, 0, max(A), K, V, leaf(H, K, V), A).

% collision_insert(Replace, LBound, UBound, K, V, Leaf, !.Array) = !:Array.
:- func collision_insert(bool, int, int, K, V, hashmap_leaf(K,V), 
	leaf_array(K,V) ) = leaf_array(K,V) is semidet.
	
collision_insert(R, I, Ub, K, V, L, A) = 
	(if K = K2
	then
		(if	V = V2
		then A
		else 
			(if	R = yes
			then
				slow_set(A, I, L)
			else collision_insert_failure
			)
		)
	else if I < Ub
	then
		collision_insert(R, I + 1, Ub, K, V, L, A)
	else
		array_snoc(L, A)
	) :-
	unsafe_lookup(A, I, leaf(_, K2, V2) ).
	
	
:- func collision_insert_failure = leaf_array(K, V) is failure.

collision_insert_failure = make_empty_array :- fail.


	
:- func collision_merge( 
		(func(hashmap_leaf(K, V), hashmap_leaf(K, V)) = bool ),
		hashmap_leaf(K, V),
		leaf_array(K, V)
	) = leaf_array(K, V).
	
:- mode collision_merge( (func(in, in) = out is det), in, in) = out is det.
:- mode collision_merge( (func(in, in) = out is semidet), in, in) = out
	is semidet.
	
collision_merge(R, L@leaf(_, K, V), A) = 
	collision_merge(R, 0, max(A), K, V, L, A).
	
:- func collision_merge(
		(func(hashmap_leaf(K, V), hashmap_leaf(K, V)) = bool ),
		int, int, K, V, hashmap_leaf(K,V), leaf_array(K,V)
	) = leaf_array(K,V).
	
:- mode collision_merge( (func(in, in) = out is det), in, in, in, in, in, in)
	= out is det.
:- mode collision_merge( (func(in, in) = out is semidet), in, in, in, in, in, 
	in) = out is semidet.
	
collision_merge(R, I, Ub, K, V, L, A) = 
	(if K = K2
	then
		(if	V = V2
		then A
		else 
			(if Replace = yes
			then
				slow_set(A, I, L)
			else 
				A
			)
		)
	else if I < Ub
	then
		collision_merge(R, I + 1, Ub, K, V, L, A)
	else
		array_snoc(L, A)
	) :-
	unsafe_lookup(A, I, L2@leaf(_, K2, V2) ), 
	R(L, L2) = Replace.
		

% Create a indexed_branch or full_branch node.
:- func indexed_or_full_branch(bitmap, hash_array(K, V)) = hashmap(K, V).

indexed_or_full_branch(Bitmap, Array) = 
	( if Bitmap = full_bitmap 
	then
		full_branch(Array)
	else
		indexed_branch(Bitmap, Array)
	).
	
% two :: Shift -> Hash -> k -> v -> Hash -> HashMap k v -> ST s (HashMap k v)
:- func two(shift, hash, K, V, hashmap_leaf(K, V)) = hm(K, V).

two(S, H1, K1, V1, L2@leaf(H2, _, _)) = indexed_branch(Bitmap, Array) :-
	mask(H1, S, Bp1),
	mask(H2, S, Bp2),
	( if Bp1 = Bp2
	then
		array.init(1, two(next_shift(S), H1, K1, V1, L2), Array),
		Bitmap = Bp1
	else
		array.init(2, leaf(H1, K1, V1), Array0),
		Index = (index(Bp1, S) < index(Bp2, S) -> 1 ; 0),
		array.set(Index, coerce(L2), Array0, Array),
		Bitmap = Bp1 \/ Bp2		
	).
	
:- pragma inline(two/5).
	


%-----------------------------------------------------------------------------%
% Bit twiddling

% Number of bits that are inspected at each level of the hash tree.

:- func bits_per_subkey = int.
bits_per_subkey = 5.

% The size of a 'Full' node, i.e. @2 ^ 'bitsPerSubkey'@.

:- func max_children = int.
max_children = unchecked_left_shift(1, bits_per_subkey).
:- pragma inline(max_children/0).

:- func subkey_mask = bitmap.
subkey_mask = unchecked_right_shift(1u, bits_per_subkey) - 1u.

% | Given a 'Hash' and a 'Shift' that indicates the level in the tree, compute
% the index into a 'Full' node or into the bitmap of a `BitmapIndexed` node.
%
% >>> index 0b0010_0010 0
% 0b0000_0010

:- func index(bitmap, shift) = int.
index(B, S) = cast_to_int(unchecked_right_shift(B, S) /\ subkey_mask).
:- pragma inline(index/2).

 % Given a 'Hash' and a 'Shift' that indicates the level in the tree, compute
 % the bitmap that contains only the 'index' of the hash at this level.

 % The result can be used for constructing one-element 'BitmapIndexed' nodes or
 % to check whether a 'BitmapIndexed' node may possibly contain the 'Hash'.

 % >>> mask 0b0010_0010 0
 % 0b0100

:- func mask(hash, shift) = bitmap.
mask(H, S) = unchecked_left_shift(1u, index(H, S)).
:- pragma inline(mask/2).

:- pred mask(hash::in, shift::in, bitmap::out) is det.
mask(H, S, mask(H, S)).
:- pragma inline(mask/3).


% This array index is computed by counting the number of 1-bits below the
% 'index' represented by the mask.
%
% >>> sparseIndex 0b0110_0110 0b0010_0000
% 2

:- func sparse_index(bitmap, bitmap) = int.
sparse_index(B, M) = weight(B /\ (M - 1u) ).
:- pragma inline(sparse_index/2).

% A bitmap with the 'maxChildren' least significant bits set, i.e.
% @0xFF_FF_FF_FF@.

:- func full_bitmap = bitmap.

% From the original documentation of Data.Hashmap, 
%-- This needs to use 'shiftL' instead of 'unsafeShiftL', to avoid UB.
%-- See issue #412.
% So I'm using <</2 instead of unchecked_left_shift/2

full_bitmap = \ ( \ 0u << max_children).
:- pragma inline(full_bitmap/0).


% Increment a 'Shift' for use at the next deeper level.
:- func next_shift(shift) = shift.
next_shift(S) = S + bits_per_subkey.

:- pragma inline(next_shift/1).


% Hamming weight, or 'popcount'
:- func weight(uint) = int.

weight(I) = 
	(if I =< 1u
	then 
		cast_to_int(I)
	else 
		weight(I /\ (I-1u), 1)
	).
	
:- pragma inline(weight/1).

:- func weight(uint, int) = int.

weight(I, N) =
	(if I =< 1u
	then 
		cast_to_int(I) + N
	else
		weight(I /\ (I-1u), N+1)
	).
	
