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

% Succeeds if the given key can be found in a hashmap
% :- pred contains(hashmap(K, _V)::in, K::in) is semidet.

% Succeeds if two hashmaps contain the same elements, regardless of internal
% structure
:- pred equal(hashmap(K, V)::in, hashmap(K, V)::in) is semidet.

%-----------------------------------------------------------------------------%
% Search

:- pred search(hashmap(K, V)::in, K::in, V::out) is semidet <= hashable(K).
:- func search(hashmap(K, V), K) = V is semidet <= hashable(K).

% Throws an exception if the key is not found
:- pred lookup(hashmap(K, V)::in, K::in, V::out) is det <= hashable(K).
:- func lookup(hashmap(K, V), K) = V is det <= hashable(K).

%-----------------------------------------------------------------------------%
% Insertion

% Fails if the element already exists in the hash map
:- pred insert(K::in, V::in, hashmap(K, V)::in, hashmap(K, V)::out) is semidet
	<= hashable(K).
:- func insert(hashmap(K, V), K, V) = hashmap(K, V) is semidet <= hashable(K).

% Inserts an element into a hashmap, overwriting element if it already exists
:- pred set(K::in, V::in, hashmap(K, V)::in, hashmap(K, V)::out) is det
	<= hashable(K).
:- func set(hashmap(K, V), K, V) = hashmap(K, V) <= hashable(K).

%-----------------------------------------------------------------------------%
% Removal

% Remove a key-value pair from a map and return the value.
% Fail if the key is not present.
:- pred remove(K::in, V::out, hashmap(K, V)::in, hashmap(K, V)::out) 
	is semidet <= hashable(K).

% Delete a key-value pair from a map.
% If the key is not present, leave the map unchanged.	
:- pred delete(K::in, hashmap(K, V)::in, hashmap(K, V)::out) is det
	<= hashable(K).
:- func delete(hashmap(K, V), K) = hashmap(K, V) <= hashable(K).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- use_module map.
:- import_module int.
:- import_module uint.
:- import_module bool.
:- import_module require.
:- import_module assoc_list.
:- import_module pair.

:- import_module mh_util.

%-----------------------------------------------------------------------------%
% Hash Array Mapped Table

:- type hashmap(K, V)
	--->	empty_tree
	;		indexed_branch(bitmap, hash_array(K, V))
	;		leaf(hash, K, V)
	;		full_branch(hash_array(K, V))
	;		collision(hash, bucket(K, V)).

:- type hash == uint.

:- type bitmap == uint.

:- type mask == uint.

:- type shift == int.

:- func hash_size = int.

hash_size = bits_per_uint.

:- type hash_array(K, V) == array(hashmap(K, V)).

:- type bucket(K, V) == map.map(K, V).

:- type hm(K, V) == hashmap(K, V).

:- pred is_leaf_or_collision(hashmap(K, V)::in) is semidet.

is_leaf_or_collision(leaf(_, _,_)).
is_leaf_or_collision(collision(_, _)).
	

%-----------------------------------------------------------------------------%
% Hashmap Leaf

:- inst hashmap_leaf
	--->	leaf(ground, ground, ground).


:- type hashmap_leaf(K, V) =< hashmap(K, V)
	---> 	leaf(hash, K, V).
	
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
count(HM, count(HM)).

:- func count(hashmap(_, _), int) = int.

count(empty_tree, N) 				= N.
count(leaf(_, _, _), N) 			= N + 1.
count(indexed_branch(_, Array), N) 	= array.foldl(count, Array, N).
count(full_branch(Array), N) 		= array.foldl(count, Array, N).
count(collision(_, Bucket), N) 			= N + map.count(Bucket).

% contains(HM, K) :- search(HM, K, _).

equal(empty_tree, empty_tree).
equal(indexed_branch(B, A1), indexed_branch(B, A2)) :- array_equal(A1, A2).
equal(leaf(H, K, V), leaf(H, K, V)).
equal(full_branch(A1), full_branch(A2)) :- array_equal(A1, A2).
equal(collision(H, B1), collision(H, B2)) :- map.equal(B1, B2).


% Call equal/2 on every element of two arrays, respectively
:- pred array_equal(hash_array(K, V)::in, hash_array(K, V)::in) is semidet.

array_equal(A1, A2) :-
	Size@size(A1) = size(A2),
	all [I] (
		nondet_int_in_range(0, Size, I),
		array.unsafe_lookup(A1, I, Elem1),
		array.unsafe_lookup(A2, I, Elem2),
		equal(Elem1, Elem2)
	).

%-----------------------------------------------------------------------------%
% Search

search(HM, K, search(HM, K)).

search(HM, K) = search(HM, K, hash(K), 0).

:- func search(hashmap(K, V),hash,  K, shift) = V is semidet <= hashable(K).

search(leaf(H, K, V), H,  K,_) = V.

search(indexed_branch(B, Array), H, K, S) =
	search(array.lookup(Array, sparse_index(B, M)), H,  K,next_shift(S))
:- 
	mask(B, S, M),
	B /\ M \= 0u. 
	
search(full_branch(Array), H,  K, S) =
	search(array.lookup(Array, index(H, S)), H, K, next_shift(S)).
	
search(collision(H, Bucket), H, K,  _) = map.search(Bucket, K).



lookup(HM, K, lookup(HM, K)).

lookup(HM, K) = 
	(if search(HM, K) = Found
	then 
		Found
	else
		report_lookup_error("hashmap.lookup: key not found", K)
	).

%-----------------------------------------------------------------------------%
% Insertion

insert(K, V, !HM) :- 
	insert_tree(hash(K), K, V, 0, no, !HM).
	
:- pragma inline(insert/4).
	
insert(!.HM, K, V) = !:HM :-
	insert(K, V, !HM).
	
:- pragma inline(insert/3).

set(K, V, !HM) :- 
	(if insert_tree(hash(K), K, V, 0, yes, !HM)
	then
		!:HM = !.HM
	else
		unexpected($module, $pred, 
			"Failure on insert_tree/6 with Replace = yes")
	).


:- pragma inline(set/4).
	
set(!.HM, K, V) = !:HM :-
	set(K, V, !HM).
	
:- pragma inline(set/3).

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
			then 
				!:HM = !.HM
			else 
				R = yes, 
				!:HM = leaf(H, K, V)
			)
		else
			!:HM = collision(H, K, V, LK, LV)	
		)
	else
		!:HM = two(S, H, K, V, leaf(LH, LK, LV))
	).
	
insert_tree(H, K, V, S, R, !.HM@indexed_branch(B, !.Array), !:HM) :-
	mask(H, S, M),
	sparse_index(B, M, I),
	( if B /\ M = 0u
	then
		array_insert(I, leaf(H, K, V), !Array),
		!:HM = indexed_or_full_branch(B \/ M, !.Array)
	else 
		array.lookup(!.Array, I, Branch0),
		insert_tree(H, K, V, next_shift(S), R, Branch0, Branch1),
		(if private_builtin.pointer_equal(Branch1, Branch0)
		then
			!:HM = !.HM
		else
			slow_set(I, Branch1, !Array),
			!:HM = indexed_branch(B, !.Array)
		)
	).
	
insert_tree(H, K, V, S, R, !.HM@full_branch(!.Array), !:HM) :-
	index(H, S, I),
	array.lookup(!.Array, I, Branch0),
	insert_tree(H, K, V, next_shift(S), R, Branch0, Branch1),
	(if private_builtin.pointer_equal(Branch1, Branch0)
	then
		!:HM = !.HM
	else
		slow_set(I, Branch1, !Array),
		!:HM = full_branch(!.Array)
	).

insert_tree(H, K, V, S, R, !.HM@collision(CH, Bucket), !:HM) :-
	(if H = CH
	then
		!:HM = collision(H, 
			(if R = yes
			then
				map.set(Bucket, K, V)
			else
				map.insert(Bucket, K, V)
			)
		)
	else
		array.init(1, !.HM, BArray),
		insert_tree(H, K, V, S, R, indexed_branch(mask(H, S), BArray), !:HM)
	).

%-----------------------------------------------------------------------------%
% Node creation



% collision(Hash, Key1, Value1, Key2, Value2) = HashMap.
% Create a 'Collision' value with two 'Leaf' values.
% Throws an exception if K1 = K2
:- func collision(hash, K, V, K, V) = hm(K, V).

collision(Hash, K1, V1, K2, V2) = collision(Hash, Bucket) :-
	map.det_insert(K2, V2, map.singleton(K1, V1), Bucket).
		

% Create a indexed_branch or full_branch node.
:- func indexed_or_full_branch(bitmap, hash_array(K, V)) = hashmap(K, V).

indexed_or_full_branch(Bitmap, Array) = 
	( if Bitmap = full_bitmap 
	then
		full_branch(Array)
	else
		indexed_branch(Bitmap, Array)
	).
	
% two(Shift, Hash, Key, Value, Leaf) = hashmap(K, V)
:- func two(shift, hash, K, V, hashmap_leaf(K, V)) = hashmap(K, V).

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
% Removal

:- pred remove(hash::in, K::in, shift::in, V::out, hashmap(K, V)::in, 
	hashmap(K, V)::out)	is semidet <= hashable(K).

remove(H, K, _, V, leaf(H, K, V), empty_tree).

remove(H, K, S, V, indexed_branch(B, Array), HM) :-
	mask(H, S, M),
	B /\ M \= 0u,
	sparse_index(B, M, I),
	array.unsafe_lookup(Array, I, Branch0),
	remove(H, K, S, V, Branch0, Branch1),
	Length = size(Array),
	(if Branch1 = empty_tree 
	then
		(if Length = 1
		then
			HM = empty_tree
		else if 
			Length = 2,
			(
				I = 0, 
				array.unsafe_lookup(Array, 1, L)
			;
				I = 1,
				array.unsafe_lookup(Array, 0, L)
			), 
			is_leaf_or_collision(L)
		then
			HM = L
		else 
			HM = indexed_branch(B /\ \ M, array_delete(Array, I))
		)
	else if Length = 1, is_leaf_or_collision(Branch1)
	then
		HM = Branch1
	else
		HM = indexed_branch(B, array.slow_set(Array, I, Branch1))
	).
	
remove(H, K, S, V, full_branch(Array), HM) :-
	index(H, S, I),
	array.unsafe_lookup(Array, I, Branch0),
	remove(H, K, S, V, Branch0, Branch1),
	(if Branch1 = empty_tree
	then
		B = full_bitmap /\ \ unchecked_left_shift(1u, I),
		!:HM = indexed_branch(B array_delete(Array, I))
	else
		!:HM = full_branch(slow_set(Array, I Branch1))
	).
	
remove(H, K, _, V, collision(H, Bucket), HM) :- 
	map.delete(K, V, Bucket, NewBucket),
	(if to_assoc_list(NewBucket, [(NK - NV)])
	then
		HM = leaf(H, NK, NV)
	else
		HM = collision(H, NewBucket)
	).

%-----------------------------------------------------------------------------%


delete(K, HM, delete(HM, K)).
:- pragma inline(delete/3).

delete(HM, K) = delete(HM, hash(K) K, 0).
:- pragma inline(delete/2).

:- func delete(hashmap(K, V), hash, K, shift) = hashmap(K, V).

delete(empty_tree, _, _, _) = empty_tree.

delete(HM@leaf(LH, LK, _), H, K, _) =
	(if 
		LH = H,
		LK = K,
	then
		empty_tree
	else
		HM
	).

% holy hell the haskell case statement for this clause was ugly, not that I 
% think my chain of if statements is any prettier
delete(!.HM@indexed_branch(B, Array), H, K, S) = !:HM :-
	mask(H, S, M),
	(if B /\ M = 0u
	then
		!:HM = !.HM
	else
		sparse_index(B, M, I),
		array.unsafe_lookup(Array, I, Branch0),
		Branch1 = delete(Branch0, H, K, S),
		Length = size(Array),
		(if private_builtin.pointer_equal(Branch1, Branch0)
		then
			!:HM = !.HM
		else if Branch1 = empty_tree 
		then
			(if Length = 1
			then
				!:HM = empty_tree
			else if 
				Length = 2,
				(
					I = 0, 
					array.unsafe_lookup(Array, 1, L)
				;
					I = 1,
					array.unsafe_lookup(Array, 0, L)
				), 
				is_leaf_or_collision(L)
			then
				!:HM = L
			else 
				!:HM = indexed_branch(B /\ \ M, array_delete(Array, I))
			)
		else if Length = 1, is_leaf_or_collision(Branch1)
		then
			!:HM = Branch1
		else
			!:HM = indexed_branch(B, array.slow_set(Array, I, Branch1))
		)
	).
	
delete(!.HM@full_branch(Array), H, K, S) = !:HM :-
	index(H, S, I),
	array.unsafe_lookup(Array, I, Branch0),
	Branch1 = delete(Branch0, H, K, S),
	(if private_builtin.pointer_equal(Branch1, Branch0)
	then
		!:HM = !.HM
	else if Branch1 = empty_tree
	then
		B = full_bitmap /\ \ unchecked_left_shift(1u, I),
		!:HM = indexed_branch(B array_delete(Array, I))
	else
		!:HM = full_branch(slow_set(Array, I Branch1))
	).
	
delete(HM@collision(CH, Bucket), H, K, _) = 
	(if H = CH
	then
		(if to_assoc_list(NewBucket, [(NK - NV)])
		then
			leaf(H, NK, NV)
		else
			NewBucket
		)
	else
		HM
	) :- 
	map.delete(K, Bucket, NewBucket).
	 
		
	

:- pragma inline(delete/4).

:- pred

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
subkey_mask = unchecked_left_shift(1u, bits_per_subkey) - 1u.

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

:- func mask(hash, shift) = mask.
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

:- func sparse_index(bitmap, mask) = int.
sparse_index(B, M) = weight(B /\ (M - 1u) ).
:- pragma inline(sparse_index/2).

:- pred sparse_index(bitmap::in, mask::in, int::out) is det.
sparse_index(B, M, sparse_index(B, M)).
:- pragma inline(sparse_index/3).

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
	
