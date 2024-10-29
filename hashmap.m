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

:- type shift == int.

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

%  pred insert_tree(Hash, Key, Value, Shift, Replace, !HashTree) is semidet.
:- pred insert_tree(hash::in, K::in, V::in, shift::in, bool::in
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
		!:HT = two(S, H, K, V, !.HT)
	).
	
insert_tree(H, K, V, S, R, !.HT@indexed_branch(B, !.Array), !:HT) :-
	M = mask(H, S),
	I = sparse_index(B, M),
	( if B /\ M = 0
	then
		
	else
	)

:- func collision(hash, hashmap_leaf(K, V), hashmap_leaf(K, V)) = ht(K, V).

collision(Hash, L1, L2) = collsision(Hash, Array) :-
	array.init(2, L1, Array0),
	array.set(1, L2, Array0, Array).
	
% two :: Shift -> Hash -> k -> v -> Hash -> HashMap k v -> ST s (HashMap k v)
:- func two(shift, hash, K, V, hashmap_leaf) = ht(K, V).

two(S, H1, K1, V1, L2@leaf(H2, K2, V2)) = indexed_branch(Bitmap, Array) :-
	mask(H1, S, Bp1),
	mask(H2, S, Bp2),
	( if Bp1 = Bp2
	then
		array.init(1, two(next_shift(S), H1, K1, V1, L2), Array),
		Bitmap = Bp1
	else
		array.init(2, leaf(H1, K1, V1), Array0),
		Index = (index(Bp1, S) < index(Bp2, S) -> 1 ; 0),
		array.set(Index, L2, Array0, Array),
		Bitmap = Bp1 \/ Bp2		
	).
	
:- pragma inline(two/5).
	


%-----------------------------------------------------------------------------%
% Bit twiddling

% Number of bits that are inspected at each level of the hash tree.

:- func bits_per_subkey = int.
bits_per_subkey = 5.

% The size of a 'Full' node, i.e. @2 ^ 'bitsPerSubkey'@.

:- func maxchildren = int.
maxchildren = unchecked_left_shift(1, bits_per_subkey).

:- func subkey_mask = bitmap.
subkey_mask = unchecked_right_shift(1, bits_per_subkey) - 1.

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
mask(H, S) = unchecked_left_shift(1, index(H, S)).
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
sparse_index(B, M) = weight(B /\ (M - 1) ).
:- pragma inline(sparse_index/2).

% A bitmap with the 'maxChildren' least significant bits set, i.e.
% @0xFF_FF_FF_FF@.

:- func full_bitmap = bitmap.

% From the original documentation of Data.Hashmap, 
%-- This needs to use 'shiftL' instead of 'unsafeShiftL', to avoid UB.
%-- See issue #412.
% So I'm using <</2 instead of unchecked_left_shift/2

full_bitmap = \ ( \ 0 << max_children).
:- pragma inline(full_bitmap/0).


% Increment a 'Shift' for use at the next deeper level.
func next_shift(shift) = shift.
next_shift(S) = S + bitsPerSubkey.

:- pragma inline(next_shift/1).


% Hamming weight, or 'popcount'
:- func weight(uint) = int.

weight(I) = 
	(if I =< 1
	then 
		cast_to_int(I)
	else 
		weight(I /\ (I-1) ) + 1
	).
	

%-----------------------------------------------------------------------------%
% Utilites

:- func hash(hash_pred(T), T) = hash.

hash(P, T) = H :- P(T, H).

