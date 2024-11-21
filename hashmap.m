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

% An attempt at implementing Haskell's Unordered Containers package
% implementation of a HAMT in Merrcury
% Original implementation found in Data.HashMap.Internal by Johan Tibell

:- interface.

:- import_module list.
:- import_module assoc_list.
:- import_module maybe.
:- import_module set.

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

% Succeeds if the main contains the given key
:- pred contains(hashmap(K, _V)::in, K::in) is semidet <= hashable(K).

% Fails if the key is not found
:- pred search(hashmap(K, V)::in, K::in, V::out) is semidet <= hashable(K).
:- func search(hashmap(K, V), K) = V is semidet <= hashable(K).

% Throws an exception if the key is not found
:- pred lookup(hashmap(K, V)::in, K::in, V::out) is det <= hashable(K).
:- func lookup(hashmap(K, V), K) = V is det <= hashable(K).

%-----------------------------------------------------------------------------%
% Insertion

% Insert an element into a hashmap, fails if the element already exists 
:- pred insert(K::in, V::in, hashmap(K, V)::in, hashmap(K, V)::out) is semidet
	<= hashable(K).
:- func insert(hashmap(K, V), K, V) = hashmap(K, V) is semidet <= hashable(K).

:- pred det_insert(K::in, V::in, hashmap(K, V)::in, hashmap(K, V)::out) is det
	<= hashable(K).
:- func det_insert(hashmap(K, V), K, V) = hashmap(K, V) is det <= hashable(K).

:- func det_insert_from_corresponding_lists(hashmap(K, V), list(K), list(V))
    = hashmap(K, V) <= hashable(K).
:- pred det_insert_from_corresponding_lists(list(K)::in,
    list(V)::in, hashmap(K, V)::in, hashmap(K, V)::out) is det <= hashable(K).
	
:- func det_insert_from_assoc_list(hashmap(K, V), assoc_list(K, V)) = 
	hashmap(K, V) <= hashable(K).
:- pred det_insert_from_assoc_list(assoc_list(K, V)::in,
    hashmap(K, V)::in, hashmap(K, V)::out) is det <= hashable(K).
	
:- pred search_insert(K::in, V::in, maybe(V)::out,
    hashmap(K, V)::in, hashmap(K, V)::out) is det <= hashable(K).

% Inserts an element into a hashmap, overwriting element if it already exists
:- pred set(K::in, V::in, hashmap(K, V)::in, hashmap(K, V)::out) is det
	<= hashable(K).
:- func set(hashmap(K, V), K, V) = hashmap(K, V) <= hashable(K).

:- func set_from_corresponding_lists(hashmap(K, V), list(K), list(V)) = 
	hashmap(K, V) <= hashable(K).
:- pred set_from_corresponding_lists(list(K)::in, list(V)::in,
    hashmap(K, V)::in, hashmap(K, V)::out) is det <= hashable(K).
	
:- func set_from_assoc_list(hashmap(K, V), assoc_list(K, V)) = 
	hashmap(K, V) <= hashable(K).
:- pred set_from_assoc_list(assoc_list(K, V)::in,
    hashmap(K, V)::in, hashmap(K, V)::out) is det <= hashable(K).

% Overwrite an already existing element in a hashmap, fail if key not found
:- pred update(K::in, V::in, hashmap(K, V)::in, hashmap(K, V)::out) 
	is semidet	<= hashable(K).
:- func update(hashmap(K, V), K, V) = hashmap(K, V) is semidet <= hashable(K).

:- pred det_update(K::in, V::in, hashmap(K, V)::in, hashmap(K, V)::out) 
	is det	<= hashable(K).
:- func det_update(hashmap(K, V), K, V) = hashmap(K, V) is det <= hashable(K).

%-----------------------------------------------------------------------------%
% Removal

% Remove a key-value pair from a map and return the value.
% Fail if the key is not present.
:- pred remove(K::in, V::out, hashmap(K, V)::in, hashmap(K, V)::out) 
	is semidet <= hashable(K).

:- pred det_remove(K::in, V::out, hashmap(K, V)::in, hashmap(K, V)::out) 
	is det <= hashable(K).

% Delete a key-value pair from a map.
% If the key is not present, leave the map unchanged.	
:- pred delete(K::in, hashmap(K, V)::in, hashmap(K, V)::out) is det
	<= hashable(K).
:- func delete(hashmap(K, V), K) = hashmap(K, V) <= hashable(K).

:- func delete_list(hashmap(K, V), list(K)) = hashmap(K, V) <= hashable(K).
:- pred delete_list(list(K)::in, hashmap(K, V)::in, hashmap(K, V)::out)
	is det <= hashable(K).

%-----------------------------------------------------------------------------%
% Field selection for maps.

% Map ^ elem(Key) = search(Map, Key).
:- func elem(K, hashmap(K, V)) = V is semidet <= hashable(K).

% Map ^ det_elem(Key) = lookup(Map, Key).
:- func det_elem(K, hashmap(K, V)) = V <= hashable(K).

% (Map ^ elem(Key) := Value) = set(Map, Key, Value).
:- func 'elem :='(K, hashmap(K, V), V) = hashmap(K, V) <= hashable(K).

% (Map ^ det_elem(Key) := Value) = det_update(Map, Key, Value).
:- func 'det_elem :='(K, hashmap(K, V), V) = hashmap(K, V) <= hashable(K).

%-----------------------------------------------------------------------------%
% Returning keys and values.

% All key value pairs stored in the hashmap, order is not garunteed
:- pred member(hashmap(K, V), K, V) <= hashable(K).
:- mode member(in, in, out) is semidet.
:- mode member(in, out, out) is nondet.

% Given a map, return a list of all the keys in the map.
:- func keys(hashmap(K, _V)) = list(K) <= hashable(K).
:- pred keys(hashmap(K, _V)::in, list(K)::out) is det <= hashable(K).

:- func sorted_keys(hashmap(K, _V)) = list(K) <= hashable(K).
:- pred sorted_keys(hashmap(K, _V)::in, list(K)::out) is det <= hashable(K).

:- func keys_as_set(hashmap(K, _V)) = set(K) <= hashable(K).
:- pred keys_as_set(hashmap(K, _V)::in, set(K)::out) is det <= hashable(K).

:- func values(hashmap(_K, V)) = list(V).
:- pred values(hashmap(_K, V)::in, list(V)::out) is det.

:- pred keys_and_values(hashmap(K, V)::in, list(K)::out, list(V)::out)
	is det <= hashable(K).

%-----------------------------------------------------------------------------%
% Operations on values.


% Update the value at the given key by applying the supplied
% transformation to it. Fails if the key is not found. This is faster
% than first searching for the value and then updating it.
:- pred transform_value(pred(V, V)::in(pred(in, out) is det), K::in,
    hashmap(K, V)::in, hashmap(K, V)::out) is semidet <= hashable(K).

% Same as transform_value/4, but throws an exception if the key is not
% found.
:- func det_transform_value(func(V) = V, K, hashmap(K, V)) = hashmap(K, V)
	<= hashable(K).
:- pred det_transform_value(pred(V, V)::in(pred(in, out) is det), K::in,
    hashmap(K, V)::in, hashmap(K, V)::out) is det <= hashable(K).

%-----------------------------------------------------------------------------%
% Conversions

 % Convert an association list to a map.
:- func from_assoc_list(assoc_list(K, V)) = hashmap(K, V) <= hashable(K).
:- pred from_assoc_list(assoc_list(K, V)::in, hashmap(K, V)::out) is det 
	<= hashable(K).

% Convert a pair of lists (which must be of the same length) to a map.
:- func from_corresponding_lists(list(K), list(V)) = hashmap(K, V)
	<= hashable(K).
:- pred from_corresponding_lists(list(K)::in, list(V)::in, hashmap(K, V)::out)
    is det <= hashable(K).

% Convert a map to an association list.
:- func to_assoc_list(hashmap(K, V)) = assoc_list(K, V) <= hashable(K).
:- pred to_assoc_list(hashmap(K, V)::in, assoc_list(K, V)::out) is det
	<= hashable(K).
	
% Consider the original map a set of key-value pairs. This predicate
% returns a map that maps each value to the set of keys it is paired with
% in the original map.
:- func reverse_map(hashmap(K, V)) = hashmap(V, set(K))
	<= (hashable(K), hashable(V)).

%-----------------------------------------------------------------------------%
% Bit twiddling

:- type hash == uint.

:- type bitmap == uint.

:- type mask == uint.

:- type shift == int.

% Bit width of hash type
:- func hash_size = int.

% Number of bits that are inspected at each level of the hash tree.
:- func bits_per_subkey = int.

% The size of a 'Full' node, i.e. @2 ^ 'bitsPerSubkey'@.
:- func max_children = int.

% Bit mask with the lowest 'bitsPerSubkey' bits set, i.e. @0b11111@.
:- func subkey_mask = bitmap.

% | Given a 'Hash' and a 'Shift' that indicates the level in the tree, compute
% the index into a 'Full' node or into the bitmap of a `BitmapIndexed` node.
%
% >>> index 0b0010_0010 0
% 0b0000_0010

:- func index(hash, shift) = int.

:- pred index(hash::in, shift::in, int::out) is det.

% Given a 'Hash' and a 'Shift' that indicates the level in the tree, compute
 % the bitmap that contains only the 'index' of the hash at this level.

 % The result can be used for constructing one-element 'BitmapIndexed' nodes or
 % to check whether a 'BitmapIndexed' node may possibly contain the 'Hash'.

 % >>> mask 0b0010_0010 0
 % 0b0100

:- func mask(hash, shift) = mask.

:- pred mask(hash::in, shift::in, mask::out) is det.

% This array index is computed by counting the number of 1-bits below the
% 'index' represented by the mask.
%
% >>> sparseIndex 0b0110_0110 0b0010_0000
% 2
:- func sparse_index(bitmap, mask) = int.

:- pred sparse_index(bitmap::in, mask::in, int::out) is det.

% A bitmap with the 'maxChildren' least significant bits set, i.e.
% @0xFF_FF_FF_FF@.
:- func full_bitmap = bitmap.

% Increment a 'Shift' for use at the next deeper level.
:- func next_shift(shift) = shift.

% Hamming weight, or 'popcount'
:- func weight(bitmap) = int.

%-----------------------------------------------------------------------------%
% Standard higher order functions on collections.

% Commented modes not supported by current array library higher order calls,
% custom implementation required.

% Perform a traversal by key of the map, applying an accumulator
% predicate for value. Order is arbitrary and cannot be garunteed.
:- func foldl(func(K, V, A) = A, hashmap(K, V), A) = A.
:- pred foldl(pred(K, V, A, A), hashmap(K, V), A, A).
:- mode foldl(in(pred(in, in, in, out) is det), in, in, out) is det.
:- mode foldl(in(pred(in, in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldl(in(pred(in, in, di, uo) is det), in, di, uo) is det.
:- mode foldl(in(pred(in, in, in, out) is semidet), in, in, out) is semidet.
:- mode foldl(in(pred(in, in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode foldl(in(pred(in, in, di, uo) is semidet), in, di, uo) is semidet.
% :- mode foldl(in(pred(in, in, in, out) is cc_multi), in, in, out) is cc_multi.
% :- mode foldl(in(pred(in, in, di, uo) is cc_multi), in, di, uo) is cc_multi.
% :- mode foldl(in(pred(in, in, mdi, muo) is cc_multi), in, mdi, muo)
    % is cc_multi.
	
:- pred foldl2(pred(K, V, A, A, B, B), hashmap(K, V), A, A, B, B).
:- mode foldl2(in(pred(in, in, in, out, in, out) is det),
    in, in, out, in, out) is det.
:- mode foldl2(in(pred(in, in, in, out, mdi, muo) is det),
    in, in, out, mdi, muo) is det.
:- mode foldl2(in(pred(in, in, in, out, di, uo) is det),
    in, in, out, di, uo) is det.
% :- mode foldl2(in(pred(in, in, di, uo, di, uo) is det),
    % in, di, uo, di, uo) is det.
:- mode foldl2(in(pred(in, in, in, out, in, out) is semidet),
    in, in, out, in, out) is semidet.
:- mode foldl2(in(pred(in, in, in, out, mdi, muo) is semidet),
    in, in, out, mdi, muo) is semidet.
:- mode foldl2(in(pred(in, in, in, out, di, uo) is semidet),
    in, in, out, di, uo) is semidet.
% :- mode foldl2(in(pred(in, in, in, out, in, out) is cc_multi),
    % in, in, out, in, out) is cc_multi.
% :- mode foldl2(in(pred(in, in, in, out, mdi, muo) is cc_multi),
    % in, in, out, mdi, muo) is cc_multi.
% :- mode foldl2(in(pred(in, in, in, out, di, uo) is cc_multi),
    % in, in, out, di, uo) is cc_multi.
% :- mode foldl2(in(pred(in, in, di, uo, di, uo) is cc_multi),
    % in, di, uo, di, uo) is cc_multi.
	
:- pred foldl3(pred(K, V, A, A, B, B, C, C), hashmap(K, V), A, A, B, B, C, C).
:- mode foldl3(in(pred(in, in, in, out, in, out, in, out) is det),
    in, in, out, in, out, in, out) is det.
:- mode foldl3(in(pred(in, in, in, out, in, out, mdi, muo) is det),
    in, in, out, in, out, mdi, muo) is det.
:- mode foldl3(in(pred(in, in, in, out, in, out, di, uo) is det),
    in, in, out, in, out, di, uo) is det.
% :- mode foldl3(in(pred(in, in, in, out, di, uo, di, uo) is det),
    % in, in, out, di, uo, di, uo) is det.
% :- mode foldl3(in(pred(in, in, di, uo, di, uo, di, uo) is det),
    % in, di, uo, di, uo, di, uo) is det.
:- mode foldl3(in(pred(in, in, in, out, in, out, in, out) is semidet),
    in, in, out, in, out, in, out) is semidet.
:- mode foldl3(in(pred(in, in, in, out, in, out, mdi, muo) is semidet),
    in, in, out, in, out, mdi, muo) is semidet.
:- mode foldl3(in(pred(in, in, in, out, in, out, di, uo) is semidet),
    in, in, out, in, out, di, uo) is semidet.
	
% Given that the hashmap stores key-value pairs in an order arbitrary to the
% hash function used, I reasoned that implementing rfold calls would be
% redundant.

% Apply a transformation predicate to all the values in a map.
:- func map_values(func(K, V) = W, hashmap(K, V)) = hashmap(K, W).
:- pred map_values(pred(K, V, W), hashmap(K, V), hashmap(K, W)).
:- mode map_values(in(pred(in, in, out) is det), in, out) is det.
% :- mode map_values(in(pred(in, in, out) is semidet), in, out) is semidet.

% Same as map_values, but do not pass the key to the given predicate.
:- func map_values_only(func(V) = W, hashmap(K, V)) = hashmap(K, W).
:- pred map_values_only(pred(V, W), hashmap(K, V), hashmap(K, W)).
:- mode map_values_only(in(pred(in, out) is det), in, out) is det.
% :- mode map_values_only(in(pred(in, out) is semidet), in, out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- use_module map.
:- import_module int.
:- import_module uint.
:- import_module bool.
:- import_module require.
:- import_module pair.

:- import_module util.

%-----------------------------------------------------------------------------%
% Hash Array Mapped Table

:- type hashmap(K, V)
	--->	empty_tree
	;		leaf(hash, K, V)
	;		indexed_branch(bitmap, hash_array(K, V))
	;		full_branch(hash_array(K, V))
	;		collision(hash, bucket(K, V)).


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

contains(Map, K) :- search(Map, K, _).

search(HM, K, search(HM, K)).

search(HM, K) = search(HM, hash(K), K, 0).

:- func search(hashmap(K, V), hash, K, shift) = V is semidet.

search(leaf(H, K, V), H,  K, _) = V.

search(indexed_branch(B, Array), H, K, S) =
	(if B /\ M = 0u
	then
		func_fail
	else
		search(Next, H, K, next_shift(S))
	) 
:- 
	mask(H, S, M),
	array.unsafe_lookup(Array, sparse_index(B, M), Next).
	
search(full_branch(Array), H,  K, S) =
	search(Next, H, K, next_shift(S))
:-
	array.unsafe_lookup(Array, index(H, S), Next).
	
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
		unsafe_array_insert(I, leaf(H, K, V), !Array), 
		!:HM = indexed_or_full_branch(B \/ M, !.Array)
	else 
		array.unsafe_lookup(!.Array, I, Branch0),
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
	array.unsafe_lookup(!.Array, I, Branch0),
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
		insert_tree(H, K, V, S, R, indexed_branch(mask(CH, S), 
			BArray), !:HM)
	).

:- pragma inline(insert/3).	

det_insert(K, V, !HM) :-
	( if insert(K, V, !.HM, NewMap) then
        !:HM = NewMap
    else
        report_lookup_error("hashmap.det_insert: key already present", K, V)
    ).
	
:- pragma inline(det_insert/4).
	
det_insert(!.HM, K, V) = !:HM :- 
	det_insert(K, V, !HM).
	
:- pragma inline(det_insert/3).
	
det_insert_from_corresponding_lists(M0, Ks, Vs) = M :-
    hashmap.det_insert_from_corresponding_lists(Ks, Vs, M0, M).

det_insert_from_corresponding_lists([], [], !Map).
det_insert_from_corresponding_lists([], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
det_insert_from_corresponding_lists([_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
det_insert_from_corresponding_lists([K | Ks], [V | Vs], !Map) :-
    hashmap.det_insert(K, V, !Map),
    hashmap.det_insert_from_corresponding_lists(Ks, Vs, !Map).

det_insert_from_assoc_list(M0, AL) = M :-
    hashmap.det_insert_from_assoc_list(AL, M0, M).

det_insert_from_assoc_list([], !Map).
det_insert_from_assoc_list([K - V | KVs], !Map) :-
    hashmap.det_insert(K, V, !Map),
    hashmap.det_insert_from_assoc_list(KVs, !Map).	

% search_insert(K, V, MaybOldV, !HM)
search_insert(K, V, MaybOldV, !HM) :-
	search_insert_tree(hash(K), K, V, 0, MaybOldV, !HM).

	
%  pred search_insert_tree(Key, Value, Shift, MaybOldV, !HashTree) 
:- pred search_insert_tree(hash::in, K::in, V::in, shift::in, maybe(V)::out,
	hashmap(K, V)::in, hashmap(K, V)::out) is det.	
 
 search_insert_tree(H, K, V, _, no, empty_tree, leaf(H, K, V)).
 
 search_insert_tree(H, K, V, S, Old, !.HM@leaf(LH, LK, LV), !:HM) :-
	(if H = LH
	then
		(if K = LK
		then
			(if V = LV
			then 
				Old = no,
				!:HM = !.HM
			else 
				Old = yes(LV), 
				!:HM = leaf(H, K, V)
			)
		else
			Old = no,
			!:HM = collision(H, K, V, LK, LV)	
		)
	else
		Old = no,
		!:HM = two(S, H, K, V, leaf(LH, LK, LV))
	).
 
 	
search_insert_tree(H, K, V, S, Old, !.HM@indexed_branch(B, !.Array), !:HM) :-
	mask(H, S, M),
	sparse_index(B, M, I),
	( if B /\ M = 0u
	then
		Old = no,
		unsafe_array_insert(I, leaf(H, K, V), !Array), 
		!:HM = indexed_or_full_branch(B \/ M, !.Array)
	else 
		array.unsafe_lookup(!.Array, I, Branch0),
		search_insert_tree(H, K, V, next_shift(S), Old, Branch0, Branch1),
		(if private_builtin.pointer_equal(Branch1, Branch0)
		then
			!:HM = !.HM
		else
			slow_set(I, Branch1, !Array),
			!:HM = indexed_branch(B, !.Array)
		)
	).
	
search_insert_tree(H, K, V, S, Old, !.HM@full_branch(!.Array), !:HM) :-
	index(H, S, I),
	array.unsafe_lookup(!.Array, I, Branch0),
	search_insert_tree(H, K, V, next_shift(S), Old, Branch0, Branch1),
	(if private_builtin.pointer_equal(Branch1, Branch0)
	then
		!:HM = !.HM
	else
		slow_set(I, Branch1, !Array),
		!:HM = full_branch(!.Array)
	).

search_insert_tree(H, K, V, S, Old, !.HM@collision(CH, Bucket), !:HM) :-
	(if H = CH
	then
		map.search_insert(K, V, Old, Bucket, NewBucket),
		(if private_builtin.pointer_equal(Bucket, NewBucket)
		then
			!:HM = !.HM
		else
			!:HM = collision(H, NewBucket)
		)
	else
		Old = no,
		array.init(1, !.HM, BArray),
		(if 
			insert_tree(H, K, V, S, yes, 
				indexed_branch(mask(CH, S), 
				BArray), NewBranch)
		then
			!:HM = NewBranch
		else
			unexpected($module, $pred, 
				"Failure on insert_tree/6 with Replace = yes")
		)
	).
 

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

set_from_corresponding_lists(M0, Ks, Vs) = M :-
    hashmap.set_from_corresponding_lists(Ks, Vs, M0, M).

set_from_corresponding_lists([], [], !Map).
set_from_corresponding_lists([], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
set_from_corresponding_lists([_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
set_from_corresponding_lists([K | Ks], [V | Vs], !Map) :-
    hashmap.set(K, V, !Map),
    hashmap.set_from_corresponding_lists(Ks, Vs, !Map).

set_from_assoc_list(M0, AL) = M :-
    hashmap.set_from_assoc_list(AL, M0, M).

set_from_assoc_list([], !Map).
set_from_assoc_list([K - V | KVs], !Map) :-
    hashmap.set(K, V, !Map),
    hashmap.set_from_assoc_list(KVs, !Map).

	
update(K, V, HM, update(HM, K, V)).

update(HM, K, V) = update(HM, hash(K), K, V, 0).

	
det_update(K, V, !HM) :-
	( if update(K, V, !.HM, NewMap) then
        !:HM = NewMap
    else
        report_lookup_error("hashmap.det_update: key not found", K, V)
    ).
	
det_update(!.HM, K, V) = !:HM :- 
	det_update(K, V, !HM).

:- func update(hashmap(K, V), hash, K, V, shift) = hashmap(K, V) is semidet.

update(leaf(H, K, _), H,  K, V, _) = leaf(H, K, V).

update(indexed_branch(B, Array), H, K, V, S) =
	(if B /\ M \= 0u
	then
		update(Next, H,  K, V, next_shift(S))
	else
		func_fail
	)
:- 
	mask(H, S, M),
	array.unsafe_lookup(Array, sparse_index(B, M), Next).
	
update(full_branch(Array), H, K, V, S) =
	update(Next, H, K, V, next_shift(S))
:-
	array.unsafe_lookup(Array, index(H, S), Next).
	
update(collision(H, Bucket), H, K, V, _) = 
	collision(H, map.update(Bucket, K, V)).
	

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
		Index = (index(H1, S) < index(H2, S) -> 1 ; 0),
		array.set(Index, coerce(L2), Array0, Array),
		Bitmap = Bp1 \/ Bp2		
	).
	
:- pragma inline(two/5).

%-----------------------------------------------------------------------------%
% Removal

remove(K, V, !HM) :- remove(hash(K), K, 0, V, !HM).

det_remove(K, V, !HM) :- 
	( if remove(K, Found, !.HM, NewMap) then
        V = Found,
		!:HM = NewMap
    else
        report_lookup_error("hashmap.det_remove: key not found", K)
    ).
	

:- pred remove(hash::in, K::in, shift::in, V::out, hashmap(K, V)::in, 
	hashmap(K, V)::out)	is semidet <= hashable(K).

remove(H, K, _, V, leaf(H, K, V), empty_tree).

remove(H, K, S, V, indexed_branch(B, Array), HM) :-
	mask(H, S, M),
	B /\ M \= 0u,
	sparse_index(B, M, I),
	array.unsafe_lookup(Array, I, Branch0),
	remove(H, K, next_shift(S), V, Branch0, Branch1),
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
			HM = indexed_branch(B /\ \ M, unsafe_array_delete(Array, I))
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
	remove(H, K, next_shift(S), V, Branch0, Branch1),
	(if Branch1 = empty_tree
	then
		B = full_bitmap /\ (\ unchecked_left_shift(1u, I)),
		HM = indexed_branch(B, unsafe_array_delete(Array, I))
	else
		HM = full_branch(slow_set(Array, I, Branch1))
	).
	
remove(H, K, _, V, collision(H, Bucket), HM) :- 
	map.remove(K, V, Bucket, NewBucket),
	(if map.to_assoc_list(NewBucket, [(NK - NV)])
	then
		HM = leaf(H, NK, NV)
	else
		HM = collision(H, NewBucket)
	).

%-----------------------------------------------------------------------------%


delete(K, HM, delete(HM, K)).
:- pragma inline(delete/3).

delete(HM, K) = delete(HM, hash(K), K, 0).
:- pragma inline(delete/2).

:- func delete(hashmap(K, V), hash, K, shift) = hashmap(K, V).

delete(empty_tree, _, _, _) = empty_tree.

delete(HM@leaf(LH, LK, _), H, K, _) =
	(if 
		LH = H,
		LK = K
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
		Branch1 = delete(Branch0, H, K, next_shift(S)),
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
				!:HM = indexed_branch(B /\ \ M, 
					unsafe_array_delete(Array, I))
			)
		else if Length = 1, is_leaf_or_collision(Branch1)
		then
			!:HM = Branch1
		else
			!:HM = indexed_branch(B, array.slow_set(Array, I, Branch1))
		)
	).

:- import_module string.

delete(!.HM@full_branch(Array), H, K, S) = !:HM :-
	index(H, S, I),
	array.unsafe_lookup(Array, I, Branch0),
	Branch1 = delete(Branch0, H, K, next_shift(S)),
	(if private_builtin.pointer_equal(Branch1, Branch0)
	then
		!:HM = !.HM
	else if Branch1 = empty_tree
	then
		B = full_bitmap /\ (\ unchecked_left_shift(1u, I)),
		!:HM = indexed_branch(B, unsafe_array_delete(Array, I))
	else
		!:HM = full_branch(slow_set(Array, I, Branch1))
	).
	
delete(HM@collision(CH, Bucket), H, K, _) = 
	(if H = CH
	then
		(if map.to_assoc_list(NewBucket, [(NK - NV)])
		then
			leaf(H, NK, NV)
		else
			collision(H, NewBucket)
		)
	else
		HM
	) :- 
	map.delete(K, Bucket, NewBucket).
	 
:- pragma inline(delete/4).

delete_list(M0, Ks) = M :-
    hashmap.delete_list(Ks, M0, M).

delete_list([], !Map).
delete_list([DeleteKey | DeleteKeys], !Map) :-
    hashmap.delete(DeleteKey, !Map),
    hashmap.delete_list(DeleteKeys, !Map).
	
%-----------------------------------------------------------------------------%
% Field selection for maps.


elem(Key, Map) = hashmap.search(Map, Key).

det_elem(Key, Map) = hashmap.lookup(Map, Key).

'elem :='(Key, Map, Value) = hashmap.set(Map, Key, Value).

'det_elem :='(Key, Map, Value) = hashmap.det_update(Map, Key, Value).


%-----------------------------------------------------------------------------%
% Returning keys and values.
	
:- pragma promise_equivalent_clauses(member/3).

member(HM::in, K::in, V::out) :- search(HM, K, V).

member(leaf(_, K, V)::in, K::out, V::out).

member(indexed_branch(_, Array)::in, K::out, V::out) :-
	array.member(Array, HM),
	member(HM, K, V).
	
member(full_branch(Array)::in, K::out, V::out) :-
	array.member(Array, HM),
	member(HM, K, V).
	
member(collision(_, Bucket)::in, K::out, V::out) :- map.member(Bucket, K, V).

keys(HM) = Ks :- keys(HM, Ks).

keys(HM, Ks) :- keys_acc(HM, [], Ks).

:- pred keys_acc(hashmap(K, _V)::in, list(K)::in, list(K)::out) is det.

keys_acc(empty_tree, !Ks).
keys_acc(leaf(_H, K, _V), Ks, [K | Ks]).
keys_acc(indexed_branch(_B, Array), !Ks) :- array.foldl(keys_acc, Array, !Ks).
keys_acc(full_branch(Array), !Ks) :- array.foldl(keys_acc, Array, !Ks).
keys_acc(collision(_H, Bucket), Ks, Ks ++ map.keys(Bucket)).

sorted_keys(HM) = Ks :- sorted_keys(HM, Ks).

sorted_keys(HM, Ks) :- to_sorted_list(keys_as_set(HM), Ks).
	
keys_as_set(HM) = Set :-
    keys_as_set(HM, Set).
	
keys_as_set(HM, Set) :-
	kset_acc(HM, set.init, Set).
	
:- pred kset_acc(hashmap(K, _V)::in, set(K)::in, set(K)::out) is det.

kset_acc(empty_tree, !S).
kset_acc(leaf(_H, K, _V), S, insert(S, K)).
kset_acc(indexed_branch(_B, Array), !S) :- array.foldl(kset_acc, Array, !S).
kset_acc(full_branch(Array), !S) :- array.foldl(kset_acc, Array, !S).
kset_acc(collision(_H, Bucket), S, union(S, map.keys_as_set(Bucket))).

values(HM) = Vs :- values(HM, Vs).

values(HM, Vs) :- vals_acc(HM, [], Vs).

:- pred vals_acc(hashmap(_K, V)::in, list(V)::in, list(V)::out) is det.

vals_acc(empty_tree, !Vs).
vals_acc(leaf(_H, _K, V), Vs, [V | Vs]).
vals_acc(indexed_branch(_B, Array), !Vs) :- array.foldl(vals_acc, Array, !Vs).
vals_acc(full_branch(Array), !Vs) :- array.foldl(vals_acc, Array, !Vs).
vals_acc(collision(_H, Bucket), Vs, Vs ++ map.values(Bucket)).



keys_and_values(HM, Ks, Vs) :- keys_and_values_acc(HM, [], Ks, [], Vs).

:- pred keys_and_values_acc(hashmap(K, V)::in, list(K)::in, list(K)::out, 
	list(V)::in, list(V)::out) is det.

keys_and_values_acc(empty_tree, !Ks, !Vs).
keys_and_values_acc(leaf(_H, K, V), Ks, [K | Ks], Vs, [V | Vs]).
keys_and_values_acc(indexed_branch(_B, Array), !Ks, !Vs) :- 
	array.foldl2(keys_and_values_acc, Array, !Ks, !Vs).
keys_and_values_acc(full_branch(Array), !Ks, !Vs) :- 
	array.foldl2(keys_and_values_acc, Array, !Ks, !Vs).
keys_and_values_acc(collision(_H, Bucket), !Ks, !Vs ) :-
	map.keys_and_values(Bucket, BKs, BVs),
	!:Ks = !.Ks ++ BKs,
	!:Vs = !.Vs ++ BVs.
	
%-----------------------------------------------------------------------------%
% Operations on values.

transform_value(P, K, !HM) :- transform_value_tree(P, hash(K), K, 0, !HM).

:- pred transform_value_tree(pred(V, V)::in(pred(in, out) is det), hash::in,
	K::in, shift::in, hashmap(K, V)::in, hashmap(K, V)::out) is semidet 
	<= hashable(K). 

transform_value_tree(P, H, K, _S, !HM) :-
	!.HM = leaf(H, K, V0),
	P(V0, V),
	(if private_builtin.pointer_equal(V0, V)
	then
		!:HM = !.HM
	else
		!:HM = leaf(H, K, V)
	).
		
	
transform_value_tree(P, H, K, S, !.HM@indexed_branch(B, !.Array), !:HM) :-
	mask(H, S, M),
	sparse_index(B, M, I),
	( if B /\ M = 0u
	then
		fail
	else 
		array.unsafe_lookup(!.Array, I, Branch0),
		transform_value_tree(P, H, K, next_shift(S), Branch0, Branch1),
		(if private_builtin.pointer_equal(Branch1, Branch0)
		then
			!:HM = !.HM
		else
			slow_set(I, Branch1, !Array),
			!:HM = indexed_branch(B, !.Array)
		)
	).
	
transform_value_tree(P, H, K, S, !.HM@full_branch(!.Array), !:HM) :-
	index(H, S, I),
	array.unsafe_lookup(!.Array, I, Branch0),
	transform_value_tree(P, H, K, next_shift(S), Branch0, Branch1),
	(if private_builtin.pointer_equal(Branch1, Branch0)
	then
		!:HM = !.HM
	else
		slow_set(I, Branch1, !Array),
		!:HM = full_branch(!.Array)
	).

transform_value_tree(P,H, K, _S, !HM) :-
	!.HM = collision(H, Bucket0),
	map.transform_value(P, K, Bucket0, Bucket),
	(if private_builtin.pointer_equal(Bucket0, Bucket)
	then
		!:HM = !.HM
	else
		!:HM = collision(H, Bucket)
	).

det_transform_value(F, K, !.HM) = !:HM :-
    det_transform_value(pred(V0::in, V::out) is det :- V = F(V0), K,
        !HM).

det_transform_value(P, K, !HM) :-
    ( if transform_value(P, K, !.HM, NewHM) then
        !:HM = NewHM
    else
        report_lookup_error("map.det_transform_value: key not found", K)
    ).
	
%-----------------------------------------------------------------------------%
% Conversions

from_assoc_list(AL) = HM :-
    from_assoc_list(AL, HM).
	
from_assoc_list(AL, HM) :- assoc_list_to_hashmap_acc(AL, empty_tree, HM).
	
:- pred assoc_list_to_hashmap_acc(assoc_list(K, V)::in,
    hashmap(K, V)::in, hashmap(K, V)::out) is det <= hashable(K).

assoc_list_to_hashmap_acc([], HM, HM).
assoc_list_to_hashmap_acc([K - V | Rest], !HM) :-
    set(K, V, !HM),
    assoc_list_to_hashmap_acc(Rest, !HM).
	
from_corresponding_lists(Ks, Vs) = HM :-
    from_corresponding_lists(Ks, Vs, HM).
	
from_corresponding_lists(Keys, Values, HashMap) :-
    assoc_list.from_corresponding_lists(Keys, Values, AssocList),
    from_assoc_list(AssocList, HashMap).
	
	
to_assoc_list(HM) = AL :-
    to_assoc_list(HM, AL).

to_assoc_list(HM, AL) :-
    to_assoc_list_acc(HM, [], AL).


:- pred to_assoc_list_acc(hashmap(K, V)::in, assoc_list(K, V)::in, 
	assoc_list(K, V)::out) is det.

to_assoc_list_acc(empty_tree, !AL).
to_assoc_list_acc(leaf(_H, K, V), ALs, [ (K - V) | ALs]).
to_assoc_list_acc(indexed_branch(_B, Array), !ALs) :- 
	array.foldl(to_assoc_list_acc, Array, !ALs).
to_assoc_list_acc(full_branch(Array), !ALs) :- 
	array.foldl(to_assoc_list_acc, Array, !ALs).
to_assoc_list_acc(collision(_H, Bucket), ALs, 
	ALs ++ map.to_assoc_list(Bucket)).
	

reverse_map(HM) = RHM :-
    foldl(reverse_map_2, HM, init, RHM).

:- pred reverse_map_2(K::in, V::in,
    hashmap(V, set(K))::in, hashmap(V, set(K))::out) is det 
	<= (hashable(K), hashable(V)).

reverse_map_2(Key, Value, !RHM) :-
    ( if search(!.RHM, Value, Keys0) then
        set.insert(Key, Keys0, Keys),
        det_update(Value, Keys, !RHM)
    else
        det_insert(Value, set.make_singleton_set(Key), !RHM)
    ).
	
%-----------------------------------------------------------------------------%
% Bit twiddling

bits_per_subkey = 5.

max_children = unchecked_left_shift(1, bits_per_subkey).
:- pragma inline(max_children/0).

subkey_mask = unchecked_left_shift(1u, bits_per_subkey) - 1u.


index(B, S) = cast_to_int(unchecked_right_shift(B, S) /\ subkey_mask).
:- pragma inline(index/2).

index(B, S, index(B, S)).
:- pragma inline(index/3).

 
mask(H, S) = unchecked_left_shift(1u, index(H, S)).
:- pragma inline(mask/2).

mask(H, S, mask(H, S)).
:- pragma inline(mask/3).


sparse_index(B, M) = weight(B /\ (M - 1u) ).
:- pragma inline(sparse_index/2).

sparse_index(B, M, sparse_index(B, M)).
:- pragma inline(sparse_index/3).


% From the original documentation of Data.Hashmap, 
%-- This needs to use 'shiftL' instead of 'unsafeShiftL', to avoid UB.
%-- See issue #412.
% So I'm using <</2 instead of unchecked_left_shift/2

full_bitmap = \ ( \ 0u << max_children).
:- pragma inline(full_bitmap/0).


next_shift(S) = S + bits_per_subkey.

:- pragma inline(next_shift/1).


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


%-----------------------------------------------------------------------------%
% Standard higher order functions on collections.

foldl(F, T, A) = B :-
    P = (pred(W::in, X::in, Y::in, Z::out) is det :- Z = F(W, X, Y) ),
    foldl(P, T, A, B).
	
foldl(_P, empty_tree, !A).
foldl(P, leaf(_H, K, V), !A) :- P(K, V, !A).
foldl(P, indexed_branch(_B, Array), !A) :- array.foldl(foldl(P), Array, !A).
foldl(P, full_branch(Array), !A) :- array.foldl(foldl(P), Array, !A).
foldl(P, collision(_H, Bucket), !A) :- 
	map.foldl(P, Bucket, !A).

foldl2(_P, empty_tree, !A, !B).
foldl2(P, leaf(_H, K, V), !A, !B) :- P(K, V, !A, !B).
foldl2(P, indexed_branch(_B, Array), !A, !B) :-
	array.foldl2(foldl2(P), Array, !A, !B).
foldl2(P, full_branch(Array), !A, !B) :- array.foldl2(foldl2(P), Array, !A, !B).
foldl2(P, collision(_H, Bucket), !A, !B) :- map.foldl2(P, Bucket, !A, !B).

foldl3(_P, empty_tree, !A, !B, !C).
foldl3(P, leaf(_H, K, V), !A, !B, !C) :- P(K, V, !A, !B, !C).
foldl3(P, indexed_branch(_B, Array), !A, !B, !C) :-
	array.foldl3(foldl3(P), Array, !A, !B, !C).
foldl3(P, full_branch(Array), !A, !B, !C) :-
	array.foldl3(foldl3(P), Array, !A, !B, !C).
foldl3(P, collision(_H, Bucket), !A, !B, !C) :-
	map.foldl3(P, Bucket, !A, !B, !C).
	
map_values(F, !.HM) = !:HM :-
    P = (pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    map_values(P, !HM).
	
map_values(_P, empty_tree, empty_tree).
map_values(P, leaf(H, K, V), leaf(H, K, W)) :- P(K, V, W).
map_values(P, indexed_branch(B, !.Array), indexed_branch(B, !:Array)) :-
	array.map(map_values(P), !Array).
map_values(P, full_branch(!.Array), full_branch(!:Array)) :-
	array.map(map_values(P), !Array).
map_values(P, collision(H, !.Bucket), collision(H, !:Bucket)) :-
	map.map_values(P, !Bucket).
	
map_values_only(F, !.HM) = !:HM :-
    P = (pred(Y::in, Z::out) is det :- Z = F(Y) ),
    map_values_only(P, !HM).
	
map_values_only(_P, empty_tree, empty_tree).
map_values_only(P, leaf(H, K, V), leaf(H, K, W)) :- P(V, W).
map_values_only(P, indexed_branch(B, !.Array), indexed_branch(B, !:Array)) :-
	array.map(map_values_only(P), !Array).
map_values_only(P, full_branch(!.Array), full_branch(!:Array)) :-
	array.map(map_values_only(P), !Array).
map_values_only(P, collision(H, !.Bucket), collision(H, !:Bucket)) :-
	map.map_values_only(P, !Bucket).
	
	
	
