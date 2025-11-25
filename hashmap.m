%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License v3 as described in the file LICENCE.
%
% The interface and comments describing such from map.m of the 
% Mercury Core Libraries (published under GPL2) are
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2013-2015, 2017-2024 The Mercury team.
%  - see the file COPYING.LIB in the Mercury distribution 
% (also provided in this distribution)
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

% Interface is made to reflect the same calls as the Mercury map.m library to
% allow hashmap to be used as a 'drop-in' replacement for map.map.
% Note that most, but not all of the calls from map.m have been replicated.


:- interface.

:- import_module list.
:- import_module assoc_list.
:- import_module maybe.
:- import_module set.
:- import_module bool. % TODO:remove the exports for bit twitddling alltogeter

:- import_module hashable.

%-----------------------------------------------------------------------------%
% Hash Array Mapped Table

:- type hash == uint.

:- type hashmap(K, V).

%-----------------------------------------------------------------------------%
% Construction

:- func init = (hashmap(K, _V)::uo) is det.
:- pred init(hashmap(K, _)::uo) is det.

:- func hash_singleton(hash, K, V) = hashmap(K, V).
:- pred hash_singleton(hash::in, K::in, V::in, hashmap(K, V)::out) is det.

:- func singleton(K, V) = hashmap(K, V) <= hashable(K).
:- pred singleton(K::in, V::in, hashmap(K, V)::out) is det <= hashable(K).

%-----------------------------------------------------------------------------%
% Basic interface

:- pred is_empty(hashmap(_, _)::in) is semidet.

	% Number of elements contained (HS size function)
:- func count(hashmap(_, _)) = int is det.
:- pred count(hashmap(_, _)::in, int::out) is det.

	% Succeeds if two hashmaps contain the same elements, regardless of 
	% internal structure
:- pred equal(hashmap(K, V)::in, hashmap(K, V)::in) is semidet.

%-----------------------------------------------------------------------------%
% Search

	% Succeeds if the map contains the given key
:- pred hash_contains(hashmap(K, _V)::in, hash::in, K::in) is semidet.
	
:- pred contains(hashmap(K, _V)::in, K::in) is semidet <= hashable(K).

	% Fails if the key is not found
	
:- pred hash_search(hashmap(K, V)::in, hash::in, K::in, V::out) is semidet.
:- func hash_search(hashmap(K, V), hash, K) = V is semidet.	
	
:- pred search(hashmap(K, V)::in, K::in, V::out) is semidet <= hashable(K).
:- func search(hashmap(K, V), K) = V is semidet <= hashable(K).

	% Throws an exception if the key is not found
:- pred hash_lookup(hashmap(K, V)::in, hash::in, K::in, V::out) is det.
:- func hash_lookup(hashmap(K, V), hash, K) = V is det.

:- pred lookup(hashmap(K, V)::in, K::in, V::out) is det <= hashable(K).
:- func lookup(hashmap(K, V), K) = V is det <= hashable(K).

	% inverse and bounds searches have not been implemented and will throw an
	% exception
:- pred inverse_search(hashmap(K, V)::in, V::in, K::out) is erroneous.
:- pred lower_bound_search(hashmap(K, V)::in, K::in, K::out, V::out)
    is erroneous.
:- pred lower_bound_lookup(hashmap(K, V)::in, K::in, K::out, V::out) 
	is erroneous.
:- pred upper_bound_search(hashmap(K, V)::in, K::in, K::out, V::out)
    is erroneous.
:- pred upper_bound_lookup(hashmap(K, V)::in, K::in, K::out, V::out) 
	is erroneous.

%-----------------------------------------------------------------------------%
% Insertion

	% Insert an element into a hashmap, fails if the element already exists 
:- pred hash_insert(hash::in, K::in, V::in, hashmap(K, V)::in, 
	hashmap(K, V)::out) is semidet.
:- func hash_insert(hashmap(K, V), hash, K, V) = hashmap(K, V) is semidet.

:- pred insert(K::in, V::in, hashmap(K, V)::in, hashmap(K, V)::out) is semidet
	<= hashable(K).
:- func insert(hashmap(K, V), K, V) = hashmap(K, V) is semidet <= hashable(K).

:- pred det_hash_insert(hash::in, K::in, V::in, hashmap(K, V)::in, 
	hashmap(K, V)::out) is det.
:- func det_hash_insert(hashmap(K, V), hash, K, V) = hashmap(K, V) is det.

:- pred det_insert(K::in, V::in, hashmap(K, V)::in, hashmap(K, V)::out) is det
	<= hashable(K).
:- func det_insert(hashmap(K, V), K, V) = hashmap(K, V) is det <= hashable(K).

:- func det_hash_insert_from_corresponding_lists(hashmap(K, V), list(hash), 
	list(K), list(V))  = hashmap(K, V).
:- pred det_hash_insert_from_corresponding_lists(list(hash)::in, list(K)::in, 
    list(V)::in, hashmap(K, V)::in, hashmap(K, V)::out) is det.
	
:- func det_insert_from_corresponding_lists(hashmap(K, V), list(K), list(V))
    = hashmap(K, V) <= hashable(K).
:- pred det_insert_from_corresponding_lists(list(K)::in,
    list(V)::in, hashmap(K, V)::in, hashmap(K, V)::out) is det <= hashable(K).
	
:- func det_insert_from_assoc_list(hashmap(K, V), assoc_list(K, V)) = 
	hashmap(K, V) <= hashable(K).
:- pred det_insert_from_assoc_list(assoc_list(K, V)::in,
    hashmap(K, V)::in, hashmap(K, V)::out) is det <= hashable(K).

:- pred hash_search_insert(hash::in, K::in, V::in, maybe(V)::out,
    hashmap(K, V)::in, hashmap(K, V)::out) is det.
	
:- pred search_insert(K::in, V::in, maybe(V)::out,
    hashmap(K, V)::in, hashmap(K, V)::out) is det <= hashable(K).

	% Inserts an element into a hashmap, overwrite element if it already exists
:- pred hash_set(hash::in, K::in, V::in, hashmap(K, V)::in, hashmap(K, V)::out)
	is det.
:- func hash_set(hashmap(K, V), hash, K, V) = hashmap(K, V).

:- pred set(K::in, V::in, hashmap(K, V)::in, hashmap(K, V)::out) is det
	<= hashable(K).
:- func set(hashmap(K, V), K, V) = hashmap(K, V) <= hashable(K).

:- func hash_set_from_corresponding_lists(hashmap(K, V), list(hash),list(K), 
	list(V)) = hashmap(K, V).
:- pred hash_set_from_corresponding_lists(list(hash)::in, list(K)::in, list(V)::in,
    hashmap(K, V)::in, hashmap(K, V)::out) is det.
	
:- func set_from_corresponding_lists(hashmap(K, V), list(K), list(V)) = 
	hashmap(K, V) <= hashable(K).
:- pred set_from_corresponding_lists(list(K)::in, list(V)::in,
    hashmap(K, V)::in, hashmap(K, V)::out) is det <= hashable(K).
	
:- func set_from_assoc_list(hashmap(K, V), assoc_list(K, V)) = 
	hashmap(K, V) <= hashable(K).
:- pred set_from_assoc_list(assoc_list(K, V)::in,
    hashmap(K, V)::in, hashmap(K, V)::out) is det <= hashable(K).

	% Overwrite an already existing element in a hashmap, fail if key not found
:- pred hash_update(hash::in, K::in, V::in, hashmap(K, V)::in, 
	hashmap(K, V)::out)	is semidet.
:- func hash_update(hashmap(K, V), hash, K, V) = hashmap(K, V) is semidet.

:- pred update(K::in, V::in, hashmap(K, V)::in, hashmap(K, V)::out) 
	is semidet	<= hashable(K).
:- func update(hashmap(K, V), K, V) = hashmap(K, V) is semidet <= hashable(K).

:- pred det_hash_update(hash::in, K::in, V::in, hashmap(K, V)::in, 
	hashmap(K, V)::out)	is det.
:- func det_hash_update(hashmap(K, V), hash, K, V) = hashmap(K, V) is det.

:- pred det_update(K::in, V::in, hashmap(K, V)::in, hashmap(K, V)::out) 
	is det	<= hashable(K).
:- func det_update(hashmap(K, V), K, V) = hashmap(K, V) is det <= hashable(K).

%-----------------------------------------------------------------------------%
% Removal

	% Remove a key-value pair from a map and return the value.
	% Fail if the key is not present.
:- pred hash_remove(hash::in, K::in, V::out, hashmap(K, V)::in, 
	hashmap(K, V)::out) is semidet.

:- pred remove(K::in, V::out, hashmap(K, V)::in, hashmap(K, V)::out) 
	is semidet <= hashable(K).
	
:- pred det_hash_remove(hash::in, K::in, V::out, hashmap(K, V)::in, 
	hashmap(K, V)::out)	is det.	

:- pred det_remove(K::in, V::out, hashmap(K, V)::in, hashmap(K, V)::out) 
	is det <= hashable(K).

	% Delete a key-value pair from a map.
	% If the key is not present, leave the map unchanged.
:- pred hash_delete(hash::in, K::in, hashmap(K, V)::in, hashmap(K, V)::out) 
	is det.

:- func hash_delete(hashmap(K, V), hash, K) = hashmap(K, V).	

:- pred delete(K::in, hashmap(K, V)::in, hashmap(K, V)::out) is det
	<= hashable(K).
:- func delete(hashmap(K, V), K) = hashmap(K, V) <= hashable(K).

:- func delete_list(hashmap(K, V), list(K)) = hashmap(K, V) <= hashable(K).
:- pred delete_list(list(K)::in, hashmap(K, V)::in, hashmap(K, V)::out)
	is det <= hashable(K).

%-----------------------------------------------------------------------------%
% Field selection for maps.

	% Hashmap ^ elem(Key) = search(Hashmap, Key).
:- func elem(K, hashmap(K, V)) = V is semidet <= hashable(K).

	% Hashmap ^ det_elem(Key) = lookup(Hashmap, Key).
:- func det_elem(K, hashmap(K, V)) = V <= hashable(K).

	% (Hashmap ^ elem(Key) := Value) = set(Hashmap, Key, Value).
:- func 'elem :='(K, hashmap(K, V), V) = hashmap(K, V) <= hashable(K).

	% (Hashmap ^ det_elem(Key) := Value) = det_update(Hashmap, Key, Value).
:- func 'det_elem :='(K, hashmap(K, V), V) = hashmap(K, V) <= hashable(K).

%-----------------------------------------------------------------------------%
% Returning keys and values.

	% All key value pairs stored in the hashmap, order is not garunteed
:- pred hash_member(hashmap(K, V), hash, K, V).
:- mode hash_member(in, in, in, out) is semidet.
:- mode hash_member(in, out, out, out) is nondet.

:- pred member(hashmap(K, V), K, V) <= hashable(K).
:- mode member(in, in, out) is semidet.
:- mode member(in, out, out) is nondet.

	% Given a map, return a list of all the keys in the map.
:- func keys(hashmap(K, _V)) = list(K).
:- pred keys(hashmap(K, _V)::in, list(K)::out) is det.

:- func sorted_keys(hashmap(K, _V)) = list(K).
:- pred sorted_keys(hashmap(K, _V)::in, list(K)::out) is det.

:- func keys_as_set(hashmap(K, _V)) = set(K).
:- pred keys_as_set(hashmap(K, _V)::in, set(K)::out) is det.

:- func values(hashmap(_K, V)) = list(V).
:- pred values(hashmap(_K, V)::in, list(V)::out) is det.

:- pred keys_and_values(hashmap(K, V)::in, list(K)::out, list(V)::out)
	is det.
	
:- func max_key(hashmap(K, _)) = K is semidet.
:- pred max_key(hashmap(K, _)::in, K::out) is semidet.

:- func min_key(hashmap(K, _)) = K is semidet.
:- pred min_key(hashmap(K, _)::in, K::out) is semidet.
	
:- func det_max_key(hashmap(K, _)) = K.
:- func det_min_key(hashmap(K, _)) = K.

%-----------------------------------------------------------------------------%
% Operations on values.


	% Update the value at the given key by applying the supplied
	% transformation to it. Fails if the key is not found. This is faster
	% than first searching for the value and then updating it.
:- pred hash_transform_value(pred(V, V)::in(pred(in, out) is det), hash::in,
	K::in, hashmap(K, V)::in, hashmap(K, V)::out) is semidet.

:- pred transform_value(pred(V, V)::in(pred(in, out) is det), K::in,
    hashmap(K, V)::in, hashmap(K, V)::out) is semidet <= hashable(K).

	% Same as transform_value/4, but throws an exception if the key is not
	% found.
:- func det_hash_transform_value(func(V) = V, hash, K, hashmap(K, V)) 
	= hashmap(K, V).
:- pred det_hash_transform_value(pred(V, V)::in(pred(in, out) is det), 
	hash::in, K::in, hashmap(K, V)::in, hashmap(K, V)::out) is det.

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
% Selecting subsets of maps and lists.

	% select takes a map and a set of keys, and returns a map
	% containing the keys in the set and their corresponding values.
:- func select(hashmap(K, V), set(K)) = hashmap(K, V) <= hashable(K).
:- pred select(hashmap(K, V)::in, set(K)::in, hashmap(K, V)::out) is det
	<= hashable(K).

	% Equivalent to select(Full, set.from_sorted_list(Keys), Selected).
	% Offers no performance benefit over select/2 and select/3
:- func select_sorted_list(hashmap(K, V), list(K)) = hashmap(K, V) 
	<= hashable(K).
:- pred select_sorted_list(hashmap(K, V)::in, list(K)::in, hashmap(K, V)::out) 
	is det <= hashable(K).
	
	% select_unselect takes a map and a set of keys, and returns two maps:
	% the first containing the keys in the set and their corresponding values,
	% the second containing the keys NOT in the set and their corresponding
	% values.
:- pred select_unselect(hashmap(K, V)::in, set(K)::in,
    hashmap(K, V)::out, hashmap(K, V)::out) is det <= hashable(K).

% S	ee slect_sorted_list	
:- pred select_unselect_sorted_list(hashmap(K, V)::in, list(K)::in,
    hashmap(K, V)::out, hashmap(K, V)::out) is det <= hashable(K).
	
	% Given a list of keys, produce a list of their corresponding
	% values in a specified map.
:- func apply_to_list(list(K), hashmap(K, V)) = list(V) <= hashable(K).
:- pred apply_to_list(list(K)::in, hashmap(K, V)::in, list(V)::out) is det
	<= hashable(K).
	
%---------------------------------------------------------------------------%
% Operations on two or more maps.


	% Note: Unlike Mercury standard map library, these calls perform the same,
	% regardless of the order of the  arguments

	% Merge the contents of the two maps.
	% Throws an exception if both sets of keys are not disjoint.

:- func merge(hashmap(K, V), hashmap(K, V)) = hashmap(K, V).
:- pred merge(hashmap(K, V)::in, hashmap(K, V)::in, hashmap(K, V)::out) is det.

	% For overlay(MapA, MapB, Map), if MapA and MapB both contain the
	% same key, then Map will map that key to the value from MapB.
	% In other words, MapB takes precedence over MapA.
:- func overlay(hashmap(K, V), hashmap(K, V)) = hashmap(K, V).
:- pred overlay(hashmap(K, V)::in, hashmap(K, V)::in, hashmap(K, V)::out)
	is det.
	
	% overlay_large_map is identical to overlay, it has been included for
	% compatability with the Mercury standard map, however it has also been
	% marked as obsolete, use overlay
:- func overlay_large_map(hashmap(K, V), hashmap(K, V)) = hashmap(K, V).
:- pred overlay_large_map(hashmap(K, V)::in, hashmap(K, V)::in, 
	hashmap(K, V)::out)  is det.
:- pragma obsolete(func(overlay_large_map/2), [overlay/2]).
:- pragma obsolete(pred(overlay_large_map/3), [overlay/3]).	
	
	% Given two maps MapA and MapB, create a third map CommonMap that
	% has only the keys that occur in both MapA and MapB. For keys
	% that occur in both MapA and MapB, look up the corresponding values.
	% If they are the same, include the key/value pair in CommonMap.
	% If they differ, do not include the key in CommonMap.
	%
	% There is no difference in performance cost based on the ordering and size
	% of the arguments
	%
	% common_subset is very similar to intersect, but can succeed
	% even with an output map that does not contain an entry for a key
	% value that occurs in both input maps.
:- func common_subset(hashmap(K, V), hashmap(K, V)) = hashmap(K, V).
:- pred common_subset(hashmap(K, V)::in, hashmap(K, V)::in, 
	hashmap(K, V)::out) is det.


	% Given two maps MapA and MapB, create a third map, IntersectMap,
	% that has only the keys that occur in both MapA and MapB. For keys
	% that occur in both MapA and MapB, compute the value in the final map
	% by applying the supplied function to the values associated with
	% the key in MapA and MapB.
	% on the values associated with some common key.
:- func intersect(func(V1, V2) = V3, hashmap(K, V1), hashmap(K, V2)) = 
	hashmap(K, V3).

	% Given two maps MapA and MapB, create a third map, IntersectMap,
	% that has only the keys that occur in both MapA and MapB. For keys
	% that occur in both MapA and MapB, compute the value in the final map
	% by applying the supplied predicate to the values associated with
	% the key in MapA and MapB. Fail if and only if this predicate fails
	% on the values associated with some common key.
:- pred intersect(pred(V1, V2, V3), hashmap(K, V1), hashmap(K, V2), 
	hashmap(K, V3)).
:- mode intersect(in(pred(in, in, out) is det), in, in, out) is det.
:- mode intersect(in(pred(in, in, out) is semidet), in, in, out) is semidet.

	% Calls intersect. Throws an exception if intersect fails.
:- func det_intersect((func(V1, V2) = V3)::in(func(in, in) = out is semidet),
    hashmap(K, V1)::in, hashmap(K, V2)::in) = (hashmap(K, V3)::out) is det.
:- pred det_intersect((pred(V1, V2, V3))::in(pred(in, in, out) is semidet),
    hashmap(K, V1)::in, hashmap(K, V2)::in, hashmap(K, V3)::out) is det.
	
	% intersect_list(Pred, HM, [M | Ms ], Result):
	% Recursively insersect HM with M and then recursively call the result with
	% Ms, folding over the entire list. If the list is empty, return HM. 
:- pred intersect_list(pred(V, V, V), hashmap(K, V), list(hashmap(K, V)), 
	hashmap(K, V)).
:- mode intersect_list(in(pred(in, in, out) is det), in, in, out) is det.
:- mode intersect_list(in(pred(in, in, out) is semidet), in, in, out) 
	is semidet.

	% intersect_list(Pred, List, Result): 
	% If List is empty, return an empty map, otherwise call the above 
	% intersect predicate with the first element and the rest of the list
	% and then recursively intersect the result with the rest of the list.
:- pred intersect_list(pred(V, V, V), list(hashmap(K, V)),hashmap(K, V)).
:- mode intersect_list(in(pred(in, in, out) is det), in, out) is det.
:- mode intersect_list(in(pred(in, in, out) is semidet), in, out) is semidet.

    % Given two maps MapA and MapB, create a third map, UnionMap, that
    % contains all the keys that occur in either MapA and MapB. For keys
    % that occur in both MapA and MapB, compute the value in the final map
    % by applying the supplied function to the values associated with the key
    % in MapA and MapB.
    %
:- func union(func(V, V) = V, hashmap(K, V), hashmap(K, V)) = hashmap(K, V).

    % Given two maps MapA and MapB, create a third map, UnionMap, that
    % contains all the keys that occur in either MapA and MapB. For keys
    % that occur in both MapA and MapB, compute the value in the final map
    % by applying the supplied predicate to the values associated with the key
    % in MapA and MapB. Fail if and only if this predicate fails on
    % the values associated with some common key.
    %
:- pred union(pred(V, V, V), hashmap(K, V), hashmap(K, V), hashmap(K, V)).
:- mode union(in(pred(in, in, out) is det), in, in, out) is det.
:- mode union(in(pred(in, in, out) is semidet), in, in, out) is semidet.

	% Calls union. Throws an exception if union fails.
:- func det_union((func(V, V) = V)::in(func(in, in) = out is semidet),
    hashmap(K, V)::in, hashmap(K, V)::in) = (hashmap(K, V)::out) is det.
:- pred det_union(pred(V, V, V)::in(pred(in, in, out) is semidet),
    hashmap(K, V)::in, hashmap(K, V)::in, hashmap(K, V)::out) is det.
	
	% union_list(Pred, HM, [M | Ms ], Result):
	% Recursively union HM with M and then recursively call the result with
	% Ms, folding over the entire list. 
:- pred union_list(pred(V, V, V), hashmap(K, V), list(hashmap(K, V)), 
	hashmap(K, V)).
:- mode union_list(in(pred(in, in, out) is det), in, in, out) is det.
:- mode union_list(in(pred(in, in, out) is semidet), in, in, out) is semidet.	
	

	% union_list(Pred, List, Result): 
	% If List is empty, return an empty map, otherwise call the above 
	% union predicate with the first element and the rest of the list
	% and then recursively union the result with the rest of the list.
:- pred union_list(pred(V, V, V), list(hashmap(K, V)),	hashmap(K, V)).
:- mode union_list(in(pred(in, in, out) is det), in, out) is det.
:- mode union_list(in(pred(in, in, out) is semidet), in, out) is semidet.

	% Take two maps and return the elements of the first map that do not have
	% equivalent keys in the second map. Take note that the value type
	% of the maps in question need not have the same type.
:- pred difference(hashmap(K, V)::in, hashmap(K, _)::in, hashmap(K, V)::out)
	is det.
	
% compose_maps(MapAB, MapBC, MapAC):
% Given each A - B pair in MapAB, return the map that pairs
% each such A with the C corresponding to its B in MapBC.
% Throw an exception if there is no value associated with B in MapBC.	
:- pred compose_maps(hashmap(A, B)::in, hashmap(B, C)::in, hashmap(A, C)::out) 
	is det <= (hashable(A), hashable(B)).
:- func compose_maps(hashmap(A, B), hashmap(B, C)) = hashmap(A, C) 
	<= (hashable(A), hashable(B)).




%-----------------------------------------------------------------------------%
% Bit twiddling

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

	% | Given a 'Hash' and a 'Shift' that indicates the level in the tree, 
	% compute the index into a 'Full' node or into the bitmap of a 
	% `BitmapIndexed` node.
	%
	% >>> index 0b0010_0010 0
	% 0b0000_0010

:- func index(hash, shift) = int.

:- pred index(hash::in, shift::in, int::out) is det.

	% Given a 'Hash' and a 'Shift' that indicates the level in the tree, 
	% compute the bitmap that contains only the 'index' of the hash at this 
	% level.

	% The result can be used for constructing one-element 'BitmapIndexed' 
	% nodes or to check whether a 'BitmapIndexed' node may possibly contain 
	% the 'Hash'.

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

	% If true, use the hacker's delight implementation for weight/1
:- func hackers_delight_weight = bool.

	% Loop impleementation of popcount
:- func weightn(bitmap) = int.

	% Hacker's Delight implementation only counting the 32 least signifigant bits
:- func weight32(bitmap) = int.

	% Hacker's Deligit implementation of the count trailing zeros operation
	% Only counts the trailing zeros in the 32 least signifigant bits
	% Returns 32 on empty bitmap
	% Throws an exception if the value is greater than 2^32
:- func ctz32(bitmap) = int.

%-----------------------------------------------------------------------------%
% Standard higher order functions on collections.

% Commented modes not supported by current array library higher order calls,
% custom implementation required.

% Perform a traversal by key of the map, applying an accumulator
% predicate for value. Order is arbitrary and cannot be garunteed.
:- func foldl(func(K, V, A) = A, hashmap(K, V), A) = A.
:- mode foldl(in(func(in, in, in) = out is det), in, in) = out is det.
:- mode foldl(in(func(in, in, in) = out is semidet), in, in) = out is semidet.

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
% :- import_module bool.
:- import_module require.
:- import_module string.
:- import_module pair.

:- import_module util.
:- import_module array_util.
:- import_module map_util.

%-----------------------------------------------------------------------------%
% Hash Array Mapped Table

:- type hashmap(K, V)
	--->	empty_tree
	;		leaf(hash, K, V)
	;		indexed_branch(bitmap, hash_array(K, V))
	;		full_branch(hash_array(K, V))
	;		collision(hash, bucket(K, V)).


:- type hash_array(K, V) == array(hashmap(K, V)).

:- type hash_list(K, V) == list(hashmap(K, V)).

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

:- pred is_hashmap_leaf_det(hashmap(_K, _V)::hashmap_leaf) is det.

is_hashmap_leaf_det(L) :-
	(if is_hashmap_leaf(L)
	then
		true
	else
		error($pred, "Could not coerce hashmap into leaf.")
	).
	
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


hash_singleton(H, K, V) = leaf(H, K, V).
hash_singleton(H, K, V, hash_singleton(H, K, V)).

singleton(K, V) = hash_singleton(hash(K), K, V).
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

hash_contains(Map, H, K) :- hash_search(Map, H, K, _).

contains(Map, K) :- hash_contains(Map, hash(K), K).



hash_search(HM, H, K, hash_search(HM, H, K)).

hash_search(HM, H, K) = search(HM, H, K, 0).

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

search(HM, K) = hash_search(HM, hash(K), K).

search(HM, K, search(HM, K)).


hash_lookup(HM, H, K, hash_lookup(HM, H, K)).

hash_lookup(HM, H, K) = 
	(if hash_search(HM, H, K) = Found
	then 
		Found
	else
		report_lookup_error("hashmap.lookup: key not found", K)
	).
	
lookup(HM, K, lookup(HM, K)).

lookup(HM, K) = hash_lookup(HM, hash(K), K).

inverse_search(_, _, _) :- sorry($module, $pred, "inverse_search").
lower_bound_search(_, _, _, _) :- sorry($module, $pred, "lower_bound_search").
lower_bound_lookup(_, _, _, _) :- sorry($module, $pred, "lower_bound_lookup").
upper_bound_search(_, _, _, _) :- sorry($module, $pred, "upper_bound_search").
upper_bound_lookup(_, _, _, _) :- sorry($module, $pred, "upper_bound_lookup").

%-----------------------------------------------------------------------------%
% Insertion

hash_insert(H, K, V, !HM) :- 
	insert_tree(H, K, V, 0, no, !HM).
	
hash_insert(!.HM, H, K, V) = !:HM :-
	hash_insert(H, K, V, !HM).
	
insert(K, V, !HM) :- hash_insert(hash(K), K, V, !HM).

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
		unsafe_insert(I, leaf(H, K, V), !Array), 
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

:- pragma inline(insert_tree/7).



det_hash_insert(H, K, V, !HM) :-
	( if hash_insert(H, K, V, !.HM, NewMap) then
        !:HM = NewMap
    else
        report_lookup_error("hashmap.det_insert: key already present", K, V)
    ).
	
:- pragma inline(det_hash_insert/5).
	
det_hash_insert(!.HM, H, K, V) = !:HM :- 
	det_hash_insert(H, K, V, !HM).
	
:- pragma inline(det_hash_insert/4).

det_insert(K, V, !HM) :- det_hash_insert(hash(K), K, V, !HM).	

:- pragma inline(det_insert/4).
	
det_insert(!.HM, K, V) = !:HM :- 
	det_insert(K, V, !HM).
	
:- pragma inline(det_insert/3).
	
det_hash_insert_from_corresponding_lists(M0, Hs, Ks, Vs) = M :-
    hashmap.det_hash_insert_from_corresponding_lists(Hs, Ks, Vs, M0, M).

det_hash_insert_from_corresponding_lists([], [], [], !Map).
det_hash_insert_from_corresponding_lists([], [], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
det_hash_insert_from_corresponding_lists([], [_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
det_hash_insert_from_corresponding_lists([_ | _], [], [], _, _) :-
    unexpected($pred, "list length mismatch").
det_hash_insert_from_corresponding_lists([_ | _], [_ | _], [],  _, _) :-
    unexpected($pred, "list length mismatch").
det_hash_insert_from_corresponding_lists([], [_ | _], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
det_hash_insert_from_corresponding_lists([_ | _], [], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
det_hash_insert_from_corresponding_lists([H | Hs], [K | Ks], [V | Vs], !Map) :-
    hashmap.det_hash_insert(H, K, V, !Map),
    hashmap.det_hash_insert_from_corresponding_lists(Hs, Ks, Vs, !Map).
	
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

% search_insert(H, K, V, MaybOldV, !HM)
hash_search_insert(H, K, V, MaybOldV, !HM) :-
	search_insert_tree(H, K, V, 0, MaybOldV, !HM).

search_insert(K, V, MaybOldV, !HM) :- 
	hash_search_insert(hash(K), K, V, MaybOldV, !HM).
	
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
		unsafe_insert(I, leaf(H, K, V), !Array), 
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
 

hash_set(H, K, V, !HM) :- 
	(if insert_tree(H, K, V, 0, yes, !HM)
	then
		!:HM = !.HM
	else
		unexpected($module, $pred, 
			"Failure on insert_tree/6 with Replace = yes")
	).
	
:- pragma inline(hash_set/5).	
	
hash_set(!.HM, H, K, V) = !:HM :-
	hash_set(H, K, V, !HM).
	
:- pragma inline(hash_set/4).

set(K, V, !HM) :- hash_set(hash(K), K, V, !HM).

:- pragma inline(set/4).
	
set(!.HM, K, V) = !:HM :-
	set(K, V, !HM).
	
:- pragma inline(set/3).

hash_set_from_corresponding_lists(M0, Hs, Ks, Vs) = M :-
    hashmap.hash_set_from_corresponding_lists(Hs, Ks, Vs, M0, M).

hash_set_from_corresponding_lists([], [], [], !Map).
hash_set_from_corresponding_lists([], [], [_ | _], _, _) :-
	unexpected($pred, "list length mismatch").
hash_set_from_corresponding_lists([], [_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
hash_set_from_corresponding_lists([_ | _], [], [], _, _) :-
    unexpected($pred, "list length mismatch").
hash_set_from_corresponding_lists([_ | _], [_ | _],[],  _, _) :-
    unexpected($pred, "list length mismatch").
hash_set_from_corresponding_lists([], [_ | _], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
hash_set_from_corresponding_lists([_ | _], [], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
hash_set_from_corresponding_lists([H | Hs], [K | Ks], [V | Vs], !Map) :-
    hashmap.hash_set(H, K, V, !Map),
    hashmap.hash_set_from_corresponding_lists(Hs, Ks, Vs, !Map).

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

	
hash_update(H, K, V, HM, hash_update(HM, H, K, V)).

hash_update(HM, H, K, V) = update(HM, H, K, V, 0).

update(K, V, HM, update(HM, K, V)).

update(HM, K, V) = hash_update(HM, hash(K), K, V).
	
det_hash_update(H, K, V, !HM) :-
	( if hash_update(H, K, V, !.HM, NewMap) then
        !:HM = NewMap
    else
        report_lookup_error("hashmap.det_update: key not found", K, V)
    ).
	
det_hash_update(!.HM, H, K, V) = !:HM :- 
	det_hash_update(H, K, V, !HM).

det_update(K, V, !HM) :- det_hash_update(hash(K), K, V, !HM).

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
:- func collision(hash, K, V, K, V) = hashmap(K, V).

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
	mask(H1, S, M1),
	mask(H2, S, M2),
	( if M1 = M2
	then
		array.init(1, two(next_shift(S), H1, K1, V1, L2), Array),
		Bitmap = M1
	else
		array.init(2, leaf(H1, K1, V1), Array0),
		Index = (index(H1, S) < index(H2, S) -> 1 ; 0),
		array.set(Index, coerce(L2), Array0, Array),
		Bitmap = M1 \/ M2		
	).

% two(Shift, Leaf1, Leaf2) = hashmap(K, V)
:- func two(shift, hashmap_leaf(K, V), hashmap_leaf(K, V)) = hashmap(K, V).	

two(S, L1@leaf(H1, _, _), L2@leaf(H2, _, _)) = indexed_branch(Bitmap, Array) :-
	mask(H1, S, M1),
	mask(H2, S, M2),
	( if M1 = M2
	then
		array.init(1, two(next_shift(S), L1, L2), Array),
		Bitmap = M1
	else
		array.init(2, coerce(L1), Array0),
		Index = (index(H1, S) < index(H2, S) -> 1 ; 0),
		array.set(Index, coerce(L2), Array0, Array),
		Bitmap = M1 \/ M2		
	).
	
:- pragma inline(two/5).

%-----------------------------------------------------------------------------%
% Removal



hash_remove(H, K, V, !HM) :- remove(H, K, 0, V, !HM).

remove(K, V, !HM) :- hash_remove(hash(K), K, V, !HM).

det_hash_remove(H, K, V, !HM) :- 
	( if hash_remove(H, K, Found, !.HM, NewMap) then
        V = Found,
		!:HM = NewMap
    else
        report_lookup_error("hashmap.det_remove: key not found", K)
    ).
	
det_remove(K, V, !HM) :- det_hash_remove(hash(K), K, V, !HM).
	

:- pred remove(hash::in, K::in, shift::in, V::out, hashmap(K, V)::in, 
	hashmap(K, V)::out)	is semidet.

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
		HM = remove_indexed_child(M, I, B, Array, Length)
	else if Length = 1, is_leaf_or_collision(Branch1)
	then
		HM = Branch1
	else
		HM = indexed_branch(B, array.slow_set(Array, I, Branch1))
	).
	
:- func remove_indexed_child(bitmap, int, bitmap, hash_array(K, V),
	int) = hashmap(K, V).

remove_indexed_child(Mask, Index, Bitmap, Array, Length) = HM :-
	(if Length = 1
	then
		HM = empty_tree
	else 
		(if 
			Length = 2,
			(
				Index = 0, 
				array.unsafe_lookup(Array, 1, Leaf)
			;
				Index = 1,
				array.unsafe_lookup(Array, 0, Leaf)
			), 
			is_leaf_or_collision(Leaf)
		then
			HM = Leaf
		else 
			unsafe_delete(Index, Array, NewArray),
			HM = indexed_branch(xor(Bitmap, Mask), NewArray)
		)
	).

:- pragma inline(remove_indexed_child/5).
	
remove(H, K, S, V, full_branch(Array), HM) :-
	index(H, S, I),
	array.unsafe_lookup(Array, I, Branch0),
	remove(H, K, next_shift(S), V, Branch0, Branch1),
	(if Branch1 = empty_tree
	then
		B = full_bitmap /\ (\ unchecked_left_shift(1u, I)),
		HM = indexed_branch(B, unsafe_delete(Array, I))
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

hash_delete(H, K, HM, hash_delete(HM, H, K)).
:- pragma inline(delete/4).

hash_delete(HM, H, K) = delete(HM, H, K, 0).
:- pragma inline(delete/3).

delete(K, HM, delete(HM, K)).
:- pragma inline(delete/3).

delete(HM, K) = hash_delete(HM, hash(K), K).
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
		(if private_builtin.pointer_equal(Branch1, Branch0)
		then
			!:HM = !.HM
		else 
			array.size(Array, Length),
			(if Branch1 = empty_tree 
			then
				!:HM = remove_indexed_child(M, I, B, Array, Length)
			else if Length = 1, is_leaf_or_collision(Branch1)
			then
				!:HM = Branch1
			else
				!:HM = indexed_branch(B, array.slow_set(Array, I, Branch1))
			)
		)
	).

delete(!.HM@full_branch(Array), H, K, S) = !:HM :-
	index(H, S, I),
	array.unsafe_lookup(Array, I, Branch0),
	Branch1 = delete(Branch0, H, K, next_shift(S)),
	(if private_builtin.pointer_equal(Branch1, Branch0)
	then
		!:HM = !.HM
	else if Branch1 = empty_tree
	then
		B = xor(full_bitmap, unchecked_left_shift(1u, I)),
		!:HM = indexed_branch(B, unsafe_delete(Array, I))
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

:- pragma promise_equivalent_clauses(hash_member/4).

hash_member(HM::in, H::in, K::in, V::out) :- hash_search(HM, H, K, V).


hash_member(leaf(H, K, V)::in, H::out, K::out, V::out).

hash_member(indexed_branch(H, Array)::in, H::out, K::out, V::out) :-
	array.member(Array, HM),
	hash_member(HM, H, K, V).
	
hash_member(full_branch(Array)::in, H::out, K::out, V::out) :-
	array.member(Array, HM),
	hash_member(HM, H, K, V).
	
hash_member(collision(H, Bucket)::in, H::out,  K::out, V::out) :- 
	map.member(Bucket, K, V).

	
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
	
max_key(HM) = K :- max_key(HM, K).
max_key(HM, K) :- foldl(max_key_acc, HM, no, yes(K)).

min_key(HM) = K :- min_key(HM, K).
min_key(HM, K) :- foldl(min_key_acc, HM, no, yes(K)).

det_max_key(HM) = K :-
	(if max_key(HM, K0)
	then
		K = K0
	else
		error($pred, "An empty hashmap has no maximum key")
	).
	
det_min_key(HM) = K :-
	(if min_key(HM, K0)
	then
		K = K0
	else
		error($pred, "An empty hashmap has no minimum key")
	).
	
	
:- pred max_key_acc(K::in, _V::in, maybe(K)::in, maybe(K)::out) is det.
max_key_acc(K, _, no, yes(K)).
max_key_acc(K, _, yes(Max), (K @> Max -> yes(K) ; yes(Max)) ). 
	
	
:- pred min_key_acc(K::in, _V::in, maybe(K)::in, maybe(K)::out) is det.
min_key_acc(K, _, no, yes(K)).
min_key_acc(K, _, yes(Min), (K @< Min -> yes(K) ; yes(Min)) ). 	


%-----------------------------------------------------------------------------%
% Operations on values.


hash_transform_value(P, H, K, !HM) :- transform_value_tree(P, H, K, 0, !HM).

transform_value(P, K, !HM) :- transform_value_tree(P, hash(K), K, 0, !HM).

:- pred transform_value_tree(pred(V, V)::in(pred(in, out) is det), hash::in,
	K::in, shift::in, hashmap(K, V)::in, hashmap(K, V)::out) is semidet. 

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

det_hash_transform_value(F, H, K, !.HM) = !:HM :-
    det_hash_transform_value(pred(V0::in, V::out) is det :- V = F(V0), H, K,
        !HM).
		
det_hash_transform_value(P, H, K, !HM) :-
    ( if hash_transform_value(P, H, K, !.HM, NewHM) then
        !:HM = NewHM
    else
        report_lookup_error("hashmap.det_hash_transform_value: key not found",
			K)
    ).


det_transform_value(F, K, !.HM) = !:HM :-
    det_transform_value(pred(V0::in, V::out) is det :- V = F(V0), K,
        !HM).

det_transform_value(P, K, !HM) :-
    ( if transform_value(P, K, !.HM, NewHM) then
        !:HM = NewHM
    else
        report_lookup_error("hashmap.det_transform_value: key not found", K)
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
% Selecting subsets of maps and lists.

select(!.HM, S) = !:HM :- select(!.HM, S, !:HM).

select(!.HM, S, !:HM) :- set.foldl(select_acc(!.HM), S, hashmap.init, !:HM).

:- pred select_acc(hashmap(K, V)::in, K::in, 
	hashmap(K, V)::in, hashmap(K, V)::out) is det <= hashable(K).
	
select_acc(Src, K, !HM) :-
	H = hash(K),
	!:HM =
		(if V = search(Src, H, K, 0)
		then
			(if insert_tree(H, K, V, 0, yes, !.HM, NewHM)
			then 
				NewHM
			else
				unexpected($module, $pred, "Insertion into new map failed."
					++ " This should not happen.")
			)
		else
			!.HM
		).
		
select_sorted_list(FullHM, Keys) = SelectHM :-
    select_sorted_list(FullHM, Keys, SelectHM).
	
select_sorted_list(FullHM, Keys, SelectHM) :-
	select(FullHM, set.from_sorted_list(Keys), SelectHM).
	
select_unselect(Src, Set, HM1, HM2) :- 
	Init = hashmap.init,
	foldl2(select_unselect_acc(Set), Src, Init, HM1, Init, HM2).
	
:- pred select_unselect_acc(set(K)::in, K::in, V::in, 
	hashmap(K, V)::in, hashmap(K, V)::out,
	hashmap(K, V)::in, hashmap(K, V)::out) is det <= hashable(K).
	
select_unselect_acc(S, K, V, !HM1, !HM2) :-
	(if contains(S, K) 
	then
		det_insert(K, V, !HM1),
		!:HM2 = !.HM2
	else
		det_insert(K, V, !HM2),
		!:HM1 = !.HM1	
	).
	
select_unselect_sorted_list(Src, Keys, HM1, HM2) :-
	select_unselect(Src, set.from_sorted_list(Keys), HM1, HM2).
	
apply_to_list(Ks, HM) = Vs :-
    apply_to_list(Ks, HM, Vs).

apply_to_list([], _, []).
apply_to_list([K | Ks], HM, [V | Vs]) :-
    lookup(HM, K, V),
    apply_to_list(Ks, HM, Vs).
	
%---------------------------------------------------------------------------%
% Operations on two or more maps.

merge(HM1, HM2) = HM :- merge(HM1, HM2, HM).

merge(HM1, HM2, HM) :- union(merge_pred, HM1, HM2, HM).

:- pred merge_pred(V::in, V::in, V::out) is det.
	
merge_pred(_V1, _V2, _V) :- 
		error("hashmap.merge/3: Attempted to merge non-disjoint " ++ 
			"hashmaps.").

:- pragma no_determinism_warning(merge_pred/3).
	
overlay(HM1, HM2) = HM :- overlay(HM1, HM2, HM).

overlay(HM1, HM2, HM) :- union(second, HM1, HM2, HM).

:- pred second(T::in, T::in, T::out) is det.

second(_, T, T).

overlay_large_map(HM1, HM2) = HM :- overlay_large_map(HM1, HM2, HM).

overlay_large_map(HM1, HM2, HM) :- overlay(HM1, HM2, HM).

%-----------------------------------------------------------------------------%
% Common Subset	
	
common_subset(HM1, HM2) = Sub :- common_subset(HM1, HM2, Sub).

common_subset(HM1, HM2, Sub) :- common_subset_tree(0, HM1, HM2, Sub).

:- pred common_subset_tree(shift::in, hashmap(K, V)::in, hashmap(K, V)::in,
	hashmap(K, V)::out) is det.
	
common_subset_tree(_S, empty_tree, empty_tree, empty_tree).

common_subset_tree(_S, empty_tree, leaf(_, _, _), empty_tree).
common_subset_tree(_S, leaf(_, _, _), empty_tree, empty_tree).

common_subset_tree(_S, empty_tree, indexed_branch(_, _), empty_tree). 
common_subset_tree(_S, indexed_branch(_, _), empty_tree, empty_tree).

common_subset_tree(_S, empty_tree, full_branch(_), empty_tree).
common_subset_tree(_S, full_branch(_), empty_tree, empty_tree).

common_subset_tree(_S, empty_tree, collision(_, _), empty_tree).
common_subset_tree(_S, collision(_, _), empty_tree, empty_tree).

common_subset_tree(_S, L@leaf(H1, K1, V1), leaf(H2, K2, V2), Int) :- 
	(if H1 = H2, K1 = K2, V1 = V2
	then
		Int = L
	else
		Int = empty_tree
	).

common_subset_tree(S, L@leaf(H, _K, _V), indexed_branch(B, Array), Int) 
:-
	mask(H, S, M),
	(if M /\ B = 0u
	then
		Int = empty_tree
	else
		sparse_index(B, M, I),
		array.unsafe_lookup(Array, I, Child),
		common_subset_tree(next_shift(S), L, Child, Int)
	).

common_subset_tree(S, L@leaf(H, _K, _V), full_branch(Array), Int) :-
	index(H, S, I),
	array.unsafe_lookup(Array, I, HMnext),
	common_subset_tree(next_shift(S), L, HMnext, Int).
	
common_subset_tree(_S, L@leaf(H1, K, V1), collision(H2, Bucket), Int) :-
	(if H1 = H2, map.search(Bucket, K, V2), V1 = V2
	then
		Int = L
	else
		Int = empty_tree
	).	
	
common_subset_tree(S, HM@indexed_branch(_, _), L@leaf(_, _, _), Int) :-
	common_subset_tree(S, L, HM, Int).
	
common_subset_tree(S, HM@full_branch(_), L@leaf(_, _, _), Int) :-
	common_subset_tree(S, L, HM, Int).
	
common_subset_tree(S, HM@collision(_, _), L@leaf(_, _, _), Int) :-
	common_subset_tree(S, L, HM, Int).

common_subset_tree(S, indexed_branch(B1, A1), indexed_branch(B2, A2), Int) :-
	common_subset_branches(S, B1, A1, B2, A2, Int).
	
common_subset_tree(S, indexed_branch(B1, A1), full_branch(A2), Int) :-
	common_subset_branches(S, B1, A1, full_bitmap, A2, Int).
	
common_subset_tree(S, full_branch(A1), indexed_branch(B2, A2), Int) :-
	common_subset_branches(S, full_bitmap, A1, B2, A2, Int).
	
common_subset_tree(S, full_branch(A1), full_branch(A2), Int) :-
	common_subset_branches(S, full_bitmap, A1, full_bitmap, A2, Int).

% common_subset_branches(Shift, Bitmap1, Array1, Bitmap2, Array2, Intersect).
:- pred common_subset_branches(shift::in, bitmap::in, hash_array(K, V)::in,
	bitmap::in, hash_array(K, V)::in, hashmap(K, V)::out) is det.

common_subset_branches(S, B1, A1, B2, A2, Int) :-
	B = B1 /\ B2,
	(if B = 0u
	then
		Int = empty_tree
	else
		NS = next_shift(S),
		Zeros = ctz32(B),
		NB = unchecked_right_shift(B, Zeros),
		M = unchecked_left_shift(1u, Zeros),
		common_subset_loop(NS, NB, B, IntB, M, B1, A1, B2, A2, [], L),
		(
			L = [],
			Int = empty_tree
		;
			L = [ C | Cs ],
			(if 
				Cs = [], 
				( C = leaf(_, _, _) ; C = collision(_, _) )
			then
				Int = C
			else
				array.from_reverse_list(L, IntArray),
				(if IntB = full_bitmap
				then
					Int = full_branch(IntArray)
				else
					Int = indexed_branch(IntB, IntArray)
				)
			)
		)
	).
	
:- pragma inline(common_subset_branches/6).

%common_subset_loop(Shift, CurrentBit, !IntersectingBitmap, Mask,
%	IndexBitmap1, Array1, 
%	IndexBitmap2, Array2, 
%	!RevList).
%common_subset_loop(S, CB, !IntB, M, B1, A1, B2, A2, !L).
:- pred common_subset_loop(shift::in, bitmap::in, bitmap::in, bitmap::out,
	bitmap::in,	bitmap::in, hash_array(K, V)::in, bitmap::in, 
	hash_array(K, V)::in, hash_list(K, V)::in, hash_list(K, V)::out) is det.
	
common_subset_loop(S, CB, !B, M, B1, Array1, B2, Array2, !L) :-
	sparse_index(B1, M, I1),
	sparse_index(B2, M, I2),
	array.unsafe_lookup(Array1, I1, Child1),
	array.unsafe_lookup(Array2, I2, Child2),
	common_subset_tree(S, Child1, Child2, ChildInt),
	(if ChildInt = empty_tree
	then
		!:L = !.L,
		!:B = xor(!.B, M) 
	else
		!:L = [ ChildInt | !.L ],
		!:B = !.B
	),
	NextBit = unchecked_right_shift(CB, 1),
	Zeros = unsafe_ctz32(NextBit),
	(if Zeros < 32
	then
		common_subset_loop(S, unchecked_right_shift(NextBit, Zeros), !B,
		unchecked_left_shift(M, 1 + Zeros), B1, Array1, B2, Array2, !L)
	else
		!:B = !.B,
		!:L = !.L
	).

common_subset_tree(_S, collision(H1, B1), collision(H2, B2), Int) :-
	(if H1 = H2
	then
		IntB = map.common_subset(B1, B2),
		(if map.is_empty(IntB)
		then
			Int = empty_tree
		else
			Int = collision(H1, IntB)
		)
	else
		Int = empty_tree
	).
	
common_subset_tree(S, C@collision(H, _), indexed_branch(B, Array), Int) 
:-
	mask(H, S, M),
	(if M /\ B = 0u
	then
		Int = empty_tree
	else
		sparse_index(B, M, I),
		array.unsafe_lookup(Array, I, Child),
		common_subset_tree(next_shift(S), C, Child, Int)
	).

common_subset_tree(S, C@collision(H, _), full_branch(Array), Int) :-
	index(H, S, I),
	array.unsafe_lookup(Array, I, HMnext),
	common_subset_tree(next_shift(S), C, HMnext, Int).
	
common_subset_tree(S, HM@indexed_branch(_, _), C@collision(_, _), Int) :-
	common_subset_tree(S, C, HM, Int).
	
common_subset_tree(S, HM@full_branch(_), C@collision(_, _), Int) :-
	common_subset_tree(S, C, HM, Int).

:- pragma inline(common_subset_tree/4).	
	
%-----------------------------------------------------------------------------%
% Intersection

intersect(F, HM1, HM2) = Int :-
    P = (pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    intersect(P, HM1, HM2, Int).
	
intersect(P, HM1, HM2, Int) :- 
	reverse(P, PR),
	intersect_tree(0, P, PR, HM1, HM2, Int).
	
	
:- pred reverse(pred(T1, T2, T3), pred(T2, T1, T3)).
:- mode reverse(in(pred(in, in, out) is det), out(pred(in, in, out) is det))
	is det.
:- mode reverse(in(pred(in, in, out) is semidet), out(pred(in, in, out) 
	is semidet)) is det.
	
:- pragma promise_equivalent_clauses(reverse/2).

	
reverse(P::in(pred(in, in, out) is det), PR::out(pred(in, in, out) is det))
:-
	PR = (pred(A::in, B::in, C::out) is det :- P(B, A, C)).
	
reverse(P::in(pred(in, in, out) is semidet), PR::out(pred(in, in, out) 
	is semidet))
:-
	PR = (pred(A::in, B::in, C::out) is semidet :- P(B, A, C)).
	
:- pragma inline(reverse/2).
	
% intersect_tree(Shift, Pred, PredReversed, Hashmap1, Hashmap2, Intersection)	
:- pred intersect_tree(shift, pred(V1, V2, V3), pred(V2, V1, V3), 
	hashmap(K, V1), hashmap(K, V2), hashmap(K, V3)).
:- mode intersect_tree(in, in(pred(in, in, out) is det), in(pred(in, in, out) 
	is det), in, in, out) is det.
:- mode intersect_tree(in, in(pred(in, in, out) is semidet), 
	in(pred(in, in, out) is semidet), in, in, out) is semidet.
	
intersect_tree(_S, _P, _PR, empty_tree, empty_tree, empty_tree).

intersect_tree(_S, _P, _PR, empty_tree, leaf(_, _, _), empty_tree).
intersect_tree(_S, _P, _PR, leaf(_, _, _), empty_tree, empty_tree).

intersect_tree(_S, _P, _PR, empty_tree, indexed_branch(_, _), empty_tree). 
intersect_tree(_S, _P, _PR, indexed_branch(_, _), empty_tree, empty_tree).

intersect_tree(_S, _P, _PR, empty_tree, full_branch(_), empty_tree).
intersect_tree(_S, _P, _PR, full_branch(_), empty_tree, empty_tree).

intersect_tree(_S, _P, _PR, empty_tree, collision(_, _), empty_tree).
intersect_tree(_S, _P, _PR, collision(_, _), empty_tree, empty_tree).

intersect_tree(_S, P, _PR, leaf(H1, K1, V1), leaf(H2, K2, V2), Int) :- 
	(if H1 = H2, K1 = K2
	then
		P(V1, V2, V),
		Int = leaf(H1, K1, V)
	else
		Int = empty_tree
	).

intersect_tree(S, P, PR, L@leaf(H, _K, _V), indexed_branch(B, Array), Int) 
:-
	mask(H, S, M),
	(if M /\ B = 0u
	then
		Int = empty_tree
	else
		sparse_index(B, M, I),
		array.unsafe_lookup(Array, I, Child),
		intersect_tree(next_shift(S), P, PR, L, Child, Int)
	).

intersect_tree(S, P, PR, L@leaf(H, _K, _V), full_branch(Array), Int) :-
	index(H, S, I),
	array.unsafe_lookup(Array, I, HMnext),
	intersect_tree(next_shift(S), P, PR, L, HMnext, Int).
	
intersect_tree(_S, P, _PR, leaf(H1, K, V1), collision(H2, Bucket), Int) :-
	(if H1 = H2, map.search(Bucket, K, V2)
	then
		P(V1, V2, V),
		Int = leaf(H1, K, V)
	else
		Int = empty_tree
	).	
	
intersect_tree(S, P, PR, HM@indexed_branch(_, _), L@leaf(_, _, _), Int) :-
	intersect_tree(S, PR, P, L, HM, Int).
	
intersect_tree(S, P, PR, HM@full_branch(_), L@leaf(_, _, _), Int) :-
	intersect_tree(S, PR, P, L, HM, Int).
	
intersect_tree(S, P, PR, HM@collision(_, _), L@leaf(_, _, _), Int) :-
	intersect_tree(S, PR, P, L, HM, Int).

intersect_tree(S, P, PR, indexed_branch(B1, A1), indexed_branch(B2, A2), Int) :-
	intersect_branches(S, P, PR, B1, A1, B2, A2, Int).
	
intersect_tree(S, P, PR, indexed_branch(B1, A1), full_branch(A2), Int) :-
	intersect_branches(S, P, PR, B1, A1, full_bitmap, A2, Int).
	
intersect_tree(S, P, PR, full_branch(A1), indexed_branch(B2, A2), Int) :-
	intersect_branches(S, P, PR, full_bitmap, A1, B2, A2, Int).
	
intersect_tree(S, P, PR, full_branch(A1), full_branch(A2), Int) :-
	intersect_branches(S, P, PR, full_bitmap, A1, full_bitmap, A2, Int).

% intersect_branches(Shift, Pred, PReversed, Bitmap1, Array1, Bitmap2, Array2,
%	Intersect).
:- pred intersect_branches(shift, pred(V1, V2, V3), pred(V2, V1, V3), bitmap, 
	hash_array(K, V1), bitmap, hash_array(K, V2), hashmap(K, V3)).
:- mode intersect_branches(in, in(pred(in, in, out) is det), 
	in(pred(in, in, out) is det), in, in, in, in, out) is det.
:- mode intersect_branches(in, in(pred(in, in, out) is semidet),
	in(pred(in, in, out) is semidet), in, in, in, in, out) is semidet.

intersect_branches(S, P, PR, B1, A1, B2, A2, Int) :-
	B = B1 /\ B2,
	(if B = 0u
	then
		Int = empty_tree
	else
		NS = next_shift(S),
		%Select the first bit mask by counting the number of trailing zeros
		FirstBit = unchecked_left_shift(1u, ctz32(B)),
		intersect_loop(NS, P, PR, FirstBit, B1, A1, B2, A2, B, IntB, [], L),
		(
			L = [],
			Int = empty_tree
		;
			L = [ C | Cs ],
			(if 
				Cs = [], 
				( C = leaf(_, _, _) ; C = collision(_, _) )
			then
				Int = C
			else
				array.from_reverse_list(L, IntArray),
				(if IntB = full_bitmap
				then
					Int = full_branch(IntArray)
				else
					Int = indexed_branch(IntB, IntArray)
				)
			)
		)
	).
	
:- pragma inline(intersect_branches/8).

%intersect_loop(Shift, Pred, PReversed, CurrentBit,
%	IndexBitmap1, Array1, 
%	IndexBitmap2, Array2, 
%	!IntersectingBitmap, !RevList).
:- pred intersect_loop(shift, pred(V1, V2, V3), pred(V2, V1, V3), bitmap, 
	bitmap,	hash_array(K, V1), 
	bitmap, hash_array(K, V2),
	bitmap, bitmap, hash_list(K, V3), hash_list(K, V3)).
:- mode intersect_loop(in, in(pred(in, in, out) is det), 
	in(pred(in, in, out) is det), in, in, in, in, in, in, out, in, out) 
	is det.
:- mode intersect_loop(in, in(pred(in, in, out) is semidet), 
	in(pred(in, in, out) is semidet), in, in, in, in, in, in, out, in, out)
	is semidet.
	
intersect_loop(S, P, PR, CB, B1, Array1, B2, Array2, !B, !L) :-
	% This trace goal is a sanity check, CB /\ !.B should always be >0
	trace [ compile_time(grade(debug) or flag("check_intersect_loop")) ] (
		(if CB /\ !.B = 0u
		then unexpected($pred, "invalid intersection index")
		else true)
	),
	sparse_index(B1, CB, I1),
	sparse_index(B2, CB, I2),
	array.unsafe_lookup(Array1, I1, Child1),
	array.unsafe_lookup(Array2, I2, Child2),
	intersect_tree(S, P, PR, Child1, Child2, ChildInt),
	(if ChildInt = empty_tree
	then
		!:L = !.L,
		!:B = xor(!.B, CB) % Remove the one bit in the bitmap at current bit
	else
		!:L = [ ChildInt | !.L ]
	),
	% exclude all one bits from the bitmap up to and including the current bit
	NextBitMask = \ (CB * 2u - 1u),
	(if unsafe_ctz32(!.B /\ NextBitMask)@Zeros < 32
	then
		%Select the next bit by masking the bitmap with NOT (CB * 2 -1) and
		%counting the zeros to the next one bit
		NextBit = unchecked_left_shift(1u, Zeros),
		intersect_loop(S, P, PR, NextBit, B1, Array1, B2, Array2, !B, !L)
	else
		!:B = !.B,
		!:L = !.L
	).


intersect_tree(_S, P, _PR, collision(H1, B1), collision(H2, B2), Int) :-
	(if H1 = H2
	then
		pred_intersect(P, B1, B2, IntB),
		(if map.is_empty(IntB)
		then
			Int = empty_tree
		else
			Int = collision(H1, IntB)
		)
	else
		Int = empty_tree
	).
	
intersect_tree(S, P, PR, C@collision(H, _), indexed_branch(B, Array), Int) 
:-
	mask(H, S, M),
	(if M /\ B = 0u
	then
		Int = empty_tree
	else
		sparse_index(B, M, I),
		array.unsafe_lookup(Array, I, Child),
		intersect_tree(next_shift(S), P, PR, C, Child, Int)
	).

intersect_tree(S, P, PR, C@collision(H, _), full_branch(Array), Int) :-
	index(H, S, I),
	array.unsafe_lookup(Array, I, HMnext),
	intersect_tree(next_shift(S), P, PR, C, HMnext, Int).
	
intersect_tree(S, P, PR, HM@indexed_branch(_, _), C@collision(_, _), Int) :-
	intersect_tree(S, PR, P, C, HM, Int).
	
intersect_tree(S, P, PR, HM@full_branch(_), C@collision(_, _), Int) :-
	intersect_tree(S, PR, P, C, HM, Int).

:- pragma inline(intersect_tree/6).	

det_intersect(PF, HM1, HM2) = Int :-
    P = (pred(X::in, Y::in, Z::out) is semidet :- Z = PF(X, Y) ),
    det_intersect(P, HM1, HM2, Int).

det_intersect(P, HM1, HM2, Int) :-
    ( if intersect(P, HM1, HM2, Int0) then
        Int = Int0
    else
        unexpected($pred, "hashmap.intersect failed")
    ).
	
intersect_list(_P, HM, [], HM).

intersect_list(P, HM, [ M | Ms ], Res) :- 
	intersect(P, HM, M, Int),
	(if Int = empty_tree
	then Res = Int
	else intersect_list(P, Int, Ms, Res)
	).
	
intersect_list(_P, [], empty_tree).

intersect_list(P, [HM | HMs], Res) :- intersect_list(P, HM, HMs, Res).
	
%-----------------------------------------------------------------------------%
% Union

union(F, HM1, HM2) = HM :-
    P = (pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    union(P, HM1, HM2, HM).
	
union(P, HM1, HM2, HM) :- 
	reverse(P, PR),
	union_tree(0, P, PR, HM1, HM2, HM).

% union_tree(Shift, Pred, PredReversed, Hashmap1, Hashmap2, Union).
:- pred union_tree(shift, pred(V, V, V), pred(V, V, V), hashmap(K, V), 
	hashmap(K, V), hashmap(K, V)).
:- mode union_tree(in, in(pred(in, in, out) is det), 
	in(pred(in, in, out) is det), in, in, out) is det.
:- mode union_tree(in, in(pred(in, in, out) is semidet), 
	 in(pred(in, in, out) is semidet), in, in, out) is semidet.

union_tree(_S, _P, _PR, empty_tree, empty_tree, empty_tree).

union_tree(_S, _P, _PR, empty_tree, L@leaf(_, _, _), L).
union_tree(_S, _P, _PR, L@leaf(_, _, _), empty_tree, L).

union_tree(_S, _P, _PR, empty_tree, B@indexed_branch(_, _), B). 
union_tree(_S, _P, _PR, B@indexed_branch(_, _), empty_tree, B).

union_tree(_S, _P, _PR, empty_tree, B@full_branch(_), B).
union_tree(_S, _P, _PR, B@full_branch(_), empty_tree, B).

union_tree(_S, _P, _PR, empty_tree, C@collision(_, _), C).
union_tree(_S, _P, _PR, C@collision(_, _), empty_tree, C).

union_tree(S, P, _PR, HM1@leaf(H1, K1, V1), HM2@leaf(H2, K2, V2), Union) :-
	(if H1 = H2
	then
		(if K1 = K2
		then
			P(V1, V2, V),
			Union = leaf(H1, K1, V)
		else
			Union = collision(H1, K1, V1, K2, V2)
		)
	else
		is_hashmap_leaf_det(HM1),
		is_hashmap_leaf_det(HM2),
		coerce(HM1) = L1:hashmap_leaf(K, V),
		coerce(HM2) = L2:hashmap_leaf(K, V),
		Union = two(S, L1, L2)
	).


union_tree(S, P, PR, L@leaf(H, _K, _V), !.HM@indexed_branch(B, BArray), !:HM) 
:-	some [!Array] (
	!:Array = BArray,
	mask(H, S, M),
	sparse_index(B, M, I),
	(if M /\ B = 0u
	then
		unsafe_insert(I, L, !Array), 
		!:HM = indexed_or_full_branch(B \/ M, !.Array)
	else
		array.unsafe_lookup(!.Array, I, Branch0),
		union_tree(next_shift(S), P, PR, L, Branch0, Branch1),
		(if private_builtin.pointer_equal(Branch1, Branch0)
		then
			!:HM = !.HM
		else
			slow_set(I, Branch1, !Array),
			!:HM = indexed_branch(B, !.Array)
		)
	)
).

union_tree(S, P, PR, B@indexed_branch(_, _),  L@leaf(_, _, _), Int) :-
	union_tree(S, PR, P, L, B, Int).

	
union_tree(S, P, PR, L@leaf(H, _K, _V), !.HM@full_branch(FArray), !:HM) 
:- some [!Array] (
	!:Array = FArray,
	index(H, S, I),
	array.unsafe_lookup(!.Array, I, Branch0),
	union_tree(next_shift(S), P, PR, L, Branch0, Branch1),
	(if private_builtin.pointer_equal(Branch1, Branch0)
	then
		true % !:HM = !.HM
	else
		slow_set(I, Branch1, !Array),
		!:HM = full_branch(!.Array)
	)
).
	
union_tree(S, P, PR, B@full_branch(_),  L@leaf(_, _, _), Union) :-
	union_tree(S, PR, P, L, B, Union).

union_tree(S, P, PR, L@leaf(H1, K1, V1), !.HM@collision(H2, Bucket), !:HM) :-
	(if H1 = H2
	then
		(if map.search(Bucket, K1, V2)
		then
			P(V1, V2, V),
			(if private_builtin.pointer_equal(V, V2)
			then
				!:HM = !.HM
			else
				map.det_update(K1, V, Bucket, NewBucket),
				!:HM = collision(H1, NewBucket)
			)
		else
			map.det_insert(K1, V1, Bucket, NewBucket),
			!:HM = collision(H1, NewBucket)
		)
	else
		array.init(1, !.HM, BArray),
		union_tree(next_shift(S), P, PR, L, indexed_branch(mask(H2, S),	
			BArray), !:HM)
	).
	
union_tree(S, P, PR, C@collision(_, _),  L@leaf(_, _, _), Int) :-
	union_tree(S, PR, P, L, C, Int).
	
union_tree(S, P, PR, HM1@indexed_branch(B1, A1), HM2@indexed_branch(B2, A2),
	Union) :- union_branches(S, P, PR, HM1, B1, A1, HM2, B2, A2, Union).
	
union_tree(S, P, PR, HM1@indexed_branch(B1, A1), HM2@full_branch(A2), Union) 
:- 
	union_branches(S, P, PR, HM1, B1, A1, HM2, full_bitmap, A2,	Union).
	
union_tree(S, P, PR, HM1@full_branch(A1), HM2@indexed_branch(B2, A2),
	Union) 
:- 
	union_branches(S, P, PR, HM1, full_bitmap, A1, HM2, B2, A2, Union).
	
union_tree(S, P, PR, HM1@full_branch(A1), HM2@full_branch(A2), Union) :- 
	union_branches(S, P, PR, HM1, full_bitmap, A1, HM2, full_bitmap, A2,
		Union).
	
	
% union_branches(Shift, Pred, PredRev, 
%	HM1, Bitmap1, Array1, 
%	HM2, Bitmap2, Array2,
% 	Union)
:- pred union_branches(shift, pred(V, V, V), pred(V, V, V), 
	hashmap(K, V), bitmap, hash_array(K, V),
	hashmap(K, V), bitmap, hash_array(K, V),
	hashmap(K, V)).
:- mode union_branches(in, in(pred(in, in, out) is det), 
	in(pred(in, in, out) is det),
	in, in, in,
	in, in, in,
	out) is det.
:- mode union_branches(in, in(pred(in, in, out) is semidet), 
	in(pred(in, in, out) is semidet),
	in, in, in,
	in, in, in,
	out) is semidet.
	
union_branches(S, P, PR, HM1, B1, A1, HM2, B2, A2, Union) :-
	B = B1 \/ B2,
	NS = next_shift(S),
	some [!A, Size] (
		(if B = full_bitmap
		then
			
			% Get the union index at the zero index
			union_index(NS, P, PR, 1u, B1, A1, B2, A2, FirstElem, Match0),
			array.init(32@Size, FirstElem, !:A),
			union_full_loop(NS, P, PR, 1, B1, A1, B2, A2, !A, Match0, Match)
		else
		
			%Select the first bit mask by counting the number of trailing zeros
			FirstBit = unchecked_left_shift(1u, ctz32(B)),
			union_index(NS, P, PR, FirstBit, B1, A1, B2, A2, FirstElem,
				Match0),			
			array.init(weight(B)@Size, FirstElem, !:A),
			(if Size > 1
			then
				
			% Find the next bit by masking out all of the bits up to and 
			% including the first bit and counting the zeros to the next bit
			NextBitZeros = unsafe_ctz32(B /\ \ (FirstBit * 2u - 1u)),
			%NextBitZeros = unsafe_ctz32(xor(B, FirstBit * 2u - 1u)),
			NextBit = unchecked_left_shift(1u, NextBitZeros),
			union_index_loop(NS, P, PR, NextBit, B, B1, A1, B2, A2, !A, 
				Match0, Match)
			else
				Match = Match0 %, !:A = !.A
			)
		),
		(
			Match = neither,
			Union = indexed_or_full_branch(B, !.A)
		;
			Match = one,
			Union = HM1
		;
			Match = two,
			Union = HM2
		)
	).


% union_full_loop(Shift, Pred, PReversed, Index, 
%	IndexBitmap1, Array1, 
%	IndexBitmap2, Array2, 
%	!NewArray, !Match).
% union_full_loop(S, P, PR, I, B1, A1, B2, A2, !A, !Match).
:- pred union_full_loop(shift, pred(V, V, V), pred(V, V, V), int, 
	bitmap, hash_array(K, V), 
	bitmap, hash_array(K, V),
	hash_array(K, V), hash_array(K, V), array_match, array_match).
:- mode union_full_loop(in, in(pred(in, in, out) is det), 
	in(pred(in, in, out) is det), in,
	in, in, 
	in, in, 
	array_di, array_uo, in, out) is det.
:- mode union_full_loop(in, in(pred(in, in, out) is semidet), 
	in(pred(in, in, out) is semidet), in,
	in, in, 
	in, in, 
	array_di, array_uo, in, out) is semidet.
	
union_full_loop(S, P, PR, I, B1, Array1, B2, Array2, !A, !M) :-
	CurrentBit = unchecked_left_shift(1u, I),
	union_index(S, P, PR, CurrentBit, B1, Array1, B2, Array2, Child, IndexM),
	!:M = 
		(if !.M = neither 
		then 
			neither
		else if !.M = IndexM
		then
			!.M
		else
			neither
		),
	array.set(I, Child, !A),
	(if I < 31
	then
		union_full_loop(S, P, PR, I + 1, B1, Array1, B2, Array2, !A, !M)
	else
		%!:A = !.A
		true
	).



% union_index_loop(Shift, Pred, PReversed, CurrentBit, UnionBitmap,
%	IndexBitmap1, Array1, 
%	IndexBitmap2, Array2, 
%	!NewArray, !Match).
% union_index_loop(S, P, PR, CB, B, B1, A1, B2, A2, !A, !Match).
:- pred union_index_loop(shift, pred(V, V, V), pred(V, V, V), mask, bitmap,
	bitmap,	hash_array(K, V), 
	bitmap, hash_array(K, V),
	hash_array(K, V), hash_array(K, V), array_match, array_match).
:- mode union_index_loop(in, in(pred(in, in, out) is det), 
	in(pred(in, in, out) is det), in, in,
	in, in,
	in, in, 
	array_di, array_uo, in, out) is det.
:- mode union_index_loop(in, in(pred(in, in, out) is semidet), 
	in(pred(in, in, out) is semidet), in, in, 
	in, in,
	in, in, 
	array_di, array_uo, in, out) is semidet.
	
union_index_loop(S, P, PR, CB, UB, B1, Array1, B2, Array2, !A, !M) :-
	union_index(S, P, PR, CB, B1, Array1, B2, Array2, Child, IndexedM),
	!:M = 
		(if !.M = neither 
		then 
			neither
		else if !.M = IndexedM
		then
			!.M
		else
			neither
		),
	sparse_index(UB, CB, I),
	array.set(I, Child, !A),
	% exclude all one bits from the bitmap up to and including the current bit
	NextBitMask = \ (CB * 2u - 1u),
	(if unsafe_ctz32(UB /\ NextBitMask)@Zeros < 32
	then
		%Select the next bit by masking the bitmap with NOT (CB * 2 -1) and
		%counting the zeros to the next one bit
		NextBit = unchecked_left_shift(1u, Zeros),
		union_index_loop(S, P, PR, NextBit, UB, B1, Array1, B2, Array2, !A, !M)
	else
		%!:A = !.A
		true
	).


:- type array_match 
	--->	one
	;		two
	;		neither.
	

% union_index(Shift, Pred, PredRev, CurrentBit, 
%	Bitmap1, Array1, 
%	Bitmap2, Array2,
% 	Union, Match) Get the union of two elements in seperate bitmapped arrays
:- pred union_index(shift, pred(V, V, V), pred(V, V, V), mask,
	bitmap,	hash_array(K, V), 
	bitmap, hash_array(K, V), 
	hashmap(K, V),	array_match).
:- mode union_index(in, in(pred(in, in, out) is det), 
	in(pred(in, in, out) is det), in, 
	in, in, 
	in, in, 
	out, out) is det.
:- mode union_index(in, in(pred(in, in, out) is semidet), 
	in(pred(in, in, out) is semidet), in, in, in, in, in, out, out) is semidet.

union_index(S, P, PR, CB, B1, A1, B2, A2, Union, Match) :-
	(if B1 /\ CB = 0u
	then
		sparse_index(B2, CB, I),
		Match = two,
		array.unsafe_lookup(A2, I, Union)
	else 
		sparse_index(B1, CB, I1),
		array.unsafe_lookup(A1, I1, Child1),
		(if B2 /\ CB = 0u
		then
			Union = Child1,
			Match = one
		else
			sparse_index(B2, CB, I2),
			array.unsafe_lookup(A2, I2, Child2),
			union_tree(S, P, PR, Child1, Child2, Union),
			(if private_builtin.pointer_equal(Child1, Union) 
			then
				Match = one
			else if private_builtin.pointer_equal(Child2, Union)
			then 
				Match = two
			else
				Match = neither
			)
		)
	).
	
:- pragma inline(union_index/10).


union_tree(S, P, PR, C@collision(H, _Bucket), !.HM@indexed_branch(B, BArray), 
	!:HM) 
:-	some [!Array] (
	!:Array = BArray,
	mask(H, S, M),
	sparse_index(B, M, I),
	(if M /\ B = 0u
	then
		unsafe_insert(I, C, !Array), 
		!:HM = indexed_or_full_branch(B \/ M, !.Array)
	else
		array.unsafe_lookup(!.Array, I, Branch0),
		union_tree(next_shift(S), P, PR, C, Branch0, Branch1),
		(if private_builtin.pointer_equal(Branch1, Branch0)
		then
			!:HM = !.HM
		else
			slow_set(I, Branch1, !Array),
			!:HM = indexed_branch(B, !.Array)
		)
	)
).

union_tree(S, P, PR, B@indexed_branch(_, _),  C@collision(_, _), Union) :-
	union_tree(S, PR, P, C, B, Union).

	
union_tree(S, P, PR, C@collision(H, _Bucket), !.HM@full_branch(FArray), !:HM) 
:- some [!Array] (
	!:Array = FArray,
	index(H, S, I),
	array.unsafe_lookup(!.Array, I, Branch0),
	union_tree(next_shift(S), P, PR, C, Branch0, Branch1),
	(if private_builtin.pointer_equal(Branch1, Branch0)
	then
		!:HM = !.HM
	else
		slow_set(I, Branch1, !Array),
		!:HM = full_branch(!.Array)
	)
).
	
union_tree(S, P, PR, B@full_branch(_),  C@collision(_, _), Union) :-
	union_tree(S, PR, P, C, B, Union).

union_tree(S, P, PR, C1@collision(H1, Bucket1), C2@collision(H2, Bucket2), 
	Union) 
:-
	(if H1 = H2
	then
		map.union(P, Bucket1, Bucket2, Bucket),
		(if private_builtin.pointer_equal(Bucket, Bucket1)
		then
			Union = C1
		else if private_builtin.pointer_equal(Bucket, Bucket2)
		then
			Union = C2
		else
			Union = collision(H1, Bucket)
		)
	else
		array.init(1, C2, BArray),
		union_tree(next_shift(S), P, PR, C1, indexed_branch(mask(H2, S),	
			BArray), Union)
	).


det_union(PF, HM1, HM2) = Union :-
    P = (pred(X::in, Y::in, Z::out) is semidet :- Z = PF(X, Y) ),
    det_union(P, HM1, HM2, Union).

det_union(P, HM1, HM2, Union) :-
    ( if union(P, HM1, HM2, Union0) then
        Union = Union0
    else
        unexpected($pred, "hashmap.union failed")
	).

	
union_list(_P, HM, [], HM).

union_list(P, HM, [ M | Ms ], Res) :- 
	union(P, HM, M, Union),
	union_list(P, Union, Ms, Res).
	
union_list(_P, [], empty_tree).

union_list(P, [HM | HMs], Res) :- union_list(P, HM, HMs, Res).

%-----------------------------------------------------------------------------%
% Difference

difference(A, B, C) :- difference_tree(0, A, B, C).

:- pred difference_tree(shift::in, hashmap(K, V)::in, hashmap(K, _)::in,
	hashmap(K, V)::out)	is det.
	
difference_tree(_, empty_tree, empty_tree, empty_tree).
difference_tree(_, L@leaf(_, _, _), empty_tree, L).
difference_tree(_, empty_tree, leaf(_, _, _), empty_tree).
difference_tree(_, B@indexed_branch(_, _), empty_tree, B).
difference_tree(_, empty_tree, indexed_branch(_, _), empty_tree).
difference_tree(_, F@full_branch(_), empty_tree, F).
difference_tree(_, empty_tree, full_branch(_), empty_tree).
difference_tree(_, C@collision(_, _), empty_tree, C).
difference_tree(_, empty_tree, collision(_, _), empty_tree).

difference_tree(_, L@leaf(H1, K1, _), leaf(H2, K2, _), Diff) :-
	(if H1 = H2, K1 = K2
	then
		Diff = empty_tree
	else
		Diff = L
	).
	
difference_tree(S, L@leaf(H, _, _), indexed_branch(B, A), Diff) :-
	mask(H, S, M),
	(if M /\ B = 0u
	then
		Diff = L
	else
		sparse_index(B, M, I),
		array.unsafe_lookup(A, I, Child),
		difference_tree(next_shift(S), L, Child, Diff)
	).
	
difference_tree(S, HM@indexed_branch(B, A), L@leaf(H, K, _), Diff) :-
	mask(H, S, M),
	(if M /\ B = 0u
	then
		Diff = HM
	else
		sparse_index(B, M, I),
		array.unsafe_lookup(A, I, Child0),
		difference_tree(next_shift(S), Child0, L, Child),
		(if private_builtin.pointer_equal(Child0, Child)
		then
			Diff = HM
		else if Child = empty_tree
		then
			Diff = delete(HM, H, K, S)
		else
			array.slow_set(I, Child, A, NewA),
			Diff = indexed_branch(B, NewA)
		)
	).
	
difference_tree(S, L@leaf(H, _, _), full_branch(A), Diff) :-
	index(H, S, I),
	array.unsafe_lookup(A, I, Child),
	difference_tree(next_shift(S), L, Child, Diff).
	
difference_tree(S, F@full_branch(A), L@leaf(H, K, _), Diff) :-
	index(H, S, I),
	array.unsafe_lookup(A, I, Child0),
	difference_tree(next_shift(S), Child0, L, Child),
	(if private_builtin.pointer_equal(Child0, Child)
	then
		Diff = F
	else if Child = empty_tree
	then
		Diff = delete(F, H, K, S)
	else
		array.slow_set(I, Child, A, NewA),
		Diff = full_branch(NewA)
	).
	
difference_tree(_, L@leaf(H1, K, _), collision(H2, Bucket), Diff) :-
	(if H1 = H2, map.contains(Bucket, K)
	then
		Diff = empty_tree
	else
		Diff = L
	).
	
difference_tree(_, C@collision(CH, Bucket), leaf(H, K, _), Diff) :-
	(if CH = H, map.contains(Bucket, K)
	then
		map.delete(K, Bucket, NewBucket),
		Diff = 
			(if map.to_assoc_list(NewBucket, [(NK - NV)])
			then
				leaf(H, NK, NV)
			else
				collision(CH, NewBucket)
			)
	else
		Diff = C
	).
	
difference_tree(S, M@indexed_branch(B1, A1), indexed_branch(B2, A2), Diff) :-
	difference_branches(S, M, B1, A1, B2, A2, Diff).
	
difference_tree(S, M@indexed_branch(B1, A1), full_branch(A2), Diff) :-
	difference_branches(S, M, B1, A1, full_bitmap, A2, Diff).

difference_tree(S, M@full_branch(A1), indexed_branch(B2, A2), Diff) :-
	difference_branches(S, M, full_bitmap, A1, B2, A2, Diff).	

difference_tree(S, M@full_branch(A1), full_branch(A2), Diff) :-
	difference_branches(S, M, full_bitmap, A1, full_bitmap, A2, Diff).	

% difference_branches(Shift, Map1, 
%	Bitmap1, Array1
%	Bitmap2, Array2,
%	Difference)
:- pred difference_branches(shift::in, hashmap(K, V)::in,
	bitmap::in, hash_array(K, V)::in,
	bitmap::in, hash_array(K, _)::in,
	hashmap(K, V)::out) is det.
	
difference_branches(S, Map1, B1, A1, B2, A2, Diff) :-
	(if B1 /\ B2 = 0u
	then
		Diff = Map1
	else
		FirstBit = unchecked_left_shift(1u, ctz32(B1)),
		difference_loop(next_shift(S), FirstBit, B1, A1, B2, A2, B1, NewB, 
			[], L, yes, Match),
		(if Match = yes
		then 
			Diff = Map1 
		else
			array.from_reverse_list(L, NewArray),
			Diff = indexed_or_full_branch(NewB, NewArray)
		)
	).
	
:- pragma inline(difference_branches/7).

% difference_loop(S, CurrentBit,
%	Bitmap1, Array1, Bitmap2, Array2, 
%	!NewBitmap,
%	!ReverseList,
%	!MatchesFirst)
:- pred difference_loop(shift::in, mask::in, 
	bitmap::in, hash_array(K, V)::in, bitmap::in, hash_array(K, _)::in,
	bitmap::in, bitmap::out, 
	list(hashmap(K, V))::in, list(hashmap(K, V))::out,
	bool::in, bool::out) is det.

% Loop starts:
% difference_loop(next_shift(S), ctz32(B1), B1, A1, B2, A2, 
%	B1, B, [], L, yes, M)

difference_loop(S, CB, B1, A1, B2, A2, !B, !L, !M) :-
	sparse_index(B1, CB, I1),
	array.unsafe_lookup(A1, I1, Child1),
	(if B2 /\ CB = 0u
	then
		!:L = [ Child1 | !.L ]
	else		
		% If the arrays share the same bitmap, then their indexes are the same
		(if B1 = B2
		then
			I2 = I1
		else
			sparse_index(B2, CB, I2)
		),
		array.unsafe_lookup(A2, I2, Child2),
		difference_tree(S, Child1, Child2, NewChild),
		(if NewChild = empty_tree
		then
			!:B = xor(!.B, CB),
			!:M = no
		else 
			(if !.M = yes, private_builtin.pointer_equal(NewChild, Child1)
			then 
				!:M = yes
			else
				!:M = no
			),
			!:L = [ NewChild | !.L ]
		)
	),
	% exclude all one bits from the bitmap up to and including the current bit
	NextBitMask = \ (CB * 2u - 1u),
	(if unsafe_ctz32(B1 /\ NextBitMask)@Zeros < 32
	then
		%Select the next bit by masking the bitmap with NOT (CB * 2 -1) and
		%counting the zeros to the next one bit
		NextBit = unchecked_left_shift(1u, Zeros),
		difference_loop(S, NextBit, B1, A1, B2, A2, !B, !L, !M)
	else
		true % return !B, !L and !M
	).
	
	
difference_tree(S, C@collision(H, _), indexed_branch(B, A), Diff) :-
	mask(H, S, M),
	(if M /\ B = 0u
	then
		Diff = C
	else
		sparse_index(B, M, I),
		array.unsafe_lookup(A, I, Child),
		difference_tree(next_shift(S), C, Child, Diff)
	).
	
difference_tree(S, HM@indexed_branch(B, A), C@collision(H, _), Diff) :-
	mask(H, S, M),
	(if M /\ B = 0u
	then
		Diff = HM
	else
		sparse_index(B, M, I),
		array.unsafe_lookup(A, I, Child0),
		difference_tree(next_shift(S), Child0, C, Child),
		(if private_builtin.pointer_equal(Child0, Child)
		then
			Diff = HM
		else if Child = empty_tree
		then
			array.size(A, L),
			Diff = remove_indexed_child(M, I, B, A, L)
		else
			array.slow_set(I, Child, A, NewA),
			Diff = indexed_branch(B, NewA)
		)
	).

	
	
difference_tree(S, C@collision(H, _), full_branch(A), Diff) :-
	index(H, S, I),
	array.unsafe_lookup(A, I, Child),
	difference_tree(next_shift(S), C, Child, Diff).
	
difference_tree(S, F@full_branch(A), C@collision(H, _), Diff) :-
	index(H, S, I),
	array.unsafe_lookup(A, I, Child0),
	difference_tree(next_shift(S), Child0, C, Child),
	(if private_builtin.pointer_equal(Child0, Child)
	then
		Diff = F
	else if Child = empty_tree
	then
		unsafe_delete(I, A, NewArray),
		Mask = unchecked_left_shift(1u, I),
		Diff = indexed_branch(xor(full_bitmap, Mask), NewArray)
	else
		array.slow_set(I, Child, A, NewA),
		Diff = full_branch(NewA)
	).
	
difference_tree(_, C@collision(H1, B1), collision(H2, B2), Diff) :-
	(if H1 = H2
	then
		map.sorted_keys(B2, Keys),
		map.delete_sorted_list(Keys, B1, NewBucket),
		Diff = 
			(if map.is_empty(NewBucket)
			then
				empty_tree
			else if map.equal(B1, NewBucket)
			then
				C
			else if map.to_assoc_list(NewBucket, [(NK - NV)])
			then
				leaf(H1, NK, NV)
			else
				collision(H1, NewBucket)
			)
	else
		Diff = C
	).	
	
%-----------------------------------------------------------------------------%
% Compose Maps

compose_maps(A, B, compose_maps(A, B)).

compose_maps(A, B) = foldl(compose_maps_acc(B), A, empty_tree).

:- func compose_maps_acc(hashmap(B, C), A, B, hashmap(A, C)) = hashmap(A, C)
	 <= (hashable(A), hashable(B)).

compose_maps_acc(B, K, V, C) = det_insert(C, K, Cval) :- lookup(B, V, Cval).

%-----------------------------------------------------------------------------%
% Bit twiddling

hash_size = bits_per_uint.

bits_per_subkey = 5.

max_children = unchecked_left_shift(1, bits_per_subkey).


subkey_mask = unchecked_left_shift(1u, bits_per_subkey) - 1u.


index(H, S) = cast_to_int(unchecked_right_shift(H, S) /\ subkey_mask).
:- pragma inline(index/2).

index(H, S, index(H, S)).
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



next_shift(S) = S + bits_per_subkey.

:- pragma inline(next_shift/1).

weight(B) = 
	(if hackers_delight_weight = yes
	then
		weight32(B)
	else
		weightn(B)
	).
	
:- pragma inline(weight/1).
	
hackers_delight_weight = yes.


weightn(I) = 
	(if I =< 1u
	then 
		cast_to_int(I)
	else 
		weightn(I /\ (I-1u), 1)
	).
	
:- pragma inline(weightn/1).

:- func weightn(uint, int) = int.

weightn(I, N) =
	(if I =< 1u
	then 
		cast_to_int(I) + N
	else
		weightn(I /\ (I-1u), N+1)
	).
	
/*
int pop(unsigned x) {
 x = x - ((x >> 1) & 0x55555555);
 x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
 x = (x + (x >> 4)) & 0x0F0F0F0F;
 x = x + (x >> 8);
 x = x + (x >> 16);
 return x & 0x0000003F;
 }
*/

weight32(!.X) = !:X :-
	%If word size is larger than 32, ensure all but the 32 lsb are set to zero
	%DCE should remove the if branch at compile time
	(if bits_per_uint > 32
	then !:X = !.X /\ full_bitmap
	else !:X = !.X),
	!:X = !.X - (unchecked_right_shift(!.X, 1) /\ 0x55555555u),
	!:X = (!.X /\ 0x33333333u) + (unchecked_right_shift(!.X,2) /\ 0x33333333u),
	!:X = ( !.X + unchecked_right_shift(!.X, 4)) /\ 0x0F0F0F0Fu,
	!:X = !.X + unchecked_right_shift(!.X, 8),
	!:X = !.X  + unchecked_right_shift(!.X, 16),
	!:X =  !.X /\ 0x0000003Fu,
	!:X = cast_to_int(!.X).

:- pragma inline(weight32/1).
	
/*
int ntz(unsigned x) {
 int n;
 if (x == 0) return(32);
 n = 1;
 if ((x & 0x0000FFFF) == 0) {n = n + 16; x = x >>16;}
 if ((x & 0x000000FF) == 0) {n = n +  8; x = x >> 8;}
 if ((x & 0x0000000F) == 0) {n = n +  4; x = x >> 4;}
 if ((x & 0x00000003) == 0) {n = n +  2; x = x >> 2;}
 return n - (x & 1);
 }
*/ 

ctz32(B) = 
	%If word size is larger than 32, ensure all but the 32 lsb are set to zero
	%DCE should remove the first condition at compile time
	(if bits_per_uint > 32, B > full_bitmap
	then
		unexpected($module, $pred, "This function is undefined for " ++
			"values greater than 2^32, there should not be one bits " ++
			"before the thirty two least signifigant bits.")
	else
		unsafe_ctz32(B)
	).

% Behavior is UNDEFINED for values greater than 2^32 (full_bitmap)
:- func unsafe_ctz32(bitmap) = int.
	
unsafe_ctz32(!.X) = !:X :-
	(if (!.X = 0u)
	then
		!:X = 32
	else 
		some [!N] (
			!:N = 1,
			(if !.X /\ 0x0000FFFFu = 0u 
			then !:N = !.N + 16, !:X = unchecked_right_shift(!.X, 16)
			else !:N = !.N, !:X = !.X),
			(if !.X /\ 0x000000FFu = 0u 
			then !:N = !.N +  8, !:X = unchecked_right_shift(!.X,  8)
			else !:N = !.N, !:X = !.X),
			(if !.X /\ 0x0000000Fu = 0u 
			then !:N = !.N +  4, !:X = unchecked_right_shift(!.X,  4)
			else !:N = !.N, !:X = !.X),
			(if !.X /\ 0x00000003u = 0u 
			then !:N = !.N +  2, !:X = unchecked_right_shift(!.X,  2)
			else !:N = !.N, !:X = !.X),
			!:X = !.N - cast_to_int(!.X /\ 1u)
		)
	).

:- pragma inline(unsafe_ctz32/1).


%-----------------------------------------------------------------------------%
% Standard higher order functions on collections.

:- pred apply_func(func(K, V, A) = A, K, V, A, A).
:- mode apply_func(in(func(in, in, in) = out is det), in, in, in, out) is det.
:- mode apply_func(in(func(in, in, in) = out is semidet), in, in, in, out)
	is semidet.
	
apply_func(F, K, V, A, F(K, V, A)).

foldl(F, T, !.A) = !:A :- 
    foldl(apply_func(F), T, !A).
	
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
	
	
:- pred hash_foldl(pred(hash, K, V, A, A), hashmap(K, V), A, A).
:- mode hash_foldl(in(pred(in, in, in, in, out) is det), in, in, out)
	is det.
:- mode hash_foldl(in(pred(in, in, in, in, out) is semidet), in, in, out)
	is semidet.
	
hash_foldl(_P, empty_tree, !A).
hash_foldl(P, leaf(H, K, V), !A) :- P(H, K, V, !A).
hash_foldl(P, indexed_branch(_B, Array), !A) :-
	array.foldl(hash_foldl(P), Array, !A).
hash_foldl(P, full_branch(Array), !A) :-
	array.foldl(hash_foldl(P), Array, !A).
hash_foldl(P, collision(H, Bucket), !A) :- 
	curry_hash(P, H, PH), 
	map.foldl(PH, Bucket, !A).
	
:- pred curry_hash(pred(hash, K, V, A, A), hash, pred(K, V, A, A)).
:- mode curry_hash(in(pred(in, in, in, in, out) is det), in,  
	out(pred(in, in, in, out) is det)) is det.
:- mode curry_hash(in(pred(in, in, in, in, out) is semidet), in, 
	out(pred(in, in, in, out) is semidet)) is det.

:- pragma promise_equivalent_clauses(curry_hash/3).

curry_hash(P::in(pred(in, in, in, in, out) is det), H::in,
	(pred(K0::in, V0::in, A0::in, A1::out) is det :- 
		P(H, K0, V0, A0, A1)
	)::out(pred(in, in, in, out) is det)
).

curry_hash(P::in(pred(in, in, in, in, out) is semidet), H::in,
	(pred(K0::in, V0::in, A0::in, A1::out) is semidet :- 
		P(H, K0, V0, A0, A1)
	)::out(pred(in, in, in, out) is semidet)
).

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
	
	
	
