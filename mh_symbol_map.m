%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_symbol_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_symbol_map.

% An attempt at implementing Haskell's Unordered Containers package
% implementation of a HAMT in Merrcury
% Original implementation found in Data.HashMap.Internal by Johan Tibell

% Interface is made to reflect the same calls as the Mercury map.m library to
% allow mh_symbol_map to be used as a 'drop-in' replacement for map.map.
% Note that most, but not all of the calls from map.m have been replicated.

:- interface.

:- import_module list.
:- import_module assoc_list.
:- import_module maybe.
:- import_module set.

:- import_module hashmap.

:- import_module mh_symbol.

%-----------------------------------------------------------------------------%
% Symbol_map

:- type mh_symbol_map(T).

%-----------------------------------------------------------------------------%
% Construction

:- func init = (mh_symbol_map(_)::uo) is det.
:- pred init(mh_symbol_map(_)::uo) is det.


:- func singleton(mh_symbol, T) = mh_symbol_map(T).
:- pred singleton(mh_symbol::in, T::in, mh_symbol_map(T)::out) is det.

%-----------------------------------------------------------------------------%
% Basic interface

:- pred is_empty(mh_symbol_map(_)::in) is semidet.

	% Number of elements contained (HS size function)
:- func count(mh_symbol_map(_)) = int is det.
:- pred count(mh_symbol_map(_)::in, int::out) is det.

	% Succeeds if two mh_symbol_maps contain the same elements, regardless of 
	% internal structure
:- pred equal(mh_symbol_map(T)::in, mh_symbol_map(T)::in) is semidet.

%-----------------------------------------------------------------------------%
% Search

	% Succeeds if the map contains the given key
:- pred contains(mh_symbol_map(_)::in, mh_symbol::in) is semidet.

	% Fails if the key is not found
:- pred search(mh_symbol_map(T)::in, mh_symbol::in, T::out) is semidet.
:- func search(mh_symbol_map(T), mh_symbol) = T is semidet.

:- pred lookup(mh_symbol_map(T)::in, mh_symbol::in, T::out) is det.
:- func lookup(mh_symbol_map(T), mh_symbol) = T is det.

	% inverse and bounds searches have not been implemented and will throw an
	% exception
:- pred inverse_search(mh_symbol_map(T)::in, T::in, mh_symbol::out)
	is erroneous.
:- pred lower_bound_search(mh_symbol_map(T)::in, mh_symbol::in, mh_symbol::out,
	T::out) is erroneous.
:- pred lower_bound_lookup(mh_symbol_map(T)::in, mh_symbol::in, mh_symbol::out,
	T::out) is erroneous.
:- pred upper_bound_search(mh_symbol_map(T)::in, mh_symbol::in, mh_symbol::out,
	T::out) is erroneous.
:- pred upper_bound_lookup(mh_symbol_map(T)::in, mh_symbol::in, mh_symbol::out,
	T::out) is erroneous.

%-----------------------------------------------------------------------------%
% Insertion

	% Insert an element into a mh_symbol_map, fails if the element already exists 
:- pred insert(mh_symbol::in, T::in, mh_symbol_map(T)::in, 
	mh_symbol_map(T)::out) is semidet.
:- func insert(mh_symbol_map(T), mh_symbol, T) = mh_symbol_map(T) is semidet.

:- pred det_insert(mh_symbol::in, T::in, mh_symbol_map(T)::in, 
	mh_symbol_map(T)::out) is det.
:- func det_insert(mh_symbol_map(T), mh_symbol, T) = mh_symbol_map(T) is det.
	
:- func det_insert_from_corresponding_lists(mh_symbol_map(T), list(mh_symbol),
	list(T)) = mh_symbol_map(T).
:- pred det_insert_from_corresponding_lists(list(mh_symbol)::in,
    list(T)::in, mh_symbol_map(T)::in, mh_symbol_map(T)::out) is det.
	
:- func det_insert_from_assoc_list(mh_symbol_map(T), assoc_list(mh_symbol, T))
	= mh_symbol_map(T).
:- pred det_insert_from_assoc_list(assoc_list(mh_symbol, T)::in,
    mh_symbol_map(T)::in, mh_symbol_map(T)::out) is det.
	
:- pred search_insert(mh_symbol::in, T::in, maybe(T)::out,
    mh_symbol_map(T)::in, mh_symbol_map(T)::out) is det.

	% Inserts an element into a mh_symbol_map, overwrite element if it 
	% already exists
:- pred set(mh_symbol::in, T::in, mh_symbol_map(T)::in, mh_symbol_map(T)::out) 
	is det.
:- func set(mh_symbol_map(T), mh_symbol, T) = mh_symbol_map(T).
	
:- func set_from_corresponding_lists(mh_symbol_map(T), list(mh_symbol), 
	list(T)) =	mh_symbol_map(T).
:- pred set_from_corresponding_lists(list(mh_symbol)::in, list(T)::in,
    mh_symbol_map(T)::in, mh_symbol_map(T)::out) is det.
	
:- func set_from_assoc_list(mh_symbol_map(T), assoc_list(mh_symbol, T)) = 
	mh_symbol_map(T).
:- pred set_from_assoc_list(assoc_list(mh_symbol, T)::in,
    mh_symbol_map(T)::in, mh_symbol_map(T)::out) is det.

% Overwrite an already existing element in a mh_symbol_map, fail if  not found
:- pred update(mh_symbol::in, T::in, mh_symbol_map(T)::in, 
	mh_symbol_map(T)::out) is semidet.
:- func update(mh_symbol_map(T), mh_symbol, T) = mh_symbol_map(T) is semidet.

:- pred det_update(mh_symbol::in, T::in, mh_symbol_map(T)::in, 
	mh_symbol_map(T)::out) is det.
:- func det_update(mh_symbol_map(T), mh_symbol, T) = mh_symbol_map(T) is det.

%-----------------------------------------------------------------------------%
% Removal

	% Remove a key-value pair from a map and return the value.
	% Fail if the key is not present.
:- pred remove(mh_symbol::in, T::out, mh_symbol_map(T)::in, 
	mh_symbol_map(T)::out) is semidet.

:- pred det_remove(mh_symbol::in, T::out, mh_symbol_map(T)::in,
	mh_symbol_map(T)::out) is det.

	% Delete a key-value pair from a map.
	% If the key is not present, leave the map unchanged.
:- pred delete(mh_symbol::in, mh_symbol_map(T)::in, mh_symbol_map(T)::out)
	is det.
:- func delete(mh_symbol_map(T), mh_symbol) = mh_symbol_map(T).

:- func delete_list(mh_symbol_map(T), list(mh_symbol)) = mh_symbol_map(T).
:- pred delete_list(list(mh_symbol)::in, mh_symbol_map(T)::in, 
	mh_symbol_map(T)::out) is det.

%-----------------------------------------------------------------------------%
% Field selection for maps.

	% Hashmap ^ elem(Key) = search(Hashmap, Key).
:- func elem(mh_symbol, mh_symbol_map(T)) = T is semidet.

	% Hashmap ^ det_elem(Key) = lookup(Hashmap, Key).
:- func det_elem(mh_symbol, mh_symbol_map(T)) = T.

	% (Hashmap ^ elem(Key) := Talue) = set(Hashmap, Key, Value).
:- func 'elem :='(mh_symbol, mh_symbol_map(T), T) = mh_symbol_map(T).

	% (Hashmap ^ det_elem(Key) := Talue) = det_update(Hashmap, Key, Value).
:- func 'det_elem :='(mh_symbol, mh_symbol_map(T), T) = mh_symbol_map(T).

%-----------------------------------------------------------------------------%
% Returning keys and values.

	% All key value pairs stored in the mh_symbol_map, order is not garunteed
:- pred member(mh_symbol_map(T), mh_symbol, T).
:- mode member(in, in, out) is semidet.
:- mode member(in, out, out) is nondet.

	% Given a map, return a list of all the keys in the map.
:- func keys(mh_symbol_map(_)) = list(mh_symbol).
:- pred keys(mh_symbol_map(_)::in, list(mh_symbol)::out) is det.

:- func sorted_keys(mh_symbol_map(_)) = list(mh_symbol).
:- pred sorted_keys(mh_symbol_map(_)::in, list(mh_symbol)::out) is det.

:- func keys_as_set(mh_symbol_map(_)) = set(mh_symbol).
:- pred keys_as_set(mh_symbol_map(_)::in, set(mh_symbol)::out) is det.

:- func values(mh_symbol_map(T)) = list(T).
:- pred values(mh_symbol_map(T)::in, list(T)::out) is det.

:- pred keys_and_values(mh_symbol_map(T)::in, list(mh_symbol)::out, list(T)::out)
	is det.
	
:- func max_key(mh_symbol_map(_)) = mh_symbol is semidet.
:- pred max_key(mh_symbol_map(_)::in, mh_symbol::out) is semidet.

:- func min_key(mh_symbol_map(_)) = mh_symbol is semidet.
:- pred min_key(mh_symbol_map(_)::in, mh_symbol::out) is semidet.
	
:- func det_max_key(mh_symbol_map(_)) = mh_symbol.
:- func det_min_key(mh_symbol_map(_)) = mh_symbol.

%-----------------------------------------------------------------------------%
% Operations on values.

	% Update the value at the given key by applying the supplied
	% transformation to it. Fails if the key is not found. This is faster
	% than first searching for the value and then updating it.
:- pred transform_value(pred(T, T)::in(pred(in, out) is det), mh_symbol::in,
    mh_symbol_map(T)::in, mh_symbol_map(T)::out) is semidet.

	% Same as transform_value/4, but throws an exception if the key is not
	% found.
:- func det_transform_value(func(T) = T, mh_symbol, mh_symbol_map(T))
	= mh_symbol_map(T).
:- pred det_transform_value(pred(T, T)::in(pred(in, out) is det), 
	mh_symbol::in, mh_symbol_map(T)::in, mh_symbol_map(T)::out) is det.

%-----------------------------------------------------------------------------%
% Conversions

:- func to_hashmap(mh_symbol_map(T)) = hashmap(mh_symbol, T).
:- func from_hashmap(hashmap(mh_symbol, T)) = mh_symbol_map(T).

%---------------------------------------------------------------------------%
% Operations on two or more maps.

	% Note: Unlike Mercury standard map library, these calls perform the same,
	% regardless of the order of the  arguments

	% Merge the contents of the two maps.
	% Throws an exception if both sets of keys are not disjoint.

:- func merge(mh_symbol_map(T), mh_symbol_map(T)) = mh_symbol_map(T).
:- pred merge(mh_symbol_map(T)::in, mh_symbol_map(T)::in,
	mh_symbol_map(T)::out) is det.

	% For overlay(MapA, MapB, Map), if MapA and MapB both contain the
	% same key, then Map will map that key to the value from MapB.
	% In other words, MapB takes precedence over MapA.
:- func overlay(mh_symbol_map(T), mh_symbol_map(T)) = mh_symbol_map(T).
:- pred overlay(mh_symbol_map(T)::in, mh_symbol_map(T)::in,
	mh_symbol_map(T)::out) is det.
	
	% overlay_large_map is identical to overlay, it has been included for
	% compatability with the Mercury standard map, however it has also been
	% marked as obsolete, use overlay
:- func overlay_large_map(mh_symbol_map(T), mh_symbol_map(T))
	= mh_symbol_map(T).
:- pred overlay_large_map(mh_symbol_map(T)::in, mh_symbol_map(T)::in, 
	mh_symbol_map(T)::out)  is det.
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
:- func common_subset(mh_symbol_map(T), mh_symbol_map(T)) = mh_symbol_map(T).
:- pred common_subset(mh_symbol_map(T)::in, mh_symbol_map(T)::in, 
	mh_symbol_map(T)::out) is det.


	% Given two maps MapA and MapB, create a third map, IntersectMap,
	% that has only the keys that occur in both MapA and MapB. For keys
	% that occur in both MapA and MapB, compute the value in the final map
	% by applying the supplied function to the values associated with
	% the key in MapA and MapB.
	% on the values associated with some common key.
:- func intersect(func(T, T) = T, mh_symbol_map(T), mh_symbol_map(T)) = 
	mh_symbol_map(T).

	% Given two maps MapA and MapB, create a third map, IntersectMap,
	% that has only the keys that occur in both MapA and MapB. For keys
	% that occur in both MapA and MapB, compute the value in the final map
	% by applying the supplied predicate to the values associated with
	% the key in MapA and MapB. Fail if and only if this predicate fails
	% on the values associated with some common key.
:- pred intersect(pred(T, T, T), mh_symbol_map(T), mh_symbol_map(T),
	mh_symbol_map(T)).
:- mode intersect(in(pred(in, in, out) is det), in, in, out) is det.
:- mode intersect(in(pred(in, in, out) is semidet), in, in, out) is semidet.

	% Calls intersect. Throws an exception if intersect fails.
:- func det_intersect((func(T, T) = T)::in(func(in, in) = out is semidet),
    mh_symbol_map(T)::in, mh_symbol_map(T)::in) = (mh_symbol_map(T)::out)
	is det.
:- pred det_intersect((pred(T, T, T))::in(pred(in, in, out) is semidet),
    mh_symbol_map(T)::in, mh_symbol_map(T)::in, mh_symbol_map(T)::out) is det.
	
	% intersect_list(Pred, M, [M | Ms ], Result):
	% Recursively insersect M with M and then recursively call the result with
	% Ms, folding over the entire list. If the list is empty, return M. 
:- pred intersect_list(pred(T, T, T), mh_symbol_map(T), list(mh_symbol_map(T)), 
	mh_symbol_map(T)).
:- mode intersect_list(in(pred(in, in, out) is det), in, in, out) is det.
:- mode intersect_list(in(pred(in, in, out) is semidet), in, in, out) 
	is semidet.

	% intersect_list(Pred, List, Result): 
	% If List is empty, return an empty map, otherwise call the above 
	% intersect predicate with the first element and the rest of the list
	% and then recursively intersect the result with the rest of the list.
:- pred intersect_list(pred(T, T, T), list(mh_symbol_map(T)),
	mh_symbol_map(T)).
:- mode intersect_list(in(pred(in, in, out) is det), in, out) is det.
:- mode intersect_list(in(pred(in, in, out) is semidet), in, out) is semidet.

    % Given two maps MapA and MapB, create a third map, UnionMap, that
    % contains all the keys that occur in either MapA and MapB. For keys
    % that occur in both MapA and MapB, compute the value in the final map
    % by applying the supplied function to the values associated with the key
    % in MapA and MapB.
    %
:- func union(func(T, T) = T, mh_symbol_map(T), mh_symbol_map(T))
	= mh_symbol_map(T).

    % Given two maps MapA and MapB, create a third map, UnionMap, that
    % contains all the keys that occur in either MapA and MapB. For keys
    % that occur in both MapA and MapB, compute the value in the final map
    % by applying the supplied predicate to the values associated with the key
    % in MapA and MapB. Fail if and only if this predicate fails on
    % the values associated with some common key.
    %
:- pred union(pred(T, T, T), mh_symbol_map(T), mh_symbol_map(T),
	mh_symbol_map(T)).
:- mode union(in(pred(in, in, out) is det), in, in, out) is det.
:- mode union(in(pred(in, in, out) is semidet), in, in, out) is semidet.

	% Calls union. Throws an exception if union fails.
:- func det_union((func(T, T) = T)::in(func(in, in) = out is semidet),
    mh_symbol_map(T)::in, mh_symbol_map(T)::in) = (mh_symbol_map(T)::out)
	is det.
:- pred det_union(pred(T, T, T)::in(pred(in, in, out) is semidet),
    mh_symbol_map(T)::in, mh_symbol_map(T)::in, mh_symbol_map(T)::out) is det.
	
	% union_list(Pred, M, [M | Ms ], Result):
	% Recursively union M with M and then recursively call the result with
	% Ms, folding over the entire list. 
:- pred union_list(pred(T, T, T), mh_symbol_map(T), list(mh_symbol_map(T)), 
	mh_symbol_map(T)).
:- mode union_list(in(pred(in, in, out) is det), in, in, out) is det.
:- mode union_list(in(pred(in, in, out) is semidet), in, in, out) is semidet.	
	

	% union_list(Pred, List, Result): 
	% If List is empty, return an empty map, otherwise call the above 
	% union predicate with the first element and the rest of the list
	% and then recursively union the result with the rest of the list.
:- pred union_list(pred(T, T, T), list(mh_symbol_map(T)),	mh_symbol_map(T)).
:- mode union_list(in(pred(in, in, out) is det), in, out) is det.
:- mode union_list(in(pred(in, in, out) is semidet), in, out) is semidet.

	% Take two maps and return the elements of the first map that do not have
	% equivalent keys in the second map. Take note that the value type
	% of the maps in question need not have the same type.
:- pred difference(mh_symbol_map(T)::in, mh_symbol_map(_)::in,
	mh_symbol_map(T)::out) is det.
	
%-----------------------------------------------------------------------------%
% Standard higher order functions on collections.

% Commented modes not supported by current array library higher order calls,
% custom implementation required.

% Perform a traversal by key of the map, applying an accumulator
% predicate for value. Order is arbitrary and cannot be garunteed.
:- func foldl(func(mh_symbol, T, A) = A, mh_symbol_map(T), A) = A.
:- pred foldl(pred(mh_symbol, T, A, A), mh_symbol_map(T), A, A).
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
	
:- pred foldl2(pred(mh_symbol, T, A, A, B, B), mh_symbol_map(T), A, A, B, B).
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
	
:- pred foldl3(pred(mh_symbol, T, A, A, B, B, C, C), mh_symbol_map(T),
	A, A, B, B, C, C).
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
	
% Given that the mh_symbol_map stores key-value pairs in an order arbitrary 
% to the hash function used, I reasoned that implementing rfold calls would be
% redundant.

% Apply a transformation predicate to all the values in a map.
:- func map_values(func(mh_symbol, T) = U, mh_symbol_map(T)) 
	= mh_symbol_map(U).
:- pred map_values(pred(mh_symbol, T, U), mh_symbol_map(T), mh_symbol_map(U)).
:- mode map_values(in(pred(in, in, out) is det), in, out) is det.
% :- mode map_values(in(pred(in, in, out) is semidet), in, out) is semidet.

% Same as map_values, but do not pass the key to the given predicate.
:- func map_values_only(func(T) = U, mh_symbol_map(T)) = mh_symbol_map(U).
:- pred map_values_only(pred(T, U), mh_symbol_map(T), mh_symbol_map(U)).
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

%-----------------------------------------------------------------------------%
% Hash Array Mapped Table

:- type mh_symbol_map(T)
	--->	symbol_map(hashmap(mh_symbol, T)).

% abbreviated constructor
:- func sm(hashmap(mh_symbol, T)) = mh_symbol_map(T).
:- mode sm(in) = out is det.
:- mode sm(out) = in is det.

sm(M) = symbol_map(M).

:- pragma inline(sm/1).

%-----------------------------------------------------------------------------%
% Construction

init = sm(hashmap.init).
init(init).

singleton(S, T) = sm(hashmap.hash_singleton(symbol_hash(S), S, T)).
singleton(S, T, singleton(S, T)).

%-----------------------------------------------------------------------------%
% Basic interface

is_empty(sm(M)) :- is_empty(M).

count(sm(M)) = count(M).
count(M, count(M)).

equal(sm(A), sm(B)) :- equal(A, B).

%-----------------------------------------------------------------------------%
% Search

contains(sm(Map), S) :- hashmap.hash_contains(Map, symbol_hash(S), S).

search(sm(M), S, hashmap.hash_search(M, symbol_hash(S), S)).

search(sm(M), S) = hashmap.hash_search(M, symbol_hash(S), S).

lookup(M, S, lookup(M, S)).

lookup(sm(M), S) = 
	(if hashmap.hash_search(M, symbol_hash(S), S) = Found
	then 
		Found
	else
		report_lookup_error("mh_symbol_map.lookup: key not found", S)
	).
	
inverse_search(_, _, _) :- sorry($module, $pred, "inverse_search").
lower_bound_search(_, _, _, _) :- sorry($module, $pred, "lower_bound_search").
lower_bound_lookup(_, _, _, _) :- sorry($module, $pred, "lower_bound_lookup").
upper_bound_search(_, _, _, _) :- sorry($module, $pred, "upper_bound_search").
upper_bound_lookup(_, _, _, _) :- sorry($module, $pred, "upper_bound_lookup").

%-----------------------------------------------------------------------------%
% Insertion

insert(S, T, sm(!.M), sm(!:M)) :- 
	hashmap.hash_insert(symbol_hash(S), S, T, !M).
	
insert(sm(!.M), S, T) = sm(!:M) :-
	insert(S, T, !M).


det_insert(S, T, !M) :-
	( if insert(S, T, !.M, NewMap) then
        !:M = NewMap
    else
        report_lookup_error("mh_symbol_map.det_insert: key already present", 
		S, T)
    ).
	
:- pragma inline(det_insert/4).
	
det_insert(!.M, S, T) = !:M :- 
	det_insert(S, T, !M).
	
:- pragma inline(det_insert/3).
	
det_insert_from_corresponding_lists(sm(M0), Ss, Ts) = sm(M) :-
    hashmap.det_insert_from_corresponding_lists(Ss, Ts, M0, M).
	
det_insert_from_corresponding_lists(Ss, Ts, M, 
	det_insert_from_corresponding_lists(M, Ss, Ts)).

det_insert_from_assoc_list(sm(M0), AL) = sm(M) :-
    hashmap.det_insert_from_assoc_list(AL, M0, M).

det_insert_from_assoc_list(AL, M, det_insert_from_assoc_list(M, AL)).

% search_insert(S, T, MaybOldT, !M)
search_insert(S, T, MaybOldT, sm(!.M), sm(!:M)) :- 
	hashmap.hash_search_insert(symbol_hash(S), S, T, MaybOldT, !M).
 

set(S, T, sm(!.M), sm(!:M)) :- hashmap.hash_set(symbol_hash(S), S, T, !M).

:- pragma inline(set/4).
	
set(!.M, S, T) = !:M :-
	set(S, T, !M).
	
:- pragma inline(set/3).

set_from_corresponding_lists(sm(M0), Ss, Ts) = sm(M) :-
    hashmap.set_from_corresponding_lists(Ss, Ts, M0, M).


set_from_corresponding_lists(Ss, Ts, M, 
	set_from_corresponding_lists(M, Ss, Ts)).

set_from_assoc_list(sm(M0), AL) = sm(M) :-
    hashmap.set_from_assoc_list(AL, M0, M).

set_from_assoc_list(AL, M, set_from_assoc_list(M, AL)).
	
update(S, T, M, update(M, S, T)).

update(sm(M), S, T) = sm(hashmap.hash_update(M, symbol_hash(S), S, T)).

	
det_update(S, T, !M) :-
	( if update(S, T, !.M, NewMap) then
        !:M = NewMap
    else
        report_lookup_error("mh_symbol_map.det_update: key not found", 
			S, T)
    ).
	
det_update(!.M, S, T) = !:M :- 
	det_update(S, T, !M).

%-----------------------------------------------------------------------------%
% Removal

remove(S, T, sm(!.M), sm(!:M)) :- 
	hashmap.hash_remove(symbol_hash(S), S, T, !M).

det_remove(S, T, !M) :- 
	( if remove(S, Found, !.M, NewMap) then
        T = Found,
		!:M = NewMap
    else
        report_lookup_error("mh_symbol_map.det_remove: key not found", S)
    ).

%-----------------------------------------------------------------------------%


delete(S, M, delete(M, S)).
:- pragma inline(delete/3).

delete(sm(M), S) = sm(hashmap.hash_delete(M, symbol_hash(S), S)).
:- pragma inline(delete/2).

delete_list(M0, Ss) = M :-
    mh_symbol_map.delete_list(Ss, M0, M).

delete_list(Symbols, sm(!.Map), sm(!:Map)) :-
    hashmap.delete_list(Symbols, !Map).
	
%-----------------------------------------------------------------------------%
% Field selection for maps.


elem(Key, Map) = mh_symbol_map.search(Map, Key).

det_elem(Key, Map) = mh_symbol_map.lookup(Map, Key).

'elem :='(Key, Map, Value) = mh_symbol_map.set(Map, Key, Value).

'det_elem :='(Key, Map, Value) = mh_symbol_map.det_update(Map, Key, Value).


%-----------------------------------------------------------------------------%
% Returning keys and values.
	
:- pragma promise_equivalent_clauses(member/3).

member(sm(M)::in, S::in, T::out) :- 
	hashmap.hash_search(M, symbol_hash(S), S, T).

member(sm(M)::in, S::out, T::out) :- hashmap.hash_member(M, _Hash, S, T).

keys(M) = Ks :- keys(M, Ks).

keys(sm(M), Ks) :- hashmap.keys(M, Ks).

sorted_keys(M) = Ks :- sorted_keys(M, Ks).

sorted_keys(sm(M), Ks) :- hashmap.sorted_keys(M, Ks).
	
keys_as_set(M) = Set :-
    keys_as_set(M, Set).
	
keys_as_set(sm(M), Set) :- hashmap.keys_as_set(M, Set).

values(M) = Ts :- values(M, Ts).

values(sm(M), Ts) :- hashmap.values(M, Ts).

keys_and_values(sm(M), Ks, Ts) :- hashmap.keys_and_values(M, Ks, Ts).
	
max_key(M) = S :- max_key(M, S).
max_key(sm(M), S) :- hashmap.max_key(M, S).

min_key(M) = S :- min_key(M, S).
min_key(sm(M), S) :- hashmap.min_key(M, S).

det_max_key(M) = S :-
	(if max_key(M, S0)
	then
		S = S0
	else
		error($pred, "An empty mh_symbol_map has no maximum key")
	).
	
det_min_key(M) = S :-
	(if min_key(M, S0)
	then
		S = S0
	else
		error($pred, "An empty mh_symbol_map has no minimum key")
	).


%-----------------------------------------------------------------------------%
% Operations on values.

transform_value(P, S, sm(!.M), sm(!:M)) :- 
	hashmap.hash_transform_value(P, symbol_hash(S), S, !M).

det_transform_value(F, S, !.M) = !:M :-
    det_transform_value(pred(V0::in, V::out) is det :- V = F(V0), S, !M).

det_transform_value(P, S, !M) :-
    ( if transform_value(P, S, !.M, NewM) then
        !:M = NewM
    else
        report_lookup_error("mh_symbol_map.det_transform_value: key not found", S)
    ).
	
%-----------------------------------------------------------------------------%
% Conversions

to_hashmap(sm(M)) = M.
from_hashmap(M) = sm(M).

	
%---------------------------------------------------------------------------%
% Operations on two or more maps.

merge(M1, M2) = M :- merge(M1, M2, M).

merge(sm(M1), sm(M2), sm(M)) :- hashmap.merge(M1, M2, M).
	
overlay(M1, M2) = M :- overlay(M1, M2, M).

overlay(sm(M1), sm(M2), sm(M)) :- hashmap.overlay(M1, M2, M).

overlay_large_map(M1, M2) = M :- overlay_large_map(M1, M2, M).

overlay_large_map(M1, M2, M) :- overlay(M1, M2, M).

%-----------------------------------------------------------------------------%
% Common Subset	
	
common_subset(M1, M2) = Sub :- common_subset(M1, M2, Sub).

common_subset(sm(M1), sm(M2), sm(Sub)) :- hashmap.common_subset(M1, M2, Sub). 
	
%-----------------------------------------------------------------------------%
% Intersection

intersect(F, sm(M1), sm(M2)) = sm(hashmap.intersect(F, M1, M2)).
	
intersect(P, sm(M1), sm(M2), sm(Int)) :- hashmap.intersect(P, M1, M2, Int).

det_intersect(PF, M1, M2) = Int :-
    P = (pred(X::in, Y::in, Z::out) is semidet :- Z = PF(X, Y) ),
    det_intersect(P, M1, M2, Int).

det_intersect(P, M1, M2, Int) :-
    ( if intersect(P, M1, M2, Int0) then
        Int = Int0
    else
        unexpected($pred, "mh_symbol_map.intersect failed")
    ).
	
intersect_list(_P, M, [], M).

intersect_list(P, SM, [ M | Ms ], Res) :- 
	intersect(P, SM, M, Int),
	(if is_empty(Int)
	then Res = Int
	else intersect_list(P, Int, Ms, Res)
	).
	
intersect_list(_P, [], init).

intersect_list(P, [M | Ms], Res) :- intersect_list(P, M, Ms, Res).
	
%-----------------------------------------------------------------------------%
% Union

union(F, sm(M1), sm(M2)) = sm(hashmap.union(F, M1, M2)).
	
union(P, sm(M1), sm(M2), sm(M)) :- hashmap.union(P, M1, M2, M).


det_union(PF, M1, M2) = Union :-
    P = (pred(X::in, Y::in, Z::out) is semidet :- Z = PF(X, Y) ),
    det_union(P, M1, M2, Union).

det_union(P, M1, M2, Union) :-
    ( if union(P, M1, M2, Union0) then
        Union = Union0
    else
        unexpected($pred, "mh_symbol_map.union failed")
	).

	
union_list(_P, SM, [], SM).

union_list(P, SM, [ M | Ms ], Res) :- 
	union(P, SM, M, Union),
	union_list(P, Union, Ms, Res).
	
union_list(_P, [], init).

union_list(P, [M | Ms], Res) :- union_list(P, M, Ms, Res).

%-----------------------------------------------------------------------------%
% Difference

difference(sm(A), sm(B), sm(C)) :- hashmap.difference(A, B, C).


%-----------------------------------------------------------------------------%
% Standard higher order functions on collections.

foldl(F, sm(M), A) = hashmap.foldl(F, M, A).
foldl(P, sm(M), !A) :- hashmap.foldl(P, M, !A).
foldl2(P, sm(M), !A, !B) :- hashmap.foldl2(P, M, !A, !B).
foldl3(P, sm(M), !A, !B, !C) :- hashmap.foldl3(P, M, !A, !B, !C).



map_values(F, sm(M)) = sm(hashmap.map_values(F, M)).
map_values(P, sm(!.M), sm(!:M)) :- hashmap.map_values(P, !M).

map_values_only(F, sm(M)) = sm(hashmap.map_values_only(F, M)).
map_values_only(P, sm(!.M), sm(!:M)) :- hashmap.map_values_only(P, !M).
	
	
