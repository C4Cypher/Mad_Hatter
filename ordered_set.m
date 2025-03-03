%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: ordered_set.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module ordered_set.

:- interface.

:- import_module list.
:- import_module array.
:- import_module assoc_list.
:- import_module map.

%-----------------------------------------------------------------------------%
% Ordered set

:- type ordered_set(T) where comparison is compare_ordered_sets.

%-----------------------------------------------------------------------------%
% Basic operations

:- func empty_set = ordered_set(T).

:- pred empty_set(ordered_set(T)::out) is det.

:- pred is_empty(ordered_set(_)::in) is semidet.

:- func singleton(T) = ordered_set(T).

:- pred singleton(T::in, ordered_set(T)::out) is det.

:- pred is_singleton(ordered_set(T)::in) is semidet.

:- func size(ordered_set(_)) = int.
:- pred size(ordered_set(_)::in, int::out) is det.

:- pred contains(ordered_set(T)::in, T::in).

% Compares the ordered values in the set, including duplicates, succeeds if
% the set contains the same values in the same order. 
:- pred equal(ordered_set(T)::in, ordered_set(T)::in) is semidet.

% Succeeds if the sets have the same values, with duplicates removed
% Equivalent to compare_ordered_sets((=)).
:- pred equivalent(ordered_set(T)::in, ordered_set(T)::in) is semidet.

% Sorts and removes duplicates from the sets and compares them according to
% the standard ordering. Used to implement mercury equality and comparison.
% This is neccecary in order to efficiently implement maps and other data
% structures for this type as *sets* ... if you need to map the values in
% the set by ordering, convert the set to an ordered list or array first.
:- pred compare_ordered_sets(comparison_result::uo, ordered_set(T)::in, 
	ordered_set::in) is det.


%-----------------------------------------------------------------------------%
% Conversion

% Converts the linear order of the set into the according container

:- func from_list(list(T)) = ordered_set(T).
:- func to_list(ordered_set(T)) = list(T).

:- func from_array(array(T)) = ordered_set(T).
:- func to_array(ordered_set(T)) = array(T).

% Sorts and removes duplicates (if any) from the set, the assigned ordering
% is ignored

:- func to_sorted_list(ordered_set(T)) = list(T).
:- func to_sorted_array(ordered_set(T)) = array(T).

%-----------------------------------------------------------------------------%
% Ordering

% Creates a new linear ordered set by sorting the members using the provided
% comparison function
:- pred order_by(comparison_func(T)::in(comparison_func), ordered_set(T)::in, 
	ordered_set(T)::out) is det.
	
:- type indexed_comparison_func(T) == 
	(func(int, int, T, T) = comparison_result).

:- inst indexed_comparison_func == (func(in, in, in, in) = out is det).

% As above, but provides the corresponding indexes in the original ordering
% to the provided comparison function.
:- pred order_by_index(
	indexed_comparison_func(T)::in(indexed_comparison_func)),
	ordered_set(T)::in, ordered_set(T)::out) is det. 

%-----------------------------------------------------------------------------%
% Set operations

% Preserve the order of the first set, appending items in the second set to the
% first in-order
:- pred union(ordered_set(T)::in, ordered_set(T)::in, ordered_set::out) is det.

:- func union(ordered_set(T), ordered_set(T)) = ordered_set(T).

% Preserve the order of the first set, removing elements not found in the
% second set.
:- pred intersect(ordered_set(T)::in, ordered_set(T)::in, ordered_set::out) 
	is det.

:- func intersect(ordered_set(T), ordered_set(T)) = ordered_set(T).

% Preserve the order of the first set, removing elements found in the second
% set
:- pred difference(ordered_set(T)::in, ordered_set(T)::in, ordered_set::out) 
	is det.
:- func difference(ordered_set(T), ordered_set(T)) = ordered_set(T).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module lazy.

:- import_module util.

%-----------------------------------------------------------------------------%

:- type sorted_array(T) == lazy(array(T)).

:- type ordered_set(T)
	--->	ordered_set(order::array(T), sorted::sorted_array(T)).

