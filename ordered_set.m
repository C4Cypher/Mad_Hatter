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
:- import_module set.

%-----------------------------------------------------------------------------%


:- type ordered_set(T) where comparison is compare_ordered_sets.

:- func init = (mh_tuple_map(T)::uo) is det.

:- func singleton(mh_tuple, T) = mh_tuple_map(T).

:- pred is_empty(mh_tuple_map(_)::in) is semidet.

% Compares the ordered values in the set, including duplicates, succeeds if
% the set contains the same values. (semidet version of unifying the values)
:- pred equal(ordered_set(T)::in, ordered_set(T)::in) is semidet.

% Succeeds if the sets have the same values, with duplicates removed
:- pred equivalent(ordered_set(T)::in, ordered_set(T)::in) is semidet.

:- pred compare_ordered_sets(comparison_result::uo, ordered_set(T)::in, 
	ordered_set::in) is det.


%-----------------------------------------------------------------------------%
% List conversion

:- func from_list(list(T)) = ordered_set(T).
:- func to_list(ordered_set(T)) = list(T).


%-----------------------------------------------------------------------------%
% Array conversion

:- func from_array(array(T)) = ordered_set(T).
:- func to_array(ordered_set(T)) = array(T).

%-----------------------------------------------------------------------------%
% Set conversion

:- func from_set(set(T)) = ordered_set(T).
:- func to_set(ordered_set(T)) = set(T).

%-----------------------------------------------------------------------------%
% Ordering

% Creates a new ordered set by 
:- pred order_by(comparison_func(T)::in, ordered_set(T)::in, 
	ordered_set(T)::out) is det.

%-----------------------------------------------------------------------------%
% Weighted set




%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%



:- type ordered_set(T)
	--->	