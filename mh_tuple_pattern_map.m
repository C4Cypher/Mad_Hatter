%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_tuple_pattern_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_tuple_pattern_map.

:- interface.

:- import_module mh_term.
:- import_module mh_tuple.

%-----------------------------------------------------------------------------%
% Tuple map

:- type mh_tuple_pattern_map(T).

:- func init = (mh_tuple_pattern_map(T)::uo) is det.
:- pred init(mh_tuple_pattern_map(_)::uo) is det.

:- func singleton(mh_tuple, T) = mh_tuple_pattern_map(T).

:- pred is_empty(mh_term_map(_)::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module list.
:- use_module map.
:- import_module pair.

:- import_module mh_term_map.

%-----------------------------------------------------------------------------%

:- type 

:- type fixed_length_tuple_map(T) == array()

:- type mh_tuple_pattern_map(T)
	--->	tuple_set_map(map.map(int, array(mh_term_map(pair(mh_tuple, T))))).
