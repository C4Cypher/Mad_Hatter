%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_tuple_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_tuple_map.

:- interface.

:- import_module mh_term.
:- import_module mh_tuple.

%-----------------------------------------------------------------------------%
% Tuple map

:- type mh_tuple_map(T).

:- func init = (mh_tuple_map(T)::uo) is det.
:- pred init(mh_tuple_map(_)::uo) is det.

:- func singleton(mh_tuple, T) = mh_tuple_map(T).

:- pred is_empty(mh_term_map(_)::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module list.
:- use_module map.

:- import_module mh_term_map.

%-----------------------------------------------------------------------------%

:- type term_list_map(T) == map.map(list(mh_term), T).

:- type mh_tuple_map(T)
	--->	tuple_map(term_list_map(T), ).
	
init = 
