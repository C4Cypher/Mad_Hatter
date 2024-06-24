%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_dictionary.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module dictionary.

:- type dictionary(T, U). %Map of Id's to values implemented in an array

%	init_id_map(ID_Set, InitialValue, NewMap).
:- pred init_dictionary(id_set(T)::in, U::in, dictionary(T, U)::out) is det.

:- func init_dictionary(id_set(T), U) = dictionary(T, U).