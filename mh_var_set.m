%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_var_set.m
% Main author: C4Cypher.
% Stability: low.

:- module mh_var_set.

:- interface.

:- import_module list.
:- import_module array.

%-----------------------------------------------------------------------------%
% Variable sets

:- type var_id_set.
