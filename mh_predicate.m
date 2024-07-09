%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_predicate.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_predicate.

:- interface.

:- import_module predicate.
:- import_module mh_arity.
:- import_module mh_mode.

%-----------------------------------------------------------------------------%



:- typeclass predicate(T) <= relation(T) where 
[
	% Todo:  predicate to call predicate under a module and scope
].

:

