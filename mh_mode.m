%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_mode.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_mode.

:- interface.

:- import_module mh_type.
:- import_module mh_relation.
:- import_module mh_index.

:- import_module list.


%-----------------------------------------------------------------------------%

:- type mh_mode 
	--->	in	
	;		out
	;		inout(relation_mode)
	;		di
	;		uo
	;		unused.
	
:- type mode_type ---> mh_type :: mh_mode.

:- type relation_mode == list(mode_type).

:- type predicate_mode == list(mode_type).

:- type function_mode == predicate_mode -> mode_type.





