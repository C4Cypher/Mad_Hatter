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
	;		unused.
	
:- type mode_type ---> mh_type :: mh_mode.

:- type relation_mode == list(mode_type).

:- type proc_mode
	---> 	predicate_mode(relation_mode)
	;		function_mode relation_mode -> mode_type.

:- type predicate_mode =< proc_mode
	---> predicate_mode(relation_mode).
	
:- type function_mode =< proc_mode
	---> relation_mode -> mode_type.





