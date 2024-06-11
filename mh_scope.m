%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_scope.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_scope.

:- interface.

:- import_module map.

:- import_module mh_term.

:- type scope
	--->	new_scope(mh_term)
	;		scope(mh_term