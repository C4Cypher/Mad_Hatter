%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
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

:- import_module array.
:- use_module term.

:- import_module mh_term.
:- import_module mh_var_set.

%-----------------------------------------------------------------------------%
% Scope

:- type mh_scope 
	--->	scope(mh_var_set, mh_var_set, scope_context, var_names)
	;		scope(mh_var_set, mh_var_set, scope_context, var_names, mh_scope).
	
:- func scope_args(mh_scope) = mh_var_set.
:- func scope_local(mh_scope) = mh_var_set.
:- func scope_context(mh_scope) = scope_context.
:- func scope_var_names(mh_scope) = var_names.
:- func parent_scope(mh_scope) = mh_scope is semidet.

%-----------------------------------------------------------------------------%
% Variable names

:- type var_names == array(string).

%-----------------------------------------------------------------------------%
% Scope context

:- type scope_context == term.context.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

