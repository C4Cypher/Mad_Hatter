%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_scope.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_scope.

:- interface.

:- import_module array.


% :- import_module mh_term.
:- import_module mh_mercury_term.
:- import_module mh_var_id.
:- import_module mh_var_set.
% :- import_module mh_arity.

%-----------------------------------------------------------------------------%
% Scope

:- type mh_scope 
	--->	root_scope(context :: scope_context, names :: var_names),
	;		child_scope(parent :: mh_scope, vars :: mh_var_set). 


:- func scope_vars(mh_scope) = mh_var_set.
:- func scope_context(mh_scope) = scope_context.
:- func scope_var_names(mh_scope) = var_names.
:- func parent_scope(mh_scope) = mh_scope is semidet.

% Fails if contains vars that are not members of parent scopes
:- pred valid_scope(mh_scope::in) is semidet. 


%-----------------------------------------------------------------------------%
% Variable names

:- type var_names == array(string).

%-----------------------------------------------------------------------------%
% Scope context

:- type scope_context == mr_context.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%
% Scope

