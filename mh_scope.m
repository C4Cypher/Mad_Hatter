%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
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
:- import_module maybe.


% :- import_module mh_term.
:- import_module mh_context.
%:- import_module mh_var_id.
:- import_module mh_var_set.
% :- import_module mh_arity.

%-----------------------------------------------------------------------------%
% Scope

:- type mh_scope 
	--->	no_scope
	;		root_scope(root_context :: mh_context, names :: var_names)
	;		child_scope(
				parent :: mh_scope, 
				child_context::maybe(mh_context), %If no, default to parent
				vars :: mh_var_set
			). 
% TODO: Add constructors for extended scopes with inlined calls
% rename root_scope to clause_scope?

/* unimplemented
:- func scope_vars(mh_scope) = mh_var_set.

:- func scope_context(mh_scope) = mh_context.

% Different from scope_context in the event of a nested lambda clause.
:- func clause_context(mh_scope) = mh_context. 

:- func scope_var_names(mh_scope) = var_names.
:- func parent_scope(mh_scope) = mh_scope is semidet.

% Fails if contains vars that are not members of parent scopes
:- pred valid_scope(mh_scope::in) is semidet. 
*/

%-----------------------------------------------------------------------------%
% Variable names

:- type var_names == array(string).

%-----------------------------------------------------------------------------%
% Scope context

%:- type scope_context == mh_context.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%
% Scope

