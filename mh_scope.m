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
	--->	root_scope(var_id_set, scope_context, var_names)
	
	% All child scopes contain a refrence to their parent scope
	;		rule_scope(mh_var_set, mh_var_set, scope_context, mh_scope). 


/*	
:- func scope_args(mh_scope) = mh_var_set.
:- func scope_local(mh_scope) = mh_var_set.
:- func scope_context(mh_scope) = scope_context.
:- func scope_var_names(mh_scope) = var_names.
:- func parent_scope(mh_scope) = mh_scope is semidet.
*/

%-----------------------------------------------------------------------------%
% Global scope

% Variable names are root to named clauses

:- inst root_scope ---> root_scope(ground, ground, ground).

:- type root_scope =< mh_scope
	--->	root_scope(var_id_set, scope_context, var_names).

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

