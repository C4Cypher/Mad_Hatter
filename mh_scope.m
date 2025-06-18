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
:- import_module varset.


:- import_module mh_term.
:- import_module mh_context.
:- import_module mh_var_id.
:- import_module mh_var_set.


:- type mr_varset(T) == varset.varset(T).
:- type mr_varset == varset.varset.

%-----------------------------------------------------------------------------%
% Scope

:- type mh_scope 
	--->	no_scope
	;		root_scope(	root_context :: mh_context, 
						id_set :: var_id_set, 
						names :: var_names
					)
	;		child_scope(
				parent :: mh_scope, 
				child_context::maybe(mh_context), %If no, default to parent
				vars :: mh_var_set
			)
	;		extended_scope(scope_car :: mh_scope, scope_cdr :: mh_scope). 


:- func scope_cons(mh_scope, mh_scope) = mh_scope.
:- mode scope_cons(in, in) = out is det.
:- mode scope_cons(out, out) = in is semidet.
 
	% Throws an exception if input term contains variables not present in 
	% varset.
:- func new_root_scope(mh_term, mh_context, mr_varset) = mh_scope.
:- pred new_root_scope(mh_term::in, mh_context::in, mr_varset::in, 
	mh_scope::out) is det.
	

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

