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

:- import_module maybe.
:- import_module varset.

:- import_module mh_term.
:- import_module mh_context.
:- import_module mh_var_map.
:- import_module mh_var_id.
:- import_module mh_var_set.


:- type mr_varset(T) == varset.varset(T).
:- type mr_varset == varset.varset.

%-----------------------------------------------------------------------------%
% Scope

:- type mh_scope 
	--->	no_scope
	;		root_scope(root_context :: mh_context, 
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
 
	% Throws an exception if input mr_varset does not contain a complete set 
	% of variable ids from 1 to N.
:- func root_scope_from_mr_varset(mh_context, mr_varset) = mh_scope.
:- pred root_scope_from_mr_varset(mh_context::in, mr_varset::in, mh_scope::out)
	is det.
	

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

:- type var_names == var_map(string).

%-----------------------------------------------------------------------------%
% Scope context

%:- type scope_context == mh_context.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.

:- import_module mh_mercury_term. % for mr_var.

%-----------------------------------------------------------------------------%
% Scope

scope_cons(Car, Cdr) = extended_scope(Car, Cdr).

root_scope_from_mr_varset(Ctx, MrVarSet) = root_scope(Ctx, IdSet, Names) :-
	new_scope_vars(MrVarSet, vars(MrVarSet), empty_var_set, VarSet,	
		empty_var_map, Names),
	(if complete_var_set(IdSet, VarSet) then true
	else error($pred, 
		"mr_varset did not contain a complete, continuous set of variable "++
		"ids starting at 1")
	).
	
	
	% new_scope_vars(VarList, !LastId, !VarSet, !Names)
:- pred new_scope_vars(
		mr_varset::in, list(mh_var)::in, 
		mh_var_set::in, mh_var_set::out,
		var_names::in, var_names::out
	) is det.
		
new_scope_vars(_, [], !LastId, !VarSet, !Names).

new_scope_vars(MrVarset, [MrVar | Vars], !LastId, !VarSet, !Names) :-
	% given that varset.vars should never produce duplicate var_id's, I'm
	% skipping the check to see if the mh_var_set already contains it
	var_set_merge_id(NewId, !VarSet), 
	(if search_name(MrVarset, MrVar, Name)
	then
		det_id_insert(NewId, Name, !Names)
	else true
	),
	new_scope_vars(MrVarset, Vars, !LastId, !VarSet, !Names).
	

