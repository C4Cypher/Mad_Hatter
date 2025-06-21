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

:- import_module varset.

:- import_module mh_context.
:- import_module mh_var_map.
:- import_module mh_term.


:- type mr_varset(T) == varset.varset(T).
:- type mr_varset == varset.varset.

%-----------------------------------------------------------------------------%
% Scope

:- type mh_scope.
 
	% Throws an exception if input mr_varset does not contain a complete set 
	% of variable ids from 1 to N. Ignores any variable bindings in mr_varset
:- func root_scope_from_mr_varset(mh_context, mr_varset) = mh_scope.
:- pred root_scope_from_mr_varset(mh_context::in, mr_varset::in, mh_scope::out)
	is det.
	

%-----------------------------------------------------------------------------%
% Scope context


:- func scope_context(mh_scope) = mh_context.
:- pred scope_context(mh_scope::in, mh_context::out) is det.

	% determines if the given scope is a child of another context
	% is_child(X), not is_root(X) <=> not is_child(X), is_root(X).
:- pred is_child(mh_scope::in) is semidet.
:- pred is_root(mh_scope::in) is semidet.

	% If the provided scope is a root context, returns the same value as
	% scope_context
:- func root_context(mh_scope) = mh_context.
:- pred root_context(mh_scope::in, mh_context::out) is det.

/* unimplemented


:- func scope_vars(mh_scope) = mh_var_set.
:- pred scope_vars(mh_scope::in, mh_var_set::out) is det.


:- func scope_var_names(mh_scope) = var_names.
:- func parent_scope(mh_scope) = mh_scope is semidet.

% Fails if contains vars that are not members of parent scopes
:- pred valid_scope(mh_scope::in) is semidet. 


% Fails if the variables in the provided term do not match those for the given
% Scope
:- pred valid_term_scope(mh_term::in, mh_scope::in).

:- func child_term_scope(mh_term, mh_scope) = mh_scope.

:- func nested_term_scope(mh_term, mh_scope) = mh_scope.
*/

%-----------------------------------------------------------------------------%
% Variable names

:- type var_names == mh_var_map(string).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.

:- import_module mh_var_id.
:- import_module mh_var_set.
:- import_module mh_mercury_term. % for mr_var.

%-----------------------------------------------------------------------------%
% Scope

:- type mh_scope 
	--->	root_scope(mh_context, var_id_set, var_names)
		%If no child context, default to parent
	;		child_scope(mh_scope, maybe(mh_context), mh_var_set) 
	;		extended_scope(scope_car :: mh_scope, scope_cdr :: mh_scope). 


:- func scope_cons(mh_scope, mh_scope) = mh_scope.
:- mode scope_cons(in, in) = out is det.
:- mode scope_cons(out, out) = in is semidet.

scope_cons(Car, Cdr) = extended_scope(Car, Cdr).

root_scope_from_mr_varset(Ctx, MrVarSet) = root_scope(Ctx, IdSet, Names) :-
	new_scope_vars(MrVarSet, vars(MrVarSet), empty_var_set, VarSet,	
		empty_var_map, Names),
	(if complete_var_set(CompleteSet, VarSet) 
	then IdSet = CompleteSet
	else error($pred, 
		"mr_varset did not contain a complete, continuous set of variable "++
		"ids starting at 1")
	).
	
root_scope_from_mr_varset(Ctx, MrVarSet, 
	root_scope_from_mr_varset(Ctx, MrVarSet)).
	
	
	% new_scope_vars(VarList, !LastId, !VarSet, !Names)
:- pred new_scope_vars(
		mr_varset::in, list(mr_var)::in, 
		mh_var_set::in, mh_var_set::out,
		var_names::in, var_names::out
	) is det.
		
new_scope_vars(_, [], !VarSet, !Names).

new_scope_vars(MrVarset, [MrVar | Vars], !VarSet, !Names) :-
	% given that varset.vars should never produce duplicate var_id's, I'm
	% skipping the check to see if the mh_var_set already contains it
	var_set_merge_id(NewId@mr_var_id(MrVar), !VarSet), 
	(if search_name(MrVarset, MrVar, Name)
	then
		det_id_insert(NewId, Name, !Names)
	else true
	),
	new_scope_vars(MrVarset, Vars, !VarSet, !Names).
	
%-----------------------------------------------------------------------------%
% Scope context

scope_context(root_scope(Ctx, _, _)) = Ctx.
scope_context(child_scope(Parent, MaybCtx, _)) = 
	(if MaybCtx = yes(Ctx)
	then Ctx
	else scope_context(Parent)
	).
scope_context(extended_scope(Car, _)) = scope_context(Car).

scope_context(Scope, scope_context(Scope)).

is_child(child_scope(_, _, _)).
is_child(extended_scope(Car, _)) :- is_child(Car).

is_root(Scope) :- not is_child(Scope).

root_context(root_scope(Ctx, _, _)) = Ctx.
root_context(child_scope(Parent, MaybCtx, _)) = root_context(Parent).
root_context(extended_scope(Car, _)) = root_context(Car).

root_context(Scope, root_context(Scope)).
	

