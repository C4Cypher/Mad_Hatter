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
:- import_module mh_var_set.
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
	
	% determines if the given scope is a child of another context
:- pred is_child(mh_scope::in) is semidet.

	% is_root(X) :- not is_child.
:- pred is_root(mh_scope::in) is semidet.

	% if a scope has a parent, return it
:- func parent(mh_scope) = mh_scope is semidet.

	% parent(Child, Parent). Note that Child scopes cannot be determined from
	% the parent.
:- pred parent(mh_scope::in, mh_scope::out) is semidet.
	

%-----------------------------------------------------------------------------%
% Scope context

	% If a child scope has it's own context, return it, otherwise return the
	% context of it's parent scope
:- func scope_context(mh_scope) = mh_context.
:- pred scope_context(mh_scope::in, mh_context::out) is det.

	% If the provided scope is a root context, returns the same value as
	% scope_context
:- func root_context(mh_scope) = mh_context.
:- pred root_context(mh_scope::in, mh_context::out) is det.

%-----------------------------------------------------------------------------%
% Scope variables

:- func scope_var_count(mh_scope) = int.

:- pred scope_contains_var(mh_scope, mh_var).
:- mode scope_contains_var(in, in) is semidet.
:- mode scope_contains_var(in, out) is nondet.

:- func scope_vars(mh_scope) = mh_var_set.
:- pred scope_vars(mh_scope::in, mh_var_set::out) is det.

%-----------------------------------------------------------------------------%
% Variable names

:- func var_name(mh_scope, mh_var) = string is semidet.
:- pred var_name(mh_scope::in, mh_var::in, string::out) is semidet.


/* unimplemented

:- func scope_var_names(mh_scope) = var_names.

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
:- import_module int.

:- import_module mh_var_id.
:- import_module mh_mercury_term. % for mr_var.

%-----------------------------------------------------------------------------%
% Scope

:- type mh_scope 
	--->	root_scope(mh_context, var_id_set, var_names)
	;		extended_scope(scope_car :: mh_scope, scope_cdr :: mh_scope)
		%If no child context, default to parent
	;		child_scope(mh_scope, maybe(mh_context), mh_var_set). 

/* Compiler error, skipping the subtyping
%%% UNDER NO CIRCUMSTANCES SHOULD A CHILD SCOPE BE PLACED AS THE FIRST %%%
%%% ARGUMENT OF extended_scope/2


:- inst root_scope 
	---> 	root_scope(ground, ground, ground)
	;		extended_scope(ground, ground).
	
:- type mh_root_scope =< mh_scope 
	--->	root_scope(mh_context, var_id_set, var_names)
	;		extended_scope(scope_car :: mh_root_scope, scope_cdr :: mh_scope).
	
:- mode scope_is_root == ground >> root_scope.

:- pred scope_is_root(mh_scope::scope_is_root) is semidet.

scope_is_root(root_scope(_, _, _)).
scope_is_root(extended_scope(_, _)).
*/


:- func scope_cons(mh_scope, mh_scope) = mh_scope.
:- mode scope_cons(in, in) = out is det.
:- mode scope_cons(out, out) = in is semidet.

scope_cons(Car, Cdr) = extended_scope(Car, Cdr).

root_scope_from_mr_varset(Ctx, MrVarSet) = root_scope(Ctx, IDSet, Names) :-
	new_scope_vars(MrVarSet, vars(MrVarSet), empty_var_set, VarSet,	
		empty_var_map, Names),
	(if complete_var_set(CompleteSet, VarSet) 
	then IDSet = CompleteSet
	else error($pred, 
		"mr_varset did not contain a complete, continuous set of variable "++
		"ids starting at 1")
	).
	
root_scope_from_mr_varset(Ctx, MrVarSet, 
	root_scope_from_mr_varset(Ctx, MrVarSet)).
	
	
	% new_scope_vars(VarList, !LastID, !VarSet, !Names)
:- pred new_scope_vars(
		mr_varset::in, list(mr_var)::in, 
		mh_var_set::in, mh_var_set::out,
		var_names::in, var_names::out
	) is det.
		
new_scope_vars(_, [], !VarSet, !Names).

new_scope_vars(MrVarset, [MrVar | Vars], !VarSet, !Names) :-
	% given that varset.vars should never produce duplicate var_id's, I'm
	% skipping the check to see if the mh_var_set already contains it
	var_set_merge_id(NewID@mr_var_id(MrVar), !VarSet), 
	(if search_name(MrVarset, MrVar, Name)
	then
		det_id_insert(NewID, Name, !Names)
	else true
	),
	new_scope_vars(MrVarset, Vars, !VarSet, !Names).

is_child(child_scope(_, _, _)).
is_child(extended_scope(Car, _)) :- is_child(Car).

is_root(Scope) :- not is_child(Scope).

parent(child_scope(Parent, _, _)) = Parent.
parent(extended_scope(Child, _)) = parent(Child). 


parent(Child, parent(Child)).
	
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

root_context(root_scope(Ctx, _, _)) = Ctx.
root_context(child_scope(Parent, _, _)) = root_context(Parent).
root_context(extended_scope(Car, _)) = root_context(Car).

root_context(Scope, root_context(Scope)).

%-----------------------------------------------------------------------------%
% Scope variables

scope_var_count(root_scope(_, IDSet, _)) = var_id_count(IDSet).
scope_var_count(extended_scope(Car, Cdr)) = 
	scope_var_count(Car) + scope_var_count(Cdr).
scope_var_count(child_scope(_, _, VarSet)) = var_set_count(VarSet).

:- func root_scope_id_set(mh_scope) = var_id_set.

root_scope_id_set(root_scope(_, IDSet, _)) = IDSet.
root_scope_id_set(extended_scope(Car, Cdr)) = 
	append_var_id_sets(root_scope_id_set(Car), sparse_scope_id_set(Cdr)).
root_scope_id_set(child_scope(_, _, _)) = unexpected($module, $pred, 
		"Attempted to derive root scope ID set from child scope.").

:- func sparse_scope_id_set(mh_scope) = var_id_set.
sparse_scope_id_set(root_scope(_, IDSet, _)) = IDSet.
sparse_scope_id_set(Scope@extended_scope(_, _)) = root_scope_id_set(Scope).
sparse_scope_id_set(child_scope(_, _, VarSet)) = 
	generate_sparse_id_set_for_var_set(VarSet).



scope_contains_var(Scope, Var) :-
	require_complete_switch [Scope] (
		(	Scope = root_scope(_, _, _) ; Scope = extended_scope(_, _) ),
		Var = var(ID), 
		contains_var_id(root_scope_id_set(Scope), ID)
	;
		Scope = child_scope(_, _, VarSet), var_set_contains(VarSet, Var)
	).

scope_vars(root_scope(_, IDSet, _)) = complete_var_set(IDSet).
scope_vars(child_scope(_, _, VarSet)) = VarSet.
scope_vars(extended_scope(_, _)@Scope) = 
	complete_var_set(root_scope_id_set(Scope)).
	
scope_vars(Scope, scope_vars(Scope)).

%-----------------------------------------------------------------------------%
% Variable names

var_name(root_scope(_, _, VarNames), Var) = search(VarNames, Var).
var_name(extended_scope(Car, Cdr)) = Name :-
	(if scope_contains_var(Car, Var)
	then var_name(Car, Var, Name)
	else
		CarIDSet = root_scope_id_set(Car),
		%Taking a break, need to reverse sparse index the var name in CDR and bind it to Name
	)
var_name(child_scope(Parent, _, ChildVarSet), Var) = var_name(Parent, Var) :-
	var_set_contains(ChildVarSet, Var).
		
var_name(Scope, Var, var_name(Scope, Var)).