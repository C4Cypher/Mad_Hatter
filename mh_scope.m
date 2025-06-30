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
:- import_module list.

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
	
	%TODO: construct a child scope by providing a scope and a varset
	%TODO: construct a root scope by extending it with another scope
	
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

:- pred scope_contains_var_semidet(mh_scope::in, mh_var::in) is semidet.


:- func scope_vars(mh_scope) = mh_var_set.
:- pred scope_vars(mh_scope::in, mh_var_set::out) is det.

%-----------------------------------------------------------------------------%
% Variable names

	% Retreives the rootmost name assigned to a variable in a scope, fails
	% if there is no name assigned to said varaible
:- func var_name(mh_scope, mh_var) = string is semidet.
:- pred var_name(mh_scope::in, mh_var::in, string::out) is semidet.

:- type var_names == mh_var_map(string).


/* unimplemented

	% produces an mh_var_map(string) of variables to root names equivalent
	% to calling var_name for each variable present in a scope
:- func var_name_map(mh_scope) = var_names.
:- pred var_name_map(mh_scope::in, var_names::out) is det.





% Fails if contains vars that are not members of parent scopes
:- pred valid_scope(mh_scope::in) is semidet. 


% Fails if the variables in the provided term do not match those for the given
% Scope
:- pred valid_term_scope(mh_term::in, mh_scope::in).

:- func child_term_scope(mh_term, mh_scope) = mh_scope.

:- func nested_term_scope(mh_term, mh_scope) = mh_scope.
*/


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



scope_contains_var(Scope, var(ID)) :- scope_contains_id(Scope, ID). 

scope_contains_var_semidet(Scope, Var) :- 
	scope_contains_var(Scope, Var).
	
:- pred scope_contains_id(mh_scope, var_id).
:- mode scope_contains_id(in, in) is semidet.
:- mode scope_contains_id(in, out) is nondet.

scope_contains_id(Scope, ID) :-
	require_complete_switch [Scope] (
		(	Scope = root_scope(_, _, _) ; Scope = extended_scope(_, _) ),
		contains_var_id(root_scope_id_set(Scope), ID)
	;
		Scope = child_scope(_, _, VarSet), var_set_contains_id(VarSet, ID)
	).
	
:- pragma inline(scope_contains_id/2).

scope_vars(root_scope(_, IDSet, _)) = complete_var_set(IDSet).
scope_vars(child_scope(_, _, VarSet)) = VarSet.
scope_vars(extended_scope(_, _)@Scope) = 
	complete_var_set(root_scope_id_set(Scope)).
	
scope_vars(Scope, scope_vars(Scope)).

%-----------------------------------------------------------------------------%
% Variable names

var_name(Scope, Var) = Name :- var_name(Scope, Var, Name).
var_name(Scope, var(ID), Name) :- id_name(Scope, ID, Name).

:- pred id_name(mh_scope::in, var_id::in, string::out) is semidet.

id_name(root_scope(_, _, VarNames), ID, id_search(VarNames, ID)).
id_name(extended_scope(Car, Cdr), ID, Name) :-
	(if scope_contains_id(Car, ID)
	then id_name(Car, ID, Name)
	else
		% Shift the index left by the weight of the Car scope
		CarOffset = offset_from_id_set(root_scope_id_set(Car)),
		var_id_offset(CdrID, ID, CarOffset),
		sparse_id_name(Cdr, CdrID, Name)
	).
	
id_name(child_scope(Parent, _, ChildVarSet), ID, Name)  :-
	%Ensure that the ID actuaally belongs to the child before looking up the
	%Name in the parent
	var_set_contains_id(ChildVarSet, ID),
	id_name(Parent, ID, Name). 

% :- pred id_name_nondet(mh_scope::in, var_id::in, string::out) is nondet.

:- pred sparse_id_name(mh_scope::in, var_id::in, string::out) is semidet.

sparse_id_name(Scope, ID, Name) :-
	% Due to the fact that variables in root scopes and extended scopes are 
	% already church encoded (1 .. N with no gaps), their sparse indexes are
	% also their direct indexes
	require_complete_switch [Scope] (
		(	Scope = root_scope(_, _, _) ; Scope = extended_scope(_, _) ),
		id_name(Scope, ID, Name)
	;
		Scope = child_scope(Parent, _, VarSet),
		% due to the fact that we're indexing by church index, index the
		% members of the child scope's varset by order, not by literal
		% var_id
		id_name(Parent, id_reverse_church_index(ID, VarSet), Name)
	).
