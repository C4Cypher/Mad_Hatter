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
:- import_module maybe.
:- import_module list.

:- import_module mh_context.
:- import_module mh_var_map.
:- import_module mh_var_set.
:- import_module mh_substitution.
:- import_module mh_term.
% :- import_module mh_relation.

:- type mr_varset(T) == varset.varset(T).
:- type mr_varset == varset.varset.

%-----------------------------------------------------------------------------%
% Scope

% The mh_scope type is a container for three things: a context (usually file
% name and line number), a set of variables, and a mapping of some of those
% variables to string names.

% A 'root' scope represents a complete set of variables from id 1 to N, the
% complete set of variables for a clause

% 'root' scopes can be extended with other root scopes, for sake of discussion
% I like to refer to the scope being extended as the 'Car' and the scope being
% embedded to extend is the 'Cdr'.  The number of variables contained by the 
% Cdr scope are appended to the end of the 'Car' scope, and renamings are 
% additionally provided to re map any terms under the 'Cdr' scope to
% match the newly extended 'Cons' scope 

% Child scopes represent a subset of another scope, unlike root scopes, they
% may contain a sparse collection of variable id's ... however, all variables
% in a child scope must also be present in a parent.  I intend child scopes to
% represent lexical scopes, reflecting partial visiblility of variables, 
% 'views' as it were, in subterms of a parent term

:- type mh_scope.

	% Throws an exception if a scope contains name mappings outside of the 
	% scope, if the root (CAR) of an extended scope is a child, or if a child 
	% scope contains variables outside the scope of it's parent.
:- pred require_valid_scope(mh_scope::in) is det.

% These two calls assume that mh_context refers to specific origins for
% variables in source code, and that each root context is unique to the
% root that was constructed with it.  The following calls may return false
% positives if the contexts the scopes were created with are not unique to
% said scopes.

	% True of two scopes if each variable in both scopes share the same 
	% context.  Two scopes may have different structures, or variable namings
	% but if the variables within are the same, and came from the same place
	% they can be considered the same scopes.
:- pred equivalent_scopes(mh_scope::in, mh_scope::in) is semidet.

	% Succeeds if the first scope is compatable with the second, true if
	% each variable in the first scope is present in the second scope and
	% has the same context for it's counterpart in the second scope.
:- pred compatable_scope(mh_scope::in, mh_scope::in) is semidet.  


%-----------------------------------------------------------------------------%
% Root Scopes

	% is_root(X) :- not is_child.
:- pred is_root(mh_scope::in) is semidet.

	% Throws an exception if provided a child scope.
:- pred require_root_scope(mh_scope::in) is det.

	% As above, but passes the provided $module and $pred values to the message
	%	require_root_scope(Module, Pred, Scope).
:- pred require_root_scope(string::in, string::in, mh_scope::in) is det.
 
	% Throws an exception if input mr_varset does not contain a complete set 
	% of variable ids from 1 to N. Ignores any variable bindings in mr_varset
:- func root_scope_from_mr_varset(/*mh_relation,*/ mh_context, mr_varset) =
	mh_scope.
:- pred root_scope_from_mr_varset(/*mh_relation::in,*/ mh_context::in, 
	mr_varset::in, mh_scope::out) is det.
	
	% A variant of the above call that renames the variables into a church 
	% encoding, if not already, also if the variables were not already
	% church endcoded, provides a variable renaming that will normalize the 
	% variables to the new church encoding in the root scope
:- pred root_scope_from_mr_varset(/*mh_relation::in,*/ mh_context::in,
	mr_varset::in, mh_scope::out, maybe(mh_renaming)::out) is det.

	% Generate a root scope from a context, varset and name mapping. If the
	% varset is not church encoded (complete from 1 to N), provide a variable
	% renaming that will normalize the variables to the new church encoding
	% in the root scope
:- pred root_scope_from_var_set(/*mh_relation::in,*/ mh_context::in, 
	mh_var_set::in, var_names::in, mh_scope::out, maybe(mh_renaming)::out) 
	is det.

	
%-----------------------------------------------------------------------------%
% Extended Root Scopes

	% Succeeds if a root scope is extended, fails if provided a child scope.
:- pred is_extended(mh_scope::in) is semidet.
	
	% All calls to extend root scopes throw an exception if provided a child
	% scope as the first argument
	
	% Extend a root scope by inlining another scope, provides a renaming to
	% re-map the variables under the inlined scope under the new scope
:- pred extend_root_scope(mh_scope::in, mh_scope::in, mh_scope::out, 
	mh_renaming::out) is det.

%TODO: extend a root scope by providing a var_set, a prefix and offset
% May implement in another module

	% Decomposes an extended scope into it's root and the scopes that extend
	% it.  If the scope is not extended, returns the input scope as root and
	% an empty list as the extended portion.
:- pred scope_composition(mh_scope::in, mh_scope::out, list(mh_scope)::out)
	is det.
	
	% Similar to scope_composition/3, but returns a single list with the root
	% at the front of the list. If the scope is not extended, returns a list
	% with the scope as the single item in it.
:- func decompose_scope(mh_scope) = list(mh_scope).

	% (was) Prerequisite test for compatable_scope/2, true if the second scope 
	% is an extended version of the first.  Fails if the scopes are the same 
	% or if either scope is a child scope
:- pred extended_scope_subset(mh_scope::in, mh_scope::in) is semidet.
	
%-----------------------------------------------------------------------------%
% Child Scopes	
	
	% Construct a child scope by providing a scope and a varset.  Throws an
	% exception if the provided mh_var_set is not a subset of the parent scope.
	% If there is no provided context, it will default to the parent's context.
	% Child scopes may use other child scopes as their parent.
:- pred create_child_scope(mh_scope::in, maybe(mh_context)::in, mh_var_set::in,
	mh_scope::out) is det.
	
:- func create_child_scope(mh_scope, maybe(mh_context), mh_var_set) = mh_scope.

	
	% determines if the given scope is a child of another context
:- pred is_child(mh_scope::in) is semidet.


	% if a scope has a parent, return it
:- func parent(mh_scope) = mh_scope is semidet.

	% parent(Child, Parent). Note that Child scopes cannot be determined from
	% the parent.
:- pred parent(mh_scope::in, mh_scope::out) is semidet.

	% if the scope is a root, return itself, otherwise, if it is a child
	% return the root_ancestor of it's parent.
:- func root_ancestor(mh_scope) = mh_scope.
:- pred root_ancestor(mh_scope::in, mh_scope::out) is det.


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

:- pred semidet_scope_contains_var(mh_scope::in, mh_var::in) is semidet.


:- func scope_vars(mh_scope) = mh_var_set.
:- pred scope_vars(mh_scope::in, mh_var_set::out) is det.


	% Given that root scopes can be extended, not all variables in a scope may
	% originate from the same root scope. This call allows us to query a 
	% scope as to the originating scope of a specific variable. When called
	% on a child scope, the root ancestor will be queried. Throws an exception
	% if variable is not a member of the scope.
:- func scope_variable_root(mh_scope, mh_var) = mh_scope.

:- pred scope_variable_root(mh_scope::in, mh_var::in, mh_scope::out) is det.

	% As above, but fails if variable is not a member of the scope.
:- func semidet_scope_variable_root(mh_scope, mh_var) = mh_scope is semidet.

:- pred semidet_scope_variable_root(mh_scope::in, mh_var::in, mh_scope::out)
	is semidet.	

	% Derives the context from results of scope_variable_context, effectively
	% scope_variable_context(S, V) = scope_context(scope_variable_root(S, V)).
:- func scope_variable_context(mh_scope, mh_var) = mh_context.

:- pred scope_variable_context(mh_scope::in, mh_var::in, mh_context::out) 
	is det.

	% Likewise, fails if variable is not a member of the provided scope.
:- func semidet_scope_variable_context(mh_scope, mh_var) = mh_context
	is semidet.

:- pred semidet_scope_variable_context(mh_scope::in, mh_var::in, 
	mh_context::out) is semidet.

%-----------------------------------------------------------------------------%
% Variable names

	% Retreives the rootmost name assigned to a variable in a scope, fails
	% if there is no name assigned to said varaible
:- func var_name(mh_scope, mh_var) = string is semidet.
:- pred var_name(mh_scope::in, mh_var::in, string::out) is semidet.

:- type var_names == mh_var_map(string).

	% produce a map of variables to names equivalent
	% to calling var_name for each variable present in a scope
:- func var_names(mh_scope) = var_names.
:- pred var_names(mh_scope::in, var_names::out) is det.



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

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

%%% UNDER NO CIRCUMSTANCES SHOULD A CHILD SCOPE BE PLACED AS THE FIRST %%%
%%% ARGUMENT OF extended_scope/2, check using is_root/1 and is_child/1 %%%
/* Compiler error, skipping the subtyping, might reintroduce later, doubtful

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
	
:- inst child_scope ---> child_scope(ground, ground, ground).

:- type mh_child_scope 
	--->	child_scope(mh_scope, maybe(mh_context), mh_var_set).
	
:- mode scope_is_child == ground >> child_scope.

:- pred scope_is_root(mh_scope::scope_is_child) is semidet.

scope_is_child(child_scope(_, _, _)).
*/

require_valid_scope(root_scope(_context, IDSet, Names)) :-
	(if first(Names, ID, _, Iterator)
	then
		valid_root_names(IDSet, Names, ID, Iterator)
	else true
	).
	
:- pred valid_root_names(var_id_set::in, var_names::in, var_id::in, 
	var_map_iterator::in) is det.
	
valid_root_names(IDSet, Names, ID, Iterator) :-
	(if contains_var_id(IDSet, ID)
	then
		(if next(Names, NextID, _, Iterator, NextIterator)
		then valid_root_names(IDSet, Names, NextID, NextIterator)
		else true
		)
	else
		unexpected($module, "require_valid_scope/1", 
			"Scope contained name mappings to variables not in scope.")
	).
	
require_valid_scope(extended_scope(Car, Cdr)) :-
	(if Car = child_scope(_, _, _)
	then
		unexpected($module, $pred, "Attempt to extend child scope")
	else
		require_valid_scope(Car),
		require_valid_scope(Cdr)
	).
	
require_valid_scope(child_scope(Parent, _, VarSet)) :-
	require_valid_scope(Parent),
	valid_child_scope(Parent, VarSet).

:- pred valid_child_scope(mh_scope::in, mh_var_set::in) is det.

valid_child_scope(Parent, VarSet) :-
	(if scope_contains_id(Parent, var_set_first_id(VarSet)@ID)
	then (if var_set_remove_id(ID, VarSet, NextVarSet)
		then (if is_empty(NextVarSet)
			then true
			else valid_child_scope(Parent, NextVarSet)
			)
		else unexpected($module, $pred, 
			"Failed to remove first id from set, should be impossible")
		)
	else unexpected($module, "require_valid_scope/1",
		"child scope contains variables not in parent scope")
	).
	

equivalent_scopes(S, S).
equivalent_scopes(S1, S2) :- 
	%scope_environment(S1) = scope_environment(S2), % Replace this, see above
	scope_vars(S1, Vars), scope_vars(S2, Vars),
	all_true(equivalent_var_context(S1, S2), Vars).
	
:- pred equivalent_var_context(mh_scope::in, mh_scope::in, mh_var::in) is semidet.

equivalent_var_context(S1, S2, V) :-
	semidet_scope_variable_context(S1, V, Ctx),
	semidet_scope_variable_context(S2, V, Ctx).

compatable_scope(S1, S2) :- 
	all_true(equivalent_var_context(S1, S2), scope_vars(S1)).


%-----------------------------------------------------------------------------%
% Root Scopes


is_root(Scope) :- not is_child(Scope).

require_root_scope(Scope) :- require_root_scope($module, $pred, Scope).

require_root_scope(Module, Pred, Scope) :- 
	if is_child(Scope)
	then
		unexpected(Module, Pred, "Child scope where root was required.")
	else true.

root_scope_from_mr_varset(Ctx, MrVarSet) = 
	root_scope(Ctx, IDSet, Names) :-
	
	new_scope_vars_from_mr_varset(MrVarSet, vars(MrVarSet), empty_var_set,
		VarSet,	empty_var_map, Names),
	(if complete_var_set(CompleteSet, VarSet) 
	then IDSet = CompleteSet
	else error($pred, 
		"mr_varset did not contain a complete, continuous set of variable "++
		"ids starting at 1")
	).
	
root_scope_from_mr_varset(Ctx, MrVarSet, 
	root_scope_from_mr_varset(Ctx, MrVarSet)).
	
root_scope_from_mr_varset(Ctx, MrVarSet, Scope, Ren) :-
	new_scope_vars_from_mr_varset(MrVarSet, vars(MrVarSet), empty_var_set,
		VarSet,	empty_var_map, Names),
	root_scope_from_var_set(Ctx, VarSet, Names, Scope, Ren).
	
	% new_scope_vars_from_mr_varset(VarList, !LastID, !VarSet, !Names)
:- pred new_scope_vars_from_mr_varset(
		mr_varset::in, list(mr_var)::in, 
		mh_var_set::in, mh_var_set::out,
		var_names::in, var_names::out
	) is det.
		
new_scope_vars_from_mr_varset(_, [], !VarSet, !Names).

new_scope_vars_from_mr_varset(MrVarset, [MrVar | Vars], !VarSet, !Names) :-
	% given that varset.vars should never produce duplicate var_id's, I'm
	% skipping the check to see if the mh_var_set already contains it
	var_set_merge_id(NewID@mr_var_id(MrVar), !VarSet), 
	(if search_name(MrVarset, MrVar, Name)
	then
		det_id_insert(NewID, Name, !Names)
	else true
	),
	new_scope_vars_from_mr_varset(MrVarset, Vars, !VarSet, !Names).
	
root_scope_from_var_set(Context, VarSet, Names, Scope, MaybRenaming) :-
	church_renaming_for_varset(VarSet, MaybRenaming, IDSet),
	(if MaybRenaming = yes(Renaming)
	then det_rename_var_map(Renaming, Names, NewNames)
	else NewNames = Names
	),
	Scope = root_scope(Context, IDSet, NewNames).
	
% Does NOT rename variables in varset that are already church encoded
% (1 .. N)
:- pred church_renaming_for_varset(mh_var_set::in, maybe(mh_renaming)::out, 
	var_id_set::out) is det.

church_renaming_for_varset(VarSet, Renaming, IDSet) :-
	%expect_non_empty_var_set(VarSet), 
	church_renaming_for_varset_loop(VarSet, init, IDMap, empty_var_id_set, 
		IDSet),
	(if empty_var_map(IDMap)
	then
		Renaming = no
	else
		Renaming = yes(ren_map(IDMap))
	).

:- pred church_renaming_for_varset_loop(mh_var_set::in,	mh_var_map(var_id)::in,
	mh_var_map(var_id)::out, var_id_set::in, var_id_set::out) is det.
	
church_renaming_for_varset_loop(VarSet, !IDMap, PrevIDSet, FullIDSet) :-
	(if is_empty(VarSet) 
	then FullIDSet = PrevIDSet
	else
		PrevID = last_var_id(PrevIDSet),
		CurrentID = next_var_id(PrevID) @ last_var_id(CurrentIDSet),
		var_set_first_id(ID, VarSet, NextVarSet),
		% If the variable id is already church encoded, don't map it
		(if ID = CurrentID
		then true
		else
			%var_set_first_id/3 should never produce duplicate IDs
			det_id_insert(ID, CurrentID, !IDMap)
		),
		church_renaming_for_varset_loop(NextVarSet, !IDMap, CurrentIDSet,
			FullIDSet)
	).
	
%-----------------------------------------------------------------------------%
% Extended Root Scopes

is_extended(extended_scope(_, _)).
	
extend_root_scope(Car, Cdr, extended_scope(Car, Cdr), Renaming) :-
	require_root_scope($module, $pred, Car),
	extended_renaming(scope_vars(Cdr), root_scope_id_set(Car), Renaming).
	
:- pred extended_renaming(mh_var_set::in, var_id_set::in, mh_renaming::out) 
	is det.

extended_renaming(Vars, CarSet, Ren) :-
	church_renaming_for_varset(Vars, MaybChurchRen, _CdrIDSet),
	ChurchRen = 
		(if MaybChurchRen = yes(HasRen)
		then HasRen
		else init_ren
		),
	Offset = offset_from_id_set(CarSet),
	fold_id(extended_renaming_loop(Offset, ChurchRen), Vars, mh_var_map.init,
		ExMap),
	Ren = ren_map(ExMap).
	
:- pred extended_renaming_loop(var_id_offset::in, mh_renaming::in, var_id::in,
	mh_var_map(var_id)::in, mh_var_map(var_id)::out) is det.
	
extended_renaming_loop(Offset, ChurchRen, FromID, !ExMap) :-
	(if ren_id_search(ChurchRen, FromID, FoundID)
	then ChurchID = FoundID
	else ChurchID = FromID
	),
	ToID = apply_var_id_offset(ChurchID, Offset),
	det_id_insert(FromID, ToID, !ExMap).
	
scope_composition(Scope, det_head(List), det_tail(List)) :-
	List = decompose_scope(Scope).

decompose_scope(Root@root_scope(_, _, _)) = [Root].
decompose_scope(extended_scope(Car, Cdr)) = decompose_scope(Car) ++ [Cdr].
decompose_scope(Child@child_scope(_, _, _)) = [Child].

extended_scope_subset(Subset, extended_scope(Subset, _)).
extended_scope_subset(Subset, extended_scope(Car, _)) :- 
	extended_scope_subset(Subset, Car).
	
	


%-----------------------------------------------------------------------------%
% Child Scopes		

create_child_scope(Parent, Ctx, Vars, child_scope(Parent, Ctx, Vars)) :-
	if all_true_id(semidet_scope_contains_id(Parent), Vars)
	then true
	else error($pred, "var_set for child contained variables not in parent.").
	
create_child_scope(Parent, Ctx, Vars) = Child :- 
	create_child_scope(Parent, Ctx, Vars, Child).

is_child(child_scope(_, _, _)).
is_child(extended_scope(Car, _)) :- is_child(Car).


parent(child_scope(Parent, _, _)) = Parent.

parent(Child, parent(Child)).

root_ancestor(Scope) = 
	(if parent(Scope, Parent) 
	then root_ancestor(Parent)
	else Scope
	).
	
root_ancestor(Scope, root_ancestor(Scope)).

	
	
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

scope_contains_var(Scope, var(ID)) :- scope_contains_id(Scope, ID). 

semidet_scope_contains_var(Scope, Var) :- 
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
	
:- pred semidet_scope_contains_id(mh_scope::in, var_id::in) is semidet.

semidet_scope_contains_id(Scope, ID) :- scope_contains_id(Scope, ID).


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
	
:- pragma inline(scope_contains_id/2).

scope_vars(root_scope(_, IDSet, _)) = complete_var_set(IDSet).
scope_vars(child_scope(_, _, VarSet)) = VarSet.
scope_vars(extended_scope(_, _)@Scope) = 
	complete_var_set(root_scope_id_set(Scope)).
	
scope_vars(Scope, scope_vars(Scope)).

scope_variable_root(Scope, Var) = 
	(if semidet_scope_variable_root(Scope, Var) = Root
	then Root
	else unexpected($module, $pred, 
		"Varaible was not a member of provided scope")).
		
scope_variable_root(Scope, Var, scope_variable_root(Scope, Var)).

semidet_scope_variable_root(root_scope(_, Set, _)@Root, var(ID)) = Root :-
	contains_var_id(Set, ID).
	
semidet_scope_variable_root(extended_scope(Car, Cdr), Var) = 
	(if semidet_scope_variable_root(Car, Var) = Root
	then Root
	else extended_variable_root(Car, Cdr, Var)
	).
	
:- func extended_variable_root(mh_scope, mh_scope, mh_var) = mh_scope 
	is semidet.

	% Determine the root of the variable in the Cdr, knowing that it has 
	% already been determined that said variable is not a member of the Car
extended_variable_root(Car, Cdr, var(ID)) = 
		semidet_scope_variable_root(Cdr, var(OffsetID)) :-
	% Shift the ID left by the weight of the Car
	var_id_offset(ID, OffsetID, car_offset(Car)).

:- pragma inline(extended_variable_root/3).
	
:- func car_offset(mh_scope) = var_id_offset.

car_offset(Scope) = offset_from_id_set(root_scope_id_set(Scope)).

:- pragma inline(car_offset/1).

semidet_scope_variable_root(child_scope(Parent, _, Set), Var@var(ID)) =
	semidet_scope_variable_root(Parent, Var) :- 
		var_set_contains_id(Set, ID).

semidet_scope_variable_root(Scope, Var, 
	semidet_scope_variable_root(Scope, Var)).
	
scope_variable_context(S, V) = scope_context(scope_variable_root(S, V)).

scope_variable_context(S, V, scope_variable_context(S, V)).

semidet_scope_variable_context(S, V) = 
	scope_context(semidet_scope_variable_root(S, V)).

semidet_scope_variable_context(S, V, semidet_scope_variable_context(S, V)).

	
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
		var_id_offset(CdrID, ID, car_offset(Car)),
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

var_names(root_scope(_, _, VarNames)) = VarNames.
var_names(extended_scope(Car, Cdr)) = VarNames :- some [!NameMap] (
	!:NameMap = var_names(Car),
	CarOffset = offset_from_id_set(root_scope_id_set(Car)), 
	CdrIDSet = sparse_scope_id_set(Cdr),
	names_from_cdr(Cdr, CarOffset, CdrIDSet, !NameMap),
	VarNames = !.NameMap	
).

:- pred names_from_cdr(mh_scope::in, var_id_offset::in, 
	var_id_set::in, var_names::in, var_names::out) is det.
	
names_from_cdr(Cdr, CarOffset, CdrIDSet, !Names) :-
	names_from_cdr_loop(Cdr, CarOffset, first_var_id, CdrIDSet, !Names).

:- pred names_from_cdr_loop(mh_scope::in, var_id_offset::in, 
	var_id::in, var_id_set::in, var_names::in, var_names::out) is det.	

names_from_cdr_loop(Cdr, CarOffset, Current, CdrIDSet, !Names) :-
	(if contains_var_id(CdrIDSet, Current)
	then
		(if sparse_id_name(Cdr, Current, Name)
		then
			var_id_offset(MapVarID, Current, CarOffset),
			% Will throw an exception on var_id collision, shouldn't happen
			det_id_insert(MapVarID, Name, !Names)
		else true
		),
		names_from_cdr_loop(Cdr, CarOffset, next_var_id(Current), CdrIDSet,
			!Names)
	else true
	).
	
var_names(child_scope(Parent, _, VarSet)) = VarNames :-
	var_names(Parent, ParentNames),
	fold_id(insert_parent_name(Parent), VarSet, ParentNames, VarNames).
	
:- pred insert_parent_name(mh_scope::in, var_id::in, var_names::in, 
	var_names::out) is det.
	
insert_parent_name(Parent, ID, !Names) :-
	(if id_name(Parent, ID, Name)
	then 
		det_id_insert(ID, Name, !Names)
	else true
	).
	
var_names(Scope, var_names(Scope)).

