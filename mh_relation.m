%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_relation.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_relation.

:- interface.

:- import_module mh_ordered_term_set.
:- import_module mh_scope.
:- import_module mh_term.
:- import_module mh_proposition.
:- import_module mh_foreign_function.
:- import_module mh_var_set.
:- import_module mh_calling_context.

%-----------------------------------------------------------------------------%
% Relation type

% A relation represents the functor of a compound term in any unifcation
% that requires more work than simple assignment of that compound term to
% a free variable.
% 
%
% X = R(Arg)
%

/* Depreciated documentation
% At it's core, they represent the 'head clause' of any horn clause, even for
%  s. In essene, a predicate in the traditional Prolog sense is a 
% Relation that unifies with a success indicator or truth value, or to be more
% specific, a function that returns a success indicator. The substitution 
% contained in the success represents the variable bindings that resolve
% to make the fact 'true'


% Relations can be  'moded', modes being pairings of pre-conditions and post
% conditions that enforce the relation's soundness and purity.
 
% Thinking about this. I can describe relations in many  different ways.
% However I need to distill the type structure down into a way that is
% unifiable and able to encapsulate clauses before and after mode analysis
% PLUS handling foreign function calls. I need to define modes. Preconditions
% and postconditions.
*/

:- type mh_relation
	--->	nil		
	%		The abscense of value, only unifies with nil
	%		T(nil) = T.
	
	;		conjunction(mh_scope, mh_ordered_term_set)
	%		A relation that contains a set of terms, and succesfully unifies
	%		with any member of the given set
	%		(a , b)(X) -> a(X) , b(X),  a = b, a(X) = b(X).
	%		conjunction(_, []) == true. 

	;		disjunction(mh_scope, mh_ordered_term_set)
	%		A relation that contains a set of terms, and succesfully unifies
	%		with any member of the given set, however the expression is not
	%		considered deterministic, representing the possibility that the
	%		term could be any one of it's members
	%		(a ; b)(X) == a(X) ; b(X).
	% 		disjunction(_, []) == false. 

	;		not(mh_scope, mh_term)
	%		'not'(a)(X) ==  a(X), false; not a(X).

	;		lambda_equivalence(mh_scope, mh_term, mh_term)
	%		lambda_equivalence(S, A, B) -> \A = B.
	%		(\A = B) -> (\A -> B), (\A <- B). 
	
	;		lambda_application(mh_scope, mh_term, mh_term)
	%		lambda_application(S, A, B) == (\A -> B). 
	%		(\A -> B)(A) = B. 
	%		The lambda expression \A evaluates to B with A substituted into B
	
	;		lambda_unification(mh_scope, mh_term, mh_term)
	%		lambda_match(S, A, B) == (\A <- B). 
	%		The lambda expresssion \A successsfully unifies with B without loss
	%		of information
	
	;		lazy(mh_scope, mh_term)
	%		lazy(E, S, C) == ?C 
	%
	%		A 'lazy' term presents an applied constraint, a body clause bound
	%		to a variable, successfully unifying with any term that is applied
	%		to it.
	%
	% 		Conjunction of term and applied constraint
	%		X, ?C = X, C(X). 
	%
	%		The 'constraint' operator ':'/2 
	%		X:C = X, ?C.
	
	;		proposition(mh_scope, mh_proposition)
	% 		Succeed or fail based on evaluation of the embedded proposition;
	%		Should evaluate to either a success or failure as defined in 
	%		mh_proposition.m
			
	;		call(mh_scope, mh_foreign_function).
			% Call a foreign function as if it were an application.
			% \(X) -> Y == F(X) := Y.

/* Refrence values ... I'll need to implement this for anything that isn't
% easily serializable to source

	;		refrence(mh_relation, univ)
			% A refrence to a foreign object. Unification of which being
			% dictated by the relation
			% X = refrence(R, O) == X = R(O)
	
	;		unique_refrence(string, univ).
			% A refrence to a foreign object that only unifies with other
			% refrences that have the same uid
*/
	
:- func relation_scope(mh_relation) = mh_scope is det.

:- func update_relation_scope(mh_scope, mh_scope, mh_relation) = mh_relation.
:- pred update_relation_scope(mh_scope::in, mh_scope::in, mh_relation::in,
	mh_relation::out) is det.
	
:- func vars_in_relation(mh_relation) = mh_var_set is det.

:- pred ground_relation(mh_relation::in) is semidet.

%-----------------------------------------------------------------------------%
% Constructors

% Construct a new relation given a calling context, if an existing scope
% is provided, it's context will be used in the constructed relation's 
% scope.

:- pred new_conjunction(mh_calling_context::in, mh_ordered_term_set::in, 
	mh_relation::out) is det.
:- func new_conjunction(mh_calling_context, mh_ordered_term_set) = mh_relation.

:- pred new_conjunction(mh_calling_context::in, mh_scope::in,
	mh_ordered_term_set::in, mh_relation::out) is det.
:- func new_conjunction(mh_calling_context, mh_scope, mh_ordered_term_set) =
	mh_relation.
	
:- pred new_disjunction(mh_calling_context::in, mh_ordered_term_set::in, 
	mh_relation::out) is det.
:- func new_disjunction(mh_calling_context, mh_ordered_term_set) = mh_relation.

:- pred new_disjunction(mh_calling_context::in, mh_scope::in,
	mh_ordered_term_set::in, mh_relation::out) is det.
:- func new_disjunction(mh_calling_context, mh_scope, mh_ordered_term_set) =
	mh_relation.
	
:- pred new_negation(mh_calling_context::in, mh_term::in, mh_relation::out)
	is det.
:- func new_negation(mh_calling_context, mh_term) = mh_relation.

:- pred new_negation(mh_calling_context::in, mh_scope::in,
	mh_term::in, mh_relation::out) is det.
:- func new_negation(mh_calling_context, mh_scope, mh_term) = mh_relation.

:- pred new_lambda_equivalence(mh_calling_context::in, 
	mh_term::in, mh_term::in, mh_relation::out) is det.
:- func new_lambda_equivalence(mh_calling_context, mh_term, mh_term) =
	mh_relation.

:- pred new_lambda_equivalence(mh_calling_context::in, mh_scope::in,
	mh_term::in, mh_term::in, mh_relation::out) is det.
:- func new_lambda_equivalence(mh_calling_context, mh_scope, mh_term, mh_term)
	= mh_relation.
	
:- pred new_lambda_application(mh_calling_context::in, 
	mh_term::in, mh_term::in, mh_relation::out) is det.
:- func new_lambda_application(mh_calling_context, mh_term, mh_term) =
	mh_relation.

:- pred new_lambda_application(mh_calling_context::in, mh_scope::in,
	mh_term::in, mh_term::in, mh_relation::out) is det.
:- func new_lambda_application(mh_calling_context, mh_scope, mh_term, mh_term)
	= mh_relation.

:- pred new_lambda_unification(mh_calling_context::in, 
	mh_term::in, mh_term::in, mh_relation::out) is det.
:- func new_lambda_unification(mh_calling_context, mh_term, mh_term) =
	mh_relation.

:- pred new_lambda_unification(mh_calling_context::in, mh_scope::in,
	mh_term::in, mh_term::in, mh_relation::out) is det.
:- func new_lambda_unification(mh_calling_context, mh_scope, mh_term, mh_term)
	= mh_relation.
	
:- pred new_lazy(mh_calling_context::in, mh_term::in, mh_relation::out)
	is det.
:- func new_lazy(mh_calling_context, mh_term) = mh_relation.

:- pred new_lazy(mh_calling_context::in, mh_scope::in,
	mh_term::in, mh_relation::out) is det.
:- func new_lazy(mh_calling_context, mh_scope, mh_term) = mh_relation.
	
:- pred new_proposition(mh_calling_context::in, mh_proposition::in,
	mh_relation::out) is det.
:- func new_proposition(mh_calling_context, mh_proposition) = mh_relation.

:- pred new_proposition(mh_calling_context::in, mh_scope::in,
	mh_proposition::in, mh_relation::out) is det.
:- func new_proposition(mh_calling_context, mh_scope, mh_proposition)
	= mh_relation.
	
:- pred new_call(mh_calling_context::in, mh_foreign_function::in,
	mh_relation::out) is det.
:- func new_call(mh_calling_context, mh_foreign_function) = mh_relation.

:- pred new_call(mh_calling_context::in, mh_scope::in,
	mh_foreign_function::in, mh_relation::out) is det.
:- func new_call(mh_calling_context, mh_scope, mh_foreign_function)
	= mh_relation.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

relation_scope(nil) = no_scope.
relation_scope(conjunction(Scope, _)) = Scope.
relation_scope(disjunction(Scope, _)) = Scope.
relation_scope(not(Scope, _)) = Scope.
relation_scope(lambda_equivalence(Scope, _, _)) = Scope.
relation_scope(lambda_application(Scope, _, _)) = Scope.
relation_scope(lambda_unification(Scope, _, _)) = Scope.
relation_scope(lazy(Scope, _)) = Scope.
relation_scope(proposition(Scope, _)) = Scope.
relation_scope(call(Scope, _)) = Scope.

update_relation_scope(OldScope, NewScope, !.Relation) = !:Relation :-
	InnerScope = relation_scope(!.Relation),
	(if compatable_scope(InnerScope, OldScope)
	then 
		assert_var_set_in_scope(NewScope, Vars@scope_vars(InnerScope)),
		require_complete_switch [!.Relation] (
			!.Relation = nil,
			!:Relation = nil %Unreachable, compatable_scope should fail
		;
			!.Relation = conjunction(_, Ots),
			scope_context(InnerScope, ScopeContext),
			create_child_scope(NewScope, yes(ScopeContext), Vars, NewInner),
			map(update_term_scope(InnerScope, NewInner), Ots, NewOts),
			!:Relation = conjunction(NewInner, NewOts)
		;
			!.Relation = disjunction(_, Ots),
			scope_context(InnerScope, ScopeContext),
			create_child_scope(NewScope, yes(ScopeContext), Vars, NewInner),
			map(update_term_scope(InnerScope, NewInner), Ots, NewOts),
			!:Relation = disjunction(NewInner, NewOts)
		;
			!.Relation = not(_, Term),
			scope_context(InnerScope, ScopeContext),
			create_child_scope(NewScope, yes(ScopeContext), Vars, NewInner),
			update_term_scope(InnerScope, NewInner, Term, NewTerm),
			!:Relation = not(NewInner, NewTerm)
		;
			!.Relation = lambda_equivalence(_, Lhs, Rhs),
			scope_context(InnerScope, ScopeContext),
			create_child_scope(NewScope, yes(ScopeContext), Vars, NewInner),
			update_term_scope(InnerScope, NewInner, Lhs, NewLhs),
			update_term_scope(InnerScope, NewInner, Rhs, NewRhs),
			!:Relation = lambda_equivalence(NewInner, NewLhs, NewRhs)
		;
			!.Relation = lambda_application(_, Lhs, Rhs),
			scope_context(InnerScope, ScopeContext),
			create_child_scope(NewScope, yes(ScopeContext), Vars, NewInner),
			update_term_scope(InnerScope, NewInner, Lhs, NewLhs),
			update_term_scope(InnerScope, NewInner, Rhs, NewRhs),
			!:Relation = lambda_application(NewInner, NewLhs, NewRhs)
		;
			!.Relation = lambda_unification(_, Lhs, Rhs),
			scope_context(InnerScope, ScopeContext),
			create_child_scope(NewScope, yes(ScopeContext), Vars, NewInner),
			update_term_scope(InnerScope, NewInner, Lhs, NewLhs),
			update_term_scope(InnerScope, NewInner, Rhs, NewRhs),
			!:Relation = lambda_unification(NewInner, NewLhs, NewRhs)
		;
			!.Relation = lazy(_, Term),
			scope_context(InnerScope, ScopeContext),
			create_child_scope(NewScope, yes(ScopeContext), Vars, NewInner),
			update_term_scope(InnerScope, NewInner, Term, NewTerm),
			!:Relation = lazy(NewInner, NewTerm)
		;
			!.Relation = proposition(_, Prop),
			scope_context(InnerScope, ScopeContext),
			create_child_scope(NewScope, yes(ScopeContext), Vars, NewInner),
			update_proposition_scope(InnerScope, NewInner, Prop, NewProp),
			!:Relation = proposition(NewInner, NewProp)
		;
			!.Relation = call(_, Func),
			scope_context(InnerScope, ScopeContext),
			create_child_scope(NewScope, yes(ScopeContext), Vars, NewInner),
			!:Relation = call(NewInner, Func)
		)
	else 
		!:Relation = !.Relation
	).

update_relation_scope(OldScope, NewScope, Rel, 
	update_relation_scope(OldScope, NewScope, Rel)).

:- func vir_fold(mh_scope, mh_term, mh_var_set) = mh_var_set.

vir_fold(Scope, Term, Set) = var_set_union(vars_in_scope(Scope, Term), Set).

vars_in_relation(nil) = empty_var_set.
vars_in_relation(conjunction(Scope, OTS)) = 
	vars_in_ordered_term_set(Scope, OTS).
vars_in_relation(disjunction(Scope, OTS)) = 
	vars_in_ordered_term_set(Scope, OTS).
vars_in_relation(not(Scope, Term)) = vars_in_scope(Scope, Term).
vars_in_relation(lambda_equivalence(Scope, L, R)) = 
	var_set_union(vars_in_scope(Scope, L), vars_in_scope(Scope, R)).
vars_in_relation(lambda_application(Scope, L, R)) =  
	var_set_union(vars_in_scope(Scope, L), vars_in_scope(Scope, R)).
vars_in_relation(lambda_unification(Scope, L, R)) =  
	var_set_union(vars_in_scope(Scope, L), vars_in_scope(Scope, R)).
vars_in_relation(lazy(Scope, Term)) = vars_in_scope(Scope, Term).
vars_in_relation(proposition(Scope, Prop)) = vars_in_proposition(Scope, Prop).
vars_in_relation(call(Scope, _)) = scope_vars(Scope).


ground_relation(_) :- sorry($module, $pred, "ground_relation/1").

:- pragma no_determinism_warning(ground_relation/1).

%-----------------------------------------------------------------------------%
% Constructors

new_conjunction(Ctx, Ots, new_conjunction(Ctx, Ots)).
new_conjunction(Ctx, Ots) = conjunction(NewScope, NewOts) :-
	Scope = Ctx ^ scope,
	vars_in_ordered_term_set(Scope, Ots, Vars),
	create_child_scope(Scope, no, Vars, NewScope),
	map(update_term_scope(Scope, NewScope), Ots, NewOts).
 
new_conjunction(Ctx, Existing, Ots, new_conjunction(Ctx, Existing, Ots)).
new_conjunction(Ctx, ExistingScope, Ots) = conjunction(NewScope, NewOts) :-
	Scope = Ctx ^ scope,
	vars_in_ordered_term_set(Scope, Ots, Vars),
	assert_compatable_scope(ExistingScope, Scope), %Check only on validation?
	scope_context(ExistingScope, ScopeContext),
	create_child_scope(Scope, yes(ScopeContext), Vars, NewScope),
	map(update_term_scope(Scope, NewScope), Ots, NewOts).
	
new_disjunction(Ctx, Ots, new_disjunction(Ctx, Ots)).
new_disjunction(Ctx, Ots) = disjunction(NewScope, NewOts) :-
	Scope = Ctx ^ scope,
	vars_in_ordered_term_set(Scope, Ots, Vars),
	create_child_scope(Scope, no, Vars, NewScope),
	map(update_term_scope(Scope, NewScope), Ots, NewOts).

new_disjunction(Ctx, Existing, Ots, new_disjunction(Ctx, Existing, Ots)).
new_disjunction(Ctx, ExistingScope, Ots) = disjunction(NewScope, NewOts) :-
	Scope = Ctx ^ scope,
	vars_in_ordered_term_set(Scope, Ots, Vars),
	assert_compatable_scope(ExistingScope, Scope), 
	scope_context(ExistingScope, ScopeContext),
	create_child_scope(Scope, yes(ScopeContext), Vars, NewScope),
	map(update_term_scope(Scope, NewScope), Ots, NewOts).

new_negation(Ctx, Term, new_negation(Ctx, Term)).
new_negation(Ctx, Term) = not(NewScope, NewTerm) :-
	Scope = Ctx ^ scope,
	vars_in_scope(Scope, Term, Vars),
	create_child_scope(Scope, no, Vars, NewScope),
	update_term_scope(Scope, NewScope, Term, NewTerm).

new_negation(Ctx, Existing, Term, new_negation(Ctx, Existing, Term)).
new_negation(Ctx, ExistingScope, Term) = not(NewScope, NewTerm) :-
	Scope = Ctx ^ scope,
	vars_in_scope(Scope, Term, Vars),
	assert_compatable_scope(ExistingScope, Scope), 
	scope_context(ExistingScope, ScopeContext),
	create_child_scope(Scope, yes(ScopeContext), Vars, NewScope),
	update_term_scope(Scope, NewScope, Term, NewTerm).
	
new_lambda_equivalence(Ctx, Lhs, Rhs, new_lambda_equivalence(Ctx, Lhs, Rhs)).
new_lambda_equivalence(Ctx, Lhs, Rhs) 
	= lambda_equivalence(NewScope, NewLhs, NewRhs) :-
	Scope = Ctx ^ scope,
	vars_in_scope(Scope, Lhs, Vars1),
	vars_in_scope(Scope, Rhs, Vars2),
	var_set_union(Vars1, Vars2, Vars),
	create_child_scope(Scope, no, Vars, NewScope),
	update_term_scope(Scope, NewScope, Lhs, NewLhs),
	update_term_scope(Scope, NewScope, Rhs, NewRhs).

new_lambda_equivalence(Ctx, Existing, Lhs, Rhs, 
	new_lambda_equivalence(Ctx, Existing, Lhs, Rhs)).
new_lambda_equivalence(Ctx, ExistingScope, Lhs, Rhs)
	= lambda_equivalence(NewScope, NewLhs, NewRhs) :-
	Scope = Ctx ^ scope,
	vars_in_scope(Scope, Lhs, Vars1),
	vars_in_scope(Scope, Rhs, Vars2),
	var_set_union(Vars1, Vars2, Vars),
	assert_compatable_scope(ExistingScope, Scope), 
	scope_context(ExistingScope, ScopeContext),
	create_child_scope(Scope, yes(ScopeContext), Vars, NewScope),
	update_term_scope(Scope, NewScope, Lhs, NewLhs),
	update_term_scope(Scope, NewScope, Rhs, NewRhs).
	
new_lambda_application(Ctx, Lhs, Rhs, new_lambda_application(Ctx, Lhs, Rhs)).
new_lambda_application(Ctx, Lhs, Rhs) 
	= lambda_application(NewScope, NewLhs, NewRhs) :-
	Scope = Ctx ^ scope,
	vars_in_scope(Scope, Lhs, Vars1),
	vars_in_scope(Scope, Rhs, Vars2),
	var_set_union(Vars1, Vars2, Vars),
	create_child_scope(Scope, no, Vars, NewScope),
	update_term_scope(Scope, NewScope, Lhs, NewLhs),
	update_term_scope(Scope, NewScope, Rhs, NewRhs).

new_lambda_application(Ctx, Existing, Lhs, Rhs, new_lambda_application(Ctx, Existing, Lhs, Rhs)).
new_lambda_application(Ctx, ExistingScope, Lhs, Rhs)
	= lambda_application(NewScope, NewLhs, NewRhs) :-
	Scope = Ctx ^ scope,
	vars_in_scope(Scope, Lhs, Vars1),
	vars_in_scope(Scope, Rhs, Vars2),
	var_set_union(Vars1, Vars2, Vars),
	assert_compatable_scope(ExistingScope, Scope), 
	scope_context(ExistingScope, ScopeContext),
	create_child_scope(Scope, yes(ScopeContext), Vars, NewScope),
	update_term_scope(Scope, NewScope, Lhs, NewLhs),
	update_term_scope(Scope, NewScope, Rhs, NewRhs).

new_lambda_unification(Ctx, Lhs, Rhs, new_lambda_unification(Ctx, Lhs, Rhs)).
new_lambda_unification(Ctx, Lhs, Rhs)
	= lambda_unification(NewScope, NewLhs, NewRhs) :-
	Scope = Ctx ^ scope,
	vars_in_scope(Scope, Lhs, Vars1),
	vars_in_scope(Scope, Rhs, Vars2),
	var_set_union(Vars1, Vars2, Vars),
	create_child_scope(Scope, no, Vars, NewScope),
	update_term_scope(Scope, NewScope, Lhs, NewLhs),
	update_term_scope(Scope, NewScope, Rhs, NewRhs).

new_lambda_unification(Ctx, Existing, Lhs, Rhs, new_lambda_unification(Ctx, Existing, Lhs, Rhs)).
new_lambda_unification(Ctx, ExistingScope, Lhs, Rhs)
	= lambda_unification(NewScope, NewLhs, NewRhs) :-
	Scope = Ctx ^ scope,
	vars_in_scope(Scope, Lhs, Vars1),
	vars_in_scope(Scope, Rhs, Vars2),
	var_set_union(Vars1, Vars2, Vars),
	assert_compatable_scope(ExistingScope, Scope), 
	scope_context(ExistingScope, ScopeContext),
	create_child_scope(Scope, yes(ScopeContext), Vars, NewScope),
	update_term_scope(Scope, NewScope, Lhs, NewLhs),
	update_term_scope(Scope, NewScope, Rhs, NewRhs).

new_lazy(Ctx, Term, new_lazy(Ctx, Term)).
new_lazy(Ctx, Term) = lazy(NewScope, NewTerm) :-
	Scope = Ctx ^ scope,
	vars_in_scope(Scope, Term, Vars),
	create_child_scope(Scope, no, Vars, NewScope),
	update_term_scope(Scope, NewScope, Term, NewTerm).

new_lazy(Ctx, Existing, Term, new_lazy(Ctx, Existing, Term)).
new_lazy(Ctx, ExistingScope, Term) = lazy(NewScope, NewTerm) :-
	Scope = Ctx ^ scope,
	vars_in_scope(Scope, Term, Vars),
	assert_compatable_scope(ExistingScope, Scope), 
	scope_context(ExistingScope, ScopeContext),
	create_child_scope(Scope, yes(ScopeContext), Vars, NewScope),
	update_term_scope(Scope, NewScope, Term, NewTerm).

new_proposition(Ctx, Prop, new_proposition(Ctx, Prop)).
new_proposition(Ctx, Prop) = proposition(NewScope, NewProp) :-
	Scope = Ctx ^ scope,
	vars_in_proposition(Scope, Prop, Vars),
	create_child_scope(Scope, no, Vars, NewScope),
	update_proposition_scope(Scope, NewScope, Prop, NewProp).

new_proposition(Ctx, Existing, Prop, new_proposition(Ctx, Existing, Prop)).
new_proposition(Ctx, ExistingScope, Prop) = proposition(NewScope, NewProp) :-
	Scope = Ctx ^ scope,
	vars_in_proposition(Scope, Prop, Vars),
	assert_compatable_scope(ExistingScope, Scope), 
	scope_context(ExistingScope, ScopeContext),
	create_child_scope(Scope, yes(ScopeContext), Vars, NewScope),
	update_proposition_scope(Scope, NewScope, Prop, NewProp).

new_call(Ctx, Func, new_call(Ctx, Func)).
new_call(Ctx, Func) = call(NewScope, Func) :-
	Scope = Ctx ^ scope,
	create_child_scope(Scope, no, empty_var_set, NewScope).

new_call(Ctx, Existing, Func, new_call(Ctx, Existing, Func)).
new_call(Ctx, ExistingScope, Func) = call(NewScope, Func) :-
	Scope = Ctx ^ scope,
	assert_compatable_scope(ExistingScope, Scope), 
	scope_context(ExistingScope, ScopeContext),
	create_child_scope(Scope, yes(ScopeContext), empty_var_set, NewScope).
	

