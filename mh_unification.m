%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_unification.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_unification.

:- interface.

:- import_module mh_term.
:- import_module mh_substitution.
:- import_module mh_calling_context.

%-----------------------------------------------------------------------------%
% Unification

	% Memoizing full operation, evaluates the propositional form of the two
	% input terms, producing a substitution that will resolve both terms to
	% the result of unification/5
:- pred unify(mh_calling_context::in, mh_calling_context::out,
	mh_term::in, mh_term::in, mh_term::out(proposition_term)) is det.
	
	% Return the resulting term resulting from unifying two terms
	% used when evaluating proposition unifications
:- pred unification(mh_calling_context::in, mh_calling_context::out,
	mh_term::in, mh_term::in, mh_proposition::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module maybe.

:- import_module mh_ordered_term_set.
:- import_module mh_evaluation.
:- import_module mh_var_set.
:- import_module mh_var_id.

%-----------------------------------------------------------------------------%
% Unification

unify(!Ctx, A, B, Result) :-
	Unification = proposition_unification(from_list([A, B])),
	eval(!Ctx, Unification, EvalResult),
	(if is_proposition(EvalResult)
	then Result = EvalResult
	else 
		Reason = message(
"mh_unification.unification evaluation produced non-proposition result."),
		Result = relation(new_proposition(!.Ctx, proposition_error(Reason)))
	).
	
unification(!Ctx, A, B, Result) :- 
	scope(!.Ctx, Scope),
	(if A = B % shortcut structural equality first
	then
		Result = proposition_true
	% If either are variables, perform binding
	% Check B first, if A is also a var, the resulting conjunction will be
	% ordered 'A, B'
	else if B = var(ID) 
	then bind_var(Scope, ID, A, Result)
	else if A = var(ID)
	then bind_var(Scope, ID, B, Result)
	else %
		
	
	)
	
:- pred bind_var(mh_scope::in, var_id::in, mh_term::in, mh_propsition::out)
	is det.

bind_var(Scope, ID, Term, Result) :-
	var_set_merge_id(ID, vars_in_scope(Scope, Term), ConjVars),
	create_child_scope(Scope, no, ConjVars, ConjScope),
	Conjunction = relation(conjunction(ConjScope, from_list([Term, var(ID)]))),
	Map = singleton_id(ID, Conjunction),
	Sub = sub_map(Map),
	Result = proposition_success(Sub).
	
:- pred relation_unification(mh_calling_context::in, mh_calling_context::out,
	mh_relation::in, mh_term::in, mh_term::out) is det.

relation_unification(!Ctx, _, _, proposition_error(message(
	"Not implemented: mh_unification.relaiton_unification"))).