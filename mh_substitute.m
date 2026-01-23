%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_substitute.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_substitute.

:- interface.

:- import_module mh_evaluation.
:- import_module mh_environment.
:- import_module mh_scope.
:- import_module mh_term.
:- import_module mh_substitution.
:- import_module mh_relation.
:- import_module mh_proposition.
:- import_module mh_tuple.
:- import_module mh_ordered_term_set.

%-----------------------------------------------------------------------------%

	% Take the first operand and substitute the variables in them for the
	% terms they map to in the substitution, if the substitution contains new
	% variables not already present in the calling scope, extend the scope
	% to include them, also prune the scope of any variables (and 
	% extended outside scopes) not present in the resulting term
:- pred substitute(eval_strategy::in,
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out,
	mh_term::in, mh_substitution::in, mh_term::out) is det.

:- pred substitute_relation(eval_strategy::in,
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out,
	mh_relation::in, mh_substitution::in, mh_relation::out) is det.

:- pred substitute_proposition(eval_strategy::in,
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out,
	mh_proposition::in, mh_substitution::in, mh_proposition::out) is det.

:- pred substitute_tuple(eval_strategy::in,
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out,
	mh_tuple::in, mh_substitution::in, mh_tuple::out) is det.

:- pred substitute_ordered_term_set(eval_strategy::in,
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out,
	mh_ordered_term_set::in, mh_substitution::in, mh_ordered_term_set::out)
	is det.	
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%

substitute(Strat, !Env, !Scope, !.Term, Sub, !:Term) :-
	require_complete_switch [!.Term] (
		(!.Term = atom(_) ; !.Term = value(_)), !:Term = !.Term
	;
		!.Term = var(ID),
		(if sub_id_search(Sub, ID, Found)
		then !:Term = Found
		else true
		)
	;
		!.Term = cons(Car, Cdr),
		substitute(Strat, !Env, !Scope, Car, Sub, NewCar),
		substitute_tuple(Strat, !Env, !Scope, Cdr, Sub, NewCdr),
		(if Car = NewCar, Cdr = NewCdr
		then true
		else !:Term = cons(NewCar, NewCdr)
		)
	;
		!.Term = relation(Relation),
		substitute_relation(Strat, !Env, !Scope, Relation, Sub, NewRelation),
		(if Relation = NewRelation
		then true
		else !:Term = relation(NewRelation)
		)
	).

substitute_relation(_, !Env, !Scope, !.Term, _, !:Term) :- 
	sorry($module, $pred, "substitute_relation/8").

:- pragma no_determinism_warning(substitute_relation/8).

substitute_proposition(_, !Env, !Scope, !.Term, _, !:Term) :- 
	sorry($module, $pred, "substitute_proposition/8").

:- pragma no_determinism_warning(substitute_proposition/8).

substitute_tuple(_, !Env, !Scope, !.Term, _, !:Term) :- 
	sorry($module, $pred, "substitute_tuple/8").

:- pragma no_determinism_warning(substitute_tuple/8).

substitute_ordered_term_set(_, !Env, !Scope, !.Term, _, !:Term) :- 
	sorry($module, $pred, "substitute/8").

:- pragma no_determinism_warning(substitute_ordered_term_set/8).

