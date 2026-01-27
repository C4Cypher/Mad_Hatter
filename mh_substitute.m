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

:- import_module array.
:- import_module bool.
:- import_module int.
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
		substitute_relation(Strat, !Env, !Scope, Relation, Sub, NewTerm),
		(if !.Term = NewTerm
		then !:Term = !.Term
		else !:Term = NewTerm
		)
	).

substitute_relation(Strat, !Env, !Scope, Relation, Sub, Result) :-
	RelationScope = relation_scope(Relation),
	(if compatable_scope(RelationScope, !.Scope)
	then
		require_complete_switch [Relation] (
			Relation = nil,
			Result = Relation
		;
			Relation = conjunction(_, Ots),
			Result = nil,
			sorry($module, $pred, "substitute_relation/8")
		;
			Relation = disjunction(_, Ots),
			Result = nil,
			sorry($module, $pred, "substitute_relation/8")
		;
			Relation = not(_, Negation),
			Result = nil,
			sorry($module, $pred, "substitute_relation/8")
		;
			Relation = lambda_equivalence(_, Lhs, Rhs),
			Result = nil,
			sorry($module, $pred, "substitute_relation/8")
		;
			Relation = lambda_application(_, Lhs, Rhs),
			Result = nil,
			sorry($module, $pred, "substitute_relation/8")
		;
			Relation = lambda_unification(_, Lhs, Rhs),
			Result = nil,
			sorry($module, $pred, "substitute_relation/8")
		;
			Relation = lazy(_, Constraint),
			Result = nil,
			sorry($module, $pred, "substitute_relation/8")
		;
			Relation = proposition(_, Proposition),
			Result = nil,
			sorry($module, $pred, "substitute_relation/8")
		)
	else
		Result = Relation
	).




substitute_proposition(_, !Env, !Scope, !.Term, _, !:Term) :- 
	sorry($module, $pred, "substitute_proposition/8").

:- pragma no_determinism_warning(substitute_proposition/8).

substitute_tuple(Strat, !Env, !Scope, !.Tuple, Sub, !:Tuple) :- 
	Size = tuple_size(!.Tuple),
	(if Size > 0 
	then
		init(Size, term_nil, NewArray),
		substitute_tuple_loop(min(NewArray), !.Tuple, NewArray, Array, 
			Strat, !Env, !Scope, Sub, no, Changed),
		(if Changed = yes
		then
			!:Tuple = from_array(Array)
		else true %Don't bother assigning new tuple if it hasn't changed
		)
	else true %If tuple was empty, return it
	).
	
:- pred substitute_tuple_loop(int::in, mh_tuple::in,
	array(mh_term)::array_di, array(mh_term)::array_uo,
	eval_strategy::in,
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out,
	mh_substitution::in,
	bool::in, bool::out
	) is det.
	
substitute_tuple_loop(Index, CurrentTuple, !Array, Strat, !Env, !Scope, Sub,
	!Changed) :-
	% If tuple_cons/3 fails, CurrentTuple is empty, loop is complete
	(if tuple_cons(CurrentTuple, CurrentTerm, NextTuple)
	then
		substitute(Strat, !Env, !Scope, CurrentTerm, Sub, NewTerm),
		set(Index, NewTerm, !Array), %TODO: Make unsafe_set after testing
		%If the tuple hasn't changed up to this point, check to see if the
		%current term has, and if so, flip the Changed flag
		(if !.Changed = no, CurrentTerm \= NewTerm
		then !:Changed = yes
		else true
		),
		substitute_tuple_loop(Index + 1, NextTuple, !Array, Strat, !Env,
			!Scope, Sub, !Changed)
	else true
	).

substitute_ordered_term_set(Strat, !Env, !Scope, !.Ots, Sub, !:Ots) :-
	Tuple = to_tuple(!.Ots),
	substitute_tuple(Strat, !Env, !Scope, Tuple, Sub, NewTuple),
	(if Tuple \= NewTuple
	then !:Ots = from_tuple(NewTuple)
	else true
	).

