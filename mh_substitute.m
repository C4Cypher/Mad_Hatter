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

:- import_module mh_calling_context.
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
:- pred substitute(mh_calling_context::in, mh_calling_context::out,
	mh_term::in, mh_substitution::in, mh_term::out) is det.

:- pred substitute_relation(mh_calling_context::in, mh_calling_context::out,
	mh_relation::in, mh_substitution::in, mh_relation::out) is det.

:- pred substitute_proposition(mh_calling_context::in, mh_calling_context::out,
	mh_proposition::in, mh_substitution::in, mh_proposition::out) is det.

:- pred substitute_tuple(mh_calling_context::in, mh_calling_context::out,
	mh_tuple::in, mh_substitution::in, mh_tuple::out) is det.

:- pred substitute_ordered_term_set(mh_calling_context::in, 
	mh_calling_context::out, mh_ordered_term_set::in, mh_substitution::in, mh_ordered_term_set::out)
	is det.	
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bool.
:- import_module int.
:- import_module require.

%-----------------------------------------------------------------------------%

substitute(!Ctx, !.Term, Sub, !:Term) :-
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
		substitute(!Ctx, Car, Sub, NewCar),
		substitute_tuple(!Ctx, Cdr, Sub, NewCdr),
		(if Car = NewCar, Cdr = NewCdr
		then true
		else !:Term = cons(NewCar, NewCdr)
		)
	;
		!.Term = relation(Relation),
		substitute_relation(!Ctx, Relation, Sub, NewRelation),
		(if Relation = NewRelation
		then !:Term = !.Term
		else !:Term = relation(NewRelation)
		)
	).

substitute_relation(!Ctx, Relation, Sub, Result) :-
	RelationScope = relation_scope(Relation),
	(if compatable_scope(RelationScope, !.Ctx)
	then
		require_complete_switch [Relation] (
			Relation = nil,
			Result = Relation
		;
			Relation = conjunction(_, Ots),
			% Possible optimizations:
			% Custom loop that checks if
			% 1. a substitution is substituting a conjunction into itself
			%	if so, skip the substitution for variables, but apply
			% 	the substiution to compound subterms
			% 2. a substitution is subtituting a term into a conjunction
			%	that already exists in the conjunction
			%	if so, skip the substitution and remove the variable
			%	from the conjunction, but apply the substitution to compound
			%	subterms
			
			substitute_ordered_term_set(!Ctx, Ots, Sub, NewOts),
			Result = new_conjunction(!.Ctx, RelationScope, NewOts) 
		;
			Relation = disjunction(_, Ots),
			substitute_ordered_term_set(!Ctx, Ots, Sub, NewOts),
			Result = new_disjunction(!.Ctx, RelationScope, NewOts) 
		;
			Relation = not(_, Negation),
			substitute(!Ctx, Negation, Sub, NewNegation),
			Result = new_negation(!.Ctx, RelationScope, NewNegation)
		;
			Relation = lambda_equivalence(_, Lhs, Rhs),
			substitute(!Ctx, Lhs, Sub, NewLhs),
			substitute(!Ctx, Rhs, Sub, NewRhs),
			Result = new_lambda_equivalence(!.Ctx, RelationScope, NewLhs,
				NewRhs)
		;
			Relation = lambda_application(_, Lhs, Rhs),
			substitute(!Ctx, Lhs, Sub, NewLhs),
			substitute(!Ctx, Rhs, Sub, NewRhs),
			Result = new_lambda_application(!.Ctx, RelationScope, NewLhs,
				NewRhs)
		;
			Relation = lambda_unification(_, Lhs, Rhs),
			substitute(!Ctx, Lhs, Sub, NewLhs),
			substitute(!Ctx, Rhs, Sub, NewRhs),
			Result = new_lambda_unification(!.Ctx, RelationScope, NewLhs,
				NewRhs)
		;
			Relation = lazy(_, Constraint),
			substitute(!Ctx, Constraint, Sub, NewConstraint),
			Result = new_lazy(!.Ctx, RelationScope, NewConstraint)
		;
			Relation = proposition(_, Proposition),
			substitute_proposition(!Ctx, Proposition, Sub, NewProposition),
			Result = new_proposition(!.Ctx, RelationScope, NewProposition)
		;
			Relation = call(_, _),
			Result = Relation
		)
	else
		Result = Relation
	).




substitute_proposition(!Ctx, !.Term, _, !:Term) :- 
	sorry($module, $pred, "substitute_proposition/5").

:- pragma no_determinism_warning(substitute_proposition/5).

substitute_tuple(!Ctx, !.Tuple, Sub, !:Tuple) :- 
	Size = tuple_size(!.Tuple),
	(if Size > 0 
	then
		init(Size, term_nil, NewArray),
		substitute_tuple_loop(min(NewArray), !.Tuple, NewArray, Array, 
			!Ctx, Sub, no, Changed),
		(if Changed = yes
		then
			!:Tuple = from_array(Array)
		else true %Don't bother assigning new tuple if it hasn't changed
		)
	else true %If tuple was empty, return it
	).
	
:- pred substitute_tuple_loop(int::in, mh_tuple::in,
	array(mh_term)::array_di, array(mh_term)::array_uo,
	mh_calling_context::in, mh_calling_context::out,
	mh_substitution::in,
	bool::in, bool::out
	) is det.
	
substitute_tuple_loop(Index, CurrentTuple, !Array, !Ctx, Sub,
	!Changed) :-
	% If tuple_cons/3 fails, CurrentTuple is empty, loop is complete
	(if tuple_cons(CurrentTuple, CurrentTerm, NextTuple)
	then
		substitute(!Ctx, CurrentTerm, Sub, NewTerm),
		set(Index, NewTerm, !Array), %TODO: Make unsafe_set after testing
		%If the tuple hasn't changed up to this point, check to see if the
		%current term has, and if so, flip the Changed flag
		(if !.Changed = no, CurrentTerm \= NewTerm
		then !:Changed = yes
		else true
		),
		substitute_tuple_loop(Index + 1, NextTuple, !Array, !Ctx, Sub, 
			!Changed)
	else true
	).

substitute_ordered_term_set(!Ctx, !.Ots, Sub, !:Ots) :-
	Tuple = to_tuple(!.Ots),
	substitute_tuple(!Ctx, Tuple, Sub, NewTuple),
	(if Tuple \= NewTuple
	then !:Ots = from_tuple(NewTuple)
	else true
	).

