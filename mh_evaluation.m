%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_evaluation.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_evaluation.

:- interface.

:- import_module mh_scope.
:- import_module mh_term.
:- import_module mh_environment.
:- import_module mh_proposition.

%-----------------------------------------------------------------------------%
% Evaluation

	% Check to see if memoization is off, if not, memoize the input with a 
	% floundering call, apply the evaluated term with a nil argument and
	% (again, if memoization isn't off) memoize the input to the result
:- pred eval(eval_strategy::in, 
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out, 
	mh_term::in, mh_term::out) is det. % A -> B

	% Core evaluation logic, if the second operand is a nil, treat like
	% a standard dethunking or evaluation
:- pred apply(eval_strategy::in,
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out, 
	mh_term::in, mh_term::in, mh_term::out) is det.	% A(B) -> C

	% Memoizing full operation, evaluates the propositional form of the two
	% input terms, producing a substitution that will resolve both terms to
	% the result of unify/8
:- pred unification(eval_strategy::in,
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out,
	mh_term::in, mh_term::in, mh_proposition::out) is det.
	
:- pred unify(eval_strategy::in,
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out,
	mh_term::in, mh_term::in, mh_term::out) is det.

%-----------------------------------------------------------------------------%
% Evaluation strategy.

:- type eval_strategy
	
	% Breadth-first, eager (collect all solutions)
	--->	bfs	

	% Depth-first, lazy (thunk disjuncts for backtracking)
	;		dfs	
	
	% Negated context, greedy (no further search on success, no solution)
    ;		negated	
	
	% First success -> commit (no backtracking, like cut)
    ;		committed_choice
	
	% Breadth-first, eager (all solutions must resolve into one solution)
	;		deterministic
	
	% Deterministic, but with the possibility of failure
	;		semideterministic
	
	% Exhaustive: all paths must be explored and consistent, may include
	% additional semantics checks not present in other strategies
    ;       validation.
	
	% Not sure if this is needed, but may be helpful, similar to deterministic
	% but tries to continue executing even if further errors are encountered?
	%Code has already thrown an error, reporting recovery or cleanup
	%;		exception.
	
%-----------------------------------------------------------------------------%
% Calling context

% Encapsulates the neccecary information to hygenically call code outside
% of the main context	

:- type mh_calling_context 
	--->	calling_context(eval_strategy, mh_environment, mh_term).
	
:- type mh_scoped_calling_context 
	--->	calling_context(eval_strategy, mh_environment, mh_scope, mh_term).
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module string.
:- import_module list.
:- import_module require.

:- import_module mh_term_map.
:- import_module mh_relation.
:- import_module mh_ordered_term_set.
:- import_module mh_symbol.
:- import_module mh_value.
:- import_module mh_tuple.

%-----------------------------------------------------------------------------%
% Evaluation

eval(Strat, !Env, !Scope, !Term) :- 
	%__before here
	% memoize the result of before as well?
	eval_loop(Strat, !Env, !Scope, !Term, memoizing(!.Env), []).
	%__after here

:- pred eval_loop(eval_strategy::in,
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out, 
	mh_term::in, mh_term::out, 
	bool::in, list(mh_term)::in) is det.

%TODO: Check for "__before" and "__after" environment variables, and if found
% execute the contained calls, passing the respective input and output terms
% through them. Delaying implementation until I have more of the core semantics
% working.

eval_loop(Strat, !Env, !Scope, !Term, Memoizing, Intermediate) :-
	Input = !.Term,
	(if memo_search(!Env, Memoizing, !Term)
	then true 
		%TODO: if Strat = validate and Memoizing = yes, evaluate the term
		% anyway and compare with the memo table result, return an error
		% if mismatch is found
	else
		% If memoization is active, map a floundering failure to the current 
		% input, ensuring that infinitely recursive calls fail, make such 
		% a flounder indicate that further evaluation is complete, otherwise
		% further evaluation is complete when evaluating a term returns itself
		(if Memoizing = yes
		then 
			Flounder = relation(proposition(!.Scope, 
				proposition_fail(flounder(Input))
			)),
			set(Input, Flounder, !Env),
			EvalComplete = Flounder
		else 
			EvalComplete = Input
		),
		
		% Apply nil to the input, performing the actual evaluation
		apply(Strat, !Env, !Scope, !.Term, term_nil, !:Term),

		InputList = [Input | Intermediate],
		
		(if  !.Term = EvalComplete
		then
			%Memoize the input and all of the intermediate results to the
			%Result
			(if Memoizing = yes
			then true
			else memo_list(InputList, !.Term, !Env)
			)
		else 
			%TODO: Check depth limit, flounder or error if exceeded
			%Add arg for depth limit
			eval_loop(Strat, !Env, !Scope, !Term, Memoizing,
				InputList)
		)
	).
	
apply(Strat, !Env, !Scope, Functor, Arg, Result) :-
	require_complete_switch [Functor] (
		(Functor = atom(_) ; Functor = var(_)),
		Result = cons(Functor, from_list([Arg]) )
	;	
		Functor = value(Value),
		Msg = "Attempted to apply term to value of type " ++
			value_type_name(Value),
		Result = apply_simple_term(Functor, Arg, Msg)
	;
		% The calls to eval call may be tailrecursive if apply is inlined 
		% into eval
		Functor = cons(Car, Cdr),
		(if Arg = term_nil
		then
			(if Cdr = tuple_cons(First, Rest) 
			then
				apply(Strat, !Env, !Scope, Car, First, Curried),
				eval(Strat, !Env, !Scope, cons(Curried, Rest), Result)
			else
				(if tuple_is_empty(Cdr)
				then ConsArg = term_nil
				else tuple_index(Cdr, 1, ConsArg) %Tuple should be a singleton
				),
				apply(Strat, !Env, !Scope, Car, ConsArg, Result)
			)
		else
			%In the future, implement adding elements to the end of tuples
			NewCdr = from_list( to_list(Cdr) ++ [ Arg ] ),
			eval(Strat, !Env, !Scope, cons(Car, NewCdr), Result)
		)
	;
		Functor = relation(Relation),
		apply_relation(Strat, !Env, !Scope, Relation, Arg, Result)
	).
	
:- pragma inline(apply/8).
	
:- func apply_simple_term(mh_term, mh_term, string) = mh_term.

apply_simple_term(Functor, Arg, Msg) =
	(if Arg = term_nil
	then Functor
	else term_fail(Msg)
	).
	
:- pragma inline(apply_simple_term/3).

:- pred apply_relation(eval_strategy::in, 
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out, 
	mh_relation::in, mh_term::in, mh_term::out) is det.

apply_relation(Strat, !Env, !Scope, Relation, Arg, Result) :- 
	sorry($module, $pred, "apply_relation/8"). /*
	require_complete_switch [Relation] (
		Relation = nil,
		Result = apply_simple_term(term_nil, Arg, 
			"Attempted to apply term to nil value.")
	;
		Relation = conjunction(InnerScope, ConjOts),
*/

:- pragma no_determinism_warning(apply_relation/8).



unification(_, !Env, !Scope, _, _, proposition_false) :- sorry($module, $pred, "unification/8").

:- pragma no_determinism_warning(unification/8).

unify(_, !Env, !Scope, _, _, term_false) :- 
	sorry($module, $pred, "unify/8").

:- pragma no_determinism_warning(unify/8).