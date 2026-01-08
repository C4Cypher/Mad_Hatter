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
:- import_module mh_term_map.
:- import_module mh_environment.
:- import_module mh_substitution.

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
	
	% Take the first operand and substitute the variables in them for the
	% terms they map to in the substitution, if the substitution contains new
	% variables not already present in the calling scope, extend the scope
	% to include them, also prune the scope of any variables (and 
	% extended outside scopes) not present in the resulting term
:- pred substitute(eval_strategy::in,
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out,
	mh_term::in, mh_substitution::in, mh_term::out) is det.
	
	
:- pred unification(eval_strategy::in,
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out,
	mh_term::in, mh_term::in, mh_substiution::out) is det.
	
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
    ;       validation
	
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

:- import_module mh_relation.
:- import_module mh_proposition.
:- import_module mh_ordered_term_set.

%-----------------------------------------------------------------------------%
% Evaluation

%TODO: Check for "__before" and "__after" environment variables, and if found
% execute the contained calls, passing the respective input and output terms
% through them. Delaying implementation until I have more of the core semantics
% working.

eval(Strat, !Env, !Scope, !Term) :-
	(if search(!.Env, !Term)
	then true
	else
		Input = !.Term,
		% If memoization is active, map a floundering failure to the current 
		% input, ensuring that infinitely recursive calls fail, make such 
		% a flounder indicate that further evaluation is complete, otherwise
		% further evaluation is complete when evaluating a term returns itself
		(if search_env(!.Env, "memo", term_value(no))
		then 
			Flounder = relation(proposition(!.Scope, 
				proposition_fail(flounder(Input))
			)),
			set(Input, Flounder, !Env),
			EvalComplete = Flounder
		else 
			EvalComplete = Input
		),
		
		%TODO: invoke __before here, 
		% memoize the result of before as well?
		
		
		% Apply nil to the input, performing the actual evaluation
		apply(Strat, !Env, !Scope, !.Term, term_nil, !:Term),
		
		Output = !.Term,
		
		% Invoke __after here?
		
		(if  !.Term = EvalComplete
		then true % No further evaluations
		else
			% form conjunction of Input and the result of further evaluating
			% !.Term (intermediate evaluation), this should form a full 
			% conjunction of all of the evaluated forms of the input.
			
			% How to sort this conjunction? Most recent evaluations to the left
			% (for now)
			eval(Strat, !Env, !Scope, Output, EvalutedOutput),
			% Note: this call can be made tail recurrsive by passing the
			% EvaluedOutput as an additional argument. 
			
			%TODO: Replace this with canonical non-evaluation conjunction
			% constructor, IF I implement one, I may not
			ResultConjunction = conjunction(!.Scope, 
				from_list([EvalutedOutput, Input])
			),
			
			% Flatten/reduce conjunction, if possible?
			
			!:Term = relation(ResultConjunction),
			
			% Evaluation is finished, memoize the result
			(if search_env(!.Env, "memo", term_value(no))
			then true
			else set(Input, !.Term, !Env)
			)
			
			% Invoke __after here?
		)
	).
	
apply(Strat, !Env, !Scope, !Term) :-
	(if
		%eval block
	then true
	else !:Term = term_error("Eval strategy not implemented: " ++ 
		string(Strat))
	).
