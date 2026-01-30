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

:- import_module mh_calling_context.
:- import_module mh_term.

%-----------------------------------------------------------------------------%
% Evaluation

	% Check to see if memoization is off, if not, memoize the input with a 
	% floundering call, apply the evaluated term with a nil argument and
	% (again, if memoization isn't off) memoize the input to the result
:- pred eval(mh_calling_context::in, mh_calling_context::out, 
	mh_term::in, mh_term::out)
	is det. % A -> B

	% Core evaluation logic, if the second operand is a nil, treat like
	% a standard dethunking or evaluation
:- pred apply(mh_calling_context::in, mh_calling_context::out,
	mh_term::in, mh_term::in, mh_term::out)
	is det.	% A(B) -> C



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
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module string.
:- import_module list.
:- import_module require.

:- import_module mh_environment.
:- import_module mh_term_map.
:- import_module mh_relation.
:- import_module mh_proposition.
:- import_module mh_ordered_term_set.
:- import_module mh_symbol.
:- import_module mh_value.
:- import_module mh_tuple.

%-----------------------------------------------------------------------------%
% Evaluation

eval(!Ctx, !Term) :- 
	%__before here
	% memoize the result of before as well?
	eval_loop(!Ctx, !Term, memoizing(!.Ctx ^ environment), []).
	%__after here

:- pred eval_loop(mh_calling_context::in,
	mh_calling_context::out,
	mh_term::in, mh_term::out,
	bool::in, list(mh_term)::in) is det.

%TODO: Check for "__before" and "__after" environment variables, and if found
% execute the contained calls, passing the respective input and output terms
% through them. Delaying implementation until I have more of the core semantics
% working.

eval_loop(!Ctx, !Term, Memoizing, Intermediate) :- some [!Env, !Scope] (
	!.Ctx = ctx(_Strat, !:Scope, !:Env),
	Input = !.Term,
	(if memo_search(!Env, Memoizing, !Term)
	then 
		!:Ctx = !.Ctx ^ environment := !.Env
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
			%Change Environment calls to pass context, not environment
			!:Ctx = !.Ctx ^ environment := !.Env, 
			EvalComplete = Flounder
		else 
			EvalComplete = Input
		),
		
		% Apply nil to the input, performing the actual evaluation
		apply(!Ctx, !.Term, term_nil, !:Term),
		InputList = [Input | Intermediate],
		(if  !.Term = EvalComplete
		then
			%Memoize the input and all of the intermediate results to the
			%Result
			(if Memoizing = yes
			then true
			else 
				memo_list(InputList, !.Term, !Env),
				!:Ctx = !.Ctx ^ environment := !.Env 
			)
		else 
			%TODO: Check depth limit, flounder or error if exceeded
			%Add arg for depth limit
			eval_loop(!Ctx, !Term, Memoizing, InputList)
		)
	)
).
	
apply(!Ctx, Functor, Arg, Result)
:-
	require_complete_switch [Functor] (
		(Functor = atom(_) ; Functor = var(_)),
		Result = cons(Functor, from_list([Arg]))
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
				apply(!Ctx, Car, First, Curried),
				eval(!Ctx, cons(Curried, Rest), Result)
			else
				(if tuple_is_empty(Cdr)
				then ConsArg = term_nil
				else tuple_index(Cdr, 1, ConsArg) %Tuple should be a singleton
				),
				apply(!Ctx, Car, ConsArg, Result)
			)
		else
			%In the future, implement adding elements to the end of tuples
			NewCdr = from_list( to_list(Cdr) ++ [ Arg ] ),
			eval(!Ctx, cons(Car, NewCdr), Result)
		)
	;
		Functor = relation(Relation),
		apply_relation(!Ctx, Relation, Arg, Result)
	).
	
:- pragma inline(apply/5).
	
:- func apply_simple_term(mh_term, mh_term, string) = mh_term.

apply_simple_term(Functor, Arg, Msg) =
	(if Arg = term_nil
	then Functor
	else term_fail(Msg)
	).
	
:- pragma inline(apply_simple_term/3).

:- pred apply_relation(mh_calling_context::in, mh_calling_context::out,
	mh_relation::in, mh_term::in, mh_term::out) is det.

apply_relation(!Ctx, Relation, Arg, Result) :- 
	sorry($module, $pred, "apply_relation/8"). /*
	require_complete_switch [Relation] (
		Relation = nil,
		Result = apply_simple_term(term_nil, Arg, 
			"Attempted to apply term to nil value.")
	;
		Relation = conjunction(InnerScope, ConjOts),
*/

:- pragma no_determinism_warning(apply_relation/5).