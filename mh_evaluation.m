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

:- pred eval(eval_strategy::in, 
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out, 
	mh_term::in, mh_term::out) is det. % A -> B

:- pred apply(eval_strategy::in,
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out, 
	mh_term::in, mh_term::in, mh_term::out) is det.	% A(B) -> C
	
:- pred substitute(eval_strategy::in,
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out,
	mh_term::in, mh_substitution::in, mh_term::out) is det.

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
	
	% Exhaustive: all paths must be explored and consistent
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

%-----------------------------------------------------------------------------%
% Evaluation

eval(Strat, !Env, !Scope, !Term) :-
	(if search(!.Env, !Term)
	then true
	else
		Input = !.Term,
		apply(Strat, !Env, !Scope, !.Term, term_nil, !:Term),
		set(Input, !.Term, !Env)
	).
	
apply(_, !Env, !Scope, !Term) :-
