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

%-----------------------------------------------------------------------------%
% Evaluation

:- pred eval(eval_strategy::in, eval_options::in,
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out, 
	mh_term::in, mh_term::out) is det. % A -> B

:- pred apply(eval_strategy::in, eval_options::in,
	mh_environment::in, mh_environment::out,
	mh_scope::in, mh_scope::out, 
	mh_term::in, mh_term::in, mh_term::out) is det.	% A(B) -> C
	
	
%-----------------------------------------------------------------------------%
% Evaluation strategy.

:- type eval_strategy

	% Depth-first, lazy (thunk disjuncts for backtracking)
	--->	dfs	
	
	% Breadth-first, eager (collect all solutions)
	;		bfs	
	
	% Negated context, greedy (no further search on success, no solution)
    ;		negated	
	
	% First success → commit (no backtracking, like cut)
    ;		committed_choice
	
	% Breadth-first, eager (all solutions must resolve into one solution)
	;		deterministic
	
	% Exhaustive: all paths must be explored and consistent
    ;       validation
	
	% Not sure if this is needed, but may be helpful, similar to deterministic
	% but tries to continue executing even if further errors are encountered?
	%Code has already thrown an error, reporting recovery or cleanup
	%;		exception.
	

