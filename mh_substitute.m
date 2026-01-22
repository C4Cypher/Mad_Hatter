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
:- import_module mh_term.
:- import_module mh_substitution.
:- import_module mh_environment.
:- import_module mh_scope.

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

substitute(_, _, !Env, !Scope, !.Term, _, !:Term) :- 
	sorry($module, $pred, "substitute/8").

:- pragma no_determinism_warning(substitute/8).