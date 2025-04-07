%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_environment.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_environment.

:- interface.

:- import_module mh_term.
:- import_module mh_term_map.

:- include_module mh_environment.map.

%-----------------------------------------------------------------------------%
% Environment

:- type mh_environment.

:- func empty_environment = mh_environment.

:- pred empty_environment(mh_environment::out) is det.

:- pred is_empty(mh_environment::in) is det.

%-----------------------------------------------------------------------------%
% Queries

% E.T := B  TODO: Find a binding operator for the query
% check to see if two terms are bound in the given environment mapping
% This is a partial check that only involves the scope of the environment
% itself. Is not reflexive, will only succeed if the first term is bound
% to the second. 

:- pred bound(mh_environment, mh_term, mh_term).
:- mode bound(in, in, in) is semidet.
:- mode bound(in, in, out) is semidet. 
:- mode bound(in, out, out) is nondet.

% E.T := _
% Succeeds if a term is bound, without retreiving the bound term
:- pred bound(mh_environment, mh_term).
:- mode bound(in, in) is semidet.
:- mode bound(in, out) is nondet.


%:- pred ask(mh_environment::in, mh_term::in, mh_term::out) is det.

%:- func ask(mh_envirronment, mh_term) = mh_term.


%-----------------------------------------------------------------------------%
% Changes


% E.T = B  TODO: Find a better operator for this, semantically the unification
% operator works, but this is a very specific context
% bind two terms, effectively unifying them. This overwrites any existing
% binding for the term. 
:- pred bind(mh_term::in, mh_term::in, 
	mh_environment::in, mh_environment::out) is det.
	
% remove an existing binding for a term, if any.
:- pred unbind(mh_term::in, mh_environment::in, mh_environment::out) is det.

%:- pred assert(mh_environment::in, mh_term::in, mh_environment::out) is det.

%:- func assert(mh_environment, mh_term) = mh_environment.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mh_substitution.

%-----------------------------------------------------------------------------%

:- type mh_environment
	--->	map_env(mh_term_map(mh_term)). % Unification map
%	;		term_env(mh_term) % The context and scope of a given term.
%	;		sub_env(mh_substitution) % only a set of variable bindings
%	;		module_env(
%		interface::mh_environment, %Replace with declarations
%		implementation::mh_environment
%		).
