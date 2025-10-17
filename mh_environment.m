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

:- import_module list.

:- import_module mh_symbol_map.
:- import_module mh_term.
:- import_module mh_symbol.

%-----------------------------------------------------------------------------%
% Environment

:- type mh_environment 
	--->	%relation_environment(mh_relation)      % later
	;		environment(mh_symbol_map(mh_term)).
	%		module_environment(mh_module).   A later extention of environments
	
	% If two environments map the same symbols to the same terms, they are
	% equivalent.
/*:- pred equivalent_environments(mh_environment::in, mh_environment::in)
	is semidet.*/
	
	% True if the given term is an environment. If successful, from_term/1 
	% will always return a meanaingful value
:- pred is_environment(mh_term) is semidet.
	
%-----------------------------------------------------------------------------%
% Queries

	% Succeedss if the environment binds a given symbol
:- pred contains(mh_evnironment::in, mh_symbol::in) is semidet.

	% Return the term bound to a given symbol in an environment
:- pred search(mh_environment::in, mh_symbol::in, mh_term::out) is semidet.
:- func search(mh_environment, mh_symbol) = mh_term is semidet.
	
	% Return a list of symbols that the given environment binds, 
	% contains/2 should succeed iff provided a symbol present in the returned
	% list.  Order of the list is implementation dependent, but there should
	% never be duplicates. Ideally the list should already be sorted, but I 
	% am not making that a requirement as of yet, given the goal is efficency.

:- pred exports(mh_environment::in, list(mh_symbol)::out) is det.
:- func exports(mh_environment) = list(mh_symbol).

%-----------------------------------------------------------------------------%
% Conversion

	% If the term is an mr_value, cast
	% it to mh_environment, otherwise fail.
:- func from_term(mh_term) = mh_environment is semidet.

	% If the environment is a relation, directly pass it as a term, otherwise
	% wrap the entire mh_environment as an mr_value
:- func to_term(mh_environment) = mh_term.

%-----------------------------------------------------------------------------%
% Calling

	% Apply an environment to a given term, if the term is an atom, search for
	% and substitute the atom for a conjunction of the atom and it's mapping
	% in the environment ... I need to include a full calling context with
	% memo table if I am going to proceed further
/*
:- pred apply_environment(mh_environment::in, mh_term::in, mh_term::out)
	is det.
*/

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%
% Environment

is_environment(Term) :- from_term(Term) = _. 
%-----------------------------------------------------------------------------%
% Queries

contains(environment(Map), S) :- mh_symbol_map.contains(Map, S).

search(Env, S, search(Env, S)).

search(environment(Map), S) = mh_symbol_map.search(Map, S).

exports(environment(Map), S) = mh_symbol_map.keys(Map, S).

%-----------------------------------------------------------------------------%
% Conversion

from_term(value(Value)) = to_mr_value(Value).

to_term(Env) = value(to_mh_value(Env)). 



%-----------------------------------------------------------------------------%
% Calling

% apply_environment(Env, !Term)