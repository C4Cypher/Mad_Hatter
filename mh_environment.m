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

%-----------------------------------------------------------------------------%
% Environment

:- type mh_environment.

:- func empty_environment = mh_environment.

:- pred empty_environment(mh_environment::out) is det.

:- pred is_empty(mh_environment::in) is det.

%-----------------------------------------------------------------------------%
% Environment map operations

% TODO: Sub module?

	% Succeeds if the environment contains a unification with the given term
:- pred contains(mh_environment::in, mh_tuple::in) is semidet.

:- pred search(mh_environment::in, mh_term::in, mh_term::out) is semidet.
:- func search(mh_environment, mh_term) = mh_term is semidet.


	% returns nil if term is not found  
:- pred lookup(mh_environment::in, mh_term::in, mh_term::out) is det.
:- func lookup(mh_environment, mh_term) = mh_term is det.

:- pred insert(mh_term::in, mh_term::in, mh_environment::in, 
	mh_environment::out) is semidet.
	
:- pred det_insert(mh_term::in, mh_term::in, mh_environment::in, 
	mh_environment::out) is det.
	
:- pred det_insert_from_corresponding_lists(list(mh_term)::in, 
	list(mh_term)::in, mh_environment::in, mh_environment::out) is det.
	
:- pred det_insert_from_assoc_list(assoc_list(mh_term, mh_term)::in,
	mh_environment::in, mh_environment::out) is det.
	
:- pred set(mh_term::in, mh_term::in, mh_term_map::in, mh_term_map::out)
	is det.
	
:- pred set_from_corresponding_lists(list(mh_term)::in, list(mh_term)::in,
	mh_environment::in, mh_environment::out) is det.
	
:- pred set_from_assoc_list(assoc_list(mh_term, mh_term)::in,
	mh_environment::in, mh_environment::out) is det.

:- pred update(mh_term::in, mh_term::in, mh_environment::in, 
	mh_term_map::out) is semidet.
	
:- pred det_update(mh_term::in, mh_term::in, mh_environment::in, 
	mh_environment::out) is det.

%-----------------------------------------------------------------------------%
% Environment modification

:- pred assert

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.



%-----------------------------------------------------------------------------%

:- type mh_environment
	--->	map_env(mh_term_map(mh_term)).
	
new_env(Map) = map_env(Map).
	
	
%-----------------------------------------------------------------------------%
% Environment lookup

search(Env, Term, search(Env, Term)).

search(map_env(Map), Term) = mh_tuple_map.search(Map, Term). 