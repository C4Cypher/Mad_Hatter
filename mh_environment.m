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

:- import_module ordered_set.

:- import_module mh_term.
:- import_module mh_term_map.
:- import_module mh_scope.
:- import_module mh_event.


%-----------------------------------------------------------------------------%
% Environment

:- type mh_environment
	--->	map_env(mh_term_map)
	;		ext_env(mh_environment, mh_term_map) % Extended env
	;		cmp_env(mh_environment, mh_environment). % Composition of envs

:- func empty_environment = mh_environment.

:- pred empty_environment(mh_environment::out) is det.

:- pred is_empty(mh_environment::in) is det.

% Compose envirornment into a single map.
:- pred compose_environment(mh_environment::in, mh_term_map::out) is det.

%-----------------------------------------------------------------------------%
% Queries

% E.T = B   

:- pred bound(mh_environment, mh_term, mh_term).
:- mode bound(in, in, in) is semidet.
:- mode bound(in, in, out) is semidet. 
:- mode bound(in, out, out) is nondet.

:- func bound(mh_environment, mh_term) = mh_term is semidet.

% E.T = _
% Succeeds if a term is bound, without retreiving the bound term
:- pred bound(mh_environment, mh_term).
:- mode bound(in, in) is semidet.
:- mode bound(in, out) is nondet.


:- pred ask(mh_scope, mh_term::in,mh_environment::in, mh_term::out) is det.

:- func ask((mh_scope, mh_term, mh_environment) = mh_term.

:- pred ask(mh_environment::in, mh_term::in, mh_term::out, event_log::out)
	is det.

:- pred query(mh_term::in, mh_term::out, event_log::in, event_log::out, 
	mh_environment::in, mh_environment::out) is det.

%-----------------------------------------------------------------------------%
% Changes


% !.E.T := B = !:E
% Produce a new environment with a new binding from T to B  
:- pred bind(mh_term::in, mh_term::in, 
	mh_environment::in, mh_environment::out) is det.
	
% !.E.T := _ = !:E.
% remove an existing binding for a term, if any.
:- pred unbind(mh_term::in, mh_environment::in, mh_environment::out) is det.

%:- pred assert(mh_environment::in, mh_term::in, mh_environment::out) is det.

%:- func assert(mh_environment, mh_term) = mh_environment.



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.


