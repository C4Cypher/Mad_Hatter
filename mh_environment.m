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

:- import_module string.

:- import_module mh_term.
:- import_module mh_term_map.

%-----------------------------------------------------------------------------%
% Environment

% An environment serves as both a lookup table for predefined symbols, and as
% a memo table for already performed evaluations. For each 'pure' evaluation
% of a term, the environment gets updated with that evaluation.
%
% In effect, for every binding in an environment, logically `Key -> Value`

:- type mh_environment == mh_term_map(mh_term).

%-----------------------------------------------------------------------------%
% Environment variables

% Environment variables map to atoms with symbols prefixed with the string
% "__". The following calls prepend that prefix. The calls with an extra
% key field are intended for 'module local' variables, appending the arguments
% seperated by an additional "_"

:- pred contains_env(mh_environment::in, string::in) is semidet.
:- pred contains_env(mh_environment::in, string::in, string::in) is semidet.

:- pred search_env(mh_environment::in, string::in, mh_term::out) is semidet.
:- pred search_env(mh_environment::in, string::in, string::in, mh_term::out)
	is semidet.
	
:- pred lookup_env(mh_environment::in, string::in, mh_term::out) is det.
:- pred lookup_env(mh_environment::in, string::in, string::in, mh_term::out)
	is det.
	
:- pred insert_env(string::in, mh_term::in, mh_environment::in,
	mh_environment::out) is semidet.
:- pred insert_env(string::in, string::in, mh_term::in, mh_environment::in,
	mh_environment::out) is semidet.
	
:- pred det_insert_env(string::in, mh_term::in, mh_environment::in,
	mh_environment::out) is det.
:- pred det_insert_env(string::in, string::in, mh_term::in, mh_environment::in,
	mh_environment::out) is det.
	
:- pred set_env(string::in, mh_term::in, mh_environment::in,
	mh_environment::out) is det.
:- pred set_env(string::in, string::in, mh_term::in, mh_environment::in,
	mh_environment::out) is det.
	
:- pred update_env(string::in, mh_term::in, mh_environment::in,
	mh_environment::out) is semidet.
:- pred update_env(string::in, string::in, mh_term::in, mh_environment::in,
	mh_environment::out) is semidet.
	
:- pred det_update_env(string::in, mh_term::in, mh_environment::in,
	mh_environment::out) is det.
:- pred det_update_env(string::in, string::in, mh_term::in, mh_environment::in,
	mh_environment::out) is det.
	
:- pred remove_env(string::in, mh_term::out, mh_environment::in,
	mh_environment::out) is semidet.
:- pred remove_env(string::in, string::in, mh_term::out, mh_environment::in,
	mh_environment::out) is semidet.
	
:- pred delete_env(string::in, mh_environment::in,	mh_environment::out)
	is det.
:- pred delete_env(string::in, string::in, mh_environment::in,
	mh_environment::out) is det.	

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mh_symbol.

%-----------------------------------------------------------------------------%
% Environment variables

:- func prefix(string) = mh_term.
prefix(Name) = atom(symbol("__" ++ Name)).

:- func prefix(string, string) = mh_term.
prefix(Module, Name) = prefix(local(Module, Name)).

:- func local(string, string) = string.
local(Module, Name) = Module ++ "__" ++ Name.

contains_env(Env, Name) :- contains(Env, prefix(Name)).
contains_env(Env, Mod, Name) :- contains_env(Env, local(Mod, Name)).

search_env(Env, Name, Term) :- search(Env, prefix(Name), Term).
search_env(Env, Mod, Name, Term) :- search_env(Env, local(Mod, Name), Term).

lookup_env(Env, Name, Term) :- lookup(Env, prefix(Name), Term).
lookup_env(Env, Mod, Name, Term) :- lookup_env(Env, local(Mod, Name), Term).

insert_env(Name, Term, !Env) :- insert(prefix(Name), Term, !Env).
insert_env(Mod, Name, Term, !Env) :- insert_env(local(Mod, Name), Term, !Env).

det_insert_env(Name, Term, !Env) :- det_insert(prefix(Name), Term, !Env).
det_insert_env(Mod, Name, Term, !Env) :- 
	det_insert_env(local(Mod, Name), Term, !Env).
	
set_env(Name, Term, !Env) :- set(prefix(Name), Term, !Env).
set_env(Mod, Name, Term, !Env) :- set_env(local(Mod, Name), Term, !Env).

update_env(Name, Term, !Env) :- update(prefix(Name), Term, !Env).
update_env(Mod, Name, Term, !Env) :- update_env(local(Mod, Name), Term, !Env).

det_update_env(Name, Term, !Env) :- det_update(prefix(Name), Term, !Env).
det_update_env(Mod, Name, Term, !Env) :- 
	det_update_env(local(Mod, Name), Term, !Env).
	
remove_env(Name, Term, !Env) :- remove(prefix(Name), Term, !Env).
remove_env(Mod, Name, Term, !Env) :- remove_env(local(Mod, Name), Term, !Env).

delete_env(Name, !Env) :- delete(prefix(Name), !Env).
delete_env(Mod, Name, !Env) :- delete_env(local(Mod, Name), !Env).
