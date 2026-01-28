%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% mh_termhis file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_calling_context.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_calling_context.

:- interface.

:- import_module mh_evaluation.
:- import_module mh_scope.
:- import_module mh_environment.
	
%-----------------------------------------------------------------------------%
% Calling context

:- type mh_calling_context
	--->	calling_context(
		strategy::eval_strategy, 
		scope::mh_scope, 
		environment::mh_environment
	).

% Abbreviated constructor
:- func ctx(eval_strategy, mh_scope, mh_environment) = mh_calling_context.
:- mode ctx(in, in, in) = out is det.
:- mode ctx(out, out, out) = in is det.

%-----------------------------------------------------------------------------%
% Environment access

	% Succeeds if the map contains the given key
:- pred contains(mh_calling_context::in, mh_term::in) is semidet.

	% Fails if the key is not found
:- pred search(mh_calling_context::in, mh_term::in, mh_term::out) is semidet.
:- func search(mh_calling_context, mh_term) = mh_term is semidet.

	% mh_termhrows an exception if the key is not found
:- pred lookup(mh_calling_context::in, mh_term::in, mh_term::out) is det.
:- func lookup(mh_calling_context, mh_term) = mh_term is det.

:- pred insert(mh_term::in, mh_term::in, mh_calling_context::in, 
	mh_calling_context::out) is semidet.
	
:- pred det_insert(mh_term::in, mh_term::in, mh_calling_context::in, 
	mh_calling_context::out) is det.
	
:- pred det_insert(mh_term::in, mh_term_set::in, mh_term_set::out)
	is det.
	
:- pred det_insert_from_corresponding_lists(list(mh_term)::in, 
	list(mh_term)::in, mh_calling_context::in, mh_calling_context::out) is det.
	
:- pred det_insert_from_assoc_list(assoc_list(mh_term, mh_term)::in,
	mh_calling_context::in, mh_calling_context::out) is det.

:- pred set(mh_term::in, mh_term::in, mh_calling_context::in,
	mh_calling_context::out) is det.
	
:- pred set_from_corresponding_lists(list(mh_term)::in, list(mh_term)::in,
	mh_calling_context::in, mh_calling_context::out) is det.
	
:- pred set_from_assoc_list(assoc_list(mh_term, mh_term)::in,
	mh_calling_context::in, mh_calling_context::out) is det.

:- pred update(mh_term::in, mh_term::in, mh_calling_context::in, 
	mh_calling_context::out) is semidet.
	
:- pred det_update(mh_term::in, mh_term::in, mh_calling_context::in, 
	mh_calling_context::out) is det.
	
:- pred remove(mh_term::in, mh_term::out, mh_calling_context::in,
	mh_calling_context::out) is semidet.
	
:- pred det_remove(mh_term::in, mh_term::out, mh_calling_context::in, 
	mh_calling_context::out) is det.
	
:- pred delete(mh_term::in,  mh_calling_context::in, mh_calling_context::out)
	is det.

:- pred delete_list(list(mh_term)::in, mh_calling_context::in, 
	mh_calling_context::out) is det.

%-----------------------------------------------------------------------------%
% Environment variables

:- pred contains_env(mh_calling_context::in, string::in) is semidet.
:- pred contains_env(mh_calling_context::in, string::in, string::in)
	is semidet.

:- pred search_env(mh_calling_context::in, string::in, mh_term::out)
	is semidet.
	
:- pred search_env(mh_calling_context::in, string::in, string::in, 
	mh_term::out) is semidet.
	
:- pred lookup_env(mh_calling_context::in, string::in, mh_term::out) is det.
:- pred lookup_env(mh_calling_context::in, string::in, string::in,
	mh_term::out) is det.
	
:- pred insert_env(string::in, mh_term::in, mh_calling_context::in,
	mh_calling_context::out) is semidet.
:- pred insert_env(string::in, string::in, mh_term::in, mh_calling_context::in,
	mh_calling_context::out) is semidet.
	
:- pred det_insert_env(string::in, mh_term::in, mh_calling_context::in,
	mh_calling_context::out) is det.
:- pred det_insert_env(string::in, string::in, mh_term::in,
	mh_calling_context::in, mh_calling_context::out) is det.
	
:- pred set_env(string::in, mh_term::in, mh_calling_context::in,
	mh_calling_context::out) is det.
:- pred set_env(string::in, string::in, mh_term::in, mh_calling_context::in,
	mh_calling_context::out) is det.
	
:- pred update_env(string::in, mh_term::in, mh_calling_context::in,
	mh_calling_context::out) is semidet.
:- pred update_env(string::in, string::in, mh_term::in, mh_calling_context::in,
	mh_calling_context::out) is semidet.
	
:- pred det_update_env(string::in, mh_term::in, mh_calling_context::in,
	mh_calling_context::out) is det.
:- pred det_update_env(string::in, string::in, mh_term::in,
	mh_calling_context::in, mh_calling_context::out) is det.
	
:- pred remove_env(string::in, mh_term::out, mh_calling_context::in,
	mh_calling_context::out) is semidet.
:- pred remove_env(string::in, string::in, mh_term::out,
	mh_calling_context::in, mh_calling_context::out) is semidet.
	
:- pred delete_env(string::in, mh_calling_context::in,
	mh_calling_context::out) is det.
:- pred delete_env(string::in, string::in, mh_calling_context::in,
	mh_calling_context::out) is det.
	
%-----------------------------------------------------------------------------%
% Scope

	% Succeeds if the scope is compatable with the context scope
:- pred compatable_scope(mh_scope::in, mh_calling_context::in) is semidet.  

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.


%-----------------------------------------------------------------------------%
% Calling context

ctx(Strat, Scope, Env) = calling_context(Strat, Scope, Env).

:- pragma inline(ctx/3).

%-----------------------------------------------------------------------------%
% Environment access

contains(Ctx, Key) :- contains(Ctx ^ environment, Key).

search(Ctx, Key, search(Ctx, Key)).
search(Ctx, Key) = search(Ctx ^ environment, Key).

lookup(Ctx, Key, lookup(Ctx, Key)).
lookup(Ctx, Key) = lookup(Ctx ^ environment, Key).

insert(Key, Value, !Ctx) :-
	insert(Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

det_insert(Key, Value, !Ctx) :-
	det_insert(Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.
	
det_insert_from_assoc_list(List, !Ctx) :-
	det_insert_from_assoc_list(List, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

set(Key, Value, !Ctx) :-
	set(Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

set_from_corresponding_lists(Keys, Values, !Ctx) :-
	set_from_corresponding_lists(Keys, Values, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.
	
set_from_assoc_list(List, !Ctx) :-
	set_from_assoc_list(List, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

update(Key, Value, !Ctx) :-
	update(Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

det_update(Key, Value, !Ctx) :-
	det_update(Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

remove(Key, Value, !Ctx) :-
	remove(Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

det_remove(Key, Value, !Ctx) :-
	det_remove(Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

delete(Key, !Ctx) :-
	delete(Key, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

delete_list(Keys, !Ctx) :-
	delete_list(Keys, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.
	
%-----------------------------------------------------------------------------%
% Environment variables

contains_env(Ctx, Key) :- contains_env(Ctx ^ environment, Key).

search_env(Ctx, Key, Value) :- search_env(Ctx ^ environment, Key, Value).

lookup_env(Ctx, Key, Value) :- lookup_env(Ctx ^ environment, Key, Value).
lookup_env(Ctx, Module, Key, Value) :- 
	lookup_env(Ctx ^ environment, Module, Key, Value).

insert_env(Key, Value, !Ctx) :-
	insert_env(Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

insert_env(Module, Key, Value, !Ctx) :-
	insert_env(Module, Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

det_insert_env(Key, Value, !Ctx) :-
	det_insert_env(Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

det_insert_env(Module, Key, Value, !Ctx) :-
	det_insert_env(Module, Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

set_env(Key, Value, !Ctx) :-
	set_env(Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

set_env(Module, Key, Value, !Ctx) :-
	set_env(Module, Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

update_env(Key, Value, !Ctx) :-
	update_env(Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

update_env(Module, Key, Value, !Ctx) :-
	update_env(Module, Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

det_update_env(Key, Value, !Ctx) :-
	update_env(Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

det_update_env(Module, Key, Value, !Ctx) :-
	update_env(Module, Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

remove_env(Key, Value, !Ctx) :-
	remove_env(Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

remove_env(Module, Key, Value, !Ctx) :-
	remove_env(Module, Key, Value, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

delete_env(Key, !Ctx) :-
	delete_env(Key, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.

delete_env(Module, Key, !Ctx) :-
	delete_env(Module, Key, !.Ctx ^ environment, Env),
	!:Ctx = !.Ctx ^ environment := Env.
	
%-----------------------------------------------------------------------------%
% Scope

compatable_scope(Scope, Ctx) :- compatable_scope(Scope, Ctx ^ Scope).



