%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_symbol.m
% Main author: C4Cypher.
% Stability: low.

:- module mh_symbol.

:- interface.

:- import_module list.

:- type symbol_set.
:- type symbol.
:- type symbols == list(symbol).


% Create a symbol set using a string hash function
:- pred init_symbol_set((func(string) = int)::in, symbol_set::out) is det.
:- func init_symbol_set(func(string) = int) = symbol_set.

% Invoke init_symbol_set with the default string.hash function
:- pred init_symbol_set(symbol_set::out) is det.
:- func init_symbol_set = symbol_set.

% Look up a string identifier and return the corresponding symbol, if it does
% Not exist, create it and add it to the set. 
%
% symbol(Name, !Set, Symbol).
% symbol(NAme, !Set) = Symbol.
:- pred new_symbol(string::in, symbol::out,
	symbol_set::in, symbol_set::out) is det.
	
% Look up a string identifier and return the corresponding symbol,
% fail if it is not found.
%
% symbol(Name, Set, Symbol).
% symbol(Name, Set) = Symbol.

:- func symbol(symbol_set, string) = symbol.
:- mode symbol(in, in) = out is semidet.
:- mode symbol(in, out) = in is semidet.

:- pred symbol(symbol_set, string, symbol).
:- mode symbol(in, in, out) is semidet.
:- mode symbol(in, out, in) is semidet.

% Fails if the corresponding symbol does not exist

:- pred symbol_exists(symbol_set::in, string::in) is semidet.


:- implementation.

:- import_module bimap.
:- import_module string.
:- import_module int.

:- type symbol == int.

:- type symbol_set ---> symbol_set(
	hash::(func(string) = int),
	map::bimap(string, symbol)
).

init_symbol_set(Hash, symbol_set(Hash, bimap.init)).
init_symbol_set(Hash) = symbol_set(Hash, bimap.init).

init_symbol_set(init_symbol_set(string.hash)).
init_symbol_set = init_symbol_set(string.hash).

new_symbol(Key, Symbol, !Set) :-
	if search(!.Set ^ map, Key, Value)
	then Symbol = Value, !:Set = !.Set
	else
		Hash_function = !.Set ^ hash,
		Hash = Hash_function(Key),
		collide_check(Key, Hash, Symbol,!Set).
		
:- pred collide_check(string::in, int::in, int::out,
	symbol_set::in, symbol_set::out) is det.

collide_check(Key, Hash, Symbol, !Set) :-
	Map0 = !.Set ^ map, (
		if contains_value(Map0, Hash) 
		then collide_check(Key, Hash + 1, Symbol, !Set)
		else det_insert(Key, Hash, Map0, Map), 
			Symbol = Hash,
			!:Set = symbol_set(!.Set ^ hash, Map)
	).

symbol(Set, Key) = Symbol :- search(Set ^ map, Key, Symbol).

symbol(Set, Key, symbol(Set, Key)).

symbol_exists(Set, Key) :- contains_key(Set ^ map, Key).