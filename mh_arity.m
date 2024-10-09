%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_arity.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_arity.

:- interface.

:- import_module int.
:- import_module list.
:- import_module array.

:- import_module mh_symbol.

%-----------------------------------------------------------------------------%
% Arity typeclass

:- typeclass arity(T) where [
	pred arity(T::in, int::out) is det
].

:- func arity(T) = int <= arity(T).

/* Template

 :- func foo_arity(mh_foo) = int.
 
 :- pred foo_arity(mh_foo::in, int::out) is det.
 
 :- instance arity(mh_foo).
	
%-----------------------------------------------------------------------------%

foo_arity(_) = _ :- sorry($module, $pred, "foo_arity/1").

:- pragma no_determinism_warning(foo_arity/1).

foo_arity(T, foo_arity(T)).

:- instance arity(mh_foo) where [ pred(arity/2) is foo_arity ].

*/



%-----------------------------------------------------------------------------%
% Indexing values with arity

:- pred valid_index(T, int) <= arity(T). % valid_index(Container, Index)
:- mode valid_index(in, in) is semidet. % succeed on valid index
:- mode valid_index(in, out) is nondet. % return all valid indexes, 

%-----------------------------------------------------------------------------%
% Symbols with arity

:- type symbol_arity ---> symbol/int.

:- func symbol_arity(symbol, T) = symbol_arity <= arity(T).

%-----------------------------------------------------------------------------%
%	Arity instances

:- instance arity(list(T)).
:- instance arity(array(T)).
:- instance arity(symbol_arity).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%
% Arity typeclass

arity(T) = A :- arity(T, A).

%-----------------------------------------------------------------------------%
% Indexing values with arity

:- pragma promise_equivalent_clauses(valid_index/2).

valid_index(T::in, I::in) :- I > 0, I =< arity(T).

valid_index(T::in, I::out) :- 
	Arity @ arity(T) > 0,
	generate_valid_index(1, Arity, I).
	
:- pred generate_valid_index(int::in, int::in, int::out) is multi.

generate_valid_index(I0, A, I) :-
		I = I0 
	; 	I0 < A,
		generate_valid_index(I0 + 1, A, I).

%-----------------------------------------------------------------------------%
% Symbols with arity

symbol_arity(S, T) = S/arity(T).

%-----------------------------------------------------------------------------%
%	Arity instances

:- instance arity(list(T)) where [ pred(arity/2) is list.length ].

:- instance arity(array(T)) where [ pred(arity/2) is array.size ].

:- instance arity(symbol_arity) where [
		arity(_/Arity, Arity)
	].