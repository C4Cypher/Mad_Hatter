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

%-----------------------------------------------------------------------------%
% Symbols

:- type mh_symbol.
:- type mh_symbols == list(mh_symbol).



:- func symbol(string) = mh_symbol.
:- mode symbol(in) = out is det.
:- mode symbol(out) = in is det.

:- func to_string(mh_symbol) = string.

%-----------------------------------------------------------------------------%
% Symbol hashes

:- pred symbol_hash(mh_symbol::in, int::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module fnv_hash.
:- import_module uint.

% TODO: Memo grades may not be thread safe or compatable.
% Revisit symbol memoization.  Hashing?

:- pragma require_feature_set([memo]). 


%-----------------------------------------------------------------------------%
% Symbols

:- type mh_symbol ---> s(symbol_ptr).

:- pragma promise_equivalent_clauses(symbol/1).
:- pragma inline(symbol/1).

symbol(String::in) = (s(Ptr)::out) :- construct_ptr(String, Ptr).

symbol(String::out) = (Symbol::in) :-
	Symbol = s(Ptr),
	promise_equivalent_solutions [String] deconstruct_ptr(Ptr, String).

to_string(symbol(String)) = String.

%-----------------------------------------------------------------------------%
% Symbol pointers

:- type symbol_ptr ---> ~string 
	where equality is ptr_equality, comparison is ptr_comparison.

:- pred construct_ptr(string::in, symbol_ptr::out) is det.

:- pragma memo(construct_ptr/2).

construct_ptr(S, '~'(S)).

:- pred deconstruct_ptr(symbol_ptr::in, string::out) is cc_multi.

:- pragma inline(deconstruct_ptr/2).

deconstruct_ptr('~'(S), S).


:- pred ptr_equality(symbol_ptr::in, symbol_ptr::in) is semidet.

ptr_equality(A, B) :- private_builtin.pointer_equal(A, B).

:- pragma inline(ptr_equality/2).

:- pred ptr_comparison(comparison_result::uo, symbol_ptr::in, symbol_ptr::in) 
	is det.
	
:- pragma inline(ptr_comparison/3).
	
ptr_comparison(R, A, B) :-
	(if ptr_equality(A, B)
	then R = (=)
	else ptr_inequality(R, A, B)
	).
	
:- pred ptr_inequality(comparison_result::uo,
	symbol_ptr::in, symbol_ptr::in)	is det.
	
:- pragma inline(ptr_inequality/3).
	
ptr_inequality(R, A, B) :-
	promise_equivalent_solutions [Astring, Bstring] (
		deconstruct_ptr(A, Astring), 
		deconstruct_ptr(B, Bstring)
	),
	(if compare((<), Astring, Bstring)
	then R = (<)
	else R = (>)
	).

%-----------------------------------------------------------------------------%
% Symbol hashes

symbol_hash(s(Ptr), cast_to_int(H)) :-
	( promise_equivalent_solutions [String] deconstruct_ptr(Ptr, String) ),
	fnv1a_hash(String, H).