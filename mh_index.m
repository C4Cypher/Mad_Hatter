%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_index.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_index.

:- interface.

:- import_module int.

%-----------------------------------------------------------------------------%

:- typeclass index(T, U) <= (T -> U) where [

	pred valid_index(T, int),
	mode valid_index(in, in) is semidet, % succeed on valid index
	mode valid_index(in, out) is nondet, % return all valid indexes

	pred index(T, int, U),
	mode index(in, in, in) is semidet, % implicit fail on inequality
	mode index(in, in, out) is semidet, % fail on invalid index
	mode index(in, out, out) is nondet, % all indexed values
	
	pred set_index(int, U, T, T),
	mode set_index(in, in, in, out) is semidet, % fail on invalid index
	mode set_index(out, in, in, out) is nondet, % update any index nondet
	
	pred map(pred(U, U), T, T),
	mode map(pred(in, out) is det, 	in, out) is det,
	mode map(pred(in, out) is cc_multi, in, out) is cc_multi,
	mode map(pred(in, out) is semidet, in, out)	is semidet,
	mode map(pred(in, out) is multi, in, out) is multi,
	mode map(pred(in, out) is nondet, in, out) is nondet
].

%-----------------------------------------------------------------------------%

:- pred det_index(T::in, int::in, U::out) is det <= index(T, U).

:- pred det_set_index(int::in, U::in, T::in, T::out) is det <= index(T, U).

%-----------------------------------------------------------------------------%

:- func index(int, T) = U is semidet <= index(T, U).

:- func 'index :='(int, T, U) = T is semidet <= index(T, U).


:- func det_index(int, T) = U <= index(T, U).

:- func 'det_index :='(int, T, U) = T <= index(T, U).

%-----------------------------------------------------------------------------%

:- func map(func(U) = U, T) = T <= index(T, U).
:- mode map(func(in) = out is det, in) = out is det.
:- mode map(func(in) = out is semidet, in) = out is semidet.





	

:- implementation.

:- import_module require.
:- import_module string.
:- import_module type_desc.

%-----------------------------------------------------------------------------%

det_index(T, I, U) :-
	if valid_index(T, I) then (
		if index(T, I, V) 
			then U = V
			else unexpected($module, $pred, 
				"Index " ++ string(I) ++ 
				" out of range for method call of index(" ++ 
				type_name(type_of(T)) ++ ", " ++ type_name(type_of(U)) ++
				"). mh_index.valid_index/2 should not have allowed this!")
		)
		else unexpected($module, $pred, 
				"Index " ++ string(I) ++ 
				" out of range for method call of index(" ++ 
				type_name(type_of(T)) ++ ", " ++ type_name(type_of(U)) ++
				").").
				
det_set_index(I, U, !T) :-
	if valid_index(!.T, I) then (
		if set_index(I, U, !.T, NewT) 
			then !:T = NewT
			else unexpected($module, $pred, 
				"Index " ++ string(I) ++ 
				" out of range for method call of index(" ++ 
				type_name(type_of(!.T)) ++ ", " ++ type_name(type_of(U)) ++
				"). mh_index.valid_index/2 should not have allowed this!")
		)
		else unexpected($module, $pred, 
				"Index " ++ string(I) ++ 
				" out of range for method call of index(" ++ 
				type_name(type_of(!.T)) ++ ", " ++ type_name(type_of(U)) ++
				").").

%-----------------------------------------------------------------------------%

T ^ index(I) = U :- index(T, I, U).

(!.T ^ index(I) := U) = !:T :- set_index(I, U, !T).

T ^ det_index(I) = U :- det_index(T, I, U).

(!.T ^ det_index(I) := U) = !:T :- det_set_index(I, U, !T).

%-----------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(map/2).

map(Function::in(func(in) = out is det), !.T::in) = (!:T::out) :- 
	Closure = (pred(U0::in, U::out) is det :- Function(U0) = U),
	map(Closure, !T).
	
map(Function::in(func(in) = out is semidet), !.T::in) = (!:T::out) :- 
	Closure = (pred(U0::in, U::out) is semidet :- Function(U0) = U),
	map(Closure, !T).




%-----------------------------------------------------------------------------%

