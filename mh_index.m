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

:- import_module list.
:- import_module array.

% :- import_module mh_arity.

%-----------------------------------------------------------------------------%

% This module was originally for the index(T -> U) typeclass, however that 
% typeclass structure ended up being unworkable, now it's to store common
% implementations of index operations for lists and arrays


%-----------------------------------------------------------------------------%
% List index implementation methods

:- pred list_index(list(T), int, T).
:- mode list_index(in, in, out) is det. 
:- mode list_index(in, out, out) is nondet. 
	
:- pred set_list_index(int, T, list(T), list(T)).
:- mode set_list_index(in, in, in, out) is det. 
:- mode set_list_index(out, in, in, out) is nondet.

:- pred fold_list_index(pred(T, A, A), list(T), A, A). 
:- mode fold_list_index(pred(in, in, out) is det, in, in, out) is det.
:- mode fold_list_index(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode fold_list_index(pred(in, di, uo) is det, in, di, uo) is det.
:- mode fold_list_index(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode fold_list_index(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode fold_list_index(pred(in, di, uo) is semidet, in, di, uo) is semidet.


:- pred map_list_index(pred(T, T), list(T), list(T)).
:- mode map_list_index(pred(in, out) is det, in, out) is det.

:- pred all_list_index(pred(T), list(T)).
:- mode all_list_index(pred(in) is semidet, in) is semidet.
	
%-----------------------------------------------------------------------------%
% Array index implementation methods

:- pred array_index(array(T), int, T).
:- mode array_index(in, in, out) is det. 
:- mode array_index(in, out, out) is nondet. 
	
:- pred set_array_index(int, T, array(T), array(T)).
:- mode set_array_index(in, in, in, out) is det. 
:- mode set_array_index(out, in, in, out) is nondet.

:- pred fold_array_index(pred(T, A, A), array(T), A, A). 
:- mode fold_array_index(pred(in, in, out) is det, in, in, out) is det.
:- mode fold_array_index(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode fold_array_index(pred(in, di, uo) is det, in, di, uo) is det.
:- mode fold_array_index(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode fold_array_index(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode fold_array_index(pred(in, di, uo) is semidet, in, di, uo) is semidet.


:- pred map_array_index(pred(T, T), array(T), array(T)).
:- mode map_array_index(pred(in, out) is det, in, out) is det.

:- pred all_array_index(pred(T), array(T)).
:- mode all_array_index(pred(in) is semidet, in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.


:- import_module int.

:- import_module mh_arity.

				
%-----------------------------------------------------------------------------%
% List index implementation methods

:- pragma promise_equivalent_clauses(list_index/3).

list_index(L::in, I::in, V::out) :- det_index1(L, I, V). 
		
list_index([V | Vs]::in, I::out, U::out) :-
	I = 1, V = U;
	I > 1, list_index(Vs @ [_ | _], I - 1, U). 


:- pragma promise_equivalent_clauses(set_list_index/4).

set_list_index(I::in, T::in, !.L::in, !:L::out) :-
	det_replace_nth(!.L, I, T, !:L).


set_list_index(I::out, T::in, !.L::in, !:L::out) :-
	Len = length(!.L),
	set_list_index(I, T, !L, Len).

:- pred set_list_index(int::out, T::in, list(T)::in, list(T)::out, int::in)
		is nondet.
		
set_list_index(I, T, [_ | Vs], [T | Vs], I).

set_list_index(I + 1, T, [V | !.Vs], [V | !:Vs], Len) :-
	Len > 1,
	set_list_index(I, T, !Vs, Len - 1).
	
	
fold_list_index(_, [], !A).

fold_list_index(Closure, [T | L], !A) :-
	Closure(T, !A),
	fold_list_index(Closure, L, !A).

/* Not the behavior I wanted
fold_list_index(Closure, [T | L], A0, A3) :-
	A3 = (if 
			Closure(T, A0, A1),
			fold_list_index(Closure, L, A1, A2)
		then
			A2
		else
			A0
		).
*/  
map_list_index(_, [], []).

map_list_index(Closure, [ !.T | !.L ], [ !:T | !:L]) :-
	Closure(!T),
	map_list_index(Closure, !L).

all_list_index(_, []).

all_list_index(Pred, [ T | L ]) :- Pred(T), all_list_index(Pred, L).
	
%-----------------------------------------------------------------------------%
% Array index implementation methods

:- pragma promise_equivalent_clauses(array_index/3).

array_index(A::in, I::in, V::out) :- unsafe_lookup(A, I - 1, V).

array_index(A::in, I::out, V::out) :-
	valid_index(A, I),
	unsafe_lookup(A, I - 1, V).

:- pragma promise_equivalent_clauses(set_array_index/4).

set_array_index(I::in, V::in, !.A::in, !:A::out) :-
	slow_set(I - 1, V, !A).
	
set_array_index(I::out, V::in, !.A::in, !:A::out) :-
	valid_index(!.A, I),
	slow_set(I - 1, V, !A).
	
:- pragma inline(fold_array_index/4).
	
fold_array_index(Closure, Array, !A) :- foldl(Closure, Array, !A).

:- pragma inline(map_array_index/3).

map_array_index(Closure, !Array) :- map(Closure, !Array).

all_array_index(Pred, Array) :- all_true(Pred, Array).