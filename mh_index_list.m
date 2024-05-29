%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_index_list.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_index_list.

:- interface.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred valid_list_index0(list(T), int).
:- mode valid_list_index0(in, in) is semidet.
:- mode valid_list_index0(in, out) is nondet. 

:- pred list_index0(list(T), int, T).
:- mode list_index0(in, in, in) is semidet.
:- mode list_index0(in, in, out) is semidet. 
:- mode list_index0(in, out, out) is nondet. 
	
:- pred set_list_index0(int, T, list(T), list(T)).
:- mode set_list_index0(in, in, in, out) is semidet. 
:- mode set_list_index0(out, in, in, out) is nondet.


%-----------------------------------------------------------------------------%

:- pred valid_list_index1(list(T), int).
:- mode valid_list_index1(in, in) is semidet.
:- mode valid_list_index1(in, out) is nondet. 

:- pred list_index1(list(T), int, T).
:- mode list_index1(in, in, in) is semidet.
:- mode list_index1(in, in, out) is semidet. 
:- mode list_index1(in, out, out) is nondet. 
	
:- pred set_list_index1(int, T, list(T), list(T)).
:- mode set_list_index1(in, in, in, out) is semidet. 
:- mode set_list_index1(out, in, in, out) is nondet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.


%-----------------------------------------------------------------------------%
	
valid_list_index0([_], 0).

valid_list_index0([_ | Vs], I) :-
	I > 0,
	valid_list_index0(Vs, I - 1).
	


:- pragma promise_equivalent_clauses(list_index0/3).

list_index0(L::in, I::in, V::in) :- index0(L, I, V).

list_index0(L::in, I::in, V::out) :- index0(L, I, V). 
		
list_index0([V | Vs]::in, I::out, U::out) :-
	I = 0, V = U;
	I > 0, list_index0(Vs @ [_ | _], I - 1, U). 


set_list_index0(I, V, !L) :- 
	valid_list_index0(!.L, I),
	det_replace_nth(!.L, I + 1, V, !:L).
	
	
%-----------------------------------------------------------------------------%
	
valid_list_index1([_], 1).

valid_list_index1([_ | Vs], I) :-
	I > 1,
	valid_list_index1(Vs, I - 1).
	


:- pragma promise_equivalent_clauses(list_index1/3).

list_index1(L::in, I::in, V::in) :- index1(L, I, V).

list_index1(L::in, I::in, V::out) :- index1(L, I, V). 
		
list_index1([V | Vs]::in, I::out, U::out) :-
	I = 1, V = U;
	I > 1, list_index1(Vs @ [_ | _], I - 1, U). 




set_list_index1(I, V, !L) :- 
	valid_list_index0(!.L, I),
	det_replace_nth(!.L, I, V, !:L).
	