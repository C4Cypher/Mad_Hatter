%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_argument.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_argument.

:- interface.

:- import_module mh_index.

%-----------------------------------------------------------------------------%

:- type argument
--->	arg(int)
;		arg(int, argument).


%-----------------------------------------------------------------------------%

:- pred valid_argument(T, argument) <= (index(T, U), index(U, U)).
:- mode valid_argument(in, in) is semidet.
:- mode valid_argument(in, out) is nondet.


:- pred argument(T, argument, U) <= (index(T, U), index(U, U)).
:- mode argument(in, in, in) is semidet.
:- mode argument(in, in, out) is semidet.
:- mode argument(in, out, out) is nondet.

:- func argument(T, argument) = U <= (index(T, U), index(U, U)).
:- mode argument(in, in) = in is semidet.
:- mode argument(in, in) = out is semidet.
:- mode argument(in, out) = out is nondet.

:- pred set_argument(argument, U, T, T) <= (index(T, U), index(U, U)).
:- mode set_argument(in, in, in, out) is semidet. 
:- mode set_argument(out, in, in, out) is nondet.

:- func set_argument(T, argument, U) = T.
:- mode set_argument(in, in, in) = out is semidet.
:- mode set_argument(in, out, in) = out is nondet.

%-----------------------------------------------------------------------------%





%-----------------------------------------------------------------------------%

:- implementation.

valid_argument(T, arg(I) ) :- valid_index(T, I).

valid_argument(T, arg(I, Arg) ) :- 
	index(T, I, V),
	valid_argument(V, Arg).

argument(T, arg(I), V) :- index(T, I, V).

argument(T, arg(I, Arg), V) :- 
	index(T, I, U),
	argument(U, Arg, V).
	
argument(T, Arg) = V :- argument(T, Arg, V).
	
set_argument(arg(I), V, !T) :- set_index(I, V, !T).

set_argument(arg(I, Arg), V, !T) :-
	index(!.T, I, U0),
	set_argument(Arg, V, U0, U1),
	set_index(I, U1, !T).
	
set_argument(!.T, Arg, V) = !:T :- set_argument(Arg, V, !T).