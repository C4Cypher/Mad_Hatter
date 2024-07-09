%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_signature.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_signature.

:- interface.

:- import_module list.

:- import_module mh_type.
:- import_module mh_mode.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Signature typeclass

:- typeclass signature(Signature, Type, Mode) where [
	pred signature(Signature, Type, Mode),
	mode signature(in, out, out) is det,
	mode signature(out, in, in) is det
].

:- func signature(Type, Mode) = Signature <= signature(Signature, Type, Mode).
:- mode signature(in, in) = out is det.
:- mode signature(out, out) = in is det.


%-----------------------------------------------------------------------------%
% Term signature

:- type term_signature ---> term_signature(mh_type, mh_mode).

:- instance signature(term_signature, mh_type, mh_mode).
	
%-----------------------------------------------------------------------------%
% Tuple signatures

:- type tuple_signature ---> tuple_signature(list(term_signature)).

:- instance signature(tuple_signature, tuple_type, tuple_mode).




	
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%
% Signature


signature(T, M) = S :- signature(S, T, M).


%-----------------------------------------------------------------------------%
% Term signature

:- instance signature(term_signature, mh_type, mh_mode) where [
	pred signature(term_signature(T, M), T, M)
].

%-----------------------------------------------------------------------------%
% Tuple modes and signatures

:- instance signature(tuple_signature, tuple_type, tuple_mode) where [

	pred signature(tuple_signature([]), tuple_type([]), tuple_mode([])),

	pred signature(
		tuple_signature([ signature(T, M) | signature(Ts, Ms) ]),
		tuple_type([ T | Ts]),
		tuple_mode([ M | Ms])
	)
].