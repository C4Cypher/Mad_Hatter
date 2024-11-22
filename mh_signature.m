%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
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

:- typeclass signature(Signature, Type, Mode) 
	<= ((Signature -> Type), (Signature -> Mode)) where [
	pred signature(Signature, Type, Mode),
	mode signature(in, out, out) is det,
	mode signature(out, in, in) is semidet
].

:- func signature(Type, Mode) = Signature <= signature(Signature, Type, Mode).
:- mode signature(in, in) = out is semidet.
:- mode signature(out, out) = in is det.


%-----------------------------------------------------------------------------%
% Term signature

:- type term_signature ---> term_signature(mh_type, mh_mode).

:- pred term_signature_list(list(term_signature), list(mh_type), 
	list(mh_mode)).
:- mode term_signature_list(in, out, out) is det.
:- mode term_signature_list(out, in, in) is semidet.

:- func term_signature_list(list(mh_type), list(mh_mode)) = 
	list(term_signature).
:- mode term_signature_list(in, in) = out is semidet.
:- mode term_signature_list(out, out) = in is det.

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

term_signature_list([], [], []).

term_signature_list(
	[ term_signature(T, M) | term_signature_list(Ts, Ms) ], 
	[ T | Ts ],	[ M | Ms ] ).

term_signature_list(T, M) = S :- term_signature_list(S, T, M).

:- instance signature(term_signature, mh_type, mh_mode) where [
	signature(term_signature(T, M), T, M)
].

%-----------------------------------------------------------------------------%
% Tuple modes and signatures

:- instance signature(tuple_signature, tuple_type, tuple_mode) where [

	signature(tuple_signature([]), tuple_type([]), tuple_mode([])),

	signature(
		tuple_signature([ signature(T, M) | term_signature_list(Ts, Ms) ]),
		tuple_type([ T | Ts]),
		tuple_mode([ M | Ms])
	) 
	
].

