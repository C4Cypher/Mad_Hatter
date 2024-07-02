%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_mode.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_mode.

:- interface.

:- import_module list.

:- import_module mh_type.

%-----------------------------------------------------------------------------%

:- type mh_mode
	--->	in	
	;		out
	;		inout(relation_signature)
	;		unused.

%-----------------------------------------------------------------------------%
%	Signatures contain all of the information require to derive the mode of
% 	a procedure

:- type term_signature ---> mh_type :: mh_mode.

:- type tuple_signature == list(term_signature).

:- type proc_signature
	---> 	predicate_signature(relation_signature)
	;		functor_signature(relation_signature, term_signature)
	;		function_signature(relation_type, mh_type).

:- type predicate_signature =< proc_signature
	---> 	predicate_signature(relation_signature).
	
:- type functor_signature =< proc_signature
	--->	functor_signature(relation_signature, term_signature).
	
:- type function_signature =< proc_signature
	---> 	function_signature(relation_type, mh_type).
	


%-----------------------------------------------------------------------------%

:- type proc_mode
	--->	predicate_mode(relation_signature)
	;		functor_mode(relation_signature, term_signature).
	
:- type predicate_mode =< proc_mode
	--->	predicate_mode(relation_signature).
	
:- type functor_mode =< proc_mode
	--->	functor_mode(relation_signature, term_signature).


%-----------------------------------------------------------------------------%

:- typeclass signature(T, S) <= (T -> S) where [
	pred signature(T, S),
	mode signature(in, in) is semidet,
	mode signature(in, out) is nondet
].

%-----------------------------------------------------------------------------%

:- typeclass to_mode(S, M) <= (S -> M) where [ func to_mode(S) = M ].

:- instance to_mode(proc_signature, proc_mode).
:- instance to_mode(predicate_signature, predicate_mode).
:- instance	to_mode(functor_signature, functor_mode).
:- instance to_mode(function_signature, functor_mode).

%-----------------------------------------------------------------------------%

:- pred mode(T, M) <= (signature(T, S), to_mode(S, M)).
:- mode mode(in, in) is semidet.
:- mode mode(in, out) is nondet.





%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- instance to_mode(proc_signature, proc_mode) where [
	to_mode(predicate_signature(S)) = predicate_mode(S),
	to_mode(functor_signature(S, R)) = functor_mode(S, R),
	to_mode(function_signature(S, R)) = functor_mode(all_in(S), R :: out)
].

:- instance to_mode(predicate_signature, predicate_mode) where [
	to_mode(predicate_signature(S)) = predicate_mode(S)
].

:- instance to_mode(functor_signature, functor_mode) where [
	to_mode(functor_signature(S, R)) = functor_mode(S, R)
].

:- instance to_mode(function_signature, functor_mode) where [
	to_mode(function_signature(S, R)) = functor_mode(all_in(S), R :: out)
].
	
	
:- func all_in(relation_type) = list(term_signature).

all_in([]) = [].
all_in([ X | XS ]) = [ X :: in | all_in(XS) ].

%-----------------------------------------------------------------------------%

mode(T, to_mode(S)) :- signature(T, S). 







