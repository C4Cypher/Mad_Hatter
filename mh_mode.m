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
	;		inout(tuple_signature)
	;		unused.

%-----------------------------------------------------------------------------%
%	Signatures contain all of the information require to derive the mode of
% 	a procedure

:- type term_signature ---> mh_type :: mh_mode.

:- type tuple_signature == list(term_signature).

:- type proc_signature
	---> 	relation_signature(tuple_signature, term_signature)
	;		predicate_signature(tuple_signature)
	;		function_signature(tuple_type, mh_type).
	
:- type relation_signature =< proc_signature
	--->	relation_signature(tuple_signature, term_signature).
	
:- type predicate_signature =< proc_signature
	---> 	predicate_signature(tuple_signature).

:- type function_signature =< proc_signature
	---> 	function_signature(tuple_type, mh_type).
	
%-----------------------------------------------------------------------------%

:- type proc_mode
	--->	relation_mode(tuple_signature, term_signature)
	;		predicate_mode(tuple_signature).
	
:- type relation_mode =< proc_mode
	--->	relation_mode(tuple_signature, term_signature).

:- type predicate_mode =< proc_mode
	--->	predicate_mode(tuple_signature).

%-----------------------------------------------------------------------------%

:- typeclass signature(T, S) <= (T -> S) where [
	pred signature(T, S),
	mode signature(in, in) is semidet,
	mode signature(in, out) is nondet
].

%-----------------------------------------------------------------------------%

:- typeclass to_mode(S, M) <= (S -> M) where [ func to_mode(S) = M ].

:- instance to_mode(proc_signature, proc_mode).
:- instance	to_mode(relation_signature, relation_mode).
:- instance to_mode(predicate_signature, predicate_mode).
:- instance to_mode(function_signature, relation_mode).

%-----------------------------------------------------------------------------%

:- pred mode(T, M) <= (signature(T, S), to_mode(S, M)).
:- mode mode(in, in) is semidet.
:- mode mode(in, out) is nondet.





%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- instance to_mode(proc_signature, proc_mode) where [
	to_mode(relation_signature(S, R)) = relation_mode(S, R),
	to_mode(predicate_signature(S)) = predicate_mode(S),
	to_mode(function_signature(S, R)) = relation_mode(all_in(S), R :: out)
].

:- instance to_mode(relation_signature, relation_mode) where [
	to_mode(relation_signature(S, R)) = relation_mode(S, R)
].

:- instance to_mode(predicate_signature, predicate_mode) where [
	to_mode(predicate_signature(S)) = predicate_mode(S)
].

:- instance to_mode(function_signature, relation_mode) where [
	to_mode(function_signature(S, R)) = relation_mode(all_in(S), R :: out)
].
	
	
:- func all_in(tuple_type) = list(term_signature).

all_in([]) = [].
all_in([ X | XS ]) = [ X :: in | all_in(XS) ].

%-----------------------------------------------------------------------------%

mode(T, to_mode(S)) :- signature(T, S). 







