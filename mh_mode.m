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
	
:- type term_signature ---> mh_type :: mh_mode.

:- type relation_signature == list(term_signature).

:- type proc_signature
	---> 	predicate_signature(relation_signature)
	;		function_signature(relation_signature, term_signature)
	;		operation_signature(list(mh_type), mh_type).

:- type predicate_signature =< proc_signature
	---> 	predicate_signature(relation_signature).
	
:- type function_signature =< proc_signature
	---> 	function_signature(relation_signature, term_signature).
	
:- type operation_signature =< proc_signature
	--->	operation_signature(list(mh_type), mh_type).





