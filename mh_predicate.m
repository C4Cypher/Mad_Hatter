%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_predicate.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_predicate.

:- interface.

:- import_module mh_term.
:- import_module mh_arity.
%-----------------------------------------------------------------------------%

:- type mh_predicate 
	--->	predicate_fact(mh_term) % \X
	;		predicate_clause(mh_term, mh_term) % \X :- Y
	;		some [T] mr_predicate(T) => predicate(T).

%-----------------------------------------------------------------------------%



:- typeclass predicate(T)  where 
[
	% Todo:  predicate to call predicate under a module and scope
].


