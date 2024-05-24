%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_relation.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_relation.

:- interface.

:- import_module mh_term.
:- import_module mh_index.
:- import_module mh_argument.
:- import_module mh_arity.

:- import_module array.
:- import_module list.

%-----------------------------------------------------------------------------%

:- typeclass relation(T) <= (index(T, mh_term), arity(T)) where [
	
	
].

%-----------------------------------------------------------------------------%
:- type relation 
	---> 	some [T] relation(T) => relation(T).
	



%-----------------------------------------------------------------------------%




	
%-----------------------------------------------------------------------------%
	


%-----------------------------------------------------------------------------%


:- implementation.




%-----------------------------------------------------------------------------%

