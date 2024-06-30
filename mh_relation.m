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

:- import_module list.
:- import_module array.

:- import_module mh_term.
:- import_module mh_index.

%-----------------------------------------------------------------------------%

:- typeclass relation(T) <= index(T, mh_term) where [
% vars  ouput vars? list or nondet?
% vars_at output arguments?
% is ground?

].


%-----------------------------------------------------------------------------%
% List relations

:- instance index(list(mh_term), mh_term).

:- instance relation(list(mh_term)).
	



%-----------------------------------------------------------------------------%
% List relations

:- instance index(array(mh_term), mh_term).

:- instance relation(array(mh_term)).



	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


:- implementation.

%-----------------------------------------------------------------------------%
% List relations

:- instance index(list(mh_term), mh_term) where [
	pred(index/3) is list_index,
	pred(set_index/4) is set_list_index,
	pred(fold_index/4) is fold_list_index,
	pred(map_index/3) is map_list_index
].

:- instance relation(list(mh_term)) where [ ].

%-----------------------------------------------------------------------------%
% Array relations

:- instance index(array(mh_term), mh_term) where [
	pred(index/3) is array_index,
	pred(set_index/4) is set_array_indexl,
	pred(fold_index/4) is fold_array_index,
	pred(map_index/3) is map_array_index
].

:- instance relation(array(mh_term)) where [ ].