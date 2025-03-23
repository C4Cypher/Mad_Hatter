%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_mode.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_mode.

:- interface.

:- import_module array.

:- import_module mh_term.
:- import_module mh_predicate.

%-----------------------------------------------------------------------------%
% Term modes


:- type mh_mode(T)
	--->	T >> T
	;		compound_mode(array(mh_mode(T))).
	
	

:- type mh_mode == mh_mode(predicate_term).
	
:- func in(T) = mh_mode(T).
:- func out(T) = mh_mode(T).

	

	
%-----------------------------------------------------------------------------%
% Tuple modes

:- type tuple_mode(T) --->	tuple_mode(list(mh_mode(T))). 



%-----------------------------------------------------------------------------%
% Relation mode a
	
:- type relation_mode
	--->	relation_mode(list(mh_mode), mh_mode).% TODO: Determinism?

	
%-----------------------------------------------------------------------------%
% Predicate mode and signature

:- type predicate_mode 
	--->	predicate_mode(list(mh_mode)). % TODO: Determinism?


	
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
