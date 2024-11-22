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

:- import_module list.

:- import_module mh_signature.

%-----------------------------------------------------------------------------%
% Term modes

:- type mh_mode
	--->	in	
	;		out
	;		inout(tuple_signature)
	;		unused.

	% TODO: something akin to unique modes?
	% implemented differently, the inst system seemed to be a dead end for
	% mercury, mercury inst's are a powerful, expressive and underutilized
	% tool ... and I'm sure that the back end performs some impressive 
	% optimizations with it, for now I'm just going to stick with ground
	% and free ... implicitly, skipping the inst step altogether when
	% performing mode optimization, for now
	
%-----------------------------------------------------------------------------%
% Tuple modes

:- type tuple_mode --->	tuple_mode(list(mh_mode)). 



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
