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

%-----------------------------------------------------------------------------%
% Term modes


:- type mh_mode
	--->	mh_constraint >> mh_constraint
	;		in(mh_constraint)
	;		out(mh_constraint)
	;		compound_mode(array(mh_mode)).


% Unify modes
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
