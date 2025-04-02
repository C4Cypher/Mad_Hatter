%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_context.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_context.

:- interface.

:- use_module term_context.

%-----------------------------------------------------------------------------%
% Context

:- type mr_context == term_context.term_context.

:- type mh_context
	--->	dummy_context % unifies with context("", 0)
	;		interactive_context(int) % For REPL commands
	;		clause_context(filename::string, linenumber::int).
	%TODO: contexts for relations loaded from foreign library modules

% Convert context from mercury term into a clause context or dummy context
:- func from_mr_context(mr_context) = mh_context.	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%
% Context

from_mr_context(context(File, Line)) = 
	(if File = "", Line = 0
	then 
		dummy_context
	else
		clause_context(File, Line)
	).