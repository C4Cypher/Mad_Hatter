%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_prefix.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_prefix.

:- interface.

% :- import_module enum.

:- import_module mh_term.

%-----------------------------------------------------------------------------%
% Prefix enums

:- type mh_term_prefix_enum % 3 bits (octal)
	---> 	penum_nil		% 0
	;		penum_atom		% 1
	;		penum_var		% 2
	;		penum_mr_value	% 3
	;		penum_cons		% 4
	;		penum_tuple		% 5
	;		penum_lazy		% 6
	;		penum_lambda.	% 7

:- type mh_lambda_prefix_enum % 2 bits (quaternary)
	;		penum_relation		% 0
	;		penum_predicate		% 1
	;		penum_function		% 2
	;		penum_constraint.	% 3

	
%-----------------------------------------------------------------------------%
% Prefix keys



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

