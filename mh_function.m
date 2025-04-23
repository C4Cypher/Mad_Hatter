%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_function.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_function.

:- interface.

:- import_module mh_term.
:- import_module mh_relation.

:- type function_call == (func(mh_term) = mh_term)

:- type mh_function
	--->	pure_function(mh_relation)
	;		compiled_function(mh_relation, function_call)
	;		


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%

apply_function_substitution(_, _, _) :- sorry($module, $pred,
	"apply_function_substitution/3").
	
:- pragma no_determinism_warning(apply_function_substitution/3).
