%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_function.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_function.

:- interface.

:- import_module mh_relation.
:- import_module mh_substitution.

:- type mh_function
	--->	mh_function(mh_relation)
	;		some [T] mr_function(T) => function(T).
	
:- pred apply_function_substitution(mh_substitution::in, mh_function::in,
	mh_function::out) is det.

:- typeclass function(T) where [].

:- pred ground_function(mh_function::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%

apply_function_substitution(_, _, _) :- sorry($module, $pred,
	"apply_function_substitution/3").
	
:- pragma no_determinism_warning(apply_function_substitution/3).

ground_function(_) :- sorry($module, $pred, "ground_function/1").

:- pragma no_determinism_warning(ground_function/1).
