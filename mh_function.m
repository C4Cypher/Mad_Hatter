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

:- import_module mh_substitution.
:- import_module mh_arity.

:- type mh_function
	--->	mh_function(relation_term)
	;		some [T] mr_function(T) => function(T).
	
:- pred apply_function_substitution(mh_substitution::in, mh_function::in,
	mh_function::out) is det.
	
:- pred ground_function(mh_function::in) is semidet.


 :- func function_arity(mh_function) = int.
 
 :- pred function_arity(mh_function::in, int::out) is det.
 
 :- instance arity(mh_function).
	
	
%-----------------------------------------------------------------------------%
% Function typeclass

:- typeclass function(T) where [].


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


function_arity(_) = _ :- sorry($module, $pred, "function_arity/1").

:- pragma no_determinism_warning(function_arity/1).

function_arity(T, function_arity(T)).

:- instance arity(mh_function) where [ pred(arity/2) is function_arity ].
