%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_predicate.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_predicate.

:- interface.

:- import_module list.

:- import_module mh_term.
:- import_module mh_substitution.
:- import_module mh_arity.
%-----------------------------------------------------------------------------%

:- type mh_predicate 
	--->	invalid_predicate(mh_term, string)
 	;		pred_success(mh_substitution)
  	;		pred_fail(pred_fail_reason)
	;		some [T] mr_predicate(T) => predicate(T).
	
	
:- pred apply_predicate_substitution(mh_substitution::in, mh_predicate::in,
	mh_predicate::out) is det.

%-----------------------------------------------------------------------------%



:- typeclass predicate(T) <= arity(T)  where 
[
	% Todo:  predicate to call predicate under a module and scope
].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%

apply_predicate_substitution(_, _, _) :- sorry($module, $pred,
	"apply_predicate_substitution/3").

:- pragma no_determinism_warning(apply_predicate_substitution/3).
