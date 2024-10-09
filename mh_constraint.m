%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_constraint.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_constraint.

:- interface.

:- import_module set.
:- import_module list.
:- import_module type_desc.

:- import_module mh_term.
:- import_module mh_clause.
:- import_module mh_substitution.
:- import_module mh_arity.

%-----------------------------------------------------------------------------%
% Term constraints

:- type mh_constraint
	---> 	invalid_constraint(mh_term, string) 	
	;		mr_type_constraint(type_desc) % mercury type constraint
	%;		mr_ptype_constraint(pseudo_type_desc)
	
	% X:Y :- Body
	% Airty 2 clause handling the application of constraint rules, differs
	% from unification in that the rhs *replaces* the lhs in substitutions
	;		constraint_clause(mh_term, mh_term, mh_clause)
	;		mr_constraint(pred(mh_term::in, mh_term::out) is semidet).
	
:- pred apply_constraint_substitution(mh_substitution::in, mh_constraint::in,
	mh_constraint::out) is det.
	
 :- func constraint_arity(mh_constraint) = int.
 
 :- pred constraint_arity(mh_constraint::in, int::out) is det.
 
 :- instance arity(mh_constraint).
	
	
%-----------------------------------------------------------------------------%
% Constraint mercury predicates

:- type mr_constraint_pred == pred(mh_term, mh_term).
:- inst mr_constraint_pred == (pred(in, out) is semidet).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%
% Term constraints

apply_constraint_substitution(_, _, _) :- sorry($module, $pred,
	"apply_constraint_substitution/3").
	
:- pragma no_determinism_warning(apply_constraint_substitution/3).

constraint_arity(_) = _ :- sorry($module, $pred, "constraint_arity/1").

:- pragma no_determinism_warning(constraint_arity/1).

constraint_arity(T, constraint_arity(T)).

:- instance arity(mh_constraint) where [ pred(arity/2) is constraint_arity ].
