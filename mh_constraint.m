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

%-----------------------------------------------------------------------------%
% Term constraints

:- type mh_constraint
	---> 	invalid_constraint(mh_term, string) 	
	;		mr_type_constraint(type_desc) % mercury type constraint
	%;		mr_ptype_constraint(pseudo_type_desc)
	
	% @X
	% constrains term to term without being ground, if the term or var in the
	% constraint is a normal term, the term constrained is unified as normal,
	% if the constraint term is a constraint, handle as if it were another 
	% branch of this type
	;		constraint_term(mh_term)
	
	% X:Y :- Body
	% Airty 2 clause handling the application of constraint rules, differs
	% from unification in that the rhs *replaces* the lhs in substitutions
	;		constraint_clause(mh_term, mh_term, mh_clause).
