%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_unification.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_unification.

:- interface.

:- import_module list.

:- import_module mh_term.
:- import_module mh_substitution.
:- import_module mh_event.
:- import_module mh_environment.

%-----------------------------------------------------------------------------%
% Term Unification

	% Memoizing full operation, evaluates the propositional form of the two
	% input terms, producing a substitution that will resolve both terms to
	% the result of unify/5
:- pred unification(mh_calling_context::in, mh_calling_context::out,
	mh_term::in, mh_term::in, mh_proposition::out) is det.
	
	% Return the resulting term resulting from unifying two terms
:- pred unify(mh_calling_context::in, mh_calling_context::out,
	mh_term::in, mh_term::in, mh_term::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%
