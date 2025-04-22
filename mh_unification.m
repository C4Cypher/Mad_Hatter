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

:- pred unify_terms(mh_term::in, mh_term::in, mh_substitution::out) is semidet.

:- pred unify_terms(mh_term::in, mh_term::in, mh_substitution::out,
	event_log::in, event_log::out) is semidet.

:- pred unify_terms(mh_term::in, mh_term::in, mh_substitution::out, 
	event_log::in, event_log::out, mh_environment::in) is semidet.
	
:- pred unify_terms(mh_term::in, mh_term::in, mh_substitution::out,
	event_log::in, event_log::out,
	mh_environment::in, mh_environment::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%
