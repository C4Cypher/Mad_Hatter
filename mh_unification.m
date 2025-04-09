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
:- import_module ordered_set.
:- import_module mh_tuple.
:- import_module mh_environment.
:- import_module mh_substitution.

%-----------------------------------------------------------------------------%
% Unification

%TODO: Implement user defined equality and comparison?

:- type mh_unification 
	--->	unification_set(ordered_set(mh_term)).	% X = Y = Z

% A unification with only one member can be easily reduced to a single term.

:- pred empty_unification(mh_unification).
:- mode empty_unification(in) is semidet.
:- mode empty_unification(out) is det.

:- func empty_unification = mh_unification.

:- pred singleton(mh_term, mh_unification).
:- mode singleton(in, out) is det.
:- mode singleton(out, in) is semidet.

:- func singleton(mh_term) = mh_unification.
:- mode singleton(in) = out is det.
:- mode singleton(out) = in is semidet.

% Binary constructor, throws an exception
:- func unificaiton(mh_term, mh_term) = mh_unification.
:- mode unificaiton(in, in) = out is det.
:- mode unification(in, out) = in is nondet.
:- mode unification(out, in) = in is nondet.
:- mode unification(out, out) = in is nondet.

% Urnary constructor with a tuple of terms.  Output terms will be sorted.
:- func unification(mh_tuple) = mh_unification.
:- mode unification(in) = out is det.
:- mode unification(out) = in is det.

% chain/merge unifications, unifications with common elements will be composed
% into a single unification, equivalent unifications will be merged, fails
% if two unifications do not share common equal terms. Does not unify subterms
:- pred compose_unifications(mh_unification::in, mh_unification::in,
	mh_unification::out) is semidet.
	
:- func compose_unifications(mh_unification, mh_unification) = mh_unification
	is semidet. 



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%
