%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_environment.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_environment.

:- interface.

:- import_module mh_term.
:- import_module mh_term_map.

%-----------------------------------------------------------------------------%
% Environment

% An environment serves as both a lookup table for predefined symbols, and as
% a memo table for already performed evaluations. For each 'pure' evaluation
% of a term, the environment gets updated with that evaluation.
%
% In effect, for every binding in an environment, logically `Key -> Value`

:- type mh_environment == mh_term_map(mh_term).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
