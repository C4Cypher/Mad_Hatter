%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_identifier.construct_id.m
% Main author: C4Cypher.
% Stability: low.

:- module mh_identifier.construct_id.

:- interface.

%-----------------------------------------------------------------------------%
% The following is intended for types that store the int identifier internally
% it is not meant for constructing id's arbitrarily, as that would break the
% intended semantics for normal usage of the mh_identifier module.

:- func id_constructor(int) = id(T).
:- mode id_constructor(in) = out is det.
:- mode id_constructor(out) = in is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

id_constructor(I) = id(I).