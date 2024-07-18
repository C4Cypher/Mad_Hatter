%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_substitution.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_substitution.

:- import_module map.
:- import_module array.
:- import_module enum.

:- import_module mh_term.
:- import_module mh_var.

:- interface.

%-----------------------------------------------------------------------------%

:- type mh_substitution
	--->	substitution_map(map(var_id, mh_term))
	;		substitution_array(array(mh_term)).
	
:- pred empty_substitution(mh_substitution).
:- mode empty_substitution(in) is semidet.
:- mode empty_substitution(out) is multi.

%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


empty_substitution(substitution_map(init)).
empty_substitution(substitution_array(make_empty_array)).