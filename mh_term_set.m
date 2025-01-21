%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_term_set.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_term_set.

:- import_module unit.

:- import_module mh_term.
:- import_module mh_term_map.


:- interface.

%-----------------------------------------------------------------------------%
% Term sets

:- type mh_term_set == mh_term_map(unit).

% :- pred init(mh_term_set::out) is det.
% :- func init = mh_term_set.

% :- func singleton(mh_term) = mh_term_set.




%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module set.
:- import_module type_desc.
:- import_module univ.

:- import_module mh_tuple.
:- import_module mh_symbol.

%-----------------------------------------------------------------------------%
% Term sets

:- type mh_term_set

	% un-treed ancestors
	--->	empty_set
	;		singleton_set(mh_term)
	;		set_tuple(mh_tuple) % input tuple of ordered terms
	
	% simple sets
	;		nil_set	% the set of nil values
	;		atom_set(symbol_set)
	;		var_set(var_id_offset, var_id_set)
	;		var_set(var_id_offset, var_id_set, mh_var_set)
	;		mr_value_set(univ_set)
	;		simple_set()
