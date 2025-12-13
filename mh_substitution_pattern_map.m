%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_substitution_pattern_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_substitution_pattern_map.

:- interface.

:- use_module map.
:- import_module list.
:- import_module assoc_list.

:- import_module mh_substitution_map.
:- import_module mh_term_map.
:- import_module mh_term.
:- import_module mh_var_map.
:- import_module mh_substitution.

%-----------------------------------------------------------------------------%
% Substitution Pattern map

:- type var_substitution_map(T) == mh_var_map(mh_substitution_map(T)).
:- type term_substitution_map(T) == mh_term_map(mh_substitution_map(T)).

:- type substitution_pattern_map(T) 
	--->	pattern_map(
				var_map :: var_substitution_map(T),
				term_map :: term_substitution_map(T)
			).

:- func init = (substitution_pattern_map(_)::out) is det.
:- pred init(substitution_pattern_map(_)::out) is det.

:- func singleton(mh_substitution, T) = substitution_pattern_map(T).

:- pred is_empty(substitution_pattern_map(_)::in) is semidet.

:- func from_exact_map(map.map(mh_substitution, T)) 
	= substitution_pattern_map(T).

%-----------------------------------------------------------------------------%
% Insertion

% does not modify map if substitution is already present
:- pred insert(mh_substitution::in, T::in, substitution_pattern_map(T)::in,
	substitution_pattern_map(T)::out)	is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- implementation.

%-----------------------------------------------------------------------------%
% Substitution Pattern map

init = pattern_map(
	mh_var_map.init,
	mh_term_map.init
).
init(init).

singleton(Sub, T) = Map :- insert(Sub, T, init, Map).

is_empty(init). % All descendants have empty constructors

:- func insert_pattern(mh_substitution, T, substitution_pattern_map(T)) 
	= substitution_pattern_map(T).
insert_pattern(P, V, !.Map) = !:Map :- insert(P, V, !Map).

from_exact_map(Exact) = map.foldl(insert_pattern, Exact, init).

%-----------------------------------------------------------------------------%
% Insertion

%- pred vsm_insert(Sub, Value, Var, !VSM).
:- pred vsm_insert(mh_var_map(mh_term)::in, T::in, mh_var::in, 
	var_substitution_map(T)::in, var_substitution_map(T)::out) is det.

vsm_insert(Sub, Val, Var, !VM) :-
		(if search(!.VM, Var, SM)
		then
			(if insert(Sub, Val, SM, NewSM)
			then
				set(Var, NewSM, !VM)
			else true
			)
		else
			det_insert(Var, singleton(Sub, Val), !VM)
		).

%- pred tsm_insert(Sub, Term, Value, !TSM).
:- pred tsm_insert(mh_var_map(mh_term)::in, mh_term::in, T::in, 
	term_substitution_map(T)::in, term_substitution_map(T)::out) is det.

tsm_insert(Sub, Term, Val, !TM) :-
		(if search(!.TM, Term, SM)
		then
			(if insert(Sub, Val, SM, NewSM)
			then
				set(Term, NewSM, !TM)
			else true
			)
		else
			det_insert(Term, singleton(Sub, Val), !TM)
		).

%- pred insert_fold(Sub, Value, Var, Term, !Pattern).	
:- pred insert_fold(mh_var_map(mh_term)::in, T::in, mh_var::in, mh_term::in,
	substitution_pattern_map(T)::in, substitution_pattern_map(T)::out) is det.
	
insert_fold(Sub, Val, Var, Term, pattern_map(!.VSM, !.TSM), 
	pattern_map(!:VSM, !:TSM)) :-
	vsm_insert(Sub, Val, Var, !VSM),
	tsm_insert(Sub, Val, Term, !TSM).
	
insert(Sub, Value, !Map) :- fold(insert_fold(Sub, Value), !Map).

	
	
	

