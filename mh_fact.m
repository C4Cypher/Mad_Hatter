%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_fact.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_fact.

:- interface.

:- import_module list.

:- import_module mh_term.
:- import_module mh_environment.
:- import_module mh_substitution.
:- import_module ordered_set.
:- import_module mh_arity.
:- import_module mh_tuple.
:- import_module mh_tuple_map.


%-----------------------------------------------------------------------------%
% Facts

%TODO: Work out how to include memo table updates with pred_successses
% hell, do it with failures.  Prerequesite for this is evaluation.

:- type mh_fact 
	--->	fact_true		% literal boolean values
	;		fact_false
	;		fact_disj(ordered_set(mh_fact))	% A ; B ; C
	;		fact_conj(ordered_set(mh_fact))	% A , B , C
	;		fact_neg(mh_fact)					% not A
	;		fact_unification(ordered_set(mh_term), mh_substitution). % X = Y
	
	
:- pred apply_fact_substitution(mh_substitution::in, mh_fact::in,
	mh_fact::out) is det.
	
 :- pred ground_fact(mh_fact::in) is semidet.

 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%

apply_fact_substitution(_, _, _) :- sorry($module, $pred,
	"apply_fact_substitution/3").

:- pragma no_determinism_warning(apply_fact_substitution/3).


ground_fact(_) :- sorry($module, $pred, "ground_fact/1").

:- pragma no_determinism_warning(ground_fact/1).
