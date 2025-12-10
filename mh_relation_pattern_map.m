%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_relation_pattern_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_relation_pattern_map.

:- interface.

:- use_module map.
:- import_module list.
:- import_module assoc_list.

:- import_module mh_relation_map.
:- import_module mh_term_map.
:- import_module mh_term.
:- import_module mh_relation.

%-----------------------------------------------------------------------------%
% Pattern Tuple map

:- type term_relation_map(T) == mh_term_map(mh_relation_map(T)).

:- type relation_pattern_map(T) 
	--->	pattern_map(
				conjunction_map :: term_relation_map(T),
				disjunction_map :: term_relation_map(T),
				negation_map :: term_relation_map(T),
				lambda_equivalence_lhs_map :: term_relation_map(T),
				lambda_equivalence_rhs_map :: term_relation_map(T),
				lambda_application_lhs_map :: term_relation_map(T),
				lambda_application_rhs_map :: term_relation_map(T),
				lambda_unification_lhs_map :: term_relation_map(T),
				lambda_unification_rhs_map :: term_relation_map(T),
				lazy_map :: term_relation_map(T),
				proposition_map ::	mh_proposition_map(mh_relation_map(T)),
				closure_map :: mh_relation_map(T),
				function_map :: mh_relation_map(T)
			).

:- func init = (relation_pattern_map(_)::out) is det.
:- pred init(relation_pattern_map(_)::out) is det.

:- func singleton(mh_relation, T) = relation_pattern_map(T).
:- func array_singleton(mh_relation, array(mh_term), T) = 
	relation_pattern_map(T).

:- pred is_empty(relation_pattern_map(_)::in) is semidet.

:- func from_exact_map(map(mh_relation, T)) = relation_pattern_map(T).

%-----------------------------------------------------------------------------%
% Insertion

% does not modify map if relation is already present
:- pred insert(mh_relation::in, T::in, relation_pattern_map::in,
	relation_pattern_map::out)	is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- implementation.

:- import_module int.
:- import_module require.

:- import_module mh_term_map.

%-----------------------------------------------------------------------------%
% Relation Pattern map

init = pattern_map(
	mh_term_map.init,
	mh_term_map.init,
	mh_term_map.init,
	mh_term_map.init,
	mh_term_map.init,
	mh_term_map.init,
	mh_term_map.init,
	mh_term_map.init,
	mh_term_map.init,
	mh_term_map.init,
	mh_proposition_map.init,
	mh_relation_map.init,
	mh_relation_map.init
).
init(init).

singleton(Relation, T) = Map :- insert(Relation, T, init, Map).

is_empty(init). % All descendants have empty constructors

from_exact_map(Exact) = map.foldl(insert, Exact, init).

%-----------------------------------------------------------------------------%
% Insertion

%- pred trm_insert(Relation, Value, Term, !TRM).
:- pred trm_insert(mh_relation::in, T::in, mh_term::in, 
	term_relation_map(T)::in, term_relation_map(T)::out) is det.

trm_insert(Rel, Val, Term, !TM) :-
		(if search(!.TM, Term, RM)
		then
			(if insert(Rel, Val, RM, NewRM)
			then
				set(Term, NewRM, !TM)
			else true
			)
		else
			det_insert(Term, singleton(Rel, Val), !TM)
		).
	
insert(Rel@conjunction(_, OS), Value, !Map) :-
	CM = !.Map ^ conjuction_map,
	NewCM = foldl(trm_insert(Rel, Value), to_array(OS), CM),
	!:Map = !.Map ^ conjunction_map := NewCM.
	
insert(Rel@disjunction(_, OS), Value, !Map) :-
	CM = !.Map ^ disjunction_map,
	NewCM = foldl(trm_insert(Rel, Value), to_array(OS), CM),
	!:Map = !.Map ^ disjunction_map := NewCM.
	
insert(Rel@not(_, Term), Value, !Map) :-
	NegMap = !.Map ^ negation_map,
	trm_insert(Rel, Value, Term, NegMap, NewNegMap),
	!:Map = !.Map ^ negation_map := NewNegMap.
	
insert(Rel@lambda_equivalence(_, LHS, RHS), Value, !Map) :-	
	LhsMap = !.Map ^ lambda_equivalence_lhs_map,
	RhsMap = !.Map ^ lambda_equivalence_rhs_map,
	trm_insert(Rel, Value, LHS, LhsMap, NewLhsMap),
	trm_insert(Rel, Value, RHS, RhsMap, NewRhsMap),
	!:Map = !.Map ^ lambda_equivalence_lhs_map := NewLhsMap,
	!:Map = !.Map ^ lambda_equivalence_rhs_map := NewRhsMap.
	
insert(Rel@lambda_application(_, LHS, RHS), Value, !Map) :-	
	LhsMap = !.Map ^ lambda_application_lhs_map,
	RhsMap = !.Map ^ lambda_application_rhs_map,
	trm_insert(Rel, Value, LHS, LhsMap, NewLhsMap),
	trm_insert(Rel, Value, RHS, RhsMap, NewRhsMap),
	!:Map = !.Map ^ lambda_application_lhs_map := NewLhsMap,
	!:Map = !.Map ^ lambda_application_rhs_map := NewRhsMap.
	
insert(Rel@lambda_unification(_, LHS, RHS), Value, !Map) :-	
	LhsMap = !.Map ^ lambda_unification_lhs_map,
	RhsMap = !.Map ^ lambda_unification_rhs_map,
	trm_insert(Rel, Value, LHS, LhsMap, NewLhsMap),
	trm_insert(Rel, Value, RHS, RhsMap, NewRhsMap),
	!:Map = !.Map ^ lambda_unification_lhs_map := NewLhsMap,
	!:Map = !.Map ^ lambda_unification_rhs_map := NewRhsMap.
	
insert(Rel@lazy(_, Term), Value, !Map) :-
	LazyMap = !.Map ^ lazy_map,
	trm_insert(Rel, Value, Term, LazyMap, NewLazyMap),
	!:Map = !.Map ^ lazy_map := NewLazyMap.

%- pred prop_map_insert(Relation, Value, Term, !TRM).
:- pred prop_map_insert(mh_relation::in, T::in, mh_term::in, 
	term_relation_map(T)::in, term_relation_map(T)::out) is det.

prop_map_insert(Rel, Val, Prop, !TM) :-
		(if search(!.TM, Prop, RM)
		then
			(if insert(Rel, Val, RM, NewRM)
			then
				set(Prop, NewRM, !TM)
			else true
			)
		else
			det_insert(Term, singleton(Rel, Val), !TM)
		).
	
insert(Rel@proposition(_, Prop), Value, !Map) :-
	PropMap = !.Map ^ proposition_map,
	prop_map_insert(Rel, Value, Prop, PropMap, NewPropMap),
	!:Map = !.Map ^ proposition_map := NewPropMap. 
	
insert(Rel@closure(_, _, _), Value, !Map) :-
	ClosureMap = !.Map ^ closure_map,
	(if insert(Rel, Value, ClosureMap, NewClosureMap)
	then
		!:Map = !.Map ^ closure_map := NewClosureMap
	else
		true
	).
	
insert(Rel@call(_, _), Value, !Map) :-
	FunctionMap = !.Map ^ function_map,
	(if insert(Rel, Value, FunctionMap, NewFunctionMap)
	then
		!:Map = !.Map ^ function_map := NewFunctionMap
	else
		true
	).
	
	
	

