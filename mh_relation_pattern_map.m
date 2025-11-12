%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
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
:- import_module maybe.
:- import_module list.
:- import_module assoc_list.

:- import_module mh_relation_map.
:- import_module mh_scope_map.
:- import_module mh_term_map.
:- import_module mh_term.
:- import_module mh_relation.

%-----------------------------------------------------------------------------%
% Pattern Tuple map

:- type term_relation_map(T) == mh_term_map(mh_relation_map(T)).
:- type substitution_relation_map(T) == 
	map(mh_substituiton, mh_relation_map(T))). 

:- type relation_pattern_map(T) 
	--->	pattern_map(
				scope_map :: mh_scope_map(mh_relation_map(T)),
				nil_map :: maybe(T),
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
				closure_term_map :: term_relation_map(T),
				closure_substitution_map :: substitution_relation_map(T)
			).
			



:- func init = (relation_pattern_map(T)::out) is det.
:- pred init(relation_pattern_map(_)::out) is det.

:- func singleton(mh_relation, T) = relation_pattern_map(T).
:- func array_singleton(mh_relation, array(mh_term), T) = 
	relation_pattern_map(T).

:- pred is_empty(relation_pattern_map(_)::in) is semidet.

:- func from_exact_map(relation_map(T)) = relation_pattern_map(T).


%-----------------------------------------------------------------------------%
% Insertion

% Throw an exception if relation already exists in map.

:- pred insert(mh_relation::in, T::in, relation_pattern_map(T)::in, 
	relation_pattern_map(T)::out) is det.


:- pred set(mh_relation::in, T::in, relation_pattern_map::in, relation_pattern_map::out)
	is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- implementation.

:- import_module int.
:- import_module require.

:- import_module mh_term_map.

%-----------------------------------------------------------------------------%
% Relation Pattern map

init = pattern_map(
	mh_scope_map.init,
	maybe.no,
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
	mh_term_map.init,
	map.init
).
init(init).

singleton(Tuple, T) = Map :- insert(Tuple, T, init, Map).

is_empty(init). % All descendants have empty constructors

from_exact_map(Exact) = mh_relation_map.fold(insert, Exact, map.init).

%-----------------------------------------------------------------------------%
% Insertion
	
insert(nil, Value, !Map) :- 
	(if !.Map ^ nil_map = yes(_)
	then report_lookup_error(
		"mh_relation_pattern_map.insert: nil already present in map",
		nil, !.Map)
	else
		!:Map = !.Map ^ nil_map := yes(Value)
	).
	

