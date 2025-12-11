%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_proposition_pattern_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_proposition_pattern_map.

:- interface.

:- use_module map.
:- import_module list.
:- import_module assoc_list.

:- import_module mh_proposition_map.
:- import_module mh_term_map.
:- import_module mh_substitution_map.
:- import_module mh_term.
:- import_module mh_proposition.

%-----------------------------------------------------------------------------%
% Proposition Pattern map

:- type substitution_proposition_map(T) 
	== mh_substitution_map(mh_proposition_map(T)).
:- type member_proposition_map(T) = mh_proposition_map(mh_proposition_map(T)).
:- type term_proposition_map(T) == mh_term_map(mh_proposition_map(T)).


:- type proposition_pattern_map(T) 
	--->	pattern_map(
				error_map :: mh_proposition_map(T),
				failure_map :: mh_proposition_map(T),
				success_map :: substitution_proposition_map(T),
				disjunction_map :: member_proposition_map(T),
				conjunction_map :: member_proposition_map(T),
				negation_map :: mh_proposition_map(T),
				unification_map :: term_proposition_map(T),
				condition_branch_map :: member_proposition_map(T),
				then_branch_map :: member_proposition_map(T),
				else_branch_map :: member_proposition_map(T)
			).

:- func init = (proposition_pattern_map(_)::out) is det.
:- pred init(proposition_pattern_map(_)::out) is det.

:- func singleton(mh_proposition, T) = proposition_pattern_map(T).
:- func array_singleton(mh_proposition, array(mh_term), T) = 
	proposition_pattern_map(T).

:- pred is_empty(proposition_pattern_map(_)::in) is semidet.

:- func from_exact_map(map(mh_proposition, T)) = proposition_pattern_map(T).

%-----------------------------------------------------------------------------%
% Insertion

% does not modify map if proposition is already present
:- pred insert(mh_proposition::in, T::in, proposition_pattern_map::in,
	proposition_pattern_map::out)	is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- implementation.

:- import_module int.
:- import_module require.

:- import_module mh_term_map.

%-----------------------------------------------------------------------------%
% Proposition Pattern map

init = pattern_map(
	mh_proposition_map.init,
	mh_proposition_map.init,
	mh_substitution_map.init,
	mh_proposition_map.init,
	mh_proposition_map.init,
	mh_proposition_map.init,
	mh_term_map.init,
	mh_proposition_map.init,
	mh_proposition_map.init,
	mh_proposition_map.init
).

init(init).

singleton(Proposition, T) = Map :- insert(Proposition, T, init, Map).

is_empty(init). % All descendants have empty constructors

from_exact_map(Exact) = map.foldl(insert, Exact, init).

%-----------------------------------------------------------------------------%
% Insertion

%- pred pm_insert(Proposition, Value, !PM).
:- pred pm_insert(mh_proposition::in, T::in, 
	substitution_proposition_map(T)::in, substitution_proposition_map(T)::out) is det.

pm_insert(Prop, Val, !PM) :-
	(if insert(Prop, Val, !.PM, NewPM)
	then !:PM = NewPM
	else true
	).

%- pred spm_insert(Proposition, Value, Sub, !SPM).
:- pred spm_insert(mh_proposition::in, T::in, mh_substitution::in, 
	substitution_proposition_map(T)::in, substitution_proposition_map(T)::out) is det.

spm_insert(Prop, Val, Sub, !SPM) :-
		(if search(!.TM, Sub, PM)
		then
			(if insert(Prop, Val, PM, NewPM)
			then
				set(Sub, NewPM, !SPM)
			else true
			)
		else
			det_insert(Sub, singleton(Prop, Val), !SPM)
		).

%- pred mpm_insert(Member, Value, Proposition, !MPM).
:- pred mpm_insert(mh_proposition::in, T::in, mh_term::in, 
	term_proposition_map(T)::in, term_proposition_map(T)::out) is det.

mpm_insert(Prop, Val, Member, !MPM) :-
		(if search(!.TM, Member, PM)
		then
			(if insert(Prop, Val, PM, NewPM)
			then
				set(Member, NewPM, !MPM)
			else true
			)
		else
			det_insert(Prop, singleton(Prop, Val), !MPM)
		).
	

%- pred tpm_insert(Proposition, Value, Term, !TPM).
:- pred tpm_insert(mh_proposition::in, T::in, mh_term::in, 
	term_proposition_map(T)::in, term_proposition_map(T)::out) is det.

tpm_insert(Prop, Val, Term, !TM) :-
		(if search(!.TM, Term, PM)
		then
			(if insert(Prop, Val, PM, NewPM)
			then
				set(Term, NewPM, !TM)
			else true
			)
		else
			det_insert(Term, singleton(Prop, Val), !TM)
		).
		
insert(Prop@proposition_error(_), Value, !Map) :-
	EM = !.Map ^ error_map,
	pm_insert(Prop, Value, EM, NewEM),
	!:Map = !.Map ^ error_map := NewEM.
		
insert(Prop@proposition_false, Value, !Map) :-
	FM = !.Map ^ failure_map,
	pm_insert(Prop, Value, FM, NewFM),
	!:Map = !.Map ^ failure_map := NewFM.
		
insert(Prop@proposition_fail(_), Value, !Map) :-
	FM = !.Map ^ failure_map,
	pm_insert(Prop, Value, FM, NewFM),
	!:Map = !.Map ^ failure_map := NewFM.
	
insert(Prop@proposition_true, Value, !Map) :-
	SM = !.Map ^ success_map,
	spm_insert(Prop, Value, init, SM, NewSM),
	!:Map = !.Map ^ success_map := NewSM.
	
insert(Prop@proposition_successs(Sub), Value, !Map) :-
	SM = !.Map ^ success_map,
	spm_insert(Prop, Value, Sub, SM, NewSM),
	!:Map = !.Map ^ success_map := NewSM.
	
insert(Prop@disjunction(OS), Value, !Map) :-
	DM = !.Map ^ disjunction_map,
	NewDM = foldl(mpm_insert(Prop, Value), to_array(OS), DM),
	!:Map = !.Map ^ disjunction_map := NewDM.
	
insert(Prop@conjunction(OS), Value, !Map) :-
	CM = !.Map ^ conjuction_map,
	NewCM = foldl(mpm_insert(Prop, Value), to_array(OS), CM),
	!:Map = !.Map ^ conjunction_map := NewCM.
	
insert(Prop@proposition_neg(Neg), Value, !Map) :-
	NegMap = !.Map ^ negation_map,
	mpm_insert(Prop, Value, Neg, NegMap, NewNegMap),
	!:Map = !.Map ^ negation_map := NewNegMap.
	
insert(Prop@proposition_unification(OS), Value, !Map) :-
	UM = !.Map ^ unification_map,
	NewUM = foldl(tpm_insert(Prop, Value), to_array(OS), UM),
	!:Map = !.Map ^ unification_map := NewUM.
	
insert(Prop@proposition_branch(Cond, Then, Else), Value, !Map) :-	
	CondMap = !.Map ^ condition_branch_map,
	ThenMap = !.Map ^ then_branch_map,
	ElseMap = !.Map ^ else_branch_map,
	tpm_insert(Prop, Value, Cond, CondMap, NewCondMap),
	tpm_insert(Prop, Value, Then, ThenMap, NewThenMap),
	tpm_insert(Prop, Value, Else, ElseMap, NewElseMap),
	!:Map = !.Map ^ condition_branch_map := NewCondMap,
	!:Map = !.Map ^ then_branch_map := NewThenMap,
	!:Map = !.Map ^ else_branch_map := NewElseMap.
	
	
	

