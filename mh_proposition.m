%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_proposition.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_proposition.

:- interface.

:- import_module list.
:- import_module string.

:- import_module mh_term.
:- import_module mh_ordered_term_set. 
:- import_module mh_substitution.
:- import_module mh_ordered_proposition_set.
:- import_module mh_tuple.
:- import_module mh_scope.
:- import_module mh_var_set.


%-----------------------------------------------------------------------------%
% Facts

%TODO: Work out how to include memo table updates with pred_successses
% hell, do it with failures.  Prerequesite for this is evaluation.

:- type mh_proposition 
			% Exception, propogates like failure, but through negation
	--->	proposition_error(reason)
	
			% Atomic false
	;		proposition_false 	
	
			% Propositional reason for failure, unifies with proposition_false
	;		proposition_fail(reason)
	
	;		proposition_flounder(mh_term)
			% Floundered evaluation, soft failure, succeeds in conjunctions
			% without other failures?
	
			% Atomic true
	;		proposition_true
	
			% X := Y
	;		proposition_successs(mh_substitution)
	
			% A ; B ; C
	;		proposition_disj(mh_ordered_proposition_set)
	
			% A , B , C
	;		proposition_conj(mh_ordered_proposition_set)
	
			% not A
	;		proposition_neg(mh_proposition)
	
			% X = Y = Z
	;		proposition_unification(mh_ordered_term_set)
	
			% if C then T else E 
			% C , T ; not C , E
	;		proposition_branch(mh_proposition, mh_proposition, mh_proposition).
	
:- func vars_in_proposition(mh_scope, mh_proposition) = mh_var_set.
:- pred vars_in_proposition(mh_scope::in, mh_proposition::in, mh_var_set::out)
	is det.
	
:- func update_proposition_scope(mh_scope, mh_scope, mh_proposition)
	= mh_proposition.
:- pred update_proposition_scope(mh_scope::in, mh_scope::in, 
	mh_proposition::in, mh_proposition::out) is det.
	
:- pred ground_proposition(mh_proposition::in) is semidet.

%-----------------------------------------------------------------------------%
% Failure reason

:- type reason
	---> 	boolean_failure %no reason given
	;		disj_failure(list(reason))
	;		conj_failure(reason)
	;		neg_failure(mh_proposition)
	;		unification_failure(mh_term, mh_term) 
	;		constraint_failure(mh_term, mh_term)
	;		message(string)
	;		message(string, mh_tuple)
	;		flounder(mh_term). %Evaluation of given term floundered

%-----------------------------------------------------------------------------%
% Term constructors	

:- func term_fail_reason(mh_scope, reason) = mh_term.

:- func term_error_reason(mh_scope, reason) = mh_term. 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- import_module mh_relation.
:- import_module mh_var_map.

%-----------------------------------------------------------------------------%

vars_in_proposition(_, _) = sorry($module, $pred, "vars_in_proposition/2").

:- pragma no_determinism_warning(vars_in_proposition/2).

vars_in_proposition(Scope, Prop, vars_in_proposition(Scope, Prop)).

update_proposition_scope(OldScope, NewScope, !.Prop) = !:Prop :-
	require_complete_switch [!.Prop] (
		( 	!.Prop = proposition_error(_); 
			!.Prop = proposition_false; 
			!.Prop = proposition_fail(_); 
			!.Prop = proposition_true;
			!.Prop = proposition_successs(ren_map(_))
		),
		!:Prop = !.Prop
	;
		!.Prop = proposition_flounder(Term),
		update_term_scope(OldScope, NewScope, Term, NewTerm),
		!:Prop = proposition_flounder(NewTerm)
	;
		!.Prop = proposition_successs(sub_map(Map)),
		mh_var_map.map(update_var_map(OldScope, NewScope), Map, NewMap),
		!:Prop = proposition_successs(sub_map(NewMap))			
	;
		!.Prop = proposition_disj(Ops),
		mh_ordered_proposition_set.map(
			update_proposition_scope(OldScope, NewScope), Ops, NewOps),
		!:Prop = proposition_disj(NewOps)
	;
		!.Prop = proposition_conj(Ops),
		mh_ordered_proposition_set.map(
			update_proposition_scope(OldScope, NewScope), Ops, NewOps),
		!:Prop = proposition_conj(NewOps)
	;
		!.Prop = proposition_neg(NegProp),
		update_proposition_scope(OldScope, NewScope, NegProp, NewNegProp),
		!:Prop = proposition_neg(NewNegProp)
	;
		!.Prop = proposition_unification(Ots),
		mh_ordered_term_set.map(
			update_term_scope(OldScope, NewScope), Ots, NewOts),
		!:Prop = proposition_unification(NewOts)
	;
		!.Prop = proposition_branch(If, Then, Else),
		update_proposition_scope(OldScope, NewScope, If, NewIf),
		update_proposition_scope(OldScope, NewScope, Then, NewThen),
		update_proposition_scope(OldScope, NewScope, Else, NewElse),
		!:Prop = proposition_branch(NewIf, NewThen, NewElse)
	).
	
:- pred update_var_map(mh_scope::in, mh_scope::in, mh_var::in, mh_term::in,
	mh_term::out) is det.

update_var_map(Old, New, _, !.Term, !:Term) :-
	update_term_scope(Old, New, !Term).

update_proposition_scope(OldScope, NewScope, Prop, 
	update_proposition_scope(OldScope, NewScope, Prop)).

ground_proposition(_) :- sorry($module, $pred, "ground_proposition/1").

:- pragma no_determinism_warning(ground_proposition/1).

%-----------------------------------------------------------------------------%
% Term constructors	

term_fail_reason(Scope, Reason) = 
	relation(proposition(Scope, proposition_fail(Reason))).
	
term_error_reason(Scope, Reason) = 
	relation(proposition(Scope, proposition_error(Reason))).
 
