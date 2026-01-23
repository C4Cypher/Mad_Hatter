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
	
			% Propositional reason for failure, evaluates to proposition_false
	;		proposition_fail(reason)
	
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

%-----------------------------------------------------------------------------%

vars_in_proposition(_, _) = sorry($module, $pred, "vars_in_proposition/2").

:- pragma no_determinism_warning(vars_in_proposition/2).

ground_proposition(_) :- sorry($module, $pred, "ground_proposition/1").

:- pragma no_determinism_warning(ground_proposition/1).

%-----------------------------------------------------------------------------%
% Term constructors	

term_fail_reason(Scope, Reason) = 
	relation(proposition(Scope, proposition_fail(Reason))).
	
term_error_reason(Scope, Reason) = 
	relation(proposition(Scope, proposition_error(Reason))).
 
