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

:- import_module ordered_set. % TODO: ordered proposition sets and sub sets

:- import_module mh_term.
:- import_module mh_substitution.
:- import_module mh_function.


%-----------------------------------------------------------------------------%
% Facts

%TODO: Work out how to include memo table updates with pred_successses
% hell, do it with failures.  Prerequesite for this is evaluation.

:- type mh_proposition 
			% Boolean truth value
	--->	proposition_false 	
	;		proposition_true	
			% Propositional reason for failure, equivalent to proposition_false
	;		proposition_fail(reason)
			% A ; B ; C
	;		proposition_disj(ordered_set(mh_proposition))
			% A , B , C
	;		proposition_conj(ordered_set(mh_proposition))
			% not A
	;		proposition_neg(mh_proposition)
			% X = Y
	;		proposition_unify(mh_term, mh_term)
			% X = Y = Z
%	;		proposition_unification(ordered_set(mh_term))
			% if C then T else E = C , T ; not C , E
	;		proposition_branch(mh_proposition, mh_proposition, mh_proposition)
			% p(X, Y) :- f(X) -> Y.
	;		proposition_apply(mh_function, mh_term, mh_term)
			% { X -> Y, Y -> Z, Z -> foo }
	;		proposition_sub(ordered_set(mh_substitution)).  

% Add a from_relation(mh_proposition, mh_relation) wrapper constructor to add
% further tracability?-
	
	
:- pred apply_proposition_substitution(mh_substitution::in, mh_proposition::in,
	mh_proposition::out) is det.
	
:- pred ground_proposition(mh_proposition::in) is semidet.

%-----------------------------------------------------------------------------%
% Failure reason

:- type reason
	---> 	disj_failure(list(reason))
	;		conj_failure(reason)
	;		neg_failure(mh_proposition)
	;		unification_failure(mh_term, mh_term).
	
 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%

apply_proposition_substitution(_, _, _) :- sorry($module, $pred,
	"apply_proposition_substitution/3").

:- pragma no_determinism_warning(apply_proposition_substitution/3).


ground_proposition(_) :- sorry($module, $pred, "ground_proposition/1").

:- pragma no_determinism_warning(ground_proposition/1).
