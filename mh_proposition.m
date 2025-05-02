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

:- import_module ordered_set.

:- import_module mh_term.
:- import_module mh_environment.
:- import_module mh_substitution.
:- import_module mh_event.


%-----------------------------------------------------------------------------%
% Facts

%TODO: Work out how to include memo table updates with pred_successses
% hell, do it with failures.  Prerequesite for this is evaluation.

:- type mh_proposition 
	--->	proposition_true		% proposition boolean values
	;		proposition_false
	;		proposition_disj(ordered_set(mh_proposition))	% A ; B ; C
	;		proposition_conj(ordered_set(mh_proposition))	% A , B , C
	;		proposition_neg(mh_proposition)					% not A
	;		proposition_unify(mh_term, mh_term) 	% X = Y 
%	;		proposition_unification(ordered_set(mh_term)) % X = Y = Z
	;		proposition_sub(mh_substitution)	% { X -> Y, Y -> Z, Z -> foo }
	
	% failed execution, semantically false, mh_error gets injected into
	% event log
	;		proposition_error(mh_error).  
	
	
:- pred apply_proposition_substitution(mh_substitution::in, mh_proposition::in,
	mh_proposition::out) is det.
	
 :- pred ground_proposition(mh_proposition::in) is semidet.

 
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
