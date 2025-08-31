%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_relation.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_relation.

:- interface.

:- import_module ordered_set.

:- import_module mh_clause.
:- import_module mh_proposition.
:- import_module mh_arity.
:- import_module mh_tuple.
:- import_module mh_scope.
:- import_module mh_term_map.
:- import_module mh_function.
:- import_module mh_term.
:- import_module mh_mode.
:- import_module mh_substitution.

%-----------------------------------------------------------------------------%
% Relation type

% A relation represents the functor of a compound term in any unifcation
% that requires more work than simple assignment of that compound term to
% a free variable.
% 
%
% X = R(Arg)
%

/* Depreciated documentation
% At it's core, they represent the 'head clause' of any horn clause, even for
%  s. In essene, a predicate in the traditional Prolog sense is a 
% Relation that unifies with a success indicator or truth value, or to be more
% specific, a function that returns a success indicator. The substitution 
% contained in the success represents the variable bindings that resolve
% to make the fact 'true'
*/

% Relations can be  'moded', modes being pairings of pre-conditions and post
% conditions that enforce the relation's soundness and purity.
 
% Thinking about this. I can describe relations in many  different ways.
% However I need to distill the type structure down into a way that is
% unifiable and able to encapsulate clauses before and after mode analysis
% PLUS handling foreign function calls. I need to define modes. Preconditions
% and postconditions.

% TODO: replace ordered_set/1  with mh_relation_set or mh_ordered_relation_set
:- type mh_relation
	--->	conjunction(mh_relation, mh_scope, ordered_set(mh_term))
	%		','(a, b)(X) == a(X) , b(X),  a = b.
	%		','() == true. 

	;		disjunction(mh_relation, mh_scope, ordered_set(mh_term))
	%		';'(a, b)(X) == a(X) ; b(X).
	% 		';'() == false. 

	;		negation(mh_relation, mh_scope, mh_term)
	%		'not'(a)(X) ==  a(X), false; not a(X).

	;		lambda(mh_relation, mh_scope, mh_term, mh_term)
	%		lambda(E, S, A, B) == \A = B.
	%		(\A = B)(A) = B.
	%		\A = B == (\A -> B), (\A <- B). 
	
	;		apply(mh_relation, mh_scope, mh_term, mh_term)
	%		apply(E, S, A, B) == \A -> B.	Substitute A into B
	
	;		depend(mh_relation, mh_scope, mh_term, mh_term)
	%		depend(E, S, A, B) == \A <- B.	Capture and unify A from pattern B
	
	;		lazy(mh_relation, mh_scope, mh_term)
	%		lazy(E, S, C) == ?C 
	%
	%		A 'lazy' term presents an applied constraint, a body clause bound
	%		to a variable, successfully unifying with any term that is applied
	%		to it.
	%
	% 		Conjunction of term and applied constraint
	%		X, ?C == X, C(X). 
	%
	%		The 'constraint' operator ':'/2 
	%		X:C == X, ?C.
	
	;		proposition(mh_relation, mh_scope, mh_proposition)
	% 		Succeed or fail based on evaluation of the embedded proposition;
	%		Should return either a success or failure as defined in 
	%		mh_proposition.m
			
			
/* sort this out later
			
	;		func_relation(mh_relation, mh_scope, mh_function) %Hmmm ...
			% \(X) -> Y == f(X) -> Y.
*/
	
:- pred apply_relation_substitution(mh_substitution::in, mh_relation::in,
	mh_relation::out) is det.
	
%:- func relation_scope(mh_relation) = mh_scope.
	
 
:- pred ground_relation(mh_relation::in) is semidet.
 
%-----------------------------------------------------------------------------%
% Relation Subterms

% Return subterms in a Relation 
:- func relation_subterms(mh_relation) = mh_tuple is semidet.
:- pred relation_subterms(mh_relation::in, mh_tuple::out) is semidet.

% Return an empty tuple if there are no subterms.
:- func det_relation_subterms(mh_term) = mh_tuple is det.
:- pred det_relation_subterms(mh_term::in, mh_tuple::out) is det.
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module list.

%-----------------------------------------------------------------------------%

apply_relation_substitution(_, _, _) :- sorry($module, $pred,
	"apply_relation_substitution/3").
	
:- pragma no_determinism_warning(apply_relation_substitution/3).

ground_relation(_) :- sorry($module, $pred, "ground_relation/1").

:- pragma no_determinism_warning(ground_relation/1).

%-----------------------------------------------------------------------------%
% Relation Subterms

relation_subterms(_) = sorry($module, $pred, "relation_subterms/1").

:- pragma no_determinism_warning(relation_subterms/1).

relation_subterms(R, relation_subterms(R)).
