%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE
%-----------------------------------------------------------------------------%
% 
% File: mh_term.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%


:- module mh_term. 

:- interface.

:- import_module univ.

:- import_module mh_var_id.
:- import_module mh_symbol.
:- import_module mh_tuple.
:- import_module mh_constraint.
:- import_module mh_relation.
:- import_module mh_scope.
:- import_module mh_predicate.
:- import_module mh_substitution.
:- import_module mh_arity.
% :- import_module mh_scope.


%-----------------------------------------------------------------------------%
% Terms

%NOTE: the ordering of constructors is relevant for structure traversal
% top- down is smallest to largest with comparison of different constructors

:- type mh_term  

	% nil, the abscence of term
	---> 	nil
	
	% atomic values
	;		atom(mh_symbol)

	% variables
	;		var(var_id)
	
	% values
	;		mr_value(univ)
	
	% compound terms
	;		cons(functor, mh_term)
	;		tuple_term(mh_tuple)
	
	% lazy constraints --- not sure if this is a good description
	;		lazy(predicate_term) 	% X:Term(...) => X @ ?Term(...) => 
									% ?Term(..., X) 
	
	% Higher order terms
	;		predicate(mh_predicate)
	;		relation(mh_relation) 
	
	% Term substitutions (lazy)
	;		term_sub(mh_term, mh_substitution).
	
:- func functor(mh_term) = functor is semidet.

:- pred functor(mh_term::in, functor::out) is semidet.

:- pred ground_term(mh_term::in) is semidet.

:- func ground_term(mh_term) = mh_term.
:- mode ground_term(in) = out is semidet.
:- mode ground_term(out) = in is semidet.


% Apply a substitution to a term, if the term is a variable, replace the
% variable with the substituted term as appropriate, if not, return the term
% with the substitution applied.

:- pred apply_term_substitution(mh_substitution::in, 
	mh_term::in, mh_term::out) is det.
:- func apply_term_substitution(mh_substitution, mh_term) = mh_term.


% :- instance tuple(mh_term).
% :- instance substitutable(mh_term).

%-----------------------------------------------------------------------------%
% Term Arity

:- func term_arity(mh_term) = int.

:- pred term_arity(mh_term::in, int::out)  is det.

:- instance arity(mh_term).

%-----------------------------------------------------------------------------%
%  Functor

:- inst functor 
	--->	atom(ground)
	;		var(ground)
	;		predicate(ground)
	;		relation(ground)
	;		term_sub(functor, ground).
	% TODO: Add tuples as functors?

:- type functor =< mh_term
	% Atoms
	---> 	atom(mh_symbol)
	
	% Variables
	;		var(var_id)
	
	% Higher order terms
	;		predicate(mh_predicate)
	;		relation(mh_relation)
	
	% Substitution
	;		term_sub(functor, mh_substitution).
	
:- pred ground_functor(functor::in) is semidet.

:- func ground_functor(functor) = functor.
:- mode ground_functor(in) = out is semidet.
:- mode ground_functor(out) = in is semidet.
	
:- pred apply_functor_substitution(mh_substitution::in, 
	functor::in, functor::out) is det.
:- func apply_functor_substitution(mh_substitution, functor) = functor.


%-----------------------------------------------------------------------------%
% Atoms

:- inst atom ---> atom(ground).

:- type atom =< functor ---> atom(mh_symbol).

:- mode is_atom == ground >> atom.

:- pred is_atom(mh_term::is_atom) is semidet.
%-----------------------------------------------------------------------------%
%	Variables

:- inst mh_var 
	--->	var(ground).
	
:- type mh_var =< mh_term 
	---> 	var(var_id).
	
	
:- mode is_var == ground >> mh_var.

:- pred is_var(mh_term::is_var) is semidet.


%-----------------------------------------------------------------------------%
%	Values

:- type mercury_value =< mh_term
	---> 	mr_value(univ).

:- func new_value_term(T) = mh_term.	
:- func new_value(T) = mercury_value.

%-----------------------------------------------------------------------------%
%	Simple terms

:- inst simple_term
	--->	nil
	;		atom(ground)
	;		var(ground)
	;		mr_value(ground)
	;		lazy(simple_term).
	
:- type simple_term =< mh_term
	--->	nil
	;		atom(mh_symbol)
	;		var(var_id)
	;		mr_value(univ)
	;		lazy(simple_term).
	
:- mode simple_term == ground >> simple_term.

:- pred simple_term(mh_term::simple_term) is semidet.

	

%-----------------------------------------------------------------------------%
%	Compound terms

:- inst compound_term
	--->	cons(ground, ground)
	;		tuple_term(ground)
	;		term_sub(compound_term, ground).

:- type compound_term =< mh_term
	--->	cons(functor, mh_term)
	;		tuple_term(mh_tuple)
	;		term_sub(compound_term, mh_substitution).
	
:- mode compound_term == ground >> compound_term.

:- pred compound_term(mh_term::compound_term) is semidet.

:- instance arity(compound_term).
% :- instance tuple(compound_term).

%-----------------------------------------------------------------------------%
%	Mad Hatter constructors
	
:- inst mh_constructor ---> cons(ground, ground).
	
:- type mh_constructor =< compound_term
	--->	cons(functor, mh_term).
	
:- mode mh_constructor == ground >> mh_constructor.

:- pred mh_constructor(mh_term::mh_constructor) is semidet.

:- instance arity(mh_constructor).
% :- instance tuple(mh_constructor).

%-----------------------------------------------------------------------------%
%	Tuple terms

:- inst tuple_term ---> tuple_term(ground).

:- type tuple_term =< compound_term
	--->	tuple_term(mh_tuple).
	
:- instance arity(tuple_term).

:- mode tuple_term == ground >> tuple_term.

:- pred tuple_term(mh_term::tuple_term) is semidet.

%-----------------------------------------------------------------------------%
% Higher Order terms

:- inst lambda
	--->	predicate(ground)
	;		relation(ground)
	;		term_sub(lambda, ground).

:- type lambda =< functor
	--->	predicate(mh_predicate)
	;		relation(mh_relation)

	;		term_sub(lambda, mh_substitution)

  % Substitution
	;		term_sub(lambda, mh_substitution).
	
%-----------------------------------------------------------------------------%
% Predicate terms


:- inst predicate_term 
	--->	predicate(ground)
	;		term_sub(predicate_term, ground).
	
:- type predicate_term =< lambda
	--->	predicate(mh_predicate)
	;		term_sub(predicate_term, mh_substitution).
	
%-----------------------------------------------------------------------------%
% Relation terms

:- inst relation_term 
	--->	relation(ground)
	;		term_sub(relation_term, ground).
	
:- type relation_term =< lambda
	--->	relation(mh_relation)
	;		term_sub(relation_term, mh_substitution).
	
%-----------------------------------------------------------------------------%
% Term substitutions (lazy)

:- inst term_sub(I) ---> term_sub(I, ground).

:- inst term_sub ---> term_sub(ground, ground).

:- type term_sub =< mh_term
	--->	term_sub(mh_term, mh_substitution).



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module type_desc.
:- import_module string.

%-----------------------------------------------------------------------------%
% Terms

functor(cons(F, _)) = F.
functor(term_sub(T, S)) = term_sub(functor(T), S).

functor(Term, functor(Term)).

ground_term(T) :-
	T = atom(_);
	T = mr_value(_);
	T = cons(ground_functor(_), ground_term(_));
	T = tuple_term(U), ground_tuple(U);
	T = relation(R), ground_relation(R);
	T = predicate(P), ground_predicate(P);
	T = function(F), ground_function(F);
	T = term_sub(T0, Sub),
		apply_term_substitution(Sub, T0, T1),
		ground_term(T1).
		
ground_term(T) = T :- ground_term(T).
	

%-----------------------------------------------------------------------------%


apply_term_substitution(Sub, !Term) :- 	require_complete_switch [!.Term] 
	(
		( 
			!.Term = nil 
		;	!.Term = atom(_) 
		;	!.Term = mr_value(_) 
		), 
		!:Term = !.Term 
		
	;	!.Term = var(ID), sub_id_lookup(Sub, ID, !:Term)
			
	;	!.Term = cons(Car0, Cdr0),
		apply_functor_substitution(Sub, Car0, Car),
		apply_term_substitution(Sub, Cdr0, Cdr),
		!:Term = cons(Car, Cdr)
		
	;	!.Term = tuple_term(Tup0),
		apply_tuple_substiution(Sub, Tup0, Tup),
		!:Term = tuple_term(Tup)
		
	;	!.Term = lazy(ConTerm),
		!:Term = lazy(term_sub(ConTerm, Sub))
	
	;	!.Term = relation(Rel0), 
		apply_relation_substitution(Sub, Rel0, Rel),
		!:Term = relation(Rel)
		
	;	!.Term = predicate(Pred0),
		apply_predicate_substitution(Sub, Pred0, Pred),
		!:Term = predicate(Pred)
		
	;	!.Term = function(Func0),
		apply_function_substitution(Sub, Func0, Func),
		!:Term = function(Func)
		
	;	!.Term = term_sub(SubTerm, Sub0),
		compose_substitutions(Sub0, Sub, Sub1),
		!:Term = term_sub(SubTerm, Sub1)
	).

apply_term_substitution(S, !.T) = !:T :- apply_term_substitution(S, !T).

%-----------------------------------------------------------------------------%
% Term Arity

% The arity of a term is the count of free (universally quantified) variables
% in the term given the current scope, for terms with their own scopes 
% (lambdas) not all of the free variables will be visible to the current 
% scope. A ground term should have arity 0

term_arity(T) = A :- require_complete_switch [T] (
		(	T = nil
		;	T = atom(_)
		;	T = var(_)
		;	T = mr_value(_)
	

		;	T = predicate(_) % TODO: proper HO arity
		;	T = relation(_)
		;	T = function(_)
		), A = 0
	
	;	T = lazy(Ct), A = term_arity(Ct)
	
	;	T = cons(_, Arg), % TODO: May need to redefine this see above
		( if Arg = tuple_term(Tuple)
		then A = arity(Tuple)
		else A = 1
		)
	;	T = tuple_term(R), A = arity(R)
	;	T = term_sub(Term, _), A = arity(Term)
	).

term_arity(T, term_arity(T)).

:- instance arity(mh_term) where [ pred(arity/2) is term_arity ].

% TODO: I need to re-think what 'arity' explicitly means at a term level
% especially in relation to tuples, constraints and predicates
% if predicates don't take arguments directly, but through relations, they should
% be arity zero


	
%-----------------------------------------------------------------------------%
%  Functor

ground_functor(F)  :-
	F = atom(_);
	F = relation(R), ground_relation(R);
	F = predicate(P), ground_predicate(P);
	F = function(Func), ground_function(Func);
	F = term_sub(F0, Sub),
		apply_functor_substitution(Sub, F0, F1),
		ground_functor(F1).

ground_functor(F) = F :- ground_functor(F).

apply_functor_substitution(Sub, !Fun) :-
		!.Fun = atom(_), !:Fun = !.Fun
		
	;	!:Fun = !.Fun @ var(_)
	
	;	!.Fun = relation(Rel0), 
		apply_relation_substitution(Sub, Rel0, Rel),
		!:Fun = relation(Rel)
		
	;	!.Fun = predicate(Pred0),
		apply_predicate_substitution(Sub, Pred0, Pred),
		!:Fun = predicate(Pred)
	
	;	!.Fun = function(Func0),
		apply_function_substitution(Sub, Func0, Func),
		!:Fun = function(Func)
	
	;	!.Fun = term_sub(SubFun, Sub0),
		compose_substitutions(Sub0, Sub, Sub1),
		!:Fun = term_sub(SubFun, Sub1).
		
apply_functor_substitution(Sub, !.Fun) = !:Fun :- 
	apply_functor_substitution(Sub, !Fun).
%-----------------------------------------------------------------------------%
%	Atoms

is_atom(atom(_)).


		
%-----------------------------------------------------------------------------%
%	Variables

is_var(var(_)).

%-----------------------------------------------------------------------------%
%	Values

new_value_term(T) = mr_value(univ(T)).
new_value(T) = mr_value(univ(T)).

%-----------------------------------------------------------------------------%
%	Simple terms

simple_term(T) :-
	T = nil;
	T = atom(_);
	T = var(_);
	T = mr_value(_);
	T = lazy(L), simple_term(L).

%-----------------------------------------------------------------------------%
%	Compound terms

compound_term(T) :-
	T = cons(_, _);
	T = tuple_term(_);
	T = term_sub(S, _), compound_term(S).

:- instance arity(compound_term) where [
	arity(T, A) :- require_complete_switch [T] 
	(	T = cons(_, Arg), 
		(	if Arg = tuple_term(Tuple)
			then A = arity(Tuple)
			else A = 1
		)
		
	;	T = tuple_term(R), A = arity(R)
	
	;	T = term_sub(Term, _), A = arity(Term)
	)
].

% :- instance tuple(compound_term) where [ ].

%-----------------------------------------------------------------------------%
%	Mad Hatter constructors

mh_constructor(cons(_, _)).

:- instance arity(mh_constructor) where [ 
	arity(cons(_, T), Arg) :-
		(	if T = tuple_term(Tuple)
			then Arg = arity(Tuple)
			else Arg = 1
		) 
].
	
%-----------------------------------------------------------------------------%
%	Tuple terms

tuple_term(tuple_term(_)).

:- instance arity(tuple_term) where [ arity(tuple_term(R), arity(R)) ].

% :- instance tuple(mercury_tuple) where [ ].

%-----------------------------------------------------------------------------%
% Higher Order terms


%-----------------------------------------------------------------------------%
% Term substitutions (lazy)


%-----------------------------------------------------------------------------%
% Utility

:- pred zero_index_err(string::in, mh_term::in) is erroneous.

zero_index_err(Action, Term) :- 
	error("Attempt to " ++ Action ++ " "  ++ term_description(Term) ++ 
	". Zero arity terms may not be indexed.").
	
:- func mr_type_name(T) = string.

mr_type_name(T) = type_name(type_of(T)).

:- func term_description(mh_term) = string.

term_description(nil) = "nil term".
term_description(atom(Symbol)) = "atom """ ++ to_string(Symbol) ++ """".
term_description(var(V)) = "variable with id " ++ string(V).
term_description(mr_value(M)) = 
	"mercury value term of type " ++ mr_type_name(M).
term_description(cons(A, R)) = 
	"constructor " ++ string(A) ++ "(" ++ mr_type_name(R) ++ ")".
term_description(lazy(Term)) =
	"lazy " ++ term_description(Term).
term_description(tuple_term(_)) = 
	"mercury tuple term".
term_description(predicate(_)) = 
	"mercury predicate term".
term_description(relation(_)) =
	"mercury relation term".
term_description(function(_)) =
	"mercury function term of type ".
term_description(term_sub(Term, _)) =
	"substitution of " ++ term_description(Term).
