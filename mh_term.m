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

:- import_module mh_symbol.
:- import_module mh_var_id.
:- import_module mh_value.
:- import_module mh_tuple.
:- import_module mh_constraint.
:- import_module mh_relation.
:- import_module mh_scope.
:- import_module mh_fact.
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
	;		value(mh_value)
	
	% compound terms
	;		cons(car::mh_term, cdr::mh_term)
	;		tuple_term(mh_tuple)
	
	% lazy constraints --- not sure if this is a good description
	;		lazy(relation_term) 	% X:Term(...) => X @ ?Term(...) => 
									% ?Term(..., X) 
	
	% Higher order terms
	;		fact(mh_fact)
	;		relation(mh_relation) 
	
	% Term substitutions (closures and var renamings into higher scopes)
	;		term_sub(mh_term, mh_substitution).


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
% Atoms

:- inst atom ---> atom(ground).

:- type atom =< mh_term ---> atom(mh_symbol).

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

:- type value_term =< mh_term
	---> 	value(mh_value).

%-----------------------------------------------------------------------------%
%	Simple terms

:- inst simple_term
	--->	nil
	;		atom(ground)
	;		var(ground)
	;		value(ground)
	;		lazy(simple_term).
	
:- type simple_term =< mh_term
	--->	nil
	;		atom(mh_symbol)
	;		var(var_id)
	;		value(mh_value)
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
	--->	cons(car::mh_term, cdr::mh_term)
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
	--->	cons(car::mh_term, cdr::mh_term).
	
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
% Constraints (lazy terms)

:- inst mh_constraint ---> lazy(ground).

:- type mh_constraint =< mh_term
	--->	lazy(relation_term).
	
:- mode is_constraint == mh_term >> mh_constraint.

:- pred is_constraint(mh_term::is_constraint) is semidet.

%-----------------------------------------------------------------------------%
% Higher Order terms

:- inst lambda
	--->	fact(ground)
	;		relation(ground)
	;		term_sub(lambda, ground).

:- type lambda =< mh_term
	--->	fact(mh_fact)
	;		relation(mh_relation)
	;		term_sub(lambda, mh_substitution).
	
%-----------------------------------------------------------------------------%
% Fact terms


:- inst fact_term 
	--->	fact(ground)
	;		term_sub(fact_term, ground).
	
:- type fact_term =< lambda
	--->	fact(mh_fact)
	;		term_sub(fact_term, mh_substitution).
	
%-----------------------------------------------------------------------------%
% Relation terms

:- inst relation_term 
	--->	relation(ground)
	;		term_sub(relation_term, ground).
	
:- type relation_term =< lambda
	--->	relation(mh_relation)
	;		term_sub(relation_term, mh_substitution).
	
%-----------------------------------------------------------------------------%
% Term substitutions (closures and var renamings into higher scopes)

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

ground_term(T) :-
	T = atom(_);
	T = mr_value(_);
	T = cons(ground_functor(_), ground_term(_));
	T = tuple_term(U), ground_tuple(U);
	T = relation(R), ground_relation(R);
	T = fact(F), ground_fact(F);
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
		
	;	!.Term = fact(Fact0),
		apply_fact_substitution(Sub, Fact0, Fact),
		!:Term = fact(Fact)
		
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
	

		;	T = fact(_) % TODO: proper HO arity
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
% especially in relation to tuples, constraints and facts
% if facts don't take arguments directly, but through relations, they should
% be arity zero


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
% Constraints (lazy terms)

is_constraint(lazy(_)).

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
term_description(fact(_)) = 
	"mercury fact term".
term_description(relation(_)) =
	"mercury relation term".
term_description(function(_)) =
	"mercury function term of type ".
term_description(term_sub(Term, _)) =
	"substitution of " ++ term_description(Term).
