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
:- import_module mh_relation.
:- import_module mh_substitution.

%-----------------------------------------------------------------------------%
% Terms

%NOTE: the ordering of constructors is relevant for structure traversal
% top- down is smallest to largest with comparison of different constructors
:- type mh_term 
	
	% atomic values
	--->	atom(mh_symbol)

	% variables
	;		var(var_id)
	
	% values
	;		value(mh_value)
	
	% compound terms (cons stands for 'constructor')
	;		cons(mh_term, mh_tuple) % F(A1, A2, A3)

	% Higher order terms
	;		relation(mh_relation).
	
%-----------------------------------------------------------------------------%
% Concrete terms

	% Terms that have a concrete representation in source code
	
:- type mh_concrete_term =< mh_term
	% atomic values
	--->	atom(mh_symbol)

	% variables
	;		var(var_id)
	
	% values
	;		value(mh_value)
	
	% compound terms (cons stands for 'constructor')
	;		cons(mh_term, mh_tuple). % F(A1, A2, A3).
	
:- inst concrete
	--->	atom(ground)
	;		var(ground)
	;		value(ground)
	;		cons(ground, ground).
	
:- mode is_concrete == ground >> concrete.

:- pred is_concrete(mh_term::is_concrete) is semidet.


%-----------------------------------------------------------------------------%
% Ground terms

	% True if a term is semantically ground 
:- pred ground_term(mh_term::in) is semidet.

	% Constraint form of ground/1, returns the same term
:- func ground_term(mh_term) = mh_term.
:- mode ground_term(in) = out is semidet.
:- mode ground_term(out) = in is semidet.


%-----------------------------------------------------------------------------%
% Substitutions

	% Apply a substitution to a term, if the term is a variable, replace the
	% variable with the substituted term as appropriate, if not, return the 
	% term with the substitution applied.

:- pred apply_term_substitution(mh_substitution::in, 
	mh_term::in, mh_term::out) is det.
:- func apply_term_substitution(mh_substitution, mh_term) = mh_term.

%-----------------------------------------------------------------------------%
% Atoms

:- func term_atom(string) = mh_term.
:- mode term_atom(in) = out is det.
:- mode term_atom(in) = in is semidet.
:- mode term_atom(out) = in is semidet.

%-----------------------------------------------------------------------------%
% Variables

:- inst mh_var 
	--->	var(ground).
	
:- type mh_var =< mh_term 
	---> 	var(var_id).
	
:- mode is_var == ground >> mh_var.

:- pred is_var(mh_term::is_var) is semidet.

	% Term constructor for variables
	% Skips the need for explicit type qualification between mh_term and mh_var

:- func term_var(var_id) = mh_term.  
:- mode	term_var(in) = out is det.
:- mode term_var(out) = in is semidet.

%-----------------------------------------------------------------------------%
% Values

	% Term constructor for value terms
	% Deconstruction will fail on type mismatch
:- func term_value(T) = mh_term.
:- mode term_value(in) = out is det.
:- mode term_value(out) = in is semidet.

:- inst value_term
	--->	value(ground).

:- mode is_value == ground >> value_term.

:- pred is_value(mh_term::is_value) is semidet.

	% Determenistic deconstructor for term values
:- some [T] func deconstruct_value_term(mh_term::in(value_term)) = (T::out) 
	is det.

%-----------------------------------------------------------------------------%
% Relations 

:- func term_nil = mh_term.
%- func term_true = mh_term.
%- func term_false = mh_term.
%- func term_fail(string) = mh_term.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module type_desc.
:- import_module string.
:- import_module list.

:- import_module util.

%-----------------------------------------------------------------------------%
% Concrete terms

is_concrete(atom(_)).
is_concrete(var(_)).
is_concrete(value(_)).
is_concrete(cons(Functor, Tuple)) :- 
	is_concrete(Functor),
	all_tuple(is_concrete, Tuple).


%-----------------------------------------------------------------------------%
% Ground terms

ground_term(T) :-
	require_complete_switch [T] (
		T = atom(_);
		T = var(_), fail;
		T = value(_);
		T = cons(ground_term(_), Tuple), ground_tuple(Tuple); 
		T = relation(R), ground_relation(R)
	).
		
ground_term(T) = T :- ground_term(T).
	

%-----------------------------------------------------------------------------%
% Substitutions

apply_term_substitution(Sub, !Term) :- 	require_complete_switch [!.Term] 
	(
		( 	!.Term = atom(_) ; !.Term = value(_) ), !:Term = !.Term 

	;	!.Term = var(ID), sub_id_lookup(Sub, ID, !:Term)
			
	;	!.Term = cons(Car0, Cdr0),
		apply_term_substitution(Sub, Car0, Car),
		apply_tuple_substiution(Sub, Cdr0, Cdr),
		!:Term = cons(Car, Cdr)	
	;	!.Term = relation(Rel0), 
		apply_relation_substitution(Sub, Rel0, Rel),
		!:Term = relation(Rel)
	).

apply_term_substitution(S, !.T) = !:T :- apply_term_substitution(S, !T).

%-----------------------------------------------------------------------------%
% Atoms	

term_atom(Name::in) = (atom(symbol(Name))::out).

% Force explicit symbol comparison, rather than deconstructing symbols for
% string comparison.
term_atom(Name::in) = (atom(Symbol)::in) :- Symbol = symbol(Name).

term_atom(Name::out) = (atom(symbol(Name))::in).

:- pragma promise_equivalent_clauses(term_atom/1).
	
%-----------------------------------------------------------------------------%
%	Variables

is_var(var(_)).

term_var(ID) = var(ID).

%-----------------------------------------------------------------------------%
% Values

term_value(T::in) = (value(from_mr_value(T))::out).
term_value(to_mr_value(Value)::out) = (value(Value)::in).

:- pragma promise_equivalent_clauses(term_value/1).

is_value(value(_)).

deconstruct_value_term(value(Value)) = to_some_mr_value(Value).



%-----------------------------------------------------------------------------%
% Relation terms

term_nil = relation(nil).


%-----------------------------------------------------------------------------%
% Higher Order terms



%-----------------------------------------------------------------------------%
% Utility

:- pred zero_index_err(string::in, mh_term::in) is erroneous.

zero_index_err(Action, Term) :- 
	error("Attempt to " ++ Action ++ " "  ++ term_description(Term) ++ 
	". Zero arity terms may not be indexed.").
	
:- func mr_type_name(T) = string.

mr_type_name(T) = type_name(type_of(T)).

:- func term_description(mh_term) = string.

term_description(atom(Symbol)) = "atom """ ++ to_string(Symbol) ++ """".
term_description(var(V)) = "variable with id " ++ string(V).
term_description(value(M)) = 
	"mercury value term of type " ++ mr_type_name(M).
term_description(cons(A, R)) = 
	"constructor " ++ string(A) ++ "(" ++ mr_type_name(R) ++ ")".
term_description(relation(_)) =
	"mercury relation term". %TODO: Relation and proposition description
