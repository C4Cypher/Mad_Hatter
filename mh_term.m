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

	% nil, the abscence of term
	---> 	nil
	
	% atomic values
	;		atom(mh_symbol)

	% variables
	;		var(var_id)
	
	% values
	;		value(mh_value)
	
	% compound terms (cons stands for 'constructor')
	;		cons(mh_term, mh_tuple) % F(A1, A2, A3)

	% Higher order terms
	;		relation(mh_relation).

%-----------------------------------------------------------------------------%
% Subterms

% Return a tuple of a term's subterms, fail if there are none
:- func subterms(mh_term) = mh_tuple is semidet.
:- pred subterms(mh_term::in, mh_tuple::out) is semidet.

% Return an empty tuple if there are no subterms.
:- func det_subterms(mh_term) = mh_tuple is det.
:- pred det_subterms(mh_term::in, mh_tuple::out) is det.

%-----------------------------------------------------------------------------%
% Ground terms


:- pred ground_term(mh_term::in) is semidet.

:- func ground_term(mh_term) = mh_term.
:- mode ground_term(in) = out is semidet.
:- mode ground_term(out) = in is semidet.

%-----------------------------------------------------------------------------%
% Substitutions


% Apply a substitution to a term, if the term is a variable, replace the
% variable with the substituted term as appropriate, if not, return the term
% with the substitution applied.

:- pred apply_term_substitution(mh_substitution::in, 
	mh_term::in, mh_term::out) is det.
:- func apply_term_substitution(mh_substitution, mh_term) = mh_term.


% :- instance tuple(mh_term).
% :- instance substitutable(mh_term).



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

:- inst value_term
	--->	value(ground).

:- type value_term =< mh_term
	---> 	value(mh_value).
	
:- mode is_value == ground >> value_term.

:- pred is_value(mh_term::is_value) is semidet.


%-----------------------------------------------------------------------------%
%	Mad Hatter constructors
	
:- inst mh_constructor ---> cons(ground, ground).
	
:- type mh_constructor =< mh_term
	--->	cons(mh_term, mh_tuple).
	
:- mode mh_constructor == ground >> mh_constructor.

:- pred mh_constructor(mh_term::mh_constructor) is semidet.


%-----------------------------------------------------------------------------%
% Relation terms

:- inst relation_term 
	--->	relation(ground).
	
:- type relation_term =< mh_term
	--->	relation(mh_relation).
	

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module type_desc.
:- import_module string.
:- import_module list.

%-----------------------------------------------------------------------------%
% Subterms

%TODO: append for mh_tuple
subterms(cons(Car, Cdr)) = 
	list_tuple( [ Car | to_list(Cdr) ] ).

subterms(relation(R)) = relation_subterms(R).
		
subterms(Term, subterms(Term)).

det_subterms(Term) = 
	(if Subterms = subterms(Term) 
	then 
		SubTerms
	else 
		list_tuple([]) 
	).

det_subterms(Term, det_subterms(Term)).
%-----------------------------------------------------------------------------%
% Ground terms

ground_term(T) :-
	require_complete_switch [T] (
		T = nil; % nil is itself considered ground
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
		( 
			!.Term = nil 
		;	!.Term = atom(_) 
		;	!.Term = value(_) 
		), 
		!:Term = !.Term 
		
	;	!.Term = var(ID), sub_id_lookup(Sub, ID, !:Term)
			
	;	!.Term = cons(Car0, Cdr0),
		apply_term_substitution(Sub, Car0, Car),
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
		
	;	!.Term = term_sub(SubTerm, Sub0),
		compose_substitutions(Sub0, Sub, Sub1),
		!:Term = term_sub(SubTerm, Sub1)
	).

apply_term_substitution(S, !.T) = !:T :- apply_term_substitution(S, !T).


%-----------------------------------------------------------------------------%
%	Atoms

is_atom(atom(_)).


		
%-----------------------------------------------------------------------------%
%	Variables

is_var(var(_)).

%-----------------------------------------------------------------------------%
%	Values

is_value(value(_)).

%-----------------------------------------------------------------------------%
%	Mad Hatter constructors

mh_constructor(cons(_, _)).

	
%-----------------------------------------------------------------------------%
%	Tuple terms

tuple_term(tuple_term(_)).

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
term_description(value(M)) = 
	"mercury value term of type " ++ mr_type_name(M).
term_description(cons(A, R)) = 
	"constructor " ++ string(A) ++ "(" ++ mr_type_name(R) ++ ")".
term_description(lazy(Term)) =
	"lazy " ++ term_description(Term).
term_description(tuple_term(_)) = 
	"mercury tuple term".
term_description(relation(_)) =
	"mercury relation term".
term_description(term_sub(Term, _)) =
	"substitution of " ++ term_description(Term).
