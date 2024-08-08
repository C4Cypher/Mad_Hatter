%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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
:- import_module mh_relation.
:- import_module mh_predicate.
:- import_module mh_function.
:- import_module mh_substitution.
:- import_module mh_arity.


%-----------------------------------------------------------------------------%
% 	mh_term

:- type mh_term 

	% nil, the abscence of term
	---> 	nil
	
	% atomic values
	;	atom(symbol)

	% variables
	;		var(var_id)
	;		anonymous
	
		% values
	;		mr_value(univ)
	
	% compound terms
	;		cons(symbol, mh_term)
	;		tuple_term(mh_tuple)
	
	% Higher order terms
	;		relation(mh_relation)
	;		predicate(mh_predicate)
	;		function(mh_function)
	
	% Substitutions
	;		sub(mh_term, mh_substitution).
	
:- func functor(mh_term) = mh_functor is semidet.
	
:- instance arity(mh_term).
% :- instance tuple(mh_term).

%-----------------------------------------------------------------------------%
%  Functor

:- type functor =< mh_term
	% Atoms
	---> 	atom(symbol)
	
	% Higher order terms
	;		relation(mh_relation)
	;		predicate(mh_predicate)
	;		function(mh_function).

%-----------------------------------------------------------------------------%
% Atoms

:- inst atom ---> atom(ground).

:- type atom =< functor ---> atom(symbol).

:- mode is_atom == ground >> atom.

:- pred is_atom(mh_term::is_atom) is semidet.
%-----------------------------------------------------------------------------%
%	Variables

:- inst mh_var 
	--->	var(ground)
	;		anonymous.
	
:- type mh_var =< mh_term 
	---> 	var(var_id)
	;		anonymous.
	
	
:- mode is_var == ground >> mh_var.

:- pred is_var(mh_term::is_var) is semidet.

%-----------------------------------------------------------------------------%
% 	Quantified Variables

:- inst quantified_var ---> var(ground).
	
:- type quantified_var =< mh_var ---> var(var_id).

:- func var_id(quantified_var) = var_id is det.

:- mode is_quantified == ground >> quantified_var.

:- pred is_quantified_var(mh_term::is_quantified) is semidet.

:- pred var_is_quantified(mh_var::is_quantified) is semidet.

%-----------------------------------------------------------------------------%

:- type var_set.



%-----------------------------------------------------------------------------%
%	Values

:- type mercury_value =< mh_term
	---> 	mr_value(univ).
	

%-----------------------------------------------------------------------------%
%	Compound terms

:- inst compound_term
	--->	cons(ground, ground)
	;		tuple_term(ground).

:- type compound_term =< mh_term
	--->	cons(symbol, mh_term)
	;		tuple_term(mh_tuple).

:- instance arity(compound_term).
% :- instance tuple(compound_term).

%-----------------------------------------------------------------------------%
%	Mad Hatter constructors
	
:- 	inst mh_constructor ---> cons(ground, ground).
	
:- type mh_constructor =< compound_term
	--->	cons(symbol, mh_term).

:- instance arity(mh_constructor).
% :- instance tuple(mh_constructor).

%-----------------------------------------------------------------------------%
%	Tuple terms

:- inst tuple_term ---> tuple_term(ground).

:- type tuple_term =< compound_term
	--->	tuple_term(mh_tuple).
	
:- instance arity(tuple_term).
% :- instance tuple(mercury_tuple).
	

%-----------------------------------------------------------------------------%
% Higher Order terms

:- type lambda =< functor
	--->	relation(mh_relation)
	;		predicate(mh_predicate)
	;		function(mh_function).



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module type_desc.
:- import_module string.

%-----------------------------------------------------------------------------%
% 	mh_term

functor(cons(F, _)) = F.

:- instance arity(mh_term) where [
	arity(T, A) :- require_complete_switch [T] (
		(	T = nil
		;	T = var(_)
		;	T = var(_, _)
		;	T = anonymous
		;	T = mr_value(_)
		;	T = predicate(_)
		;	T = relation(_)
		;	T = function(_)
		), A = 0
		
	;	T = cons(_, Arg), 
		(	if Arg = tuple_term(Tuple)
			then A = arity(Tuple)
			else A = 1
		)
	;	T = tuple_term(R), A = arity(R)
	)
].


		
% :- instance tuple(mh_term) where [ ].
	
		

%-----------------------------------------------------------------------------%
%	Atoms

is_atom(atom(_)).


		
%-----------------------------------------------------------------------------%
%	Variables

is_var(T) :- T = anonymous ; T = var(_).

%-----------------------------------------------------------------------------%
% 	Quantified Variables

var_id(var(ID)) = ID.

is_quantified_var(var(_)).
var_is_quantified(var(_)).


%-----------------------------------------------------------------------------%
%	Values

%-----------------------------------------------------------------------------%
%	Compound terms

:- instance arity(compound_term) where [
	arity(T, A) :- require_complete_switch [T] 
	(	T = cons(_, Arg), 
		(	if Arg = tuple_term(Tuple)
			then A = arity(Tuple)
			else A = 1
		)
	;	T = tuple_term(R), A = arity(R)
	)
].




% :- instance tuple(compound_term) where [ ].

%-----------------------------------------------------------------------------%
%	Mad Hatter compound terms

:- instance arity(mh_constructor) where [ 
	arity(cons(_, T), Arg), 
		(	if Arg = tuple_term(Tuple)
			then A = arity(Tuple)
			else A = 1
		) 
].


	
% :- instance tuple(mh_compound) where [ ].	
		
	
%-----------------------------------------------------------------------------%
%	Relations

:- instance arity(tuple_term) where [ arity(tuple_term(R), arity(R)) ].


% :- instance tuple(mercury_tuple) where [ ].

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
term_description(var(V)) = "variable with id " ++ string(V).
term_description(anonymous) = "anonymous variable".
term_description(mr_value(M)) = 
	"mercury value term of type " ++ mr_type_name(M).
term_description(cons(A, R)) = 
	"constructor " ++ to_string(A) ++ "(" ++ mr_type_name(R) ++ ")".
term_description(tuple_term(R)) = 
	"mercury tuple term of type " ++	mr_type_name(R).
term_description(predicate(P)) = 
	"mercury predicate term of type " ++ mr_type_name(P).
term_description(relation(F)) =
	"mercury relation term of type " ++ mr_type_name(F).
term_description(function(F)) =
	"mercury function term of type " ++ mr_type_name(F).