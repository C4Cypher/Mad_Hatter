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

:- import_module mh_var.
:- import_module mh_symbol.
:- import_module mh_relation.
:- import_module mh_procedure.
:- import_module mh_arity.
:- import_module mh_index.


%-----------------------------------------------------------------------------%
% 	mh_term

:- type mh_term 

	% nil, the abscence of term
	---> 	nil

	% variables
	;		var(var_id::var_id)
	;		anonymous
	
	% atomic terms
	;		atom(symbol)
	
	% values
	;		some [T] mr_value(T)
	
	% compound terms
	;		some [T] compound(symbol, T) => relation(T)
	;		some [T] mr_relation(T) => relation(T)
	
	% Higher order terms
	;		some [T] predicate(T) => predicate(T)
	;		some [T] functor(T) => functor(T)
	;		some [T] function(T) => function(T).
	
:- instance arity(mh_term).

:- instance index(mh_term, mh_term).

% :- instance relation(mh_term).


%-----------------------------------------------------------------------------%
%	Variables

:- inst mh_var 
	--->	var(ground)
	;		anonymous.
	
:- type mh_var =< mh_term 
	---> 	var(var_id::var_id)
	;		anonymous.
	
:- mode is_var == ground >> mh_var.

:- pred is_var(mh_term::is_var) is semidet.


%-----------------------------------------------------------------------------%
%	Atoms

:- type atom =< mh_term
	--->	atom(symbol).
	
%-----------------------------------------------------------------------------%
%	Values

:- type mercury_value =< mh_term
	---> 	some [T] mr_value(T).
	

%-----------------------------------------------------------------------------%
%	Compound terms

:- inst compound_term
	--->	compound(ground, ground)
	;		mr_relation(ground).

:- type compound_term =< mh_term
	--->	some [T] compound(symbol, T) => relation(T)
	;		some [T] mr_relation(T) => relation(T).
	
:- instance arity(compound_term).
	
:- 	inst mh_compound ---> compound(ground, ground).
	
:- type mh_compound =< compound_term
	--->	some [T] compound(symbol, T) => relation(T).

:- instance arity(mh_compound).

%-----------------------------------------------------------------------------%
%	Relations

:- inst mercury_relation ---> mr_relation(ground).

:- type mercury_relation =< compound_term
	--->	some [T] mr_relation(T) => relation(T).
	
:- instance arity(mercury_relation).
	





%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module type_desc.
:- import_module string.

%-----------------------------------------------------------------------------%
% 	mh_term

:- instance arity(mh_term) where [
	arity(T, A) :- require_complete_switch [T] (
		(	T = nil
		;	T = var(_)
		;	T = anonymous
		;	T = atom(_)
		;	T = mr_value(_)
		;	T = predicate(_)
		;	T = functor(_)
		;	T = function(_)
		), A = 0
		
	;	T = compound(_, R), A = arity(R)
	;	T = mr_relation(R), A = arity(R)
	)
].

:- instance index(mh_term, mh_term) where [
	index(T, I, U) :- require_complete_switch [T] (
		(	T = nil
		;	T = var(_)
		;	T = anonymous
		;	T = atom(_)
		;	T = mr_value(_)
		;	T = predicate(_)
		;	T = functor(_)
		;	T = function(_)
		), zero_index_err("index", T) 	
		
		;	T = compound(_, R), index(R, I, U)
		;	T = mr_relation(R), index(R, I, U)
	),
		
	set_index(I, U, !T) :- require_complete_switch [!.T] (
		(	!.T = nil
		;	!.T = var(_)
		;	!.T = anonymous
		;	!.T = atom(_)
		;	!.T = mr_value(_)
		;	!.T = predicate(_)
		;	!.T = functor(_)
		;	!.T = function(_)
		), zero_index_err("set index of", !.T) 	
		
		; 	!.T = compound(F, R0),	set_index(I, U, R0, R), 
			!:T = 'new compound'(F, R)
		;	!.T = mr_relation(R0), set_index(I, U, R0, R),
			!:T = 'new mr_relation'(R) 
	),
		
	fold_index(P, T, !A) :- require_complete_switch [T] (
		(	T = nil
		;	T = var(_)
		;	T = anonymous
		;	T = atom(_)
		;	T = mr_value(_)
		;	T = predicate(_)
		;	T = functor(_)
		;	T = function(_)
		), zero_index_err("fold index on", T)
		
		;	T = compound(_, R), fold_index(P, R, !A)
		;	T = mr_relation(R), fold_index(P, R, !A)
	),
	
	map_index(P, !T) :- require_complete_switch [!.T] (
		(	!.T = nil
		;	!.T = var(_)
		;	!.T = anonymous
		;	!.T = atom(_)
		;	!.T = mr_value(_)
		;	!.T = predicate(_)
		;	!.T = functor(_)
		;	!.T = function(_)
		), zero_index_err("map index on", !.T) 	
		
		; 	!.T = compound(F, R0), map_index(P, R0, R), 
			!:T = 'new compound'(F, R)
		;	!.T = mr_relation(R0), map_index(P, R0, R),
			!:T = 'new mr_relation'(R)
	)
].
		
	
	
		


		
%-----------------------------------------------------------------------------%
%	Variables

is_var(T) :- T = anonymous ; T = var(_).


%-----------------------------------------------------------------------------%
%	Atoms


%-----------------------------------------------------------------------------%
%	Values

%-----------------------------------------------------------------------------%
%	Compound terms

:- instance arity(compound_term) where [
	arity(T, A) :- require_complete_switch [T] 
	(	T = compound(_, R), A = arity(R)
	;	T = mr_relation(R), A = arity(R)
	)
].

:- instance arity(mh_compound) where [ arity(compound(_, T), arity(T)) ].
	
		
	
%-----------------------------------------------------------------------------%
%	Relations


:- instance arity(mercury_relation) where [ arity(mr_relation(R), arity(R)) ].

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
term_description(atom(A)) = "atomic term '" ++ to_string(A) ++ "'".
term_description(mr_value(M)) = 
	"mercury value term of type " ++ mr_type_name(M).
term_description(compound(A, R)) = 
	"compound term " ++ to_string(A) ++ "(" ++ mr_type_name(R) ++ ")".
term_description(mr_relation(R)) = 
	"mercury relation term of type " ++	mr_type_name(R).
term_description(predicate(P)) = 
	"mercury predicate term of type " ++ mr_type_name(P).
term_description(functor(F)) =
	"mercury functor term of type " ++ mr_type_name(F).
term_description(function(F)) =
	"mercury function term of type " ++ mr_type_name(F).