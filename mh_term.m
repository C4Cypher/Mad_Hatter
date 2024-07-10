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

:- import_module mh_var.
:- import_module mh_symbol.
:- import_module mh_tuple.
:- import_module mh_relation.
:- import_module mh_arity.


%-----------------------------------------------------------------------------%
% 	mh_term

:- type mh_term 

	% nil, the abscence of term
	---> 	nil

	% variables
	;		var(var_id)
	;		anonymous
	
		% values
	;		mr_value(univ)
	
	% compound terms
	;		some [T] constructor(symbol, T) => tuple(T)
	;		some [T] mr_tuple(T) => tuple(T)
	
	% Higher order terms
	;		some [T] relation(T) => relation(T)
	;		some [T] predicate(T) => predicate(T)
	;		some [T] function(T) => function(T).
	
:- func functor(mh_term) = symbol is semidet.
	
:- instance arity(mh_term).
% :- instance tuple(mh_term).


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
%	Values

:- type mercury_value =< mh_term
	---> 	mr_value(univ).
	

%-----------------------------------------------------------------------------%
%	Compound terms

:- inst compound_term
	--->	constructor(ground, ground)
	;		mr_tuple(ground).

:- type compound_term =< mh_term
	--->	some [T] constructor(symbol, T) => tuple(T)
	;		some [T] mr_tuple(T) => tuple(T).

:- instance arity(compound_term).
% :- instance tuple(compound_term).

%-----------------------------------------------------------------------------%
%	Mad Hatter constructors
	
:- 	inst mh_constructor ---> constructor(ground, ground).
	
:- type mh_constructor =< compound_term
	--->	some [T] constructor(symbol, T) => tuple(T).

:- instance arity(mh_constructor).
% :- instance tuple(mh_constructor).

%-----------------------------------------------------------------------------%
%	Relations

:- inst mercury_tuple ---> mr_tuple(ground).

:- type mercury_tuple =< compound_term
	--->	some [T] mr_tuple(T) => tuple(T).
	
:- instance arity(mercury_tuple).
% :- instance tuple(mercury_tuple).
	





%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module type_desc.
:- import_module string.

%-----------------------------------------------------------------------------%
% 	mh_term

functor(constructor(F, _)) = F.

:- instance arity(mh_term) where [
	arity(T, A) :- require_complete_switch [T] (
		(	T = nil
		;	T = var(_)
		;	T = anonymous
		;	T = mr_value(_)
		;	T = predicate(_)
		;	T = relation(_)
		;	T = function(_)
		), A = 0
		
	;	T = constructor(_, R), A = arity(R)
	;	T = mr_tuple(R), A = arity(R)
	)
].

% :- instance index(mh_term, mh_term) where [
	% index(T, I, U) :- require_complete_switch [T] (
		% (	T = nil
		% ;	T = var(_)
		% ;	T = anonymous
		% ;	T = atom(_)
		% ;	T = mr_value(_)
		% ;	T = predicate(_)
		% ;	T = functor(_)
		% ;	T = function(_)
		% ), zero_index_err("index", T) 	
		
		% ;	T = constructor(_, R), index(R, I, U)
		% ;	T = mr_tuple(R), index(R, I, U)
	% ),
		
	% set_index(I, U, !T) :- require_complete_switch [!.T] (
		% (	!.T = nil
		% ;	!.T = var(_)
		% ;	!.T = anonymous
		% ;	!.T = atom(_)
		% ;	!.T = mr_value(_)
		% ;	!.T = predicate(_)
		% ;	!.T = functor(_)
		% ;	!.T = function(_)
		% ), zero_index_err("set index of", !.T) 	
		
		% ; 	!.T = constructor(F, R0),	set_index(I, U, R0, R), 
			% !:T = 'new compound'(F, R)
		% ;	!.T = mr_tuple(R0), set_index(I, U, R0, R),
			% !:T = 'new mr_tuple'(R) 
	% ),
		
	% fold_index(P, T, !A) :- require_complete_switch [T] (
		% (	T = nil
		% ;	T = var(_)
		% ;	T = anonymous
		% ;	T = atom(_)
		% ;	T = mr_value(_)
		% ;	T = predicate(_)
		% ;	T = functor(_)
		% ;	T = function(_)
		% ), zero_index_err("fold index on", T)
		
		% ;	T = constructor(_, R), fold_index(P, R, !A)
		% ;	T = mr_tuple(R), fold_index(P, R, !A)
	% ),
	
	% map_index(P, !T) :- require_complete_switch [!.T] (
		% (	!.T = nil
		% ;	!.T = var(_)
		% ;	!.T = anonymous
		% ;	!.T = atom(_)
		% ;	!.T = mr_value(_)
		% ;	!.T = predicate(_)
		% ;	!.T = functor(_)
		% ;	!.T = function(_)
		% ), zero_index_err("map index on", !.T) 	
		
		% ; 	!.T = constructor(F, R0), map_index(P, R0, R), 
			% !:T = 'new compound'(F, R)
		% ;	!.T = mr_tuple(R0), map_index(P, R0, R),
			% !:T = 'new mr_tuple'(R)
	% )
% ].
		
% :- instance tuple(mh_term) where [ ].
	
		


		
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
	(	T = constructor(_, R), A = arity(R)
	;	T = mr_tuple(R), A = arity(R)
	)
].


% :- instance index(compound_term, mh_term) where [
	% index(T, I, U) :- require_complete_switch [T] (
		% T = constructor(_, R), index(R, I, U)
	% ;	
		% T = mr_tuple(R), index(R, I, U)
	% ),
		
	% set_index(I, U, !T) :- require_complete_switch [!.T] (
		% !.T = constructor(F, R0),	set_index(I, U, R0, R), 
		% !:T = 'new compound'(F, R)
	% ;
		% !.T = mr_tuple(R0), set_index(I, U, R0, R),
		% !:T = 'new mr_tuple'(R) 
	% ),
		
	% fold_index(P, T, !A) :- require_complete_switch [T] (
		% T = constructor(_, R), fold_index(P, R, !A)
	% ;	
		% T = mr_tuple(R), fold_index(P, R, !A)
	% ),
	
	% map_index(P, !T) :- require_complete_switch [!.T] (
		% !.T = constructor(F, R0), map_index(P, R0, R), 
		% !:T = 'new compound'(F, R)
	% ;	
		% !.T = mr_tuple(R0), map_index(P, R0, R),
		% !:T = 'new mr_tuple'(R)
	% )
% ].

% :- instance tuple(compound_term) where [ ].

%-----------------------------------------------------------------------------%
%	Mad Hatter compound terms

:- instance arity(mh_constructor) where [ arity(constructor(_, T), arity(T)) ].

% :- instance index(mh_compound, mh_term) where [ 
	% index(constructor(_, R), I, U) :- index(R, I, U),
	
	% set_index(I, U, !T) :- (
		% !.T = constructor(F, R0),	set_index(I, U, R0, R), 
		% !:T = 'new compound'(F, R)
	% ),
		
	% fold_index(P, constructor(_, R), !A) :- 
		% fold_index(P, R, !A),
	
	% map_index(P, !T) :- (
		% !.T = constructor(F, R0), map_index(P, R0, R), 
		% !:T = 'new compound'(F, R)
	% )
% ].
	
% :- instance tuple(mh_compound) where [ ].	
		
	
%-----------------------------------------------------------------------------%
%	Relations

:- instance arity(mercury_tuple) where [ arity(mr_tuple(R), arity(R)) ].

% :- instance index(mercury_tuple, mh_term) where [ 
	% index(mr_tuple(R), I, U) :- index(R, I, U),
	
	% set_index(I, U, !T) :- (
		% !.T = mr_tuple(R0),	set_index(I, U, R0, R), 
		% !:T = 'new mr_tuple'(R)
	% ),
		
	% fold_index(P, mr_tuple(R), !A) :- 
		% fold_index(P, R, !A),
	
	% map_index(P, !T) :- (
		% !.T = mr_tuple(R0), map_index(P, R0, R), 
		% !:T = 'new mr_tuple'(R)
	% )
% ].

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
term_description(constructor(A, R)) = 
	"constructor " ++ to_string(A) ++ "(" ++ mr_type_name(R) ++ ")".
term_description(mr_tuple(R)) = 
	"mercury tuple term of type " ++	mr_type_name(R).
term_description(predicate(P)) = 
	"mercury predicate term of type " ++ mr_type_name(P).
term_description(relation(F)) =
	"mercury relation term of type " ++ mr_type_name(F).
term_description(function(F)) =
	"mercury function term of type " ++ mr_type_name(F).