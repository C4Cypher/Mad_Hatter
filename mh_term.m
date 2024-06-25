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
	;		some [T] mr_struct(T) 
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
	;		mr_struct(ground)
	;		mr_relation(ground).

:- type compound_term =< mh_term
	--->	some [T] compound(symbol, T) => relation(T)
	;		some [T] mr_struct(T) 
	;		some [T] mr_relation(T) => relation(T).
	
:- instance arity(compound_term).
	
:- 	inst mh_compound ---> compound(ground, ground).
	
:- type mh_compound =< compound_term
	--->	some [T] compound(symbol, T) => relation(T).

:- instance arity(mh_compound).

%-----------------------------------------------------------------------------%
%  Structures - Mercury values indexed via deconstruction

:- type mercury_structure =< compound_term
	---> 	some [T] mr_struct(T).
	
:- instance arity(mercury_structure).
	
%-----------------------------------------------------------------------------%
%	Relations

:- inst mercury_relation ---> mr_relation(ground).

:- type mercury_relation =< compound_term
	--->	some [T] mr_relation(T) => relation(T).
	
:- instance arity(mercury_relation).
	





%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module deconstruct.
:- import_module univ.
:- import_module construct.

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
	;	T = mr_struct(S), A = struct_arity(S)
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
		), fail	
		
		;	T = compound(_, R), index(R, I, U)
		;	T = mr_struct(S), struct_index(S, I, U)
		;	T = mr_relation(R), index(R, I, U),
		
	set_index(I, U, !T) :-
		(	T = nil
		;	T = var(_)
		;	T = anonymous
		;	T = atom(_)
		;	T = mr_value(_)
		;	T = predicate(_)
		;	T = functor(_)
		;	T = function(_)
		), fail	
		
		
	
	
		

:- func struct_arity(T) = int.

struct_arity(T) = A :- functor(T, canonicalize, _, A).
		
:- pred struct_index(T, int, mh_term) <= index(T, _).
:- mode struct_index(in, in, mh_term) is semidet.
:- mode struct_index(in, out, mh_term) is nondet.

:- pragma promise_equivalent_clauses(struct_index/2).

struct_index(T::in, I::in, U::out) :- 
	det_arg(T, canonicalize, I, V),
	U = 'new mr_value'(univ_value(V)).
	
struct_index(T::in, I::out, U::out) :-
	deconstruct(T, canonicalize, _, _, Args),
	univ_list_index(Args, I, U).
	
	
:- pred univ_list_index(list(univ)::in, int::out, mh_term::out) is nondet.

univ_list_index([ X | XS], I, Term) :-
	Term = 'new mr_value'(X),
	I = 1, 
	(
		XS = []
	;	univ_list_index(XS, I + 1, Term)
	).
		
:- pred set_struct_index(int, mh_term, T, T).
:- mode set_struct_index(in, in, in, out) is det.
:- mode set_struct_index(out, in, in, out) is nondet.

:- pragma promise_equivalent_clauses(set_struct_index/2).

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
	;	T = mr_struct(S), A = struct_arity(S)
	;	T = mr_relation(R), A = arity(R)
	)
].

:- instance arity(mh_compound) where [ arity(compound(_, T), arity(T)) ].
	
		
	
%-----------------------------------------------------------------------------%
% Structures

:- instance arity(mercury_structure) where [ 
	 arity(mr_struct(T), type_ctor_arity(type_ctor(type_of(T))))
].

%-----------------------------------------------------------------------------%
%	Relations


:- instance arity(mercury_relation) where [ arity(mr_relation(R), arity(R)) ].