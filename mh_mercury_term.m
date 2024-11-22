%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_mercury_term.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_mercury_term.

:- interface.

:- use_module term.

:- import_module mh_term.


%-----------------------------------------------------------------------------%
% Mercury terms

:- type mr_term(T) == term.term(T).
:- type mr_term == term.term.

:- type mr_var(T) == term.var(T).
:- type mr_var == term.var.

:- type mr_context == term.context.

%-----------------------------------------------------------------------------%
% Mercury term conversion

:- func convert_mr_term(mr_term(_)) = mh_term.

:- pred convert_mr_term(mr_term(_)::in, mh_term::out) is det.

:- pred convert_mr_term(mr_term(_)::in, mh_term::out, mr_context::out) is det.

%-----------------------------------------------------------------------------%
% Mercury primitives

% :- pred mercury_primitive_type(T::unused) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module univ.
:- import_module list.

:- import_module mh_symbol.
:- import_module mh_tuple.
:- import_module mh_var_id.

%-----------------------------------------------------------------------------%
% Mercury terms


%-----------------------------------------------------------------------------%
% Mercury term conversion

convert_mr_term(M) = 
	( if 
		(
			M = term.functor(Const, SubTerms, _),
			(
				Const = term.atom(Symbol),
				(
					SubTerms = [],
					Atom:mh_term = atom(symbol(Symbol)),
					Term = Atom
				;
					SubTerms = [ Arg | Args ],
					Atom:functor = atom(symbol(Symbol)),
					( if Args = []
					then 
						Term = cons(Atom, convert_mr_term(Arg) )
					else
						Tuple = list_tuple(map(convert_mr_term, SubTerms)),
						Term = cons(Atom,  tuple_term(Tuple))
					)
				)
			)
		;
			M = term.variable(V, _),
			Term = var(mr_var_id(V))
		)
	then 
		Term 
	else 
		new_value_term(M)		
	).
	
convert_mr_term(M, convert_mr_term(M)).

convert_mr_term(M, Term, Context) :-  
	( if 
		(
			M = term.functor(Const, SubTerms, C),
			(
				Const = term.atom(Symbol),
				(
					SubTerms = [],
					Atom:mh_term = atom(symbol(Symbol)),
					T = Atom
				;
					SubTerms = [ Arg | Args ],
					Atom:functor = atom(symbol(Symbol)),
					( if Args = []
					then 
						T = cons(Atom, convert_mr_term(Arg) )
					else
						Tuple = list_tuple(map(convert_mr_term, SubTerms)),
						T = cons(Atom,  tuple_term(Tuple))
					)
				)
			)
		;
			M = term.variable(V, C),
			T = var(mr_var_id(V))
		)
	then 
		Term = T, 
		Context = C
	else 
		Term = new_value_term(M),
		Context = term.get_term_context(M)
	).
	

