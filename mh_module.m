%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_module.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_module.

:- interface.


:- import_module type_desc.

:- import_module mh_term.
:- import_module mh_symbol.



%-----------------------------------------------------------------------------%
% Mad Hatter modules

:- type mh_module.

:- pred init_module(string, mh_module).
:- mode init_module(in, out) is det.

:- func init_module(string) = mh_module.
:- mode init_module(in) = out is det.

:- func module_name(mh_module) = string.

%-----------------------------------------------------------------------------%
% Module level atom substitution

:- pred insert_atom(mh_symbol::in, mh_term::in, mh_module::in, mh_module::out)
	is det.
	
:- func atom_lookup(mh_module, mh_symbol) = mh_term.
:- pred atom_lookup(mh_module::in, mh_symbol::in, mh_term::out) is det.

:- func atom_search(mh_module, mh_symbol) = mh_term is semidet.
:- pred atom_search(mh_module::in, mh_symbol::in, mh_term::out) is semidet.


%-----------------------------------------------------------------------------%
% Mercury types

/*
 % predicate to declare a valid mercury type for a module
:- pred add_mr_type(mh_symbol, type_desc, mh_module, mh_module).
:- mode add_mr_type(in, in, in, out).
:- mode add_mr_type(in, in, di, uo ).
*/

%-----------------------------------------------------------------------------%


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module set.


%-----------------------------------------------------------------------------%
% Mad Hatter modules

:- type symbol_map == map(mh_symbol, mh_term). 
:- type mr_type_set == set(type_desc).

:- type mh_module
	--->	symbolic_module(
				mh_symbol, 		% module name
				symbol_map, 	% map of atoms to terms
				mr_type_set		% set of mercury types to convert from terms
				% declarations
				% imported modules
				% used modules
			).
			
init_module(Name, init_module(Name)).

init_module(Name) = symbolic_module(symbol(Name), map.init, set.init).

module_name(symbolic_module(symbol(Name), _, _)) = Name.

%-----------------------------------------------------------------------------%
% Module level atom substitution

insert_atom(Symbol, Term, 
	symbolic_module(Name, !.Map, Types),
	symbolic_module(Name, !:Map, Types)
) :-
	det_insert(Symbol, Term, !Map).
	
atom_lookup(symbolic_module(_, Map, _), Symbol) = map.lookup(Map, Symbol).
atom_lookup(Module, Symbol, atom_lookup(Module, Symbol)).

atom_search(symbolic_module(_, Map, _), Symbol) = map.search(Map, Symbol).
atom_search(Module, Symbol, atom_search(Module, Symbol)).







