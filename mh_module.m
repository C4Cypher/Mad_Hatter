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
:- import_module mh_environment.
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
% Module environment

:- pred environment(mh_module::in, mh_environment::out) is det.
:- func environment(mh_module) = mh_environment.

% Insert a new atom to the module's environment, fail if it already exists
:- pred insert_atom_substitution(mh_symbol::in, mh_term::in, 
	mh_module::in, mh_module::out)	is semidet.

% Replace an existing atom in the module's environment, fail if the atom
% does not exist in the environment
:- pred replace_atom_substitution(mh_symbol::in, mh_term::in,
	mh_module::in, mh_module::out)	is semidet.



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

:- type mr_type_set == set(type_desc).

:- type mh_module
	--->	symbolic_module(
				mh_symbol, 		% module name
				mh_environment, % map of atoms to terms
				mr_type_set		% set of mercury types to convert from terms
				% declarations
				% imported modules
				% used modules
			).
			
init_module(Name, init_module(Name)).

init_module(Name) = symbolic_module(symbol(Name), new_env(map.init), set.init).

module_name(symbolic_module(symbol(Name), _, _)) = Name.

%-----------------------------------------------------------------------------%
% Module environment

environment(M, environment(M)).

environment(symbolic_module(_, Env, _)) = Env.

insert_atom_substitution(Symb, Term, 
	symbolic_module(Name, map_env(!.Env), Types),
	symbolic_module(Name, map_env(!:Env), Types)
) :-
	map.insert(Symb, Term, !Env).
	
	
replace_atom_substitution(Symb, Term, 
	symbolic_module(Name, map_env(!.Env), Types),
	symbolic_module(Name, map_env(!:Env), Types)
) :-
	map.update(Symb, Term, !Env).









