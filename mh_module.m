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

:- import_module mh_symbol.
:- import_module mh_type.
:- import_module mh_term.

:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

:- type mh_module.

	% init_module(Name, Module) true iff Module is a new module with name Name
:- pred init_module(string, mh_module).
:- mode init_module(in, in) is semidet.
:- mode init_module(in, out) is det.
:- mode init_module(in, uo) is det.

:- func init_module(string) = mh_module.
:- mode init_module(in) = out is det.
:- mode init_module(in) = uo is det.

:- pred is_init_module(mh_module::in) is semidet.

:- func module_name(mh_module) = string.


%-----------------------------------------------------------------------------%

 % predicate to declare a primitive mercury type for a module
:- pred add_primitive(string, type_desc, mh_module, mh_module).
:- mode add_primitive(in, in, in, out).
:- mode add_primitive(in, in, di, uo ).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.



%-----------------------------------------------------------------------------%

:- type mh_module
	--->	mh_module(functor_table, type_table).






