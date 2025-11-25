%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: util.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module util.

:- interface.

:- import_module list.
:- import_module string.


%-----------------------------------------------------------------------------%
% Deterministic dynamic casting

	% if unsafe_dynamic_cast is true, det_dynamic_cast will perform a naked
	% type cast, otherwise it will perform a normal dynamic cast, throwing
	% an exception if the cast fails.
	
	% I intend to leave unsafe_dynamic_cast as false until I get to profiling
	% Mad Hatter, to see if naked type casts work as intended, and to see
	% if there is any performance benefit to naked type casting.
	
:- pred unsafe_dynamic_cast is semidet.

	% DO NOT USE THESE PREDICATES unless you have already verified that the 
	% variables being cast are the same type. If this conditition is not
	% met, det_dynamic_cast will throw an exceptiosn IF unsafe_dynamic_cast
	% is false, otherwise you will get undefined behavior
	
:- func det_dynamic_cast(T) = V.

:- pred det_dynamic_cast(T::in, V::out) is det.
	
%-----------------------------------------------------------------------------%
% Exceptions

	
% bounds_error(Pred, Msg)
% Throw an array.index_out_of_bounds exception
:- pred bounds_error(string::in, string::in) is erroneous.

% format_error(Pred, Msg, Vars)
% Throw a software error while formatting Msg with Vars
:- pred format_error(string::in, string::in, 
	list(poly_type)::in) is erroneous.

% format_out_of_bounds_error(Pred, Msg, Vars)
% Same, but with an out of bounds exception
:- pred format_bounds_error(string::in, string::in, 
	list(poly_type)::in) is erroneous.
	
% Call report_lookup_error/2 as an erroneous function.
:- func report_lookup_error(string, K) = _ is erroneous.

:- func report_lookup_error(string, K, V) = _ is erroneous.

%-----------------------------------------------------------------------------%
% Misc

:- func func_fail = _ is failure.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module exception.
:- import_module array.

%-----------------------------------------------------------------------------%
% Deterministic dynamic casting


% Set to true at your own risk! the Mercury designers never intended for
% the use of unchecked type casting outside of the internal operation of
% the Mercury Melbourne Compiler, and Mercury runtime

unsafe_dynamic_cast :- false.

:- pragma no_determinism_warning(unsafe_dynamic_cast/0).



det_dynamic_cast(T) = V :- 
	(if unsafe_dynamic_cast
	then
		private_builtin.unsafe_type_cast(T, V)  % !!!!!!!!!!
	else
		( if dynamic_cast(T, U)
		then V = U
		else unexpected($module, $pred, "Dynamic cast failure.")
		)
	).
	


det_dynamic_cast(T, det_dynamic_cast(T)).

	
%-----------------------------------------------------------------------------%
% Exceptions
	
bounds_error(Pred, Msg) :-
	throw(array.index_out_of_bounds(Pred ++ ": " ++ Msg)).

format_error(Pred, Msg, Vars) :-
	string.format(Pred ++ ": " ++ Msg, Vars, Err),
	error(Err).
	
format_bounds_error(Pred, Msg, Vars) :-
	string.format(Msg, Vars, Err),
	bounds_error(Pred, Err).
	
report_lookup_error(Msg, K) = _ :-
	report_lookup_error(Msg, K).
	
report_lookup_error(Msg, K, V) = _ :-
	report_lookup_error(Msg, K, V).
	
%-----------------------------------------------------------------------------%
% Misc

func_fail = _ :- fail.