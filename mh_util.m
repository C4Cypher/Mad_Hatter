%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_util.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_util.

:- interface.

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
	% variables being cast are the same type. If this condititions is not
	% met, det_dynamic_cast will throw an exceptiosn IF unsafe_dynamic_cast
	% is false, otherwise you will get undefined behavior
	
:- func det_dynamic_cast(T) = V.

:- pred det_dynamic_cast(T::in, V::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.


:- import_module require.

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
