%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_index.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_index.

:- interface.

:- import_module list.

:- import_module array.

%-----------------------------------------------------------------------------%

:- typeclass index(T, U) <= (T -> U) where [

	pred valid_index(T, int),
	mode valid_index(in, in) is semidet, % suceed on valid index
	mode valid_index(in, out) is nondet, % return all valid indexes

	pred index(T, int, U),
	mode index(in, in, in) is semidet, % implicit fail on inequality
	mode index(in, in, out) is semidet, % fail on invalid index
	mode index(in, out, out) is nondet, % all indexed values
	
	pred set_index(int, U, T, T),
	mode set_index(in, in, in, out) is semidet, % fail on invalid index
	mode set_index(out, in, in, out) is nondet % update any index nondet
].

% TODO: Subclass index with unique set modes 

%-----------------------------------------------------------------------------%

