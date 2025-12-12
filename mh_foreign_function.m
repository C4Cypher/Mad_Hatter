%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_function.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_foreign_function.

:- interface.

:- import_module mh_term.

:- type function_call == (func(mh_term) = mh_term).

:- type mh_foreign_function
			% f(X) -> Y :- r(X) = Y.
	--->	mr_function(string, function_call).

:- func foreign_function_uid(mh_foreign_function) = string.

% call_foreign_function ... call function_call with a try catch

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% :- import_module require.

foreign_function_uid(mr_function(UID, _)) = UID.

%-----------------------------------------------------------------------------%
