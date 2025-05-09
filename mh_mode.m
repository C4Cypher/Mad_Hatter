%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_mode.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_mode.

:- interface.

:- import_module array.

:- import_module mh_term.
:- import_module mh_tuple.

%-----------------------------------------------------------------------------%
% Modes

:- type mh_mode
	--->	mh_term >> mh_term
	% 		in(T) == T >> T	
	;		in(mh_term)
	% 		out(T) == free >> T
	;		out(mh_term)
	%		binds(T, U) == T >> (T , U)
	;		binds(mh_term, mh_term)
	%		compound_mode({ in(A), out(B) }) == { A, free } >> { A, B }
	;		compound_mode(array(mh_mode)).
	
%-----------------------------------------------------------------------------%
% Contracts

:- inst mh_contract ---> ground >> ground.

:- type mh_contract =< mh_mode
	---> 	mh_term >> mh_term.
	
:- mode is_contract == mh_mode >> mh_contract.
:- pred is_contract(mh_mode::is_contract) is semidet.
	
	
:- pred mode_contract(mh_mode, mh_contract).
:- mode mode_contract(in, out) is det.
:- mode mode_contract(out, in) is multi.






% Unify modes
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.



%-----------------------------------------------------------------------------%
% Contracts

is_contract(_ >> _).