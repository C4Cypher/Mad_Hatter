%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: fnv_hash.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module fnv_hash.

:- interface.

%-----------------------------------------------------------------------------%
% FNV Hash functions

:- func fnv1_hash(string) = uint.
:- pred fnv1_hash(string::in, uint::out) is det.

:- func fnv1a_hash(string) = uint.
:- pred fnv1a_hash(string::in, uint::out) is det.

%-----------------------------------------------------------------------------%
% Hash constants

:- func fnv_prime = uint.

:- func fnv_offset = uint.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module uint.
:- import_module string.


%-----------------------------------------------------------------------------%
% FNV Hash functions

fnv1_hash(String) = Hash :-  fnv1_hash(String, Hash).

fnv1_hash(String, Hash) :-
	length(String, Length),
	fnv1_loop(String, 0, Length, fnv_offset, Hash).
	
	
:- pred fnv1_loop(string::in, int::in, int::in, uint::in, uint::out)
    is det.
	
fnv1_loop(String, Index, Length, !Hash) :- 
	( if Index < Length 
	then
        unsafe_index_code_unit(String, Index, Char),
        !:Hash = !.Hash * fnv_prime,
        !:Hash = !.Hash `xor` det_from_int(Char),
        fnv1_loop(String, Index + 1, Length, !Hash)
    else
        true
    ).
	
%-----------------------------------------------------------------------------%
	
fnv1a_hash(String) = Hash :-  fnv1a_hash(String, Hash).	

fnv1a_hash(String, Hash) :-
	length(String, Length),
	fnv1a_loop(String, 0, Length, fnv_offset, Hash).
:- pred fnv1a_loop(string::in, int::in, int::in, uint::in, uint::out)
    is det.
	
fnv1a_loop(String, Index, Length, !Hash) :- 
	( if Index < Length 
	then
        unsafe_index_code_unit(String, Index, Char),
        !:Hash = !.Hash `xor` det_from_int(Char),
        !:Hash = !.Hash * fnv_prime,
        fnv1a_loop(String, Index + 1, Length, !Hash)
    else
        true
    ).
	

%-----------------------------------------------------------------------------%
% Hash constants

fnv_prime = 
	(if bits_per_uint = 32
	then
		16777619_u
	else
		1099511628211_u	
	).
	
fnv_offset = 
	(if bits_per_uint = 32
	then
		2166136261_u
	else
		14695981039346656037_u
	).