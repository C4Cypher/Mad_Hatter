%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_value.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_value.

:- interface.

:- import_module bool.

%-----------------------------------------------------------------------------%

:- type mh_value
	--->	mh_value(
		c_pointer::word, 
		char::size, 
		bool::literal, 
		mh_value_format::format
	).
	
:- type mh_value_format
	--->	void	% foreign void *
	;		term	% bytecode term
	;		signed_int
	;		unsigned_int
	;		float
	;		string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

