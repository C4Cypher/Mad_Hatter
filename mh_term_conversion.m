%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_term_conversion.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_term_conversion.

:- interface.

:- import_module term.

:- import_module mh_term.

%-----------------------------------------------------------------------------%
% Term conversion result

:- type term_conversion_result
	--->	ok(mh_term)
	;		error(string).
	
%-----------------------------------------------------------------------------%
% Term conversion


:- pred term_to_mh_term(term(T)::in, term_conversion_result::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


%-----------------------------------------------------------------------------%
% Term Conversion


