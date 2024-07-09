%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_value.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_value.

:- interface.

:- import_module char.
:- import_module type_desc.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Mad Hatter values

:- type mh_value
	--->	some [T] mr_value(T)
	;		mh_int(int)
	;		mh_int8(int8)
	;		mh_int16(int16)
	;		mh_int32(int32)
	;		mh_int64(int64)
	;		mh_uint(uint)
	;		mh_uint8(int8)
	;		mh_uint16(int16)
	;		mh_uint32(int32)
	;		mh_uint64(int64)
	;		mh_float(float)
	;		mh_char(char)
	;		mh_string(string).
	
% :- func value_type_desc(mh_value) = type_desc.

% Todo value/2 pred and value/1 function

	
%-----------------------------------------------------------------------------%
% Mercury values (boxed)

:- type mr_value =< mh_value
	--->	some [T] mr_value(T).

:- pred mercury_value(T, mr_value).
:- mode mercury_value(in, out) is det.
:- mode mercury_value(out, in) is semidet.

:- func mercury_value(T) = mr_value.
:- mode mercury_value(in) = out is det.
:- mode mercury_value(out) = in is semidet.
	
%-----------------------------------------------------------------------------%
% Primitive values

:- type primitive_value =< mh_value
	--->	mh_int(int)
	;		mh_int8(int8)
	;		mh_int16(int16)
	;		mh_int32(int32)
	;		mh_int64(int64)
	;		mh_uint(uint)
	;		mh_uint8(int8)
	;		mh_uint16(int16)
	;		mh_uint32(int32)
	;		mh_uint64(int64)
	;		mh_float(float)
	;		mh_char(char)
	;		mh_string(string).
	
:- inst primitive_value
	--->	mh_int(ground)
	;		mh_int8(ground)
	;		mh_int16(ground)
	;		mh_int32(ground)
	;		mh_int64(ground)
	;		mh_uint(ground)
	;		mh_uint8(ground)
	;		mh_uint16(ground)
	;		mh_uint32(ground)
	;		mh_uint64(ground)
	;		mh_float(ground)
	;		mh_char(ground)
	;		mh_string(ground).
	
:- pred primitive_value(mh_value::(ground >> primitive_value)) is semidet.

:- func type_to_primitive(T) = primitive_value is semidet.

% :- pred primitive(T, primitive_value).
% :- mode primitive(in, out) is semidet.
% :- mode primitive(out, in) is det.

%-----------------------------------------------------------------------------%
% Value conversion

/*
:- some [T] pred value(T, mh_value).
:- mode value(in, out) is det.
:- mode value(out, in) is semidet.
*/


%-----------------------------------------------------------------------------%
% Equality



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- import_module int.
:- import_module int8.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module uint.
:- import_module uint8.
:- import_module uint16.




%-----------------------------------------------------------------------------%
% Mad Hatter values

%-----------------------------------------------------------------------------%
% Mercury values (boxed)

:- pragma promise_equivalent_clauses(mercury_value/2).


mercury_value(T::in, 'new mr_value'(T)::out).

mercury_value(T::out, mr_value(U)::in) :-
	dynamic_cast(U, T).
	
mercury_value(T) = V :- mercury_value(T, V).

%-----------------------------------------------------------------------------%
% Primitive values

primitive_value(T) :-
	T = mh_int(_);
	T = mh_int8(_);
	T = mh_int16(_);
	T = mh_int32(_);
	T = mh_int64(_);
	T = mh_uint(_);
	T = mh_uint8(_);
	T = mh_uint16(_);
	T = mh_uint32(_);
	T = mh_uint64(_);
	T = mh_float(_);
	T = mh_char(_);
	T = mh_string(_).
	
type_to_primitive(T) = V :-
	promise_equivalent_solutions [V] (	
		dynamic_cast(T, U), V = mh_int(U);
		dynamic_cast(T, U), V = mh_int8(U);
		dynamic_cast(T, U), V = mh_int16(U);
		dynamic_cast(T, U), V = mh_int32(U);
		dynamic_cast(T, U), V = mh_int64(U);
		dynamic_cast(T, U), V = mh_uint(U);
		dynamic_cast(T, U), V = mh_uint8(U);
		dynamic_cast(T, U), V = mh_uint16(U);
		dynamic_cast(T, U), V = mh_uint32(U);
		dynamic_cast(T, U), V = mh_uint64(U);
		dynamic_cast(T, U), V = mh_float(U);
		dynamic_cast(T, U), V = mh_char(U);
		dynamic_cast(T, U), V = mh_string(U)
	).