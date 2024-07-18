%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2023 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_var.m
% Main author: C4Cypher.
% Stability: low.

:- module mh_var.

:- interface.

:- import_module list.
:- import_module array.

%-----------------------------------------------------------------------------%
% Variable ID's

:- type var_id.

:- type var_ids == list(var_id).

:- pred valid_var_id(var_id::in) is semidet.

:- pred require_valid_var_id(var_id::in) is det.

:- pred expect_valid_var_id(var_id::in, string::in, string::in) is det.


%-----------------------------------------------------------------------------%
% Indexing variable ID's

% :- pred 

%-----------------------------------------------------------------------------%
% Variable sets

:- type mh_varset.

:- func init = mh_varset.

:- pred init(mh_varset::out) is det.

:- pred valid_varset(mh_varset::in) is semidet.

:- pred require_valid_varset(mh_varset::in) is det.

:- pred expect_valid_varset(mh_varset::in, string::in, string::in) is det.

%-----------------------------------------------------------------------------%
% Operations on var_ids and var_sets

:- pred new_var_id(var_id::out, mh_varset::in, mh_varset::out) is det.

:- pred new_var_ids(var_ids::out, mh_varset::in, mh_varset::out) is cc_multi.

:- pred new_var_ids(int::in, var_ids::out, mh_varset::in, mh_varset::out) 
	is det.

:- pred contains_var_id(mh_varset, var_id).
:- mode contains_var_id(in, in) is semidet.
:- mode contains_var_id(in, out) is nondet.

%-----------------------------------------------------------------------------%
% Indexing Arrays by var_id 

:- pred var_id_in_bounds(array(_T)::in, var_id::in) is semidet.

:- pred var_id_lookup(array(T)::in, var_id::in, T::out) is det.

:- func var_id_lookup(array(T), var_id) = T.

:- func var_id_elem(var_id, array(T)) = T.

:- pred var_id_set(var_id::in, T::in, array(T)::in, array(T)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module string.

:- import_module mh_index.

%-----------------------------------------------------------------------------%
% Variable ID's

:- type var_id == int.

valid_var_id(I) :- I > 0.

require_valid_var_id(I) :- require(valid_var_id(I), "Invalid var_id:" ++
	string(I) ++ " less than zero.").
	
expect_valid_var_id(I, Module, Proc) :- expect(valid_var_id(I), Module, Proc,
	"Invalid var_id:" ++ string(I) ++ " less than zero.").

%-----------------------------------------------------------------------------%
% Variable sets

:- type mh_varset ---> var_set(last_id::var_id).

init = var_set(0).

init(init).

require_valid_varset(Set) :- 
	require(valid_varset(Set), "Invalid varset. Last var_id: " ++ string(Set) 
	++	" was less than zero.").

expect_valid_varset(Set, Module, Proc) :- 
	expect(valid_varset(Set), Module, Proc, "Invalid varset. Last var_id: " 
	++ string(Set) ++ " was less than zero.").

%-----------------------------------------------------------------------------%
% Operations on var_ids and var_sets



new_var_id(ID, var_set(ID - 1), var_set(ID)).

new_var_ids([], !Set).

new_var_ids([ ID | IDs ], !Set) :-
	new_var_id(ID, !Set), 
	(
		IDs = [], !:Set = !.Set
	;
		new_var_ids(IDs, !Set)
	).

new_var_ids(Number, List, !Set) :-
	compare(NumCmp, Number, 0), require_complete_switch [NumCmp] (
		NumCmp = (<), unexpected($module, $pred, 
			"Cannot generate negative sized list of variable ids.") ;
		NumCmp = (=), List = [] ;
		NumCmp = (>), ( 
			new_var_id(ID, !Set),
			new_var_ids(Number - 1, IDs, !Set),
			List = [ ID | IDs ]
		)
	).

:- pragma promise_equivalent_clauses(contains_var_id/2).

contains_var_id(var_set(Last)::in, ID::in) :- 
	expect_valid_var_id(ID, $module, $pred),
	ID =< Last.
	
contains_var_id(var_set(Last)::in, ID::out) :-
	expect_valid_var_id(ID, $module, $pred),
	Last > 0,
	all_ids_to(1, Last, ID).
	

:- pred all_ids_to(var_id::in, var_id::in, var_id::out) is multi.

all_ids_to(First, Last, This) :-
	( if First = Last
		then This = Last
		else (
			This = First ;
			all_ids_to(First + 1, Last, This)
		)
	).

valid_varset(var_set(Last)) :- Last >= 0.

%-----------------------------------------------------------------------------%
% Indexing Arrays by var_id

var_id_in_bounds(Array, ID) :- in_bounds(Array, ID - 1).

var_id_lookup(Array, ID, T) :- lookup(Array, ID - 1, T).

var_id_lookup(Array, ID) = lookup(Array, ID - 1).

var_id_elem(ID, Array) = elem(ID - 1, Array).
