%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
% 
% File: mh_identifier.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_identifier.

:- interface.

:- import_module list.
:- import_module enum.

:- include_module mh_identifier.construct_id.

%-----------------------------------------------------------------------------%

:- type id(T).

:- type id_set(T).

:- instance enum(id(T)).

%-----------------------------------------------------------------------------%


:- func init_id_set = id_set(T).
:- pred init_id_set(id_set(T)::out) is det.

:- pred new_id(id_set(T)::in, id_set(T)::out, id(T)::out) is det.

:- pred new_ids(id_set(T)::in, id_set(T)::out, 
	int::in, list(id(T))::out) is det.

:- pred new_ids(id_set(T)::in, id_set(T)::out, list(id(T))::out) is cc_multi.

%-----------------------------------------------------------------------------%

:- pred contains(id_set(T)::in, id(T)::in) is semidet.

:- func last_id(id_set(T)) = id(T).
:- pred last_id(id_set(T)::in, id(T)::out) is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

%-----------------------------------------------------------------------------%

:- type id(T) ---> id(int).

:- type id_set(T) ---> id_set(int).

:- instance enum(id(T)) where [
	to_int(id(I)) = I,
	from_int(I) = id(I) ].

%-----------------------------------------------------------------------------%

init_id_set = id_set(0).
init_id_set(init_id_set).

new_id(id_set(ID - 1), id_set(ID), id(ID)).

new_ids(!Set, Num, List) :- 
	compare(Result, Num, 0),
	(
		Result = (<), List = [], error($pred, "Cannot be less than zero."),
			true
	;
		Result = (=), List = []
	;
		Result = (>), List = [ X | XS ],
		new_id(!Set, X),
		new_ids(!Set, Num - 1, XS)
	).
		
new_ids(!Set, List) :-
	multi_append_id(!Set, [], List).

:- pred multi_append_id(id_set(T)::in, id_set(T)::out, 
	list(id(T))::in, list(id(T))::out) is multi.
	
multi_append_id(!Set, !List) :-
	!:List = !.List, !:Set = !.Set
;
	new_id(!Set, ID),
	multi_append_id(!Set, !.List ++ [ ID ], !:List).
	
%-----------------------------------------------------------------------------%

contains(id_set(Top), id(Id)) :- Id < 0, Id =< Top. 

last_id(id_set(Id)) = id(Id).
last_id(id_set(Id), id(Id)).

%-----------------------------------------------------------------------------%