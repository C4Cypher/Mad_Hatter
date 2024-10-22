%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_value_map.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_value_map.

:- interface.

:- import_module univ.
:- import_module type_desc.

%-----------------------------------------------------------------------------%
% Value maps

:- type mh_value_map(T).

:- func init = (mh_value_map(T)::uo) is det.
:- pred init(mh_value_map(_)::uo) is det.

:- func singleton(K, V) = mh_value_map(V).
:- func singleton_univ(univ, T) = mh_value_map(T).

:- pred is_empty(mh_value_map(_)::in) is semidet.

%-----------------------------------------------------------------------------%
% Search and lookup

:- pred contains(mh_value_map(_T)::in, K::in) is semidet.
:- pred contains_univ(mh_value_map(_T)::in, univ::in) is semidet.
:- pred contains_type(mh_value_map(_T)::in, type_desc::in) is semidet.

% TODO: Function versions of calls?
% TODO: Det versions of semidet calls?

:- pred search(mh_value_map(V)::in, K::in, V::out) is semidet.
:- pred search_univ(mh_value_map(T)::in, univ::in, T::out) is semidet.

:- pred lookup(mh_value_map(V)::in, K::in, V::out) is det.
:- pred lookup_univ(mh_value_map(T)::in, univ::in, T::out) is det.

%TODO: Bound search and min max keys?

%-----------------------------------------------------------------------------%
% Insertions

:- pred insert(K::in, V::in, mh_value_map(V)::in, mh_value_map(V)::out) 
	is semidet.

:- pred insert_univ(univ::in, T::in, mh_value_map(T)::in, mh_value_map(T)::out) 
	is semidet.

:- pred update(K::in, V::in, mh_value_map(V)::in, mh_value_map(V)::out) 
	is semidet.

:- pred update_univ(univ::in, T::in, mh_value_map(T)::in, mh_value_map(T)::out) 
	is semidet.
	
:- pred set(K::in, V::in, mh_value_map(V)::in, mh_value_map(V)::out) is det.
:- pred set_univ(univ::in, T::in, mh_value_map(T)::in, mh_value_map(T)::out)
	is det.

%-----------------------------------------------------------------------------%
% Deletions

:- pred delete(K::in, mh_value_map(_V)::in, mh_value_map(_V)::out) is det.
:- pred delete_univ(univ::in, mh_value_map(_V)::in, mh_value_map(_V)::out) 
	is det.

:- pred remove(K::in, V::out,  mh_value_map(V)::in, mh_value_map(V)::out) is semidet.
:- pred remove_univ(univ::in, V::out, 
	mh_value_map(V)::in, mh_value_map(V)::out) 	is semidet.
	
	% Remove the smallest value of a given type, fail if there are no
	% values of the given type
:- pred remove_smallest_typed(K::out, V::out, 
	mh_value_map(V)::in, mh_value_map(V)::out) 	is semidet.
	
	% Remove values from the map, starting frorm the smallest type in the 
	% standard ordering, removing the smallest value of that type, fail if
	% the map is empty
:- some [K] pred remove_smallest(K::out, V::out, 
	mh_value_map(V)::in, mh_value_map(V)::out) 	is semidet.
	
%-----------------------------------------------------------------------------%
% Nondeterminsitic lookup

:- some [K] pred member(mh_value_map(V)::in, K::out, V::out) is nondet.

:- pred member_univ(mh_value_map(V)::in, univ::out, V::out) is nondet.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module list.
:- import_module require.
:- import_module maybe.

:- import_module mh_util.

%-----------------------------------------------------------------------------%
% Value maps

:- type mh_value_map(T) 
	--->	value_map(map(type_desc, type_map(T))).
	
:- type type_map(T)
	--->	some [U] type_map(map(U, T)).
		
init = value_map(map.init).

init(init).

singleton(K, V) = value_map(map.singleton(Ktype, TypeMap)) :-
	Ktype = type_of(K),
	TypeMap = 'new type_map'(map.singleton(K, V)).
	
singleton_univ(U, V) = value_map(map.singleton(Ktype, TypeMap)) :-
	Ktype = univ_type(U),
	K = univ_value(U),
	TypeMap = 'new type_map'(map.singleton(K, V)).
	
is_empty(value_map(M)) :- map.is_empty(M).



%-----------------------------------------------------------------------------%
% Search and lookup

contains(value_map(VM), K) :-
	Ktype = type_of(K),
	map.search(VM, Ktype, type_map(TM)),
	map.contains(TM, det_dynamic_cast(K)).
	
contains_univ(value_map(VM), U) :-
	Ktype = univ_type(U),
	map.search(VM, Ktype, type_map(TM)),
	det_univ_to_type(U, K),
	map.contains(TM, K).
	
contains_type(value_map(M), T) :- map.contains(M, T).

search(value_map(VM), K, V) :-
	Ktype = type_of(K),
	map.search(VM, Ktype, type_map(TM)),
	map.search(TM, det_dynamic_cast(K), V).
	
search_univ(value_map(VM), U, V) :-
	Ktype = univ_type(U),
	map.search(VM, Ktype, type_map(TM)),
	det_univ_to_type(U, K),
	map.search(TM, K, V).

lookup(value_map(VM), K, V) :-
	Ktype = type_of(K),
	map.lookup(VM, Ktype, type_map(TM)),
	map.lookup(TM, det_dynamic_cast(K), V).
	
lookup_univ(value_map(VM), U, V) :-
	Ktype = univ_type(U),
	map.lookup(VM, Ktype, type_map(TM)),
	det_univ_to_type(U, K),
	map.lookup(TM, K, V).

%-----------------------------------------------------------------------------%
% Insertions 

insert(K, V, value_map(!.VM), value_map(!:VM)) :-
	Ktype = type_of(K),
	( if map.search(!.VM, Ktype, type_map(TM0))
	then
		map.insert(det_dynamic_cast(K), V, TM0, TM),
		map.det_update(Ktype, 'new type_map'(TM), !VM)
	else
		map.det_insert(Ktype, 'new type_map'(map.singleton(K, V)), !VM)
	).
	
insert_univ(U, V, !M) :-
	insert(univ_value(U), V, !M).
	
update(K, V, value_map(!.VM), value_map(!:VM)) :-
	Ktype = type_of(K),
	map.search(!.VM, Ktype, type_map(TM0)),
	map.update(det_dynamic_cast(K), V, TM0, TM),
	map.det_update(Ktype, 'new type_map'(TM), !VM).
	
update_univ(U, V, !M) :-
	update(univ_value(U), V, !M).
	
set(K, V, value_map(!.VM), value_map(!:VM)) :-
	Ktype = type_of(K),
	( if map.search(!.VM, Ktype, type_map(TM0))
	then
		map.set(det_dynamic_cast(K), V, TM0, TM),
		map.det_update(Ktype, 'new type_map'(TM), !VM)
	else
		map.det_insert(Ktype, 'new type_map'(map.singleton(K, V)), !VM)
	).
	
set_univ(U, V, !M) :-
	set(univ_value(U), V, !M).

%-----------------------------------------------------------------------------%
% Deletions

delete(K, value_map(!.VM), value_map(!:VM)) :-
	Ktype = type_of(K),
	( if 
		map.search(!.VM, Ktype, type_map(TM0)), 
		map.remove(det_dynamic_cast(K), _, TM0, TM)
	then 
		( if map.is_empty(TM)
		then map.delete(Ktype, !VM)
		else map.det_update(Ktype, 'new type_map'(TM), !VM)
		)
	else
		!:VM = !.VM
	).
	
delete_univ(U, !M) :-
	delete(univ_value(U), !M).
	
remove(K, V, value_map(!.VM), value_map(!:VM)) :-
	Ktype = type_of(K),
	map.search(!.VM, Ktype, type_map(TM0)),
	map.remove(det_dynamic_cast(K), V, TM0, TM),
	( if map.is_empty(TM)
	then map.delete(Ktype, !VM)
	else map.det_update(Ktype, 'new type_map'(TM), !VM)
	).
	
remove_univ(U, V, !M) :-
	remove(univ_value(U), V,  !M).
	
remove_smallest_typed(K, V, value_map(!.VM), value_map(!:VM)) :-
	Ktype = type_of(K),
	map.search(!.VM, Ktype, type_map(TM0)),
	map.remove_smallest(U, V, TM0, TM),
	det_dynamic_cast(U, K),
	( if map.is_empty(TM)
	then map.delete(Ktype, !VM)
	else map.det_update(Ktype, 'new type_map'(TM), !VM)
	).
	
remove_smallest(K, V, value_map(!.VM), value_map(!:VM)) :-
	Ktype = min_key(!.VM),
	map.search(!.VM, Ktype, type_map(TM0)),
	( if remove_smallest_empty_type_map_check, is_empty(TM0)
	then unexpected($module, $pred, 
		"Empty type map found in value map when attempting ordered removal.")
	else true
	),
	map.remove_smallest(K, V, TM0, TM),	
	( if map.is_empty(TM)
	then map.delete(Ktype, !VM)
	else map.det_update(Ktype, 'new type_map'(TM), !VM)
	).

:- pred remove_smallest_empty_type_map_check is semidet.

:- pragma no_determinism_warning(remove_smallest_empty_type_map_check/0).

remove_smallest_empty_type_map_check :- true.
%-----------------------------------------------------------------------------%
% Nondeterminsitic lookup
	
member(value_map(VM), K, V) :-
	map.member(VM, _, type_map(TM)),
	map.member(TM, K, V).
	
member_univ(VM, U, V) :- member(VM, K,V), type_to_univ(K, U).