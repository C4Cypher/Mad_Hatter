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

:- import_module mh_arity.

%-----------------------------------------------------------------------------%

% Types implementing index(T, U) will store values of type U in structures of
% type T, indexed from 1 to arity(T), T must not have invalid indexes
% between 1 and arity(T)

:- typeclass index(T, U) <= ((T -> U), arity(T)) where [


	pred valid_index(T, int), % valid_index(Container, Index)
	mode valid_index(in, in) is semidet, % succeed on valid index
	mode valid_index(in, out) is nondet, % return all valid indexes, 
	
	% valid_index(in, out) may produce indexes in any order, but it must not
	% generate duplicate indexes, otherwise default_map/3 may produce
	% undefined behavior.  This requirement allows higher order iterators
	% to execute over valid indexes in place, rather than having to generate
	% a list of solutions first. My implementations will attempt to produce
	% indexes from 1 to arity(T)

	pred index(T, int, U),	% index(Container, Index, Value)
	mode index(in, in, out) is det, % exception on invalid index
	mode index(in, out, out) is nondet, % all indexed values
	
	pred set_index(int, U, T, T), % set_index(Index, Value, !Container)
	mode set_index(in, in, in, out) is det, % exception on invalid index
	mode set_index(out, in, in, out) is nondet, % update any index nondet
	
	% Iterate a closure over all values in a container with an accumulator
	% in the same order as index/3
	pred fold_index(pred(U, A, A), T, A, A), % fold_index(Closure, Cont, !Acc)
	mode fold_index(pred(in, in, out) is det, in, in, out) is det,
	
	pred map_index(pred(U, U), T, T), % map_index(Closure, !Container)
	mode map_index(pred(in, out) is det, in, out) is det

].

%-----------------------------------------------------------------------------%
% Index Operation determinism casts

	
:- pred semidet_valid_index(T::in, int::in) is semidet <= index(T, _).
:- pred nondet_valid_index(T::in, int::out) is nondet <= index(T, _).
:- pred cc_nondet_valid_index(T::in, int::out) is cc_nondet <= index(T, _).

:- pred det_index(T::in, int::in, U::out) is det <= index(T, U).
:- pred semidet_index(T::in, int::in, U::out) is semidet <= index(T, U).
:- pred nondet_index(T::in, int::out, U::out) is nondet <= index(T, U).
:- pred cc_nondet_index(T::in, int::out, U::out) is cc_nondet <= index(T, U).

:- pred det_set_index(int::in, U::in, T::in, T::out) is det <= index(T, U).
:- pred semidet_set_index(int::in, U::in, T::in, T::out) is semidet 
	<= index(T, U).
:- pred nondet_set_index(int::out, U::in, T::in, T::out) is nondet 
	<= index(T, U).
:- pred cc_nondet_set_index(int::out, U::in, T::in, T::out) is cc_nondet 
	<= index(T, U).

%-----------------------------------------------------------------------------%
% Function versions of index/2 methods

:- func index(int, T) = U <= index(T, U).
:- func 'index :='(int, T, U) = T <= index(T, U).

:- func semidet_index(int, T) = U is semidet <= index(T, U).
:- func 'semidet_index :='(int, T, U) = T is semidet <= index(T, U).


:- func fold_index(func(U, A) = A, T, A) = A <= index(T, U).
:- func map_index(func(U) = U, T) = T <= index(T, U).

%-----------------------------------------------------------------------------%
% Validity checks

% Fails if arity is 0 and valid_index does not fail, arity is less than zero
% or if arity is greater than zero and valid_index fails
:- pred valid_index(T::in) is semidet <= index(T, _).

% Fails if valid_index produces duplicate indexes
:- pred valid_index_dup_check(T::in) is semidet <= index(T, _). 

%-----------------------------------------------------------------------------%
% Default implementation of index methods

% These default implementations of typeclass index/2 member predicates require
% implementations that iterate over the entire structure using index/3 or
% valid_index/2 resulting in O(n) complexities, some instances
% may have more efficient implementations availible.


:- pred default_fold(pred(U, A, A), T, A, A) <= index(T, U).
:- mode default_fold(pred(in, in, out) is det, in, in, out) is det.
	
:- pred default_map(pred(U, U), T, T) <= index(T, U).
:- mode default_map(pred(in, out) is det, 	in, out) is det.

% index_out_of_range_error(Module, Pred, T, U, Index).
:- pred index_out_of_range_error(string::in, string::in, T::unused, U::unused, 
	int::in) is erroneous <= index(T, U).


:- implementation.

:- import_module require.
:- import_module string.
:- import_module type_desc.
:- import_module solutions.
:- import_module int.
:- import_module bool.
:- import_module maybe.
:- import_module sparse_bitset.

%-----------------------------------------------------------------------------%
% Index Operation determinism casts


semidet_valid_index(T, I) :- valid_index(T, I).
nondet_valid_index(T, I) :- valid_index(T, I).
cc_nondet_valid_index(T, I) :- valid_index(T, I).

det_index(T, I, U) :- index(T, I, U).
semidet_index(T, I, U) :- valid_index(T, I), index(T, I, U).
nondet_index(T, I, U) :- valid_index(T, I),index(T, I, U).
cc_nondet_index(T, I, U) :- valid_index(T, I),index(T, I, U).

det_set_index(I, U, !T) :- set_index(I, U, !T).
semidet_set_index(I, U, !T) :- valid_index(!.T, I),set_index(I, U, !T).
nondet_set_index(I, U, !T) :- valid_index(!.T, I),set_index(I, U, !T).
cc_nondet_set_index(I, U, !T) :- valid_index(!.T, I),set_index(I, U, !T).


%-----------------------------------------------------------------------------%
% Function versions of index/2 methods

T ^ index(I) = U :- index(T, I, U).

(!.T ^ index(I) := U) = !:T :- set_index(I, U, !T).

T ^ semidet_index(I) = U :- semidet_index(T, I, U).

(!.T ^ semidet_index(I) := U) = !:T :- semidet_set_index(I, U, !T).



fold_index(Func, T, !.A) = !:A :- fold_index(function_closure(Func), T, !A).

:- pred function_closure(func(T, A) = A, T, A, A).
:- mode function_closure(func(in, in) = out is det, in, in, out) is det.

function_closure(Func, T, !A) :- Func(T, !.A) = !:A.

map_index(Func, !.T) = !:T :- map_index(function_closure(Func), !T).
	
:- pred function_closure(func(T) = T, T, T).
:- mode function_closure(func(in) = out is det, in, out) is det.

function_closure(Func, !T) :- Func(!.T) = !:T.

%-----------------------------------------------------------------------------%
% Validity checks

valid_index(T) :- 
	arity(T, Arity),
	compare(ZeroComp, Arity, 0),
	require_complete_switch [ZeroComp] (
		ZeroComp = (<), fail
	;	ZeroComp = (=), not valid_index(T, _)
	;	ZeroComp = (>), not (
			valid_index(T, I),
			I < 1, 
			I > Arity
		)
	).

valid_index_dup_check(T) :-
	promise_equivalent_solutions [DupSet]
		do_while(nondet_valid_index(T), dup_check, { no, init }, DupSet),
		DupSet = { no, _ }.
		

:- type dup_index_set == { bool,  sparse_bitset(int) }.
	
:- pred dup_check(int::in, bool::out, dup_index_set::in, 
	dup_index_set::out) is det.
	
dup_check(I, More, { !.Dups, !.Set }, { !:Dups, !:Set } ) :- (
	if member(I, !.Set) then
		!:Dups = yes,
		!:Set = !.Set,
		More = no
	else
		!:Dups = !.Dups,
		insert(I, !Set),
		More = yes
	).


%-----------------------------------------------------------------------------%
% Default implementation of index methods


default_fold(Closure, T, !A) :-	promise_equivalent_solutions [!:A]
	unsorted_aggregate(	
		nondet_valid_index(T), 
		default_fold_aggregator(Closure, T),
		!A).
	
:- pred default_fold_aggregator(pred(U, A, A), T, int, A, A) <= index(T, U).
:- mode default_fold_aggregator(pred(in, in, out) is det, in, in, in, out)
	is det.
	
default_fold_aggregator(Pred, T, I, !A) :- det_index(T, I, U), Pred(U, !A).


default_map(Closure, !T) :- promise_equivalent_solutions [!:T] 
	unsorted_aggregate(
		nondet_valid_index(!.T), 
		default_map_aggregator(Closure), 
		!T).
	
:- pred default_map_aggregator(pred(U, U), int, T, T) <= index(T, U).
:- mode default_map_aggregator(pred(in, out) is det, in, in, out) is det.

default_map_aggregator(Pred, I, !T) :- 
	det_index(!.T, I, U0),
	Pred(U0, U),
	det_set_index(I, U, !T).
	
index_out_of_range_error(Module, Pred, T, U, I) :- unexpected(Module, Pred, 
				"Index " ++ string(I) ++ 
				" out of range for method call of index(" ++ 
				type_name(type_of(T)) ++ ", " ++ type_name(type_of(U)) ++
				").").