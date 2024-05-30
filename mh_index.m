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

%-----------------------------------------------------------------------------%

:- typeclass index(T, U) <= (T -> U) where [


	pred valid_index(T, int), % valid_index(Container, Index)
	mode valid_index(in, in) is semidet, % succeed on valid index
	mode valid_index(in, out) is nondet, % return all valid indexes, 
	
	% valid_index(in, out) may produce indexes in any order, but it must not
	% generate duplicate indexes, otherwise default_map/3 may produce
	% undefined behavior.  This requirement allows higher order iterators
	% to execute over valid indexes in place, rather than having to generate
	% a list of solutions first

	pred index(T, int, U),	% index(Container, Index, Value)
	mode index(in, in, out) is semidet, % fail on invalid index
	mode index(in, out, out) is nondet, % all indexed values
	
	pred set_index(int, U, T, T), % set_index(Index, Value, !Container)
	mode set_index(in, in, in, out) is semidet, % fail on invalid index
	mode set_index(out, in, in, out) is nondet, % update any index nondet
	
	pred index_size(T::in, int::out) is det,  % Number of valid indexes
	
	% Index bounds, fail if there are no valid indexes
	pred max_index(T::in, int::out) is semidet,	% highest valid index
	pred min_index(T::in, int::out) is semidet, % lowest valid index
	
	% true if there are no invalid indexes between min and max
	pred consecutive_index(T::in) is semidet,
	
	% Iterate a closure over all values in a container with an accumulator
	% in the same order as index/3
	pred fold_index(pred(U, A, A), T, A, A), % fold_index(Closure, Cont, !Acc)
	mode fold_index(pred(in, in, out) is det, in, in, out) is det,
	
	pred map_index(pred(U, U), T, T), % map_index(Closure, !Container)
	mode map_index(pred(in, out) is det, in, out) is det

].

%-----------------------------------------------------------------------------%
% Index Operation determinism casts

	% Throws an exception if index is out of range.

:- pred det_index(T::in, int::in, U::out) is det <= index(T, U).

:- pred det_set_index(int::in, U::in, T::in, T::out) is det <= index(T, U).

:- pred semidet_valid_index(T::in, int::in) is semidet <= index(T, _).
:- pred nondet_valid_index(T::in, int::out) is nondet <= index(T, _).

:- pred semidet_index(T::in, int::in, U::out) is semidet <= index(T, U).
:- pred nondet_index(T::in, int::out, U::out) is nondet <= index(T, U).

:- pred semidet_set_index(int::in, U::in, T::in, T::out) is semidet 
	<= index(T, U).
:- pred nondet_set_index(int::out, U::in, T::in, T::out) is nondet 
	<= index(T, U).

%-----------------------------------------------------------------------------%
% Function versions of index/2 methods

:- func index(int, T) = U is semidet <= index(T, U).
:- func 'index :='(int, T, U) = T is semidet <= index(T, U).

:- func det_index(int, T) = U <= index(T, U).
:- func 'det_index :='(int, T, U) = T <= index(T, U).

:- func index_size(T) = int <= index(T, _).

:- func max_index(T) = int is semidet <= index(T, _).
:- func min_index(T) = int is semidet <= index(T, _).

:- func fold_index(func(U, A) = A, T, A) = A <= index(T, U).
:- func map_index(func(U) = U, T) = T <= index(T, U).

%-----------------------------------------------------------------------------%
% Validity checks

% Succeeds if min_index(T) =< max_index(T) 
:- pred valid_index_bounds_check(T::in) is semidet <= index(T, _).

% Fails if valid_index produces duplicate indexes
:- pred valid_index_dup_check(T::in) is semidet <= index(T, _). 


%-----------------------------------------------------------------------------%
% Default implementation of index methods

% These default implementations of typeclass index/2 member predicates require
% implementations that iterate over the entire structure using index/3 or
% valid_index/2 resulting in O(n) complexities, some instances
% may have more efficient implementations availible.

:- pred default_size(T::in, int::out) is det <= index(T, _).

:- pred default_max(T::in, int::out) is semidet <= index(T, _).
:- pred default_min(T::in, int::out) is semidet <= index(T, _).

:- pred default_consecutive(T::in) is semidet <= index(T, _).

:- pred default_fold(pred(U, A, A), T, A, A) <= index(T, U).
:- mode default_fold(pred(in, in, out) is det, in, in, out) is det.
	
:- pred default_map(pred(U, U), T, T) <= index(T, U).
:- mode default_map(pred(in, out) is det, 	in, out) is det.


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

det_index(T, I, U) :-
	if valid_index(T, I) then (
		if index(T, I, V) then U = V
		else unexpected($module, $pred, 
			"Index " ++ string(I) ++ 
			" out of range for method call of index(" ++ 
			type_name(type_of(T)) ++ ", " ++ type_name(type_of(U)) ++
			"). mh_index.valid_index/2 should not have allowed this!")
		)
		else unexpected($module, $pred, 
				"Index " ++ string(I) ++ 
				" out of range for method call of index(" ++ 
				type_name(type_of(T)) ++ ", " ++ type_name(type_of(U)) ++
				").").
				
det_set_index(I, U, !T) :-
	if valid_index(!.T, I) then (
		if set_index(I, U, !.T, NewT) 
			then !:T = NewT
			else unexpected($module, $pred, 
				"Index " ++ string(I) ++ 
				" out of range for method call of index(" ++ 
				type_name(type_of(!.T)) ++ ", " ++ type_name(type_of(U)) ++
				"). mh_index.valid_index/2 should not have allowed this!")
		)
		else unexpected($module, $pred, 
				"Index " ++ string(I) ++ 
				" out of range for method call of index(" ++ 
				type_name(type_of(!.T)) ++ ", " ++ type_name(type_of(U)) ++
				").").

semidet_valid_index(T, I) :- valid_index(T, I).
nondet_valid_index(T, I) :- valid_index(T, I).

semidet_index(T, I, U) :- index(T, I, U).
nondet_index(T, I, U) :- index(T, I, U).

semidet_set_index(I, U, !T) :- set_index(I, U, !T).
nondet_set_index(I, U, !T) :- set_index(I, U, !T).


%-----------------------------------------------------------------------------%
% Function versions of index/2 methods

T ^ index(I) = U :- index(T, I, U).

(!.T ^ index(I) := U) = !:T :- set_index(I, U, !T).

T ^ det_index(I) = U :- det_index(T, I, U).

(!.T ^ det_index(I) := U) = !:T :- det_set_index(I, U, !T).

index_size(T) = I :- index_size(T, I).

max_index(T) = M :- max_index(T, M).
min_index(T) = M :- min_index(T, M).


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

valid_index_bounds_check(T) :- min_index(T) =< max_index(T).



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

default_size(T, S) :- promise_equivalent_solutions [S] 
	unsorted_aggregate(
		nondet_valid_index(T),
		aggregate_size, 0, S).

:- pred aggregate_size(int::in, int::in, int::out) is det.

aggregate_size(_, S, S+1).

%-----------------------------------------------------------------------------%


default_max(T, Max) :- 
	M = yes(Max),
	promise_equivalent_solutions [M] 
		unsorted_aggregate(nondet_valid_index(T), aggregate_max, no, M).

:- pred aggregate_max(int::in, maybe(int)::in, maybe(int)::out) is det.

aggregate_max(I, no, yes(I)).
aggregate_max(I, yes(M0), yes(M)) :- I < M0 -> M = I ; M = M0.  

%-----------------------------------------------------------------------------%


default_min(T, Min) :-
	M = yes(Min),
	promise_equivalent_solutions [M]
		unsorted_aggregate(nondet_valid_index(T), aggregate_min, no, M).

:- pred aggregate_min(int::in, maybe(int)::in, maybe(int)::out) is det.

aggregate_min(I, no, yes(I)).
aggregate_min(I, yes(M0), yes(M)) :- I > M0 -> M = I ; M = M0.  

%-----------------------------------------------------------------------------%

default_consecutive(T) :- 
	contiguous(min_index(T), max_index(T), T).

:- pred contiguous(int::in, int::in, T::in) is semidet <= index(T, _).

contiguous(I, Max, T) :- 
	valid_index(T, I),
	(if I < Max then contiguous(I + 1, Max, T) else true).


%-----------------------------------------------------------------------------%

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
	
	