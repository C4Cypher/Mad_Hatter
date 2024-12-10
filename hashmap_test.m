%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module hashmap_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module benchmarking.
:- import_module int.
:- import_module uint.
:- import_module list.
:- import_module require.
:- import_module string.

:- import_module hashmap.
:- import_module hashable.

%---------------------------------------------------------------------------%

:- instance hashable(int) where [
	hash(I) = cast_from_int(int.hash(I))
].

/*
:- pred print_key_stats(T::in, int::in, uint::in, uint::out, io::di, io::uo) 
	is det <= hashable(T).

print_key_stats(T, S, !B, !IO) :-
	io.write_string("\n\nKey value: " ++ string(T), !IO),
	Hash:uint = hashable.hash(T),
	io.write_string("\nHash: " ++ string(Hash), !IO),
	index(Hash, S, Index),
	io.write_string("\nIndex at shift " ++ string(S) ++ ": " ++ string(Index), 
		!IO),
	mask(Hash, S, Mask),
	io.write_string("\nMask at shift " ++ string(S) ++ ": " ++ string(Mask),
		!IO),
	MaskBinary = int_to_base_string(cast_to_int(Mask), 2),
	io.write_string("\nMask in binary: " ++ MaskBinary, !IO),
	!:B = !.B \/ Mask,
	io.write_string("\nNew Bitmap: " ++ string(!.B), !IO),
	BitmapBinary = int_to_base_string(cast_to_int(!.B), 2),
	io.write_string("\nBitmap in binary: " ++ BitmapBinary, !IO),
	io.write_string("\nBitmap weight: " ++ string(weight(!.B)), !IO),
	io.write_string("\nSparse index: " ++ string(sparse_index(!.B, Mask):int), 
		!IO),
	Mminus1Binary:string = int_to_base_string(cast_to_int(Mask - 1u), 2),
	BandMminus1 = !.B /\ (Mask - 1u),
	io.write_string("\nB /\\ (M - 1) = " ++ string(BandMminus1:uint), !IO).
*/

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    (
        Args = [],
        A = "1000000"
        % B = "0.9"
    ;/*
        Args = [A],
        B = "0.9"
    ;*/
        Args = [A | _]
    ),
    Max = string.det_to_int(A),
    % MaxOccupancy = string.det_to_float(B),
	
	/*
	io.write_string("Subkey mask in bin: " ++ 
		int_to_base_string(cast_to_int(subkey_mask), 2), !IO),
	*/
	
	/*
	some [!B] (
		!.B = 0u,
		print_key_stats(0, 0, !B, !IO),
		print_key_stats(1, 0, !B, !IO),
		print_key_stats(2, 0, !B, !IO)
	),
	*/
	
	/*
	some [!HM] (
		!:HM = hashmap.init,
		det_insert(0, 0, !HM),
		det_insert(1, 1, !HM),
		det_insert(2, 2, !HM),
		io.write_string("\nHash table with 0, 1, 2 keys and values:\n" ++ 
			string(!.HM), !IO),
			
		Bitmap = 1130496u,
		io.write_string("\nIndexed Branch Bitmap: " ++ string(Bitmap), !IO),
		io.write_string("\nBitmap in binary: " ++
			int_to_base_string(cast_to_int(Bitmap), 2), !IO),
		io.write_string("\nBitmap weight:" ++ string(weight(Bitmap)), !IO),
		io.write_string("\nKey 2 Mask: " ++ 
			string(Mask2@mask(Hash2@hash(2):uint, 0):uint), !IO)
		
	),
	*/
    some [!HT] (
        !:HT = hashmap.init,

        io.write_string("\n\nInserting elements\n", !IO),
        inst_preserving_fold_up(do_insert, 0, Max - 1, !HT),
        trace [ runtime(env("HASH_TABLE_STATS")) ] (
            impure report_stats
        ),
		
		FullMap = !.HT,

        io.write_string("Looking up elements\n", !IO),
        inst_preserving_fold_up(do_lookup, 0, Max - 1, !HT),
        trace [ runtime(env("HASH_TABLE_STATS")) ] (
            impure report_stats
        ),

        NumOccupants0 = hashmap.count(!.HT),
        ( if NumOccupants0 = Max then
            true
        else
            error("count failed Occupants: " ++ string(NumOccupants0)
				++ " Max: " ++ string(Max))
        ),

        Half = Max / 2,
        io.write_string("Deleting some elements\n", !IO),
        inst_preserving_fold_up(do_delete, 0, Half - 1, !HT),
        trace [ runtime(env("HASH_TABLE_STATS")) ] (
            impure report_stats
        ),
		
        NumOccupants = hashmap.count(!.HT),
        ( if NumOccupants = Max - Half then
            true
        else
            error("count failed Occupants: " ++ string(NumOccupants)
				++ " Max - Half: " ++ string(Max))
        ),
		
		HalfMap = !.HT,
		
		io.write_string("Converting to assoc list\n", !IO),
        AL = hashmap.to_assoc_list(!.HT),
        ( if list.length(AL) = NumOccupants then
            true
        else
            error("to_assoc_list failed")
        ),

        io.write_string("Setting negative elements\n", !IO),
        inst_preserving_fold_up(do_set_neg, 0, Max - 1, !HT),
        trace [runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),

        io.write_string("Looking up negative elements\n", !IO),
        inst_preserving_fold_up(do_lookup_neg, 0, Max - 1, !HT),
        trace [runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),
		
		
		
		io.write_string("Differencee between full map and half map\n", !IO),
		difference(FullMap, HalfMap, DiffMap),
		trace [runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),
		
		io.write_string("Intersection between half Map and diff map\n", !IO),
		intersect(set_merge_pred, HalfMap, DiffMap, DiffInt),
		trace [runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),
		
		io.write_string("Equality test beetween intersect and empty map\n", !IO),
		(if is_empty(DiffInt) then
			true
		else
			error("Intersection between Half map and difference map not empty\n")
		),
		
		io.write_string("Intersection between half Map and full map\n", !IO),
		intersect(set_merge_pred, FullMap, HalfMap, Intersection),
		trace [runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),
		
		io.write_string("Equality test beetween intersect and half map\n", !IO),
		(if equal(Intersection, HalfMap) then
			true
		else
			error("Equality test of intersection and half map failed\n")
		),
		
		
		io.write_string("merge test of Half map and Difference map\n", !IO),
		merge(HalfMap, DiffMap, MergeMap),
		trace [runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),
		
		io.write_string("Equality test beetween full map and merged map\n", !IO),
		(if equal(FullMap, MergeMap) then
			true
		else
			error("Equality test of full map and merge map failed\n")
		),
		
		io.write_string("Union test of Half map and full map\n", !IO),
		union(set_merge_pred, HalfMap, FullMap, UnionMap),
		trace [runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),
		
		io.write_string("Equality test beetween full map and union map\n", !IO),
		(if equal(FullMap, UnionMap) then
			true
		else
			error("Equality test of full map and union map failed\n")
		),
		
        _ = !.HT
    ).

    % Or simply in, out.
:- pred inst_preserving_fold_up(pred(int, T, T), int, int, T, T).
:- mode inst_preserving_fold_up(pred(in, di(I), out(I)) is det,
    in, in, di(I), out(I)) is det.

inst_preserving_fold_up(P, Lo, Hi, !A) :-
    ( if Lo =< Hi then
        P(Lo, !A),
        inst_preserving_fold_up(P, Lo + 1, Hi, !A)
    else
        true
    ).

:- pred do_insert(int::in, hashmap(int, int)::in,
    hashmap(int, int)::out) is det.

do_insert(I, !HT) :-
    (if hashmap.insert(I, I, !HT)
	then
		!:HT = !.HT
	else
		error("do_insert failed")
	).

:- pred do_lookup(int::in, hashmap(int, int)::in,
    hashmap(int, int)::out) is det.

do_lookup(I, !HT) :-
    V = hashmap.lookup(!.HT, I),
    ( if I = V then
        true
    else
        error("do_lookup failed")
    ).

:- pred do_lookup_neg(int::in, hashmap(int, int)::in,
    hashmap(int, int)::out) is det.

do_lookup_neg(I, !HT) :-
    V = hashmap.lookup(!.HT, I),
    ( if -I = V then
        true
    else
        error("do_lookup_neg failed")
    ).

:- pred do_delete(int::in, hashmap(int, int)::in,
    hashmap(int, int)::out) is det.

do_delete(I, !HT) :-
	% Before = count(!.HT),
    hashmap.delete(I, !HT).
	
	/*
	After = count(!.HT),
	(if After = Before - 1
	then
		true
	else
		error("hashmap.delete failed to delete element " ++ string(I) ++ 
			" Count before: " ++ string(Before) ++ " Count after: " ++ 
			string(After)) 
	).		
	*/
			
	

:- pred do_set_neg(int::in, hashmap(int, int)::in,
    hashmap(int, int)::out) is det.

do_set_neg(I, !HT) :-
    hashmap.set(I, -I, !HT).
	
:- pred set_merge_pred(T::in, T::in, T::out) is det.
set_merge_pred(T, _, T).


