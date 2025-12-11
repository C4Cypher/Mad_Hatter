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
% :- import_module fnv_hash.
:- import_module hashable.

%---------------------------------------------------------------------------%

:- instance hashable(int) where [
	hash(I) = cast_from_int(int.hash(I))
].

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    (
	
        Args = [],
        A = "1000000"
        % B = "0.9"
    ;
	/*
        Args = [A],
        % B = "0.9"
    ;
	*/
        Args = [A | _]
    ),
    Max = string.det_to_int(A),
    % MaxOccupancy = string.det_to_float(B),
    some [!HT] (
        !:HT = hashmap.init,

        io.write_string("Inserting elements\n", !IO),
        inst_preserving_fold_up(do_insert, 0, Max - 1, !HT),
        trace [runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),
		

        io.write_string("Looking up elements\n", !IO),
        inst_preserving_fold_up(do_lookup, 0, Max - 1, !HT),
        trace [runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),

        NumOccupants0 = hashmap.count(!.HT),
        ( if NumOccupants0 = Max then
            true
        else
            error("count failed")
        ),

        Half = Max / 2,
        io.write_string("Deleting some elements\n", !IO),
        inst_preserving_fold_up(do_delete, 0, Half - 1, !HT),
        trace [runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),

        NumOccupants = hashmap.count(!.HT),
        ( if NumOccupants = Max - Half then
            true
        else
            error("count failed")
        ),

		/*
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
		*/
		
        _ = !.HT
    ).


    % Or simply hashmap_di, hashmap_uo.
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
    hashmap.delete(I, !HT).

:- pred do_set_neg(int::in, hashmap(int, int)::in,
    hashmap(int, int)::out) is det.

do_set_neg(I, !HT) :-
    hashmap.set(I, -I, !HT).
