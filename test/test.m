:- module test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.


:- import_module string.

:- import_module set.
:- import_module list.
:- import_module solutions.

:- import_module set_util.


main(!IO) :- helloworld(!IO),
	Union = list_to_set([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
	solutions(union_pairs(Union), Pairs),
	io.write_string("power union of " ++ string(Union) ++ " is " ++ 
		string(Pairs), !IO).
	
	
	

:- pred helloworld(io::di, io::uo) is det.


helloworld(!IO) :- io.write_string("Hello World!\n", !IO).

	

:- pred union_pairs(set(T)::in, { set(T), set(T) }::out) is multi.

union_pairs(C, { A, B }) :- nondet_union(A, B, C).