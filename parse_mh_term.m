:- module parse_mh_term.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module mercury_term_parser.
:- import_module term_io.
:- use_module varset.
:- import_module mh_term.
:- import_module mh_mercury_term.
:- import_module string.

main(!IO) :-
	mercury_term_parser.read_term(ReadTerm, !IO),
	(if ReadTerm = term_io.term(_:varset.varset, Term:mr_term)
	then
		io.print(convert_mr_term(Term):mh_term, !IO)
	else
		io.print("Parse error: " ++ string(ReadTerm), !IO)
	).