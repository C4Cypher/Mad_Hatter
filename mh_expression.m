%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2024 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mh_expression.m
% Main author: C4Cypher.
% Stability: low.
%-----------------------------------------------------------------------------%

:- module mh_expression.

:- interface.

:- import_module mh_term.

:- import_module list.

:- type expression
	--->	predicate(functor)
	;		negated_predicate(functor)

	;		mh_true
	;		mh_false

	;		conjunction(list(logical_expression))
	; 		and(logical_expression, logical_expression)

	;		disjunction(list(logical_expression))
	;		or(logical_expression, logical_expression)

	;		negation(logical_expression)

	;		xor(logical_expression, logical_expression)
	;		implication(logical_expression, logical_expression)
	;		iff(logical_expression, logical_expression)

	;		equal(expression, expression)
	;		inequal(expression, expression)
	;		greater_than(term_expression, term_expression)
	;		greater_than_or_equal(term_expression, term_expression)
	;		less_than(term_expression, term_expression)
	;		less_than_or_equal(term_expression, term_expression)

	;		term(mh_term) 

	;		sum(list(numeric_expression))
	;		add(numeric_expression, numeric_expression)
	;		subtract(numeric_expression, numeric_expression)

	;		product(list(numeric_expression))
	;		multiply(numeric_expression, numeric_expression)
	;		divide(numeric_expression, numeric_expression).


%-----------------------------------------------------------------------------%

:- inst logical_expression
	--->	predicate(ground)
	;		negated_predicate(ground)

	;		mh_true
	;		mh_false

	;		conjunction(ground)
	; 		and(ground, ground)

	;		disjunction(ground)
	;		or(ground, ground)

	;		negation(ground)

	;		xor(ground, ground)
	;		implication(ground, ground)
	;		iff(ground, ground)

	;		equal(ground, ground)
	;		inequal(ground, ground)
	;		greater_than(ground, ground)
	;		greater_than_or_equal(ground, ground)
	;		less_than(ground, ground)
	;		less_than_or_equal(ground, ground).

:- inst logical_expression(I)
	--->	predicate(I)
	;		negated_predicate(I)

	;		mh_true
	;		mh_false

	;		conjunction(I)
	; 		and(I, I)

	;		disjunction(I)
	;		or(I, I)

	;		negation(I)

	;		xor(I, I)
	;		implication(I, I)
	;		iff(I, I)

	;		equal(I, I)
	;		inequal(I, I)
	;		greater_than(I, I)
	;		greater_than_or_equal(I, I)
	;		less_than(I, I)
	;		less_than_or_equal(I, I).

:- type logical_expression =< expression
	--->	predicate(functor)
	;		negated_predicate(functor)

	;		mh_true
	;		mh_false

	;		conjunction(list(logical_expression))
	; 		and(logical_expression, logical_expression)

	;		disjunction(list(logical_expression))
	;		or(logical_expression, logical_expression)

	;		negation(logical_expression)

	;		xor(logical_expression, logical_expression)
	;		implication(logical_expression, logical_expression)
	;		iff(logical_expression, logical_expression)

	;		equal(expression, expression)
	;		inequal(expression, expression)
	;		greater_than(term_expression, term_expression)
	;		greater_than_or_equal(term_expression, term_expression)
	;		less_than(term_expression, term_expression)
	;		less_than_or_equal(term_expression, term_expression).

:- pred is_logical_expression(expression::in) is semidet.

:- func to_logical_expression(expression) = logical_expression is semidet.

:- pred coerce_logical_expression(expression, logical_expression).
:- mode coerce_logical_expression(in, out) is semidet.
:- mode coerce_logical_expression(out, in) is det.

:- func coerce_logical_expression(expression) = logical_expression.
:- mode coerce_logical_expression(in) = out is semidet.
:- mode coerce_logical_expression(out) = in is det.

%-----------------------------------------------------------------------------%

:- inst atom 	---> predicate(ground).
:- inst atom(I)	---> predicate(I).

:- type atom =< logical_expression ---> predicate(functor).

:- inst literal 
	--->	predicate(ground)
	;		negated_predicate(ground).

:- inst literal(I)
	--->	predicate(I)
	;		negated_predicate(I).

:- type literal =< logical_expression
	--->	predicate(functor)
	;		negated_predicate(functor).



%-----------------------------------------------------------------------------%

:- inst negation ---> negation(ground).

:- inst negation(I) ---> negation(I).

:- inst not_negation
	--->	predicate(ground)
	;		negated_predicate(ground)

	;		mh_true
	;		mh_false

	;		conjunction(ground)
	; 		and(ground, ground)

	;		disjunction(ground)
	;		or(ground, ground)

	;		xor(ground, ground)
	;		implication(ground, ground)
	;		iff(ground, ground)

	;		equal(ground, ground)
	;		inequal(ground, ground)
	;		greater_than(ground, ground)
	;		greater_than_or_equal(ground, ground)
	;		less_than(ground, ground)
	;		less_than_or_equal(ground, ground)

	;		term(ground) 

	;		sum(ground)
	;		add(ground, ground)
	;		subtract(ground, ground)

	;		product(ground)
	;		multiply(ground, ground)
	;		divide(ground, ground).

:- type negation =< logical_expression ---> negation(logical_expression).

:- pred is_negation(expression::in) is semidet.

:- func to_negation(expression) = negation is semidet.

:- pred coerce_negation(expression, negation).
:- mode coerce_negation(in, out) is semidet.
:- mode coerce_negation(out, in) is det.

:- func coerce_negation(expression) = negation.
:- mode coerce_negation(in) = out is semidet.
:- mode coerce_negation(out) = in is det.


%-----------------------------------------------------------------------------%

:- inst conjunction 
	 --->		conjunction(ground)
	; 			and(ground, ground).

:- inst conjunction(I)
	--->		conjunction(list(I))
	;			and(I, I).

:- inst not_conjunction
	--->	predicate(ground)
	;		negated_predicate(ground)

	;		mh_true
	;		mh_false

	;		disjunction(ground)
	;		or(ground, ground)

	;		negation(ground)

	;		xor(ground, ground)
	;		implication(ground, ground)
	;		iff(ground, ground)

	;		equal(ground, ground)
	;		inequal(ground, ground)
	;		greater_than(ground, ground)
	;		greater_than_or_equal(ground, ground)
	;		less_than(ground, ground)
	;		less_than_or_equal(ground, ground)

	;		term(ground) 

	;		sum(ground)
	;		add(ground, ground)
	;		subtract(ground, ground)

	;		product(ground)
	;		multiply(ground, ground)
	;		divide(ground, ground).

:- type conjunction =< logical_expression
	--->		conjunction(list(logical_expression))
	; 			and(logical_expression, logical_expression).

:- pred is_conjunction(expression::in) is semidet.

:- func to_conjunction(expression) = conjunction is semidet.

:- pred coerce_conjunction(expression, conjunction).
:- mode coerce_conjunction(in, out) is semidet.
:- mode coerce_conjunction(out, in) is det.

:- func coerce_conjunction(expression) = conjunction.
:- mode coerce_conjunction(in) = out is semidet.
:- mode coerce_conjunction(out) = in is det.


%-----------------------------------------------------------------------------%
:- inst disjunction 
	 --->		disjunction(ground)
	; 			or(ground, ground).

:- inst disjunction(I)
	 --->		disjunction(list(I))
	; 			or(I, I).

:- inst not_disjunction
	--->	predicate(ground)
	;		negated_predicate(ground)

	;		mh_true
	;		mh_false

	;		conjunction(ground)
	; 		and(ground, ground)

	;		negation(ground)

	;		xor(ground, ground)
	;		implication(ground, ground)
	;		iff(ground, ground)

	;		equal(ground, ground)
	;		inequal(ground, ground)
	;		greater_than(ground, ground)
	;		greater_than_or_equal(ground, ground)
	;		less_than(ground, ground)
	;		less_than_or_equal(ground, ground)

	;		term(ground) 

	;		sum(ground)
	;		add(ground, ground)
	;		subtract(ground, ground)

	;		product(ground)
	;		multiply(ground, ground)
	;		divide(ground, ground).

:- type disjunction =< logical_expression
	--->		disjunction(list(logical_expression))
	; 			or(logical_expression, logical_expression).

:- pred is_disjunction(expression::in) is semidet.

:- func to_disjunction(expression) = disjunction is semidet.

:- pred coerce_disjunction(expression, disjunction).
:- mode coerce_disjunction(in, out) is semidet.
:- mode coerce_disjunction(out, in) is det.

:- func coerce_disjunction(expression) = disjunction.
:- mode coerce_disjunction(in) = out is semidet.
:- mode coerce_disjunction(out) = in is det.


%-----------------------------------------------------------------------------%

% Term expression is the complement of logical_expression

:- inst term_expression  
	--->	term(ground) 

	;		sum(ground)
	;		add(ground, ground)
	;		subtract(ground, ground)

	;		product(ground)
	;		multiply(ground, ground)
	;		divide(ground, ground).

:- inst term_expression(I)
	--->	term(I) 

	;		sum(list(I))
	;		add(I, I)
	;		subtract(I, I)

	;		product(list(I))
	;		multiply(I, I)
	;		divide(I, I).

:- type term_expression =< expression
	--->	term(mh_term) 

	;		sum(list(numeric_expression))
	;		add(numeric_expression, numeric_expression)
	;		subtract(numeric_expression, numeric_expression)

	;		product(list(numeric_expression))
	;		multiply(numeric_expression, numeric_expression)
	;		divide(numeric_expression, numeric_expression).

:- pred is_term_expression(expression::in) is semidet.

:- func to_term_expression(expression) = term_expression is semidet.

:- pred coerce_term_expression(expression, term_expression).
:- mode coerce_term_expression(in, out) is semidet.
:- mode coerce_term_expression(out, in) is det.

:- func coerce_term_expression(expression) = term_expression.
:- mode coerce_term_expression(in) = out is semidet.
:- mode coerce_term_expression(out) = in is det.

%-----------------------------------------------------------------------------%


:- inst numeric_expression
	--->	term(numeric_term) 

	;		sum(ground)
	;		add(ground, ground)
	;		subtract(ground, ground)

	;		product(ground)
	;		multiply(ground, ground)
	;		divide(ground, ground). 

:- inst numeric_expression(I)
	--->	term(numeric_term(I)) 

	;		sum(I)
	;		add(I, I)
	;		subtract(I, I)

	;		product(I)
	;		multiply(I, I)
	;		divide(I, I). 

:- inst not_numeric_expression
	--->	predicate(ground)
	;		negated_predicate(ground)

	;		mh_true
	;		mh_false

	;		conjunction(ground)
	; 		and(ground, ground)

	;		disjunction(ground)
	;		or(ground, ground)

	;		negation(ground)

	;		xor(ground, ground)
	;		implication(ground, ground)
	;		iff(ground, ground)

	;		equal(ground, ground)
	;		inequal(ground, ground)
	;		greater_than(ground, ground)
	;		greater_than_or_equal(ground, ground)
	;		less_than(ground, ground)
	;		less_than_or_equal(ground, ground)

	; 		term(not_numeric_term).

:- type numeric_expression =< term_expression
	--->	term(numeric_term) 

	;		sum(list(numeric_expression))
	;		add(numeric_expression, numeric_expression)
	;		subtract(numeric_expression, numeric_expression)

	;		product(list(numeric_expression))
	;		multiply(numeric_expression, numeric_expression)
	;		divide(numeric_expression, numeric_expression). 

:- pred is_numeric_expression(expression::in) is semidet.

:- func to_numeric_expression(expression) = numeric_expression is semidet.

:- pred coerce_numeric_expression(expression, numeric_expression).
:- mode coerce_numeric_expression(in, out) is semidet.
:- mode coerce_numeric_expression(out, in) is det.

:- func coerce_numeric_expression(expression) = numeric_expression.
:- mode coerce_numeric_expression(in) = out is semidet.
:- mode coerce_numeric_expression(out) = in is det.
%-----------------------------------------------------------------------------%

:- inst addition 
	 --->		sum(ground)
	; 			add(ground, ground).

:- inst not_addition
	--->	predicate(ground)
	;		negated_predicate(ground)

	;		mh_true
	;		mh_false

	;		conjunction(ground)
	; 		and(ground, ground)

	;		disjunction(ground)
	;		or(ground, ground)

	;		negation(ground)

	;		xor(ground, ground)
	;		implication(ground, ground)
	;		iff(ground, ground)

	;		equal(ground, ground)
	;		inequal(ground, ground)
	;		greater_than(ground, ground)
	;		greater_than_or_equal(ground, ground)
	;		less_than(ground, ground)
	;		less_than_or_equal(ground, ground)

	;		term(ground) 

	;		subtract(ground, ground)

	;		product(ground)
	;		multiply(ground, ground)
	;		divide(ground, ground).

:- type addition =< numeric_expression
	--->	sum(list(numeric_expression))
	; 		add(numeric_expression, numeric_expression).

:- pred is_addition(expression::in) is semidet.

:- func to_addition(expression) = addition is semidet.

:- pred coerce_addition(expression, addition).
:- mode coerce_addition(in, out) is semidet.
:- mode coerce_addition(out, in) is det.

:- func coerce_addition(expression) = addition.
:- mode coerce_addition(in) = out is semidet.
:- mode coerce_addition(out) = in is det.




%-----------------------------------------------------------------------------%

:- inst multiplication 
	--->	product(ground)
	;		multiply(ground, ground).

:- inst not_multiplication
	--->	predicate(ground)
	;		negated_predicate(ground)

	;		mh_true
	;		mh_false

	;		conjunction(ground)
	; 		and(ground, ground)

	;		disjunction(ground)
	;		or(ground, ground)

	;		negation(ground)

	;		xor(ground, ground)
	;		implication(ground, ground)
	;		iff(ground, ground)

	;		equal(ground, ground)
	;		inequal(ground, ground)
	;		greater_than(ground, ground)
	;		greater_than_or_equal(ground, ground)
	;		less_than(ground, ground)
	;		less_than_or_equal(ground, ground)

	;		term(ground) 

	;		sum(ground)
	;		add(ground, ground)
	;		subtract(ground, ground)

	;		divide(ground, ground).

:- type multiplication =< term_expression
	--->	product(list(numeric_expression))
	;		multiply(numeric_expression, numeric_expression).

:- pred is_multiplication(expression::in) is semidet.

:- func to_multiplication(expression) = multiplication is semidet.

:- pred coerce_multiplication(expression, multiplication).
:- mode coerce_multiplication(in, out) is semidet.
:- mode coerce_multiplication(out, in) is det.

:- func coerce_multiplication(expression) = multiplication.
:- mode coerce_multiplication(in) = out is semidet.
:- mode coerce_multiplication(out) = in is det.



%-----------------------------------------------------------------------------%

:- pred identity(expression, expression).
:- mode identity(in, out) is det.
:- mode identity(out, in) is det.

:- pred permutation(expression, expression).
:- mode permutation(in, out) is multi.
:- mode permutation(out, in) is multi.
:- mode permutation(in, out) is cc_multi.
:- mode permutation(out, in) is cc_multi.

:- pred negation(logical_expression, logical_expression).
:- mode negation(in, out) is det.
:- mode negation(out, in) is det.

:- func negation_of(logical_expression) = logical_expression.
:- mode negation_of(in) = out is det.
:- mode negation_of(out) = in is det.

:- pred negate_list(list(logical_expression), list(logical_expression)).
:- mode negate_list(in, out) is det.
:- mode negate_list(out, in) is det.

:- func negate_list(list(logical_expression)) = list(logical_expression).
:- mode negate_list(in) = out is det.
:- mode negate_list(out) = in is det.

%-----------------------------------------------------------------------------%
% Flatten operations

:- pred flatten(expression, expression).
:- mode flatten(in, out) is det.

:- func flatten(expression) = expression.
:- mode flatten(in) = out is det.

:- pred flatten_list(list(expression), list(expression)).
:- mode flatten_list(in, out) is det.

:- func flatten_list(list(expression)) = list(expression).

:- pred flatten_logic(logical_expression, logical_expression).
:- mode flatten_logic(in, out) is det.

:- func flatten_logic(logical_expression) = logical_expression.

:- pred flatten_logic_list(list(logical_expression), 
	list(logical_expression)).
:- mode flatten_logic_list(in, out) is det.

:- func flatten_logic_list(list(logical_expression))
	= list(logical_expression).
:- mode flatten_logic_list(in) = out is det.

:- pred flatten_term_expression(term_expression, term_expression).
:- mode flatten_term_expression(in, out) is det.

:- func flatten_term_expression(term_expression) = term_expression.
:- mode flatten_term_expression(in) = out is det.

:- pred flatten_numeric_expression(numeric_expression, numeric_expression).
:- mode flatten_numeric_expression(in, out) is det.

:- func flatten_numeric_expression(numeric_expression) = numeric_expression.
:- mode flatten_numeric_expression(in) = out is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list_util.
:- import_module require.

%-----------------------------------------------------------------------------%
% Expression unification and comparison

		
%-----------------------------------------------------------------------------%
% logical_expression conversions

is_logical_expression(A) :-
	A = predicate(_);
	A = negated_predicate(_);
	A = mh_true;
	A = mh_false;
	is_conjunction(A);
	is_disjunction(A);
	A = negation(_);
	A = xor(_, _);
	A = implication(_, _);
	A = iff(_, _);
	A = equal(_, _);
	A = inequal(_, _);
	A = greater_than(_, _);
	A = greater_than_or_equal(_, _);
	A = less_than(_, _);
	A = less_than_or_equal(_, _).


to_logical_expression(predicate(X)) = predicate(X).
to_logical_expression(negated_predicate(X)) = negated_predicate(X).
to_logical_expression(mh_true) = mh_true.
to_logical_expression(mh_false) = mh_false.
to_logical_expression(conjunction(X)) = conjunction(X).
to_logical_expression(and(X, Y)) = and(X, Y).
to_logical_expression(disjunction(X)) = disjunction(X).
to_logical_expression(negation(X)) = negation(X).
to_logical_expression(xor(X, Y)) = xor(X, Y).
to_logical_expression(implication(X, Y)) = implication(X, Y).
to_logical_expression(iff(X, Y)) = iff(X, Y).
to_logical_expression(equal(X, Y)) = equal(X, Y).
to_logical_expression(inequal(X, Y)) = inequal(X, Y).
to_logical_expression(greater_than(X, Y)) = greater_than(X, Y).
to_logical_expression(greater_than_or_equal(X, Y)) 
	= greater_than_or_equal(X, Y).
to_logical_expression(less_than(X, Y)) = less_than(X, Y).
to_logical_expression(less_than_or_equal(X, Y)) = less_than_or_equal(X, Y).
	
:- pragma promise_equivalent_clauses(coerce_logical_expression/2).

coerce_logical_expression(LogiExpr::in,	to_logical_expression(LogiExpr)::out).

coerce_logical_expression(coerce(LogiExpr)::out, LogiExpr::in).

coerce_logical_expression(LogiExpr) = Expr :- 
	coerce_logical_expression(LogiExpr, Expr).



%-----------------------------------------------------------------------------%
% negation conversions

is_negation(negation(_)).

to_negation(negation(X)) = negation(X).

:- pragma promise_equivalent_clauses(coerce_negation/2).

coerce_negation(Expr::in, to_negation(Expr)::out).
coerce_negation(coerce(Negation)::out, Negation::in).

coerce_negation(Negation) = Expr :- coerce_negation(Negation, Expr).


%-----------------------------------------------------------------------------%
% conjunction conversions

is_conjunction(conjunction(_)).
is_conjunction(and(_, _)).


to_conjunction(conjunction(X)) =  conjunction(X).
to_conjunction(and(X, Y)) = and(X, Y).

:- pragma promise_equivalent_clauses(coerce_conjunction/2).

coerce_conjunction(Conj::in, to_conjunction(Conj)::out).
coerce_conjunction(coerce(Conj)::out, Conj::in).

coerce_conjunction(Conj) = Expr :- coerce_conjunction(Conj, Expr).

	
%-----------------------------------------------------------------------------%
% disjunction conversions

is_disjunction(disjunction(_)).
is_disjunction(or(_, _)).

to_disjunction(disjunction(X)) =  disjunction(X).
to_disjunction(or(X, Y)) = or(X, Y).

:- pragma promise_equivalent_clauses(coerce_disjunction/2).
coerce_disjunction(Disj::in, to_disjunction(Disj)::out).
coerce_disjunction(coerce(Disj)::out, Disj::in).

coerce_disjunction(Disj) = Expr :- 
	coerce_disjunction(Disj, Expr).

%-----------------------------------------------------------------------------%
% term expression conversions

is_term_expression(TermExp) :-
	TermExp = term(_);
	is_addition(TermExp);
	TermExp = subtract(_, _);
	is_multiplication(TermExp);
	TermExp = divide(_, _).
	
to_term_expression(term(X)) = term(X).
to_term_expression(sum(X)) = sum(X).
to_term_expression(add(X, Y)) = add(X, Y).
to_term_expression(subtract(X, Y)) = subtract(X, Y).
to_term_expression(product(X)) = product(X).
to_term_expression(multiply(X, Y)) = multiply(X, Y).
to_term_expression(divide(X, Y)) = multiply(X, Y).

:- pragma promise_equivalent_clauses(coerce_term_expression/2).
coerce_term_expression(TermExp::in, to_term_expression(TermExp)::out).
coerce_term_expression(coerce(TermExp)::out, TermExp::in).

coerce_term_expression(TermExp) = Expr :- 
	coerce_term_expression(TermExp, Expr).
	
%-----------------------------------------------------------------------------%
% numeric expression conversions

is_numeric_expression(NumExp) :-
	NumExp = term(var(_));
	NumExp = term(named_var(_));
	NumExp = term(int(_));
	NumExp = term(float(_));
	NumExp = term(functor(_, _));
	is_addition(NumExp);
	NumExp = subtract(_, _);
	is_multiplication(NumExp);
	NumExp = divide(_, _).
	
to_numeric_expression(term(var(X))) = term(var(X)).
to_numeric_expression(term(named_var(X))) = term(named_var(X)).
to_numeric_expression(term(int(X))) = term(int(X)).
to_numeric_expression(term(float(X))) = term(float(X)).
to_numeric_expression(term(functor(X, Y))) = term(functor(X, Y)).
to_numeric_expression(sum(X)) = sum(X).
to_numeric_expression(add(X, Y)) = add(X, Y).
to_numeric_expression(subtract(X, Y)) = subtract(X, Y).
to_numeric_expression(product(X)) = product(X).
to_numeric_expression(multiply(X, Y)) = multiply(X, Y).
to_numeric_expression(divide(X, Y)) = multiply(X, Y).

:- pragma promise_equivalent_clauses(coerce_numeric_expression/2).
coerce_numeric_expression(NumExp::in, to_numeric_expression(NumExp)::out).
coerce_numeric_expression(coerce(NumExp)::out, NumExp::in).

coerce_numeric_expression(NumExp) = Expr :- 
	coerce_numeric_expression(NumExp, Expr).
	
%-----------------------------------------------------------------------------%
% addition conversions
	
is_addition(sum(_)).
is_addition(add(_, _)).

to_addition(sum(X)) = sum(X).
to_addition(add(X, Y)) = add(X, Y).

:- pragma promise_equivalent_clauses(coerce_addition/2).
coerce_addition(Addition::in, to_addition(Addition)::out).
coerce_addition(coerce(Addition)::out, Addition::in).

coerce_addition(Addn) = Expr :- coerce_addition(Addn, Expr).


%-----------------------------------------------------------------------------%
% multiplication conversions
	
is_multiplication(Multi) :-
	Multi = product(_);
	Multi = multiply(_, _).

to_multiplication(product(X)) = product(X).
to_multiplication(multiply(X, Y)) = multiply(X, Y).

:- pragma promise_equivalent_clauses(coerce_multiplication/2).
coerce_multiplication(Multi::in, to_multiplication(Multi)::out).
coerce_multiplication(coerce(Multi)::out, Multi::in).

coerce_multiplication(Multi) = Expr :- coerce_multiplication(Multi, Expr).


%-----------------------------------------------------------------------------%
% Permutation

:- pragma promise_equivalent_clauses(permutation/2).

%-----------------------------------------------------------------------------%
% Identities

% p(A) = p(A)		
identity(predicate(A), predicate(A)).


% -p(A) = -p(A)
identity(negated_predicate(A), negated_predicate(A)).

% true = true
identity(mh_true, mh_true).
 
% false = false
identity(mh_false, mh_false).

% conj(A) = conj(A).
identity(conjunction(A), conjunction(A)).

% and(A, B) = and(A, B)
identity(and(A, B), and(A, B)).

% disj(A) = disj(A).
identity(disjunction(A), disjunction(A)).

% or(A, B) = or(A, B)
identity(or(A, B), or(A, B)).

% not(A) = not(A).
identity(negation(A), negation(A)).

% xor(A, B) = xor(A, B)
identity(xor(A, B), xor(A, B)).

% A :- B = A :- B
identity(implication(A, B), implication(A, B)).

% A <-> B = A <-> B
identity(iff(A, B), iff(A, B)).

% (A = B) = (A = B)
identity(equal(A, B), equal(A, B)).

% (A != B) = (A != B)
identity(inequal(A, B), inequal(A, B)).

% A > B = A > B
identity(greater_than(A, B), greater_than(A, B)).

% A => B = A => B
identity(greater_than_or_equal(A, B), greater_than_or_equal(A, B)).
	
% A < B = A < B
identity(less_than(A, B), less_than(A, B)).

% A =< B = A =< B
identity(less_than_or_equal(A, B), less_than_or_equal(A, B)).

% A = A
identity(term(A), term(A)).

% sum(A) = sum(A)
identity(sum(A), sum(A)).

% A + B = A + B
identity(add(A, B), add(A, B)).

% A - B = A - B
identity(subtract(A, B), subtract(A, B)).

% product(A) = product(A)
identity(product(A), product(A)).

% A * B = A * B
identity(multiply(A, B), multiply(A, B)).

% A / B = A / B
identity(divide(A, B), divide(A, B)).

% A = A
permutation(A, B) :- identity(A, B).

%-----------------------------------------------------------------------------%
% Inverse identities

% and(A, B) = and(B, A).
permutation(and(A, B), and(B, A)).

% or(A, B) = or(B, A).
permutation(or(A, B), or(B, A)).

% xor(A, B) = xor(B, A)
permutation(xor(A, B), xor(B, A)).

% A <-> B = B <-> A
permutation(iff(A, B), iff(B, A)).

% (A = B) = (B = A)
permutation(equal(A, B), equal(B, A)).

% (A != B) = (B != A)
permutation(inequal(A, B), inequal(B, A)).

% A + B = B + A
permutation(add(A, B), add(B, A)).

% A * B = B * A
permutation(multiply(A, B), multiply(B, A)).

%-----------------------------------------------------------------------------%
% Negated predicates
 
% -p(A) = not p(A)
permutation(negated_predicate(A), negation(predicate(A))).

% not p(A) = -p(A)
permutation(negation(predicate(A)), negated_predicate(A)).

% p(A) = not -p(A)
permutation(predicate(A), negation(negated_predicate(A))).

% not -p(A) = p(A)
permutation(negation(negated_predicate(A)), predicate(A)).


%-----------------------------------------------------------------------------%
% Conjunction transformations

% and(A, B) = conj([A, B]).
permutation(and(A, B), conjunction([A, B])).
permutation(and(A, B), conjunction([B, A])).

%  conj([A, B]) = and(A, B).
permutation(conjunction([A, B]), and(A, B)).
permutation(conjunction([A, B]), and(B, A)).

% conj([B, A]) = conj([A, B]).	
permutation(conjunction(A), conjunction(B)) :- 
	unify_conjunctions(A, B).

/* A predicate I wrote to unify lists with the same members in any order
unify_unordered([], []).

unify_unordered([A | As], Bs) :-
	delete(Bs, A, Remainder),
	unify_unordered(As, Remainder).
*/

:- pred unify_conjunctions(list(logical_expression), list(logical_expression)).
:- mode unify_conjunctions(in, in) is semidet.
:- mode unify_conjunctions(in, out) is multi.
:- mode unify_conjunctions(out, in) is multi.

unify_conjunctions([], []).

unify_conjunctions([A | As], Bs) :-
	remove_conjunct(Bs, A, Remainder),
	unify_conjunctions(As, Remainder).
	
unify_conjunctions(A, B) :- unify_conjunctions(B, A).
	
:- promise all [A, B] 
	( unify_conjunctions(A, B) <=> unify_conjunctions(B, A) ).

/* list.m implementation of delete
delete([X | Xs], ToDelete, Xs) :-
    X = ToDelete.
delete([X | Xs], ToDelete, [X | DXs]) :-
    list.delete(Xs, ToDelete, DXs).
*/

:- pred remove_conjunct(list(logical_expression), logical_expression, 
	list(logical_expression)).
:- mode remove_conjunct(in, in, in) is semidet.
:- mode remove_conjunct(in, in, out) is nondet.
:- mode remove_conjunct(in, out, out) is nondet.
:- mode remove_conjunct(out, in, in) is multi.

remove_conjunct([A | As], X, B) :-
	% If the first element of the list A is a conjunction, remove X from the
	% conjunction
	 	A = conjunction(SubConj),
		remove_conjunct(SubConj, X, NewSubConj),
		B = [conjunction(NewSubConj) | As]
	; 
	% Same, but with and/2 constructor
		A = and(X, Y),
		remove_conjunct(B, X, B1),
		remove_conjunct(B1, Y, As)
	;
	% If not, remove the element from B
		X = A,
		B = As
	;
	% Alteratively, traverse to the next element on the list
		B = [A | Bs],
		remove_conjunct(As, X, Bs).
		

	
%-----------------------------------------------------------------------------%
% Disjunction transformations
	
% or(A, B) = disj([A, B]).
permutation(or(A, B), disjunction([A, B])).
permutation(or(A, B), disjunction([B, A])).

%  disj([A, B]) = or(A, B).
permutation(disjunction([A, B]), or(A, B)).
permutation(disjunction([A, B]), or(B, A)). 

% disj([B, A]) = disj([A, B]).	
permutation(disjunction(A), disjunction(B)) :- 
	unify_unordered(A, B).
	
:- pred unify_disjunctions(list(logical_expression), list(logical_expression)).
:- mode unify_disjunctions(in, in) is semidet.
:- mode unify_disjunctions(in, out) is multi.
:- mode unify_disjunctions(out, in) is multi.

unify_disjunctions([], []).

unify_disjunctions([A | As], Bs) :-
	remove_disjunct(Bs, A, Remainder),
	unify_disjunctions(As, Remainder).
	
unify_disjunctions(A, B) :- unify_disjunctions(B, A).
	
:- promise all [A, B] 
	( unify_disjunctions(A, B) <=> unify_disjunctions(B, A) ).

:- pred remove_disjunct(list(logical_expression), logical_expression, 
	list(logical_expression)).
:- mode remove_disjunct(in, in, in) is semidet.
:- mode remove_disjunct(in, in, out) is nondet.
:- mode remove_disjunct(in, out, out) is nondet.
:- mode remove_disjunct(out, in, in) is multi.

remove_disjunct([A | As], X, B) :-
	% If the first element of the list A is a disjunction, remove X from the
	% disjunction
	 	A = disjunction(SubDisj),
		remove_disjunct(SubDisj, X, NewSubDisj),
		B = [disjunction(NewSubDisj) | As]
	; 
	% Same, but with or/2 constructor
		A = or(X, Y),
		remove_disjunct(B, X, B1),
		remove_disjunct(B1, Y, As)
	;
	% If not, remove the element from B
		X = A,
		B = As
	;
	% Alteratively, traverse to the next element on the list
		B = [A | Bs],
		remove_disjunct(As, X, Bs).

%-----------------------------------------------------------------------------%
% De Morgan's laws

% not and(A, B) = or(not A, not B)
permutation(negation(disjunction(A)), disjunction(B)) :-
	negate_list(A, B). 

% or(not A, not B) = not and(A, B)
permutation(disjunction(A), negation(disjunction(B))) :-
	negate_list(A, B).
	
% not or(A, B) = and(not A, not B)
permutation(negation(disjunction(A)), disjunction(B)) :-
	negate_list(A, B). 
	
% and(not A, not B) = not or(A, B)
permutation(disjunction(A), negation(disjunction(B))) :-
	negate_list(A, B).

	
%-----------------------------------------------------------------------------%
% Double negation transformations

/* Permutation will not terminate with this implementation of double negation

% A = not not A
permutation(coerce_logical_expression(A), negation(negation(A))).

% not not A = A
permutation(negation(negation(A)), coerce_logical_expression(A)).
	
*/


%-----------------------------------------------------------------------------%
% Exclusive OR transformation

% xor(A, B) = or(and(A, not B), and(not A, B)).
permutation(xor(A, B), or(and(A, negation_of(B)), and(negation_of(A), B))).

% xor(A, B) = and(or(A, B), or(not A, not B)).
permutation(xor(A, B), and(or(A, B), or(negation_of(A), negation_of(B)))).

% xor(A, B) = and(or(A, B), not and(A, B)).
permutation(xor(A, B), and(or(A, B), negation(and(A, B)))).


%-----------------------------------------------------------------------------%
% Implication transformation
	
% if(A, B) = not(A) or B  
permutation(implication(A, B), or(negation_of(A), B)).


%-----------------------------------------------------------------------------%
% Biconditional transformation

% iff(A, B) = and(or(not A, B), or(not B, A))
permutation(iff(A, B), and(or(negation_of(A), B), or(negation_of(B), A))).


%-----------------------------------------------------------------------------%
% Addition transformations

% add(A, B) = sum([A, B]).
permutation(add(A, B), sum([A, B])).
permutation(add(A, B), sum([B, A])).

%  sum([A, B]) = add(A, B).
permutation(sum([A, B]), add(A, B)).
permutation(sum([A, B]), add(B, A)). 

% sum([B, A]) = sum([A, B]).	
permutation(sum(A), sum(B)) :- 
	unify_unordered(A, B).
	
	
%-----------------------------------------------------------------------------%
% Multiplication transformations

% multiply(A, B) = product([A, B]).
permutation(multiply(A, B), product([A, B])).
permutation(multiply(A, B), product([B, A])).


%  product([A, B]) = multiply(A, B).
permutation(product([A, B]), multiply(A, B)).
permutation(product([A, B]), multiply(B, A)).

% product([B, A]) = product([A, B]).	
permutation(product(A), product(B)) :- 
	unify_unordered(A, B).


% End permutation/2


%-----------------------------------------------------------------------------%
% Negation

:- pragma promise_equivalent_clauses(negation/2).

negation(A::in, B::out) :- 
	if A = negation(X) then
		B = X
	else
		B = negation(A).

negation(A::out, B::in) :- negation(B, A).


negation_of(A) = B :- negation(A, B).

negate_list([], []).

negate_list([ X | A], [ negation_of(X) | B]) :- negate_list(A, B).

negate_list(A) = B :- negate_list(A, B).


%-----------------------------------------------------------------------------%	
% Flatten


flatten(Expr, Flat) :-
	 
	(
		Expr = mh_true,
		Flat = mh_true
	;
		Expr = mh_false,
		Flat = mh_false
	;
		Expr = predicate(A),
		Flat = predicate(A)
	;
		Expr = negated_predicate(A),
		Flat = negated_predicate(A)
	;
		Expr = conjunction(Conjunction),
		Flat = conjunction(flatten_conjunction(Conjunction))
	;
		Expr = and(A, B),
		Flat = disjunction(flatten_disjunction([A, B]))
	;
		Expr = disjunction(Disjunction),
		Flat = disjunction(flatten_disjunction(Disjunction))
	;
		Expr = or(A, B),
		Flat = disjunction(flatten_disjunction([A, B]))
	;
		Expr = negation(X),
		Flat = negation(flatten_logic(X))
	;
		Expr = xor(A, B),
		Flat = xor(flatten_logic(A), flatten_logic(B))
	;
		Expr = implication(A, B),
		Flat = implication(flatten_logic(A), flatten_logic(B))
	;
		Expr = iff(A, B),
		Flat = iff(flatten_logic(A), flatten_logic(B))
	;
		Expr = equal(A, B),
		Flat = equal(flatten(A), flatten(B))
	;
		Expr = inequal(A, B),
		Flat = inequal(flatten(A), flatten(B))
	;
		Expr = greater_than(A, B),
		Flat = greater_than(flatten_term_expression(A), 
			flatten_term_expression(B))
	;
		Expr = greater_than_or_equal(A, B),
		Flat = greater_than_or_equal(flatten_term_expression(A), 
			flatten_term_expression(B))
	;
		Expr = less_than(A, B),
		Flat = less_than(flatten_term_expression(A), 
			flatten_term_expression(B))
	;
		Expr = less_than_or_equal(A, B),
		Flat = less_than_or_equal(flatten_term_expression(A), 
			flatten_term_expression(B))
	;
		Expr = term(A),
		Flat = term(A)
	;
		Expr = sum(Sum),
		Flat = sum(flatten_sum(Sum))
	;
		Expr = add(A, B),
		Flat = sum(flatten_sum([A, B]))
	;
		Expr = subtract(A, B),
		Flat = subtract(flatten_numeric_expression(A), 
			flatten_numeric_expression(B))
	;
		Expr = product(Product),
		Flat = product(flatten_product(Product))
	;
		Expr = multiply(A, B),
		Flat = product(flatten_product([A, B]))
	;
		Expr = divide(A, B),
		Flat = divide(flatten_numeric_expression(A),
			flatten_numeric_expression(B))
	).

flatten(Expr) = Flat :- flatten(Expr, Flat).

flatten_list([], []).

flatten_list([X | Xs], [ flatten(X) | flatten_list(Xs)] ).

flatten_list(X) = Y :- flatten_list(X, Y).
	

flatten_logic(Expr, Flat) :- 
	 
	(
		Expr = mh_true,
		Flat = mh_true
	;
		Expr = mh_false,
		Flat = mh_false
	;
		Expr = predicate(A),
		Flat = predicate(A)
	;
		Expr = negated_predicate(A),
		Flat = negated_predicate(A)
	;
		Expr = conjunction(Conjunction),
		Flat = conjunction(flatten_conjunction(Conjunction))
	;
		Expr = and(A, B),
		Flat = disjunction(flatten_disjunction([A, B]))
	;
		Expr = disjunction(Disjunction),
		Flat = disjunction(flatten_disjunction(Disjunction))
	;
		Expr = or(A, B),
		Flat = disjunction(flatten_disjunction([A, B]))
	;
		Expr = negation(X),
		Flat = negation(flatten_logic(X))
	;
		Expr = xor(A, B),
		Flat = xor(flatten_logic(A), flatten_logic(B))
	;
		Expr = implication(A, B),
		Flat = implication(flatten_logic(A), flatten_logic(B))
	;
		Expr = iff(A, B),
		Flat = iff(flatten_logic(A), flatten_logic(B))
	;
		Expr = equal(A, B),
		Flat = equal(flatten(A), flatten(B))
	;
		Expr = inequal(A, B),
		Flat = inequal(flatten(A), flatten(B))
	;
		Expr = greater_than(A, B),
		Flat = greater_than(flatten_term_expression(A), 
			flatten_term_expression(B))
	;
		Expr = greater_than_or_equal(A, B),
		Flat = greater_than_or_equal(flatten_term_expression(A), 
			flatten_term_expression(B))
	;
		Expr = less_than(A, B),
		Flat = less_than(flatten_term_expression(A), 
			flatten_term_expression(B))
	;
		Expr = less_than_or_equal(A, B),
		Flat = less_than_or_equal(flatten_term_expression(A), 
			flatten_term_expression(B))
	).
	

		

:- func flatten_conjunction(list(logical_expression)) 
	= list(logical_expression).
	
:- mode flatten_conjunction(in) = out is det.

flatten_conjunction([]) = [].

flatten_conjunction([C | Cs]) =  F :-
	if C = conjunction(X) 
	then F = append(flatten_conjunction(X), flatten_conjunction(Cs))
	else if C = and(X, Y)
	then F = append(flatten_conjunction([X, Y]), flatten_conjunction(Cs))
	else F = [ flatten_logic(C) | flatten_conjunction(Cs) ].
	

:- func flatten_disjunction(list(logical_expression)) 
	= list(logical_expression).
	
:- mode flatten_disjunction(in) = out is det.

flatten_disjunction([]) = [].

flatten_disjunction([D | Ds]) =  F :-
	if D = disjunction(X) 
	then F = append(flatten_disjunction(X), flatten_disjunction(Ds))
	else if D = and(X, Y)
	then F = append(flatten_disjunction([X, Y]), flatten_disjunction(Ds))
	else F = [ flatten_logic(D) | flatten_disjunction(Ds) ].
	

flatten_logic(Expr) = Flat :- flatten_logic(Expr, Flat).

flatten_logic_list([], []).

flatten_logic_list([X | Xs], [ flatten_logic(X) | flatten_logic_list(Xs)] ).

flatten_logic_list(X) = Y :- flatten_logic_list(X, Y).

% see if the type system will let me get away with this without coercion
flatten_term_expression(Expr, Flat) :- 
	 
	(
		Expr = term(A),
		Flat = term(A)
	;
		Expr = sum(Sum),
		Flat = sum(flatten_sum(Sum))
	;
		Expr = add(A, B),
		Flat = sum(flatten_sum([A, B]))
	;
		Expr = subtract(A, B),
		Flat = subtract(flatten_numeric_expression(A), 
			flatten_numeric_expression(B))
	;
		Expr = product(Product),
		Flat = product(flatten_product(Product))
	;
		Expr = multiply(A, B),
		Flat = product(flatten_product([A, B]))
	;
		Expr = divide(A, B),
		Flat = divide(flatten_numeric_expression(A), 
			flatten_numeric_expression(B))
	).

flatten_term_expression(Expr) = Flat :- flatten_term_expression(Expr, Flat).

flatten_numeric_expression(Expr, Flat) :- 
	 
	(
		Expr = term(A),
		Flat = term(A)
	;
		Expr = sum(Sum),
		Flat = sum(flatten_sum(Sum))
	;
		Expr = add(A, B),
		Flat = sum(flatten_sum([A, B]))
	;
		Expr = subtract(A, B),
		Flat = subtract(flatten_numeric_expression(A), 
			flatten_numeric_expression(B))
	;
		Expr = product(Product),
		Flat = product(flatten_product(Product))
	;
		Expr = multiply(A, B),
		Flat = product(flatten_product([A, B]))
	;
		Expr = divide(A, B),
		Flat = divide(flatten_numeric_expression(A), 
			flatten_numeric_expression(B))
	).
	
flatten_numeric_expression(Expr) = Flat :- 
	flatten_numeric_expression(Expr, Flat).


:- func flatten_sum(list(numeric_expression)) 
	= list(numeric_expression).
	
:- mode flatten_sum(in) = out is det.

flatten_sum([]) = [].

flatten_sum([S | Ss]) =  F :-
	if S = sum(X) 
	then F = append(flatten_sum(X), flatten_sum(Ss))
	else if S = add(X, Y)
	then F = append(flatten_sum([X, Y]), flatten_sum(Ss))
	else F = [ flatten_numeric_expression(S) | flatten_sum(Ss) ].
	
	
:- func flatten_product(list(numeric_expression)) 
	= list(numeric_expression).
	
:- mode flatten_product(in) = out is det.

flatten_product([]) = [].

flatten_product([P | Ps]) =  F :-
	if P = product(X)
	then F = append(flatten_product(X), flatten_product(Ps))
	else if P = multiply(X, Y)
	then F = append(flatten_product([X, Y]), flatten_product(Ps))
	else F = [ flatten_numeric_expression(P) | flatten_product(Ps) ].