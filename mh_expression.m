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

;		equal(term_expression, term_expression)
;		inequal(term_expression, term_expression)
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
;		divide(numeric_expression, numeric_expression) 
where equality is unify_expressions, comparison is compare_expressions.

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

;		equal(term_expression, term_expression)
;		inequal(term_expression, term_expression)
;		greater_than(term_expression, term_expression)
;		greater_than_or_equal(term_expression, term_expression)
;		less_than(term_expression, term_expression)
;		less_than_or_equal(term_expression, term_expression).

:- pred expression_is_logical_expression(expression::in) is semidet.
:- pred expression_to_logical_expression(expression::in, 
	logical_expression::out) is semidet.

:- pred coerce_logical_expression(expression, logical_expression).
:- mode coerce_logical_expression(in, out) is semidet.
:- mode coerce_logical_expression(out, in) is det.

:- func coerce_logical_expression(expression) = logical_expression.
:- mode coerce_logical_expression(in) = out is semidet.
:- mode coerce_logical_expression(out) = in is det.

%-----------------------------------------------------------------------------%

:- inst negation ---> negation(ground).

:- inst negation(T) ---> negation(T).

:- type negation =< logical_expression ---> negation(logical_expression).

:- pred expression_is_negation(expression::in) is semidet.
:- pred expression_to_negation(expression::in, negation::out) is semidet.

:- pred coerce_negation(expression, negation).
:- mode coerce_negation(in, out) is semidet.
:- mode coerce_negation(out, in) is det.

:- func coerce_negation(expression) = negation.
:- mode coerce_negation(in) = out is semidet.
:- mode coerce_negation(out) = in is det.



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
;		less_than_or_equal(ground, ground).

:- type not_negation =< logical_expression
--->	predicate(functor)
;		negated_predicate(functor)

;		mh_true
;		mh_false

;		conjunction(list(logical_expression))
; 		and(logical_expression, logical_expression)

;		disjunction(list(logical_expression))
;		or(logical_expression, logical_expression)

;		xor(logical_expression, logical_expression)
;		implication(logical_expression, logical_expression)
;		iff(logical_expression, logical_expression)

;		equal(term_expression, term_expression)
;		inequal(term_expression, term_expression)
;		greater_than(term_expression, term_expression)
;		greater_than_or_equal(term_expression, term_expression)
;		less_than(term_expression, term_expression)
;		less_than_or_equal(term_expression, term_expression).

:- pred expression_is_not_negation(expression::in) is semidet.
:- pred expression_to_not_negation(expression::in, negation::out) 
	is semidet.
	
:- pred coerce_not_negation(expression, not_negation).
:- mode coerce_not_negation(in, out) is semidet.
:- mode coerce_not_negation(out, in) is det.

:- func coerce_not_negation(expression) = not_negation.
:- mode coerce_not_negation(in) = out is semidet.
:- mode coerce_not_negation(out) = in is det.


%-----------------------------------------------------------------------------%


:- inst conjunction 
 --->		conjunction(ground)
; 			and(ground, ground).

:- inst conjunction(T)
--->		conjunction(list(T))
;			and(T, T).

:- type conjunction =< logical_expression
--->		conjunction(list(logical_expression))
; 			and(logical_expression, logical_expression).

:- pred expression_is_conjunction(expression::in) is semidet.
:- pred expression_to_conjuction(expression::in, conjunction::out) is semidet.

:- pred coerce_conjunction(expression, conjunction).
:- mode coerce_conjunction(in, out) is semidet.
:- mode coerce_conjunction(out, in) is det.

:- func coerce_conjunction(expression) = conjunction.
:- mode coerce_conjunction(in) = out is semidet.
:- mode coerce_conjunction(out) = in is det.

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
;		less_than_or_equal(ground, ground).

:- type not_conjunction =< logical_expression
--->	predicate(functor)
;		negated_predicate(functor)

;		mh_true
;		mh_false

;		disjunction(list(logical_expression))
;		or(logical_expression, logical_expression)

;		negation(logical_expression)

;		xor(logical_expression, logical_expression)
;		implication(logical_expression, logical_expression)
;		iff(logical_expression, logical_expression)

;		equal(term_expression, term_expression)
;		inequal(term_expression, term_expression)
;		greater_than(term_expression, term_expression)
;		greater_than_or_equal(term_expression, term_expression)
;		less_than(term_expression, term_expression)
;		less_than_or_equal(term_expression, term_expression).

:- pred expression_is_not_conjunction(expression::in) is semidet.
:- pred expression_to_not_conjuction(expression::in, conjunction::out) 
	is semidet.
	
:- pred coerce_not_conjunction(expression, not_conjunction).
:- mode coerce_not_conjunction(in, out) is semidet.
:- mode coerce_not_conjunction(out, in) is det.

:- func coerce_not_conjunction(expression) = not_conjunction.
:- mode coerce_not_conjunction(in) = out is semidet.
:- mode coerce_not_conjunction(out) = in is det.


%-----------------------------------------------------------------------------%
:- inst disjunction 
 --->		disjunction(ground)
; 			or(ground, ground).

:- inst disjunction(T)
 --->		disjunction(list(T))
; 			or(T, T).

:- type disjunction =< logical_expression
--->		disjunction(list(logical_expression))
; 			or(logical_expression, logical_expression).

:- pred expression_is_disjunction(expression::in) is semidet.
:- pred expression_to_conjuction(expression::in, disjunction::out) is semidet.

:- pred coerce_disjunction(expression, disjunction).
:- mode coerce_disjunction(in, out) is semidet.
:- mode coerce_disjunction(out, in) is det.

:- func coerce_disjunction(expression) = disjunction.
:- mode coerce_disjunction(in) = out is semidet.
:- mode coerce_disjunction(out) = in is det.

:- inst not_disjunction
--->	predicate(ground)
;		negated_predicate(ground)

;		conjunction(ground)
;		and(ground, ground)

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

:- type not_disjunction =< logical_expression
--->	predicate(functor)
;		negated_predicate(functor)

;		mh_true
;		mh_false

;		conjunction(list(logical_expression))
; 		and(logical_expression, logical_expression)

;		negation(logical_expression)

;		xor(logical_expression, logical_expression)
;		implication(logical_expression, logical_expression)
;		iff(logical_expression, logical_expression)

;		equal(term_expression, term_expression)
;		inequal(term_expression, term_expression)
;		greater_than(term_expression, term_expression)
;		greater_than_or_equal(term_expression, term_expression)
;		less_than(term_expression, term_expression)
;		less_than_or_equal(term_expression, term_expression).

:- pred expression_is_not_disjunction(expression::in) is semidet.
:- pred expression_to_not_conjuction(expression::in, disjunction::out) 
	is semidet.
	
:- pred coerce_not_disjunction(expression, not_disjunction).
:- mode coerce_not_disjunction(in, out) is semidet.
:- mode coerce_not_disjunction(out, in) is det.

:- func coerce_not_disjunction(expression) = not_disjunction.
:- mode coerce_not_disjunction(in) = out is semidet.
:- mode coerce_not_disjunction(out) = in is det.

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

:- inst term_expression(T)
--->	term(T) 

;		sum(list(T))
;		add(T, T)
;		subtract(T, T)

;		product(list(T))
;		multiply(T, T)
;		divide(T, T).

:- type term_expression =< expression
--->	term(mh_term) 

;		sum(list(numeric_expression))
;		add(numeric_expression, numeric_expression)
;		subtract(numeric_expression, numeric_expression)

;		product(list(numeric_expression))
;		multiply(numeric_expression, numeric_expression)
;		divide(numeric_expression, numeric_expression).

%-----------------------------------------------------------------------------%


:- inst numeric_expression
--->	term(numeric_term) 

;		sum(ground)
;		add(ground, ground)
;		subtract(ground, ground)

;		product(ground)
;		multiply(ground, ground)
;		divide(ground, ground). 

:- type numeric_expression =< term_expression
--->	term(numeric_term) 

;		sum(list(numeric_expression))
;		add(numeric_expression, numeric_expression)
;		subtract(numeric_expression, numeric_expression)

;		product(list(numeric_expression))
;		multiply(numeric_expression, numeric_expression)
;		divide(numeric_expression, numeric_expression). 
%-----------------------------------------------------------------------------%

:- inst addition 
 --->		sum(ground)
; 			add(ground, ground).

:- type addition =< numeric_expression
--->	sum(list(numeric_expression))
; 		add(numeric_expression, numeric_expression).

:- pred expression_is_addition(expression::in) is semidet.
:- pred expression_to_addition(expression::in, addition::out) is semidet.

:- pred coerce_addition(expression, addition).
:- mode coerce_addition(in, out) is semidet.
:- mode coerce_addition(out, in) is det.

:- func coerce_addition(expression) = addition.
:- mode coerce_addition(in) = out is semidet.
:- mode coerce_addition(out) = in is det.

:- inst not_addition
--->	subtract(ground, ground)

;		product(ground)
;		multiply(ground, ground)
;		divide(ground, ground).

:- type not_addition =< numeric_expression
--->	subtract(numeric_expression, numeric_expression)

;		product(list(numeric_expression))
;		multiply(numeric_expression, numeric_expression)
;		divide(numeric_expression, numeric_expression).

:- pred expression_is_not_addition(expression::in) is semidet.
:- pred expression_to_not_addition(expression::in, addition::out) 
	is semidet.
	
:- pred coerce_not_addition(expression, not_addition).
:- mode coerce_not_addition(in, out) is semidet.
:- mode coerce_not_addition(out, in) is det.

:- func coerce_not_addition(expression) = not_addition.
:- mode coerce_not_addition(in) = out is semidet.
:- mode coerce_not_addition(out) = in is det.


%-----------------------------------------------------------------------------%

:- inst multiplication 
--->	product(ground)
;		multiply(ground, ground).

:- type multiplication =< numeric_expression
--->	product(list(numeric_expression))
;		multiply(numeric_expression, numeric_expression).

:- pred expression_is_multiplication(expression::in) is semidet.
:- pred expression_to_multiplication(expression::in, multiplication::out) 
	is semidet.

:- pred coerce_multiplication(expression, multiplication).
:- mode coerce_multiplication(in, out) is semidet.
:- mode coerce_multiplication(out, in) is det.

:- func coerce_multiplication(expression) = multiplication.
:- mode coerce_multiplication(in) = out is semidet.
:- mode coerce_multiplication(out) = in is det.

:- inst not_multiplication.
--->	sum(ground)
;		add(ground, ground)
;		subtract(ground, ground)

;		divide(ground, ground).

:- type not_multiplication
--->	sum(list(numeric_expression))
;		add(numeric_expression, numeric_expression)
;		subtract(numeric_expression, numeric_expression)

;		divide(numeric_expression, numeric_expression).

:- pred expression_is_not_multiplication(expression::in) is semidet.
:- pred expression_to_not_multiplication(expression::in, multiplication::out) 
	is semidet.
	
:- pred coerce_not_multiplication(expression, not_multiplication).
:- mode coerce_not_multiplication(in, out) is semidet.
:- mode coerce_not_multiplication(out, in) is det.

:- func coerce_not_multiplication(expression) = not_multiplication.
:- mode coerce_not_multiplication(in) = out is semidet.
:- mode coerce_not_multiplication(out) = in is det.

%-----------------------------------------------------------------------------%

:- pred unify_expressions(expression, expression).
:- mode unify_expressions(in, in) is semidet.
:- mode unify_expressions(in, out) is cc_multi.
:- mode unify_expressions(out, in) is cc_multi.

:- pred compare_expressions(comparison_result::uo, 
	expression::in, expression::in) is det.
	
:- pred identity(expression, expression).
:- mode identity(in, in) is semidet.
:- mode identity(in, out) is det.
:- mode identity(out, in) is det.
	
:- pred permutation(expression, expression).
:- mode permutation(in, out) is multi.
:- mode permutation(out, in) is multi.

:- pred negation(logical_expression, logical_expression).
:- mode negation(in, in) is semidet.
:- mode negation(in, out) is det.
:- mode negation(out, in) is det.

:- func negation_of(logical_expression) = logical_expression.
:- mode negation_of(in) = in is semidet.
:- mode negation_of(in) = out is det.
:- mode negation_of(out) = in is det.

:- pred flatten(expression, expression).
:- mode flatten(in, out) is det.
:- mode flatten(out, in) is multi.

:- func flatten(expression) = expression.
:- mode flatten(in) = out is det.
:- mode flatten(out) = in is multi.

:- pred flatten_list(list(expression), list(expression)).
:- mode flatten(in, out) is det.
:- mode flatten(out, in) is multi.

:- func flatten_list(list(expression)) = list(expression).
:- mode flatten_list(in) = out is det.
:- mode flatten_list(out) = in is multi.

:- pred flatten_logic(logical_expression, logical_expression).
:- mode flatten_logic(in, out) is det.
:- mode flatten_logic(out, in) is multi.

:- func flatten_logic(logical_expression) = logical_expression.
:- mode flatten_logic(in) = out is det.
:- mode flatten_logic(out) = in is multi.

:- pred flatten_logic_list(list(logical_expression), 
	list(logical_expression)).
:- mode flatten_logic_list(in, out) is det.
:- mode flatten_logic_list(out, in) is multi.

:- func flatten_logic_list(list(logical_expression))
	= list(logical_expression).
:- mode flatten_logic_list(in) = out is det.
:- mode flatten_logic_list(out) = in is multi.

:- pred flatten_terms(term_expression, term_expression).
:- mode flatten_terms(in, out) is det.
:- mode flatten_terms(out, in) is multi.

:- func flatten_terms(term_expression) = term_expression.
:- mode flatten_terms(in) = out is det.
:- mode flatten_terms(out) = in is multi.



:- pred flatten_math(numeric_expression, numeric_expression).
:- mode flatten_math(in, out) is det.
:- mode flatten_math(out, in) is multi.

:- func flatten_math(numeric_expression) = numeric_expression.
:- mode flatten_math(in) = out is det.
:- mode flatten_math(out) = in is multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list_util.
:- import_module require.


unify_expressions(X, Y) :- permutation(X, Y).

compare_expressions(Result, X, Y) :-
	if permutation(X, Y) then
		Result = (=)
	else
		compare(Result, X, Y).
		
%-----------------------------------------------------------------------------%
% logical_expression conversions

expression_is_logical_expression(_::in(logical_expression)).
expression_to_logical_expression(LogiExpr::in(logical_expression), 
	coerce(LogiExpr)::out).

:- promise_equivalent_clauses(coerce_logical_expression/2).
coerce_logical_expression(LogiExpr::in(logical_expression), 
	coerce(LogiExpr)::out).
coerce_logical_expression(LogiExpr::out, LogiExpr::in).

coerce_logical_expression(Conj) = Expr :- 
	coerce_logical_expression(Conj, Expr).



%-----------------------------------------------------------------------------%
% negation conversions

expression_is_negation(_::in(negation)).
expression_to_negation(Negation::in(negation), coerce(Negation)::out).

:- promise_equivalent_clauses(coerce_negation/2).
coerce_negation(Negation::in(negation), coerce(Negation)::out).
coerce_negation(Negation::out, Negation::in).

coerce_negation(Conj) = Expr :- coerce_negation(Conj, Expr).

expression_is_not_negation(_::in(not_negation)).
expression_to_not_negation(NotNegation::in(not_negation), 
	coerce(NotNegation)).
	
:- promise_equivalent_clauses(coerce_not_negation/2).
coerce_not_negation(NotNeg::in(not_negation), coerce(NotNeg)::out).
coerce_not_negation(NotNeg::out, NotNeg::in).

coerce_not_negation(NotNeg) = Expr :- 
	coerce_not_negation(NotNeg, Expr).


:- promise_equivalent_clauses(negation/2).

negation(A::in(not_negation), negation(A)::in).
negation(negation(A)::in, A::in(not_negation)).


negation(A::in(not_negation), negation(coerce(A))::out).
negation(negation(A)::in, A::out).

negation(A::out, negation(A)::in).
negation(negation(coerce(A))::out, A::in(not_negation)).

negation_of(A) = B :- negation(A, B).

%-----------------------------------------------------------------------------%
% conjunction conversions

expression_is_conjunction(_::in(conjunction)).
expression_to_conjunction(Conjunction::in(conjunction), coerce(Conjunction)::out).

:- promise_equivalent_clauses(coerce_conjunction/2).
coerce_conjunction(Conjunction::in(conjunction), coerce(Conjunction)::out).
coerce_conjunction(Conjunction::out, Conjunction)::in).

coerce_conjunction(Conj) = Expr :- coerce_conjunction(Conj, Expr).

expression_is_not_conjunction(_::in(not_conjunction)).
expression_to_not_conjunction(NotConjunction::in(not_conjunction), 
	coerce(NotConjunction)).
	
:- promise_equivalent_clauses(coerce_not_conjunction/2).
coerce_not_conjunction(NotConj::in(not_conjunction), coerce(NotConj)::out).
coerce_not_conjunction(NotConj::out, NotConj::in).

coerce_not_conjunction(NotConj) = Expr :- 
	coerce_not_conjunction(NotConj, Expr).
	
%-----------------------------------------------------------------------------%
% disjunction conversions

expression_is_disjunction(_::in(disjunction)).
expression_to_disjunction(Disjunction::in(disjunction), coerce(Disjunction)).

:- promise_equivalent_clauses(coerce_disjunction/2).
coerce_disjunction(Disj::in(disjunction), coerce(Disj)::out).
coerce_disjunction(Disj::out, Disj::in).

coerce_disjunction(Disj) = Expr :- 
	coerce_disjunction(Disj, Expr).

expression_is_not_disjunction(_::in(not_disjunction)).
expression_to_not_disjunction(NotDisjunction::in(not_disjunction), 
	coerce(NotDisjunction)).
	
:- promise_equivalent_clauses(coerce_not_disjunction/2).
coerce_not_disjunction(NotDisj::in(not_disjunction), coerce(NotDisj)::out).
coerce_not_disjunction(NotDisj::out, NotDisj::in).

coerce_not_disjunction(NotDisj) = Expr :- 
	coerce_not_disjunction(NotDisj, Expr).
	
%-----------------------------------------------------------------------------%
% addition conversions
	
expression_is_addition(_::in(addition)).
expression_to_addition(Addition::in(addition), coerce(Addition)).

:- promise_equivalent_clauses(coerce_addition/2).
coerce_addition(Addition::in(addition), coerce(Addition)::out).
coerce_addition(Addition::out, Addition::in).

coerce_addition(Addn) = Expr :- coerce_addition(Addn, Expr).

expression_is_not_addition(_::in(not_addition)).
expression_to_not_addition(NotAddition::in(not_addition), 
	coerce(NotAddition)).
	
:- promise_equivalent_clauses(coerce_not_addition/2).
coerce_not_addition(NotAddn::in(not_addition), coerce(NotAddn)::out).
coerce_not_addition(NotAddn::out, NotAddn::in).

coerce_not_addition(NotAddn) = Expr :- 
	coerce_not_addition(NotAddn, Expr).


%-----------------------------------------------------------------------------%
% multiplication conversions
	
expression_is_multiplication(_::in(multiplication)).
expression_to_multiplication(Multi::in(multiplication), coerce(Multi)).

:- promise_equivalent_clauses(coerce_multiplication/2).
coerce_multiplication(Multi::in(multiplication), coerce(Multi)::out).
coerce_multiplication(Multi::out, Multi::in).

coerce_multiplication(Addn) = Expr :- coerce_multiplication(Addn, Expr).

expression_is_not_multiplication(_::in(not_multiplication)).
expression_to_not_multiplication(NotMulti::in(not_multiplication), 
	coerce(NotMulti)).
	
:- promise_equivalent_clauses(coerce_not_multiplication/2).
coerce_not_multiplication(NotMulti::in(not_multiplication), 
	coerce(NotMulti)::out).
coerce_not_multiplication(NotMulti::out, NotMulti::in).

coerce_not_multiplication(NotMulti) = Expr :- 
	coerce_not_multiplication(NotMulti, Expr).

		

%-----------------------------------------------------------------------------%
% Identity

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
identity(implication(A, B), implication(A, B))).

% A <-> B = A <-> B
identity(iff(A, B), iff(A, B)).

% (A = B) = (A = B)
identity(equal(A, B), equal(A, B)).

% (A != B) = (A != B)
identity(inequal(A, B), inequal(A, B).

% A > B = A > B
identity(greater_than(A, B), greater_than(A, B)).

% A => B = A => B
identity(greater_than_or_equal(A, B), greater_than_or_equal(A, B)).
	
% A < B = A < B
identity(less_than(A, B), less_than(A, B)).

% A =< B = A =< B
identity(greater_than_or_equal(A, B), greater_than_or_equal(A, B)).

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
permutation(divide(A, B), divide(A, B)).

%-----------------------------------------------------------------------------%
% Permutation

:- pragma promise_equivalent_clauses(permutation/2).
:- pragma does_not_terminate(permutation/2).

% A = A
permutation(A, B) :- identity(A, B).

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
permutation(inequal(A, B), inequal(B, A).

% A + B = B + A
permutation(add(A, B), add(B, A)).

% A * B = B * A
permutation(multiply(A, B), multiply(B, A)).

%-----------------------------------------------------------------------------%
% Negated predicates
 
% -p(A) = not p(A)
permutation(negated_predicate(A), negation(predicate(A)).

% not p(A) = -p(A)
permutation(negation(predicate(A)), negated_predicate(A)).

% p(A) = not -p(A)
permutation(predicate(A), negation(negated_predicate(A)).

% not -p(A) = p(A)
permutation(negation(negated_predicate(A), predicate(A)).

%-----------------------------------------------------------------------------%
% Conjunction transformations



% and(A, B) = conj([A, B]).
permutation(and(A, B), conjunction(C)) :- 
	permutation(conjunction([A, B]), conjunction(C)).

%  conj([A, B]) = and(A, B).
permutation(conjunction(A), and(B, C)) :- 
	permutation(and(B, C), conjunction(A)). 

% conj([B, A]) = conj([A, B]).	
permutation(conjunction(A), conjunction(B)) :- 

	
	).
	
% A = conj([A])
permutation(A, conjunction([A])).
 
% conj([A]) = A
permutation(conjunction([A]), A).
	
%-----------------------------------------------------------------------------%
% Disjunction transformations
	
% A = disj([A])
permutation(A, disjunction(C)) :- 
	coerce_not_disjunction(A, B),  singleton_set(B, C).
 
% disj([A]) = A
permutation(disjunction(C), A) :- 
	coerce_disjunction(A, B), singleton_set(B, C).

% or(A, B) = disj([A, B]).
permutation(or(A, B), disjunction(C)) :- 
	singleton_set(A, As),
	singleton_set(B, Bs),
	multi_union(As, Bs, C).
	
% and(A, B) = conj([A, B]).
permutation(disjunction(A), or(B, C)) :- 
	permutation(or(B, C), disjunction(A)). 

%-----------------------------------------------------------------------------%
% De Morgan's laws

% not and(A, B) = or(not A, not B)
permutation(negation(conjunction(A)), disjunction(B)) :-
	negate_set_members(A, B). % TODO TEST May result in type conversion errors 

% or(not A, not B) = not and(A, B)
permutation(disjunction(A), negation(conjunction(B))) :-
	negate_set_members(A, B).
	
% not or(A, B) = and(not A, not B)
permutation(negation(disjunction(A)), conjunction(B)) :-
	negate_set_members(A, B). 
	
% and(not A, not B) = not or(A, B)
permutation(conjunction(A), negation(disjunction(B))) :-
	negate_set_members(A, B).




:- pred negate_set_members(set(logical_expression), set(logical_expression)).
:- mode negate_set_members(in, out) is det.
:- mode negate_set_members(out, in) is det.

:- promise_equivalent_clauses(negate_set_members/2).


negate_set_members(A::in, B::out) :-
	map(negation, A, B).
	
negate_set_members(A::out, B::in) :- negate_set_members(B, A).





%-----------------------------------------------------------------------------%
% Exclusive OR transformation

% xor(A, B) = or(
permutation


	

;		xor(logical_expression, logical_expression)
;		negation(logical_expression)
;		implication(logical_expression, logical_expression)

;		equal(term_expression, term_expression)
;		inequal(term_expression, term_expression)
;		greater_than(term_expression, term_expression)
;		less_than(term_expression, term_expression)

;		term(mh_term) 

;		sum(list(numeric_expression))
;		add(numeric_expression, numeric_expression)
;		subtract(numeric_expression, numeric_expression)

;		product(list(numeric_expression))
;		multiply(numeric_expression, numeric_expression)
;		divide(numeric_expression, numeric_expression) 
where equality is unify_expressions, comparison is compare_expressions.

%-----------------------------------------------------------------------------%	
% Flatten


flatten(Expr, Flat) :-
	require_compete_switch [Expr] (
		Expr = Flat = mh_true
	;
		Expr = Flat = predicate(_)
	;
		Expr = Flat = negated_predicate(_)
	;
		Expr = conjunction(Conjunction),
		Flat = conjunction(flatten_conjunction(Conjunction))
	;
		Expr = and(A, B),
		Flat = conjunction(flatten_conjunction([A, B]))
	;
		Expr = disjunction(Disjunction),
		Flat = disjunction(flatten_disjunction(Disjunction))
	;
		Expr = or(A, B),
		Flat = disjunction(flatten_disjunction([A, B])
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
		Expr = Flat = equal(_, _)
	;
		Expr = Flat = inequal(_, _)
	;
		Expr = Flat = greater_than(_, _)
	;
		Expr = Flat = greater_than_or_equal(_, _)
	;
		Expr = Flat = less_than(_, _)
	;
		Expr = Flat = less_than_or_equal(_, _)
	;
		Expr = Flat = term(_)
	;
		Expr = sum(Sum)
		Flat = sum(flatten_sum(Sum))
	;
		Expr = add(A, B)
		Flat = sum(flatten_sum([A, B])
	;
		Expr = subtract(A, B)
		Flat = subtract(flatten_math(A), flatten_math(B))
	;
		Expr = product(Product)
		Flat = product(flatten_product(Product))
	;
		Expr = multiply(A, B)
		Flat = product(flatten_product([A, B]))
	;
		Expr = divide(A, B)
		Flat = divide(flatten_terms(A), flatten_terms(B))
	).

flatten(Expr) = Flat :- flatten(Expr, Flat).



flatten_logic(Expr, Flat) :- 
	require_compete_switch [Expr] (
		Expr = Flat = mh_true
	;
		Expr = Flat = predicate(_)
	;
		Expr = Flat = negated_predicate(_)
	;
		Expr = conjunction(Conjunction),
		Flat = conjunction(flatten_conjunction(Conjunction))
	;
		Expr = and(A, B),
		Flat = conjunction(flatten_conjunction([A, B]))
	;
		Expr = disjunction(Disjunction),
		Flat = disjunction(flatten_disjunction(Disjunction))
	;
		Expr = or(A, B),
		Flat = disjunction(flatten_disjunction([A, B])
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
		Expr = equal(A, B)
		Flat = equal(flatten_terms(A), flatten_terms(B))
	;
		Expr = inequal(A, B)
		Flat = inequal(flatten_terms(A), flatten_terms(B))
	;
		Expr = greater_than(A, B)
		Flat = greater_than(flatten_terms(A), flatten_terms(B))
	;
		Expr = greater_than_or_equal(A, B)
		Flat = greater_than_or_equal(flatten_terms(A), flatten_terms(B))
	;
		Expr = less_than(A, B)
		Flat = less_than(flatten_terms(A), flatten_terms(B))
	;
		Expr = less_than_or_equal(A, B)
		Flat = less_than_or_equal(flatten_terms(A), flatten_terms(B))
	).
		

:- func flatten_conjunction(list(logical_expression)) 
	= list(logical_expression).
	
:- mode flatten_conjunction(in) = out is det.
:- mode flatten_conjunction(out) = in is multi.

flatten_conjunction([]) = [].

flatten_conjunction([C | Cs]) =  F :-
	C = conjunction(X), 
	F = append(flatten_conjunction(X), flatten_conjunction(Cs))
;
	C /= conjunction(_),
	F = [ flatten_logic(C) | flatten_conjunction(Cs) ].
	

flatten_logic(Expr) = Flat :- flatten_logic(Expr, Flat).

:- func flatten_disjunction(list(logical_expression)) 
	= list(logical_expression).
	
:- mode flatten_disjunction(in) = out is det.
:- mode flatten_disjunction(out) = in is multi.

flatten_disjunction([]) = [].

flatten_disjunction([D | Ds]) =  F :-
	D = disjunction(X),  
	F = append(flatten_disjunction(X), flatten_disjunction(Cs)
;
	D /= disjunction(_),
	F = [ flatten_logic(C) | flatten_disjunction(Cs) ].
	

flatten_logic(Expr) = Flat :- flatten_logic(Expr, Flat).








% see if the type system will let me get away with this without coercion
flatten_terms(Expr, Flat) :- 
	require_compete_switch [Expr] (
		Expr = Flat = term(_)
	;
		Expr = sum(Sum)
		Flat = sum(flatten_sum(Sum))
	;
		Expr = add(A, B)
		Flat = sum(flatten_sum([A, B])
	;
		Expr = subtract(A, B)
		Flat = subtract(flatten_math(A), flatten_math(B))
	;
		Expr = product(Product)
		Flat = product(flatten_product(Product))
	;
		Expr = multiply(A, B)
		Flat = product(flatten_product([A, B]))
	;
		Expr = divide(A, B)
		Flat = divide(flatten_math(A), flatten_math(B))
	).

flatten_terms(Expr) = Flat :- flatten_terms(Expr, Flat).

flatten_math(Expr, Flat) :- 
	require_compete_switch [Expr] (
		Expr = Flat = term(_)
	;
		Expr = sum(Sum)
		Flat = sum(flatten_sum(Sum))
	;
		Expr = add(A, B)
		Flat = sum(flatten_sum([A, B])
	;
		Expr = subtract(A, B)
		Flat = subtract(flatten_math(A), flatten_math(B))
	;
		Expr = product(Product)
		Flat = product(flatten_product(Product))
	;
		Expr = multiply(A, B)
		Flat = product(flatten_product([A, B]))
	;
		Expr = divide(A, B)
		Flat = divide(flatten_math(A), flatten_math(B))
	).
	
flatten_math(Expr) = Flat :- flatten_math(Expr, Flat).


:- func flatten_sum(list(term_expression)) 
	= list(term_expression).
	
:- mode flatten_sum(in) = out is det.
:- mode flatten_sum(out) = in is multi.

flatten_sum([]) = [].

flatten_sum([S | Ss]) =  F :-
	S = sum(X), 
	F = append(flatten_sum(X), flatten_sum(Ss))
;
	S /= sum(_),
	F = [ flatten_terms(S) | flatten_sum(Ss) ].
	
	
:- func flatten_product(list(term_expression)) 
	= list(term_expression).
	
:- mode flatten_product(in) = out is det.
:- mode flatten_product(out) = in is multi.

flatten_product([]) = [].

flatten_product([P | Ps]) =  F :-
	P = product(X), 
	F = append(flatten_product(X), flatten_product(Ps))
;
	P /= product(_),
	F = [ flatten_terms(P) | flatten_product(Cs) ].