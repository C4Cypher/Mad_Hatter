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
:- import_module set.

:- type expression
--->	predicate(functor)
;		negated_predicate(functor)

;		mh_true
;		mh_false

;		conjunction(set(not_conjunction))
; 		and(logical_expression, logical_expression)

;		disjunction(set(not_disjunction))
;		or(logical_expression, logical_expression)

;		xor(logical_expression, logical_expression)
;		negation(not_negation)
;		implication(logical_expression, logical_expression)

;		equal(term_expression, term_expression)
;		inequal(term_expression, term_expression)
;		greater_than(term_expression, term_expression)
;		less_than(term_expression, term_expression)

;		term(mh_term) 

;		sum(set(not_addition))
;		add(numeric_expression, numeric_expression)
;		subtract(numeric_expression, numeric_expression)

;		product(set(not_multiplication))
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

;		xor(ground, ground)
;		negation(ground)
;		implication(ground, ground)

;		equal(ground, ground)
;		inequal(ground, ground)
;		greater_than(ground, ground)
;		less_than(ground, ground).

:- type logical_expression =< expression
--->	predicate(functor)
;		negated_predicate(functor)

;		mh_true
;		mh_false

;		conjunction(set(not_conjunction))
; 		and(logical_expression, logical_expression)

;		disjunction(set(not_disjunction))
;		or(logical_expression, logical_expression)

;		xor(logical_expression, logical_expression)
;		negation(not_negation)
;		implication(logical_expression, logical_expression)

;		equal(term_expression, term_expression)
;		inequal(term_expression, term_expression)
;		greater_than(term_expression, term_expression)
;		less_than(term_expression, term_expression).

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

:- inst negation ---> negation(logical_expression).

:- type negation =< logical_expression ---> negation(logical_expression).

:- pred expression_is_negation(expression::in) is semidet.
:- pred expression_to_negation(expression::in, negation::out) is semidet.

:- pred coerce_negation(expression, negation).
:- mode coerce_negation(in, out) is semidet.
:- mode coerce_negation(out, in) is det.

:- func coerce_negation(expression) = negation.
:- mode coerce_negation(in) = out is semidet.
:- mode coerce_negation(out) = in is det.

:- pred negate_expression(logical_expression, logical_expression).
:- mode negate_expression(in, in) is semidet.
:- mode negate_expression(in, out) is det.
:- mode negate_expression(out, in) is det.

:- func negate_expression(logical_expression) = logical_expression.
:- mode negate_expression(in) = in is semidet.
:- mode negate_expression(in) = out is det.
:- mode negate_expression(out) = in is det.

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

;		equal(ground, ground)
;		inequal(ground, ground)
;		greater_than(ground, ground)
;		less_than(ground, ground).

:- type not_negation =< logical_expression
--->	predicate(functor)
;		negated_predicate(functor)

;		mh_true
;		mh_false

;		conjunction(set(not_conjunction))
; 		and(logical_expression, logical_expression)

;		disjunction(set(not_disjunction))
;		or(logical_expression, logical_expression)

;		xor(logical_expression, logical_expression)
;		implication(logical_expression, logical_expression)

;		equal(term_expression, term_expression)
;		inequal(term_expression, term_expression)
;		greater_than(term_expression, term_expression)
;		less_than(term_expression, term_expression).

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

:- type conjunction =< logical_expression
--->		conjunction(set(not_conjunction))
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

;		xor(ground, ground)
;		negation(ground)
;		if(ground, ground)

;		equal(ground, ground)
;		inequal(ground, ground)
;		greater_than(ground, ground)
;		less_than(ground, ground).

:- type not_conjunction =< logical_expression
--->	predicate(functor)
;		negated_predicate(functor)

;		mh_true
;		mh_false

;		disjunction(set(not_disjunction))
;		or(logical_expression, logical_expression)

;		xor(logical_expression, logical_expression)
;		negation(logical_expression)
;		implication(logical_expression, logical_expression).

;		equal(term_expression, term_expression)
;		inequal(term_expression, term_expression)
;		greater_than(term_expression, term_expression)
;		less_than(term_expression, term_expression).

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

:- type disjunction =< logical_expression
--->		disjunction(set(not_disjunction))
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

;		xor(ground, ground)
;		negation(ground)
;		if(ground, ground)

;		equal(ground, ground)
;		inequal(ground, ground)
;		greater_than(ground, ground)
;		less_than(ground, ground).

:- type not_disjunction =< logical_expression
--->	predicate(functor)
;		negated_predicate(functor)

;		mh_true
;		mh_false

;		conjunction(set(not_conjunction))
; 		and(logical_expression, logical_expression)

; 		xor(logical_expression, logical_expression)
;		negation(logical_expression)
;		implication(logical_expression, logical_expression).

;		equal(term_expression, term_expression)
;		inequal(term_expression, term_expression)
;		greater_than(term_expression, term_expression)
;		less_than(term_expression, term_expression).

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

:- type term_expression =< expression
--->	term(mh_term) 

;		sum(set(not_addition))
;		add(numeric_expression, numeric_expression)
;		subtract(numeric_expression, numeric_expression)

;		product(set(not_multiplication))
;		multiply(numeric_expression, numeric_expression)
;		divide(numeric_expression, numeric_expression).

%-----------------------------------------------------------------------------%


:- inst numeric_expression
--->	term(numeric_term) 

;		sum(set(not_addition))
;		add(numeric_expression, numeric_expression)
;		subtract(numeric_expression, numeric_expression)

;		product(set(not_multiplication))
;		multiply(numeric_expression, numeric_expression)
;		divide(numeric_expression, numeric_expression). 

:- type numeric_expression =< term_expression
--->	term(numeric_term) 

;		sum(set(not_addition))
;		add(numeric_expression, numeric_expression)
;		subtract(numeric_expression, numeric_expression)

;		product(set(not_multiplication))
;		multiply(numeric_expression, numeric_expression)
;		divide(numeric_expression, numeric_expression). 
%-----------------------------------------------------------------------------%

:- inst addition 
 --->		sum(ground)
; 			add(ground, ground).

:- type addition =< numeric_expression
--->	sum(set(not_addition))
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

;		product(set(not_multiplication))
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
--->	product(set(not_multiplication))
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
--->	sum(set(not_multiplication))
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
	
:- pred permutation(expression, expression).
:- mode permutation(in, out) is multi.
:- mode permutation(out, in) is multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module set_util.m.


unify_expressions(X, Y) :- permutation(X, Y).

compare_expressions(Result, X, Y) :-
	if permutation(X, Y) then
		Result = (=)
	else
		compare(Result, X, Y).

%-----------------------------------------------------------------------------%
		

% p(A) = p(A)		
permutation(predicate(A), predicate(A)).

%-----------------------------------------------------------------------------%

% -p(A) = -p(A)
permutation(negated_predicate(A), negated_predicate(A)).

%-----------------------------------------------------------------------------%
% Identities

% true = true
permutation(mh_true, mh_true).
 
% false = false
permutation(mh_false, mh_false).

% conj(A) = conj(A).
permutation(conjunction(A), conjunction(A)).

% and(A, B) = and(A, B)
permutation(and(A, B), and(A, B)).

% and(A, B) = and(B, A).
permutation(and(A, B), and(B, A)).

% disj(A) = disj(A).
permutation(disjunction(A), disjunction(A)).

% or(A, B) = or(A, B)
permutation(or(A, B), or(A, B)).

% or(A, B) = or(B, A).
permutation(or(A, B), or(B, A)).

% not(A) = not(A).
permutation(negation(A), negation(A)).

% xor(A, B) = xor(A, B)
permutation(xor(A, B), xor(A, B)).

% xor(A, B) = xor(B, A)
permutation(xor(A, B), xor(B, A)).

% A :- B = A :- B
permutation(implies(A, B), implies(A, B))).

% (A = B) = (A = B)
permutation(equal(A, B), equal(A, B)).

% (A = B) = (B = A)
permutation(equal(A, B), equal(B, A)).

% (A != B) = (A != B)
permutation(inequal(A, B), inequal(A, B).

% (A != B) = (B != A)
permutation(inequal(A, B), inequal(B, A).

% A > B = A > B
permutation(greater_than(A, B), greater_than(A, B)).

% A < B = A < B
permutation(less_than(A, B), less_than(A, B)).

% A = A
permutation(term(A), term(A)).

% sum(A) = sum(A)
permutation(sum(A), sum(A)).

% A + B = A + B
permutation(add(A, B), add(A, B)).

% A + B = B + A
permutation(add(A, B), add(B, A)).

% A - B = A - B
permutation(subtract(A, B), subtract(A, B)).

% product(A) = product(A)
permutation(product(A), product(A)).

% A * B = A * B
permutation(multiply(A, B), multiply(A, B)).

% A * B = B * A
permutation(multiply(A, B), multiply(B, A)).

% A / B = A / B
permutation(divide(A, B), divide(A, B)).


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

% A = conj([A])
permutation(A, conjunction(C)) :- 
	coerce_not_conjunction(A, B),  singleton_set(B, C).
 
% conj([A]) = A
permutation(conjunction(C), A) :- 
	coerce_negation(A, B), singleton_set(B, C).

% and(A, B) = conj([A, B]).
permutation(and(A, B), conjunction(C)) :- 

	coerce_conjunction(A, X),
	(
		% and(A, B) = conj([A, B]).
		coerce_not_conjunction(B, Y),
		singleton_set(X, As),
		singleton_set(Y, Bs),
		nondet_union(As, Bs, C)
	;
		% and(A, conj(B)) = insert(A, B).
		coerce_conjunction(B, negation(Y)),
		nondet_insert(X, Y, C)
	)
;
	coerce_conjunction(A, conjunction(X)),
	(
		% and(conj(A), B) = insert(B, A).
		coerce_not_conjunction(B, Y),
		nondet_insert(Y, X, C)
	;
		% and(conj(A), conj(B)) = union(A, B).
		coerce_conjunction(B, conjunction(Y)),
		nondet_union(X, Y, C)
	).
	
% and(A, B) = conj([A, B]).
permutation(conjunction(A), and(B, C)) :- 
	permutation(and(B, C), conjunction(A)). 
	
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

	coerce_not_disjunction(A, X),
	(
		% or(A, B) = disj([A, B]).
		coerce_not_disjunction(B, Y),
		singleton_set(X, As),
		singleton_set(Y, Bs),
		nondet_union(As, Bs, C)
	;
		% or(A, disj(B)) = insert(A, B).
		coerce_disjunction(B, disjunction(Y)),
		nondet_insert(X, Y, C)
	)
;
	coerce_disjunction(A, disjunction(X)),
	(
		% or(disj(A), B) = insert(B, A).
		coerce_not_disjunction(B, Y),
		nondet_insert(Y, X, C)
	;
		% or(disj(A), disj(B)) = union(A, B).
		coerce_disjunction(B, disjunction(Y)),
		nondet_union(X, Y, C)
	).
	
% and(A, B) = conj([A, B]).
permutation(disjunction(A), or(B, C)) :- 
	permutation(or(B, C), disjunction(A)). 
	

%-----------------------------------------------------------------------------%
% Exclusive OR


	

;		xor(logical_expression, logical_expression)
;		negation(logical_expression)
;		implication(logical_expression, logical_expression)

;		equal(term_expression, term_expression)
;		inequal(term_expression, term_expression)
;		greater_than(term_expression, term_expression)
;		less_than(term_expression, term_expression)

;		term(mh_term) 

;		sum(set(not_addition))
;		add(numeric_expression, numeric_expression)
;		subtract(numeric_expression, numeric_expression)

;		product(set(not_multiplication))
;		multiply(numeric_expression, numeric_expression)
;		divide(numeric_expression, numeric_expression) 
where equality is unify_expressions, comparison is compare_expressions.

%-----------------------------------------------------------------------------%

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


:- promise_equivalent_clauses(negate_expression/2).

negate_expression(A::in(not_negation), negation(A)::in).
negate_expression(negation(A)::in, A::in(not_negation)).


negate_expression(A::in(not_negation), negation(coerce(A))::out).
negate_expression(negation(A)::in, A::out).

neggate_expression(A::out, negation(A)::in).
negate_expression(negation(coerce(A))::out, A::in(not_negation)).

%-----------------------------------------------------------------------------%

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