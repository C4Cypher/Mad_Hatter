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
:- pred expression_to_conjunction(expression::in, conjunction::out) is 
	cc_nondet.

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

:- inst disjunction(T)
 --->		disjunction(list(T))
; 			or(T, T).

:- type disjunction =< logical_expression
--->		disjunction(list(logical_expression))
; 			or(logical_expression, logical_expression).

:- pred expression_is_disjunction(expression::in) is semidet.
:- pred expression_to_disjunction(expression::in, disjunction::out) is 
	cc_nondet.

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

:- pred expression_is_term_expression(expression::in) is semidet.
:- pred expression_to_term_expression(expression::in, term_expression::out) 
	is cc_nondet.

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

:- type numeric_expression =< term_expression
--->	term(numeric_term) 

;		sum(list(numeric_expression))
;		add(numeric_expression, numeric_expression)
;		subtract(numeric_expression, numeric_expression)

;		product(list(numeric_expression))
;		multiply(numeric_expression, numeric_expression)
;		divide(numeric_expression, numeric_expression). 

:- pred expression_is_numeric_expression(expression::in) is semidet.
:- pred expression_to_numeric_expression(expression::in, 
	numeric_expression::out) is semidet.

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




%-----------------------------------------------------------------------------%

:- inst multiplication 
--->	product(ground)
;		multiply(ground, ground).

:- type multiplication =< term_expression
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



%-----------------------------------------------------------------------------%

:- pred permutation(expression, expression).
:- mode permutation(in, out) is multi.
:- mode permutation(out, in) is multi.

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

:- pred flatten(expression, expression).
:- mode flatten(in, out) is det.
:- mode flatten(out, in) is multi.

:- func flatten(expression) = expression.
:- mode flatten(in) = out is det.
:- mode flatten(out) = in is multi.

:- pred flatten_list(list(expression), list(expression)).
:- mode flatten_list(in, out) is det.
:- mode flatten_list(out, in) is multi.

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

:- pred flatten_term_expression(term_expression, term_expression).
:- mode flatten_term_expression(in, out) is det.
:- mode flatten_term_expression(out, in) is multi.

:- func flatten_term_expression(term_expression) = term_expression.
:- mode flatten_term_expression(in) = out is det.
:- mode flatten_term_expression(out) = in is multi.



:- pred flatten_numeric_expression(numeric_expression, numeric_expression).
:- mode flatten_numeric_expression(in, out) is det.
:- mode flatten_numeric_expression(out, in) is multi.

:- func flatten_numeric_expression(numeric_expression) = numeric_expression.
:- mode flatten_numeric_expression(in) = out is det.
:- mode flatten_numeric_expression(out) = in is multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list_util.
:- import_module require.


		
%-----------------------------------------------------------------------------%
% logical_expression conversions

expression_is_logical_expression(A) :-
	A = predicate(_);
	A = negated_predicate(_);
	A = mh_true;
	A = mh_false;
	expression_is_conjunction(A);
	expression_is_disjunction(A);
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


expression_to_logical_expression(LogiExpr, coerce(LogiExpr)).

:- pragma promise_equivalent_clauses(coerce_logical_expression/2).

coerce_logical_expression(LogiExpr::in,	coerce(LogiExpr)::out) :- 
	expression_is_logical_expression(LogiExpr).

coerce_logical_expression(coerce(LogiExpr)::out, LogiExpr::in).

coerce_logical_expression(LogiExpr) = Expr :- 
	coerce_logical_expression(LogiExpr, Expr).



%-----------------------------------------------------------------------------%
% negation conversions

expression_is_negation(negation(_)).

expression_to_negation(Negation, coerce(Negation)) :- 
	expression_is_negation(Negation).

:- pragma promise_equivalent_clauses(coerce_negation/2).

coerce_negation(Negation::in, coerce(Negation)::out) :- 
	expression_is_negation(Negation).

coerce_negation(coerce(Negation)::out, Negation::in).

coerce_negation(Negation) = Expr :- coerce_negation(Negation, Expr).

expression_is_not_negation(NotNeg) :- not expression_is_negation(NotNeg).


negation(A::in, B::out) :- 
	if expression_is_negation(coerce(A)) then
		A = negation(X),
		B = X
	else
		B = negation(A).

negation(A::out, B::in) :- negation(B, A).


negation_of(A) = B :- negation(A, B).

negate_list([], []).

negate_list([ X | A], [ negation_of(X) | B]) :- negate_list(A, B).

negate_list(A) = B :- negate_list(A, B).

%-----------------------------------------------------------------------------%
% conjunction conversions

expression_is_conjunction(conjunction(_)).
expression_is_conjunction(and(_, _)).

expression_to_conjunction(conjunction(A), coerce(conjunction(A):conjunction)).
expression_to_conjunction(and(A, B), coerce(and(A,B):conjunction)).

:- pragma promise_equivalent_clauses(coerce_conjunction/2).

coerce_conjunction(Conj::in, coerce(Conj)::out) :- 
	expression_is_conjunction(Conj). 
	
coerce_conjunction(coerce(Conj)::out, Conj::in).


coerce_conjunction(Conj) = Expr :- coerce_conjunction(Conj, Expr).

	
%-----------------------------------------------------------------------------%
% disjunction conversions

expression_is_disjunction(disjunction(_)).
expression_is_disjunction(or(_, _)).

expression_to_disjunction(disjunction(A), coerce(disjunction(A):disjunction)).
expression_to_disjunction(or(A, B), coerce(or(A, B):disjunction)).

:- pragma promise_equivalent_clauses(coerce_disjunction/2).
coerce_disjunction(Disj::in, coerce(Disj)::out).
coerce_disjunction(coerce(Disj)::out, Disj::in).

coerce_disjunction(Disj) = Expr :- 
	coerce_disjunction(Disj, Expr).

%-----------------------------------------------------------------------------%
% term expression conversions

expression_is_term_expression(TermExp) :-
	TermExp = term(_);
	expression_is_addition(TermExp);
	TermExp = subtract(_, _);
	expression_is_multiplication(TermExp);
	TermExp = divide(_, _).
	

expression_to_term_expression(TermExp, coerce(TermExp)) :-
	expression_is_term_expression(TermExp).

:- pragma promise_equivalent_clauses(coerce_term_expression/2).
coerce_term_expression(TermExp::in, coerce(TermExp)::out) :-
	expression_is_term_expression(TermExp).
	
coerce_term_expression(coerce(TermExp)::out, TermExp::in).

coerce_term_expression(TermExp) = Expr :- 
	coerce_term_expression(TermExp, Expr).
	
%-----------------------------------------------------------------------------%
% numeric expression conversions

expression_is_numeric_expression(NumExp) :-
	NumExp = term(var(_));
	NumExp = term(named_var(_));
	NumExp = term(int(_));
	NumExp = term(float(_));
	NumExp = term(f(_, _));
	expression_is_addition(NumExp);
	NumExp = subtract(_, _);
	expression_is_multiplication(NumExp);
	NumExp = divide(_, _).
	

expression_to_numeric_expression(NumExp, coerce(NumExp)) :-
	expression_is_term_expression(NumExp).

:- pragma promise_equivalent_clauses(coerce_numeric_expression/2).
coerce_numeric_expression(NumExp::in, coerce(NumExp)::out) :-
	expression_is_numeric_expression(NumExp).
	
coerce_numeric_expression(coerce(NumExp)::out, NumExp::in).

coerce_numeric_expression(NumExp) = Expr :- 
	coerce_numeric_expression(NumExp, Expr).
	
%-----------------------------------------------------------------------------%
% addition conversions
	
expression_is_addition(sum(_)).
expression_is_addition(add(_, _)).

expression_to_addition(Addition, coerce(Addition)) :- 
	expression_is_addition(Addition).

:- pragma promise_equivalent_clauses(coerce_addition/2).
coerce_addition(Addition::in, coerce(Addition)::out) :- 
	expression_is_addition(Addition).
coerce_addition(coerce(Addition)::out, Addition::in).

coerce_addition(Addn) = Expr :- coerce_addition(Addn, Expr).




%-----------------------------------------------------------------------------%
% multiplication conversions
	
expression_is_multiplication(Multi) :-
	Multi = product(_);
	Multi = multiply(_, _).


expression_to_multiplication(Multi, coerce(Multi)) :- 
	expression_is_multiplication(Multi).

:- pragma promise_equivalent_clauses(coerce_multiplication/2).
coerce_multiplication(Multi::in, coerce(Multi)::out).
coerce_multiplication(coerce(Multi)::out, Multi::in).

coerce_multiplication(Multi) = Expr :- coerce_multiplication(Multi, Expr).

	

%-----------------------------------------------------------------------------%
% Permutation

:- pragma promise_equivalent_clauses(permutation/2).

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
permutation(and(A, B), conjunction(C)) :- 
	permutation(conjunction([A, B]), conjunction(C)).

%  conj([A, B]) = and(A, B).
permutation(conjunction(A), and(B, C)) :- 
	permutation(and(B, C), conjunction(A)). 

% conj([B, A]) = conj([A, B]).	
permutation(conjunction(A), conjunction(B)) :- 
	unify_unordered(flatten_conjunction(A), flatten_conjunction(B)).
		
%-----------------------------------------------------------------------------%
% Disjunction transformations
	
% or(A, B) = disj([A, B]).
permutation(or(A, B), disjunction(C)) :- 
	permutation(disjunction([A, B]), disjunction(C)).

%  disj([A, B]) = or(A, B).
permutation(disjunction(A), or(B, C)) :- 
	permutation(or(B, C), disjunction(A)). 

% disj([B, A]) = disj([A, B]).	
permutation(disjunction(A), disjunction(B)) :- 
	unify_unordered(flatten_disjunction(A), flatten_disjunction(B)).

%-----------------------------------------------------------------------------%
% De Morgan's laws

% not and(A, B) = or(not A, not B)
permutation(negation(conjunction(A)), disjunction(B)) :-
	negate_list(A, B). 

% or(not A, not B) = not and(A, B)
permutation(disjunction(A), negation(conjunction(B))) :-
	negate_list(A, B).
	
% not or(A, B) = and(not A, not B)
permutation(negation(disjunction(A)), conjunction(B)) :-
	negate_list(A, B). 
	
% and(not A, not B) = not or(A, B)
permutation(conjunction(A), negation(disjunction(B))) :-
	negate_list(A, B).
	
%-----------------------------------------------------------------------------%
% Double negation transformations

/* Permutation will not terminate with this implementation of double negation

% A = not not A
permutation(A::in, negation(negation(B))::out) :- 
	expression_is_not_negation(A),
	coerce_logical_expression(A, B).
	
permutation(coerce(A)::out, negation(negation(A))::in).

% not not A = A
permutation(negation(negation(A))::in, coerce(A)::out).

permutation(negation(negation(A))::out, B::in) :- 
	expression_is_not_negation(B),
	coerce_logical_expression(B, A).
	
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
permutation(add(A, B), sum(C)) :- 
	permutation(sum([A, B]), sum(C)).

%  sum([A, B]) = add(A, B).
permutation(sum(A), add(B, C)) :- 
	permutation(add(B, C), sum(A)). 

% sum([B, A]) = sum([A, B]).	
permutation(sum(A), sum(B)) :- 
	unify_unordered(flatten_sum(A), flatten_sum(B)).
		
%-----------------------------------------------------------------------------%
% Multiplication transformations

% multiply(A, B) = product([A, B]).
permutation(multiply(A, B), product(C)) :- 
	permutation(product([A, B]), product(C)).

%  product([A, B]) = multiply(A, B).
permutation(product(A), multiply(B, C)) :- 
	permutation(multiply(B, C), product(A)). 

% product([B, A]) = product([A, B]).	
permutation(product(A), product(B)) :- 
	unify_unordered(flatten_product(A), flatten_product(B)).


% End permutation/2
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%	
% Flatten


flatten(Expr, Flat) :-
	 
	(
		Expr = mh_true,
		Flat = mh_true
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
		Flat = conjunction(flatten_conjunction([A, B]))
	;
		Expr = disjunction(Disjunction),
		Flat = disjunction(flatten_disjunction(Disjunction))
	;
		Expr = or(A, B),
		Flat = disjunction(flatten_disjunction([A, B]))
	;
		Expr = negation(X), not(X = negation(_)),
		Flat = negation(flatten_logic(X))
	;
		Expr = negation(negation(X)),
		Flat = coerce(flatten_logic(X))
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
		Flat = equal(flatten_term_expression(A), flatten_term_expression(B))
	;
		Expr = equal(A, B) ,
		Flat = inequal(flatten_term_expression(A), flatten_term_expression(B))
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
		Flat = conjunction(flatten_conjunction([A, B]))
	;
		Expr = disjunction(Disjunction),
		Flat = disjunction(flatten_disjunction(Disjunction))
	;
		Expr = or(A, B),
		Flat = disjunction(flatten_disjunction([A, B]))
	;
		Expr = negation(X), not(X = negation(_)),
		Flat = negation(flatten_logic(X))
	;
		Expr = negation(negation(X)),
		Flat = flatten_logic(X)
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
		Flat = equal(flatten_term_expression(A), flatten_term_expression(B))
	;
		Expr = inequal(A, B),
		Flat = inequal(flatten_term_expression(A), flatten_term_expression(B))
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
	
flatten_logic_list([], []).

flatten_logic_list([X | Xs], [ flatten_logic(X) | flatten_logic_list(Xs)] ).

flatten_logic_list(X) = Y :- flatten_logic_list(X, Y).
		

:- func flatten_conjunction(list(logical_expression)) 
	= list(logical_expression).
	
:- mode flatten_conjunction(in) = out is det.
:- mode flatten_conjunction(out) = in is multi.

flatten_conjunction([]) = [].

flatten_conjunction([C | Cs]) =  F :-
	C = conjunction(X), 
	F = append(flatten_conjunction(X), flatten_conjunction(Cs))
;
	C \= conjunction(_),
	F = [ flatten_logic(C) | flatten_conjunction(Cs) ].
	

flatten_logic(Expr) = Flat :- flatten_logic(Expr, Flat).

:- func flatten_disjunction(list(logical_expression)) 
	= list(logical_expression).
	
:- mode flatten_disjunction(in) = out is det.
:- mode flatten_disjunction(out) = in is multi.

flatten_disjunction([]) = [].

flatten_disjunction([D | Ds]) =  F :-
	D = disjunction(X),  
	F = append(flatten_disjunction(X), flatten_disjunction(Ds))
;
	D \= disjunction(_),
	F = [ flatten_logic(D) | flatten_disjunction(Ds) ].
	

flatten_logic(Expr) = Flat :- flatten_logic(Expr, Flat).








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
:- mode flatten_sum(out) = in is multi.

flatten_sum([]) = [].

flatten_sum([S | Ss]) =  F :-
	S = sum(X), 
	F = append(flatten_sum(X), flatten_sum(Ss))
;
	S \= sum(_),
	F = [ flatten_numeric_expression(S) | flatten_sum(Ss) ].
	
	
:- func flatten_product(list(numeric_expression)) 
	= list(numeric_expression).
	
:- mode flatten_product(in) = out is det.
:- mode flatten_product(out) = in is multi.

flatten_product([]) = [].

flatten_product([P | Ps]) =  F :-
	P = product(X), 
	F = append(flatten_product(X), flatten_product(Ps))
;
	P \= product(_),
	F = [ flatten_numeric_expression(P) | flatten_product(Cs) ].