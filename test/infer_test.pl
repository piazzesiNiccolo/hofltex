:- use_module(library(plunit)).

:- begin_tests(infer).

:- use_module("../src/infer").

test(infer_int,[true(T=int(1))]):-
    derive(_,red(int(1),T)).

test(infer_lambda,[true(T = lambda(var(x),var(x)))]):-
    derive(_,red(lambda(var(x),var(x)),T)).

test(infer_lambda_with_free_variables,fail):-
    derive(_,red(lambda(var(x),var(y)),_)).

test(infer_add,[true(T = int(5))]) :-
    derive(_,red(add(int(2),int(3)),T)).

test(infer_add_fail_if_not_number,fail) :-
    derive(_,red(add(int(2),var(x)),_)).

test(infer_mul,[true(T = int(-10))]) :-
    derive(_,red(mul(int(2),int(-5)),T)).

test(infer_mul_fail_if_not_number,fail) :-
    derive(_,red(mul(int(2),var(x)),_)).

test(infer_minus,[true(T = int(0))]) :-
    derive(_,red(minus(int(100),int(100)),T)).

test(infer_minus_fail_if_not_number,fail) :-
    derive(_,red(minus(int(2),var(x)),_)).



:-end_tests(infer).
