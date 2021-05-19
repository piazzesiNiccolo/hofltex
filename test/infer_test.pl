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
    derive(_,red(bin_op("+",int(2),int(3)),T)).

test(infer_add_fail_if_not_number,fail) :-
    derive(_,red(bin_op("+",int(2),var(x)),_)).

test(infer_mul,[true(T = int(-10))]) :-
    derive(_,red(mul(int(2),int(-5)),T)).

test(infer_mul_fail_if_not_number,fail) :-
    derive(_,red(mul(int(2),var(x)),_)).

test(infer_minus,[true(T = int(0))]) :-
    derive(_,red(bin_op("-",int(100),int(100)),T)).

test(infer_minus_fail_if_not_number,fail) :-
    derive(_,red(bin_op("-",int(2),var(x)),_)).

test(tuple_with_closed_terms,[true(T = tuple(int(1),int(2)))]):-
    derive(_,red(tuple(int(1),int(2)),T)).

test(tuple_with_open_term,fail):-
    derive(_,red(tuple(var(x),int(1)))).

test(fst_with_tuple,[true(T=int(1))]):-
    derive(_,red(fst(tuple(int(1),var(x))),T)).

test(fst_without_tuple,fail):-
    derive(_,red(fst(var(x)),_)).

test(snd_with_tuple,[true(T=int(1))]):-
    derive(_,red(snd(tuple(var(x),int(1))),T)).

test(snd_without_tuple,fail):-
    derive(_,red(snd(var(x)),_)).

test(cond_true_term_with_canonical_form,[true(T = int(5))]):-
    derive(_,red(cond(int(0),bin_op("+",int(2),int(3)),var(x)),T)).

test(cond_true_term_without_canonical_form,fail):-
    derive(_,red(cond(int(0),var(x),_),_)).


test(cond_false_term_with_canonical_form,[true(T = int(5))]):-
    derive(_,red(cond(int(1),var(x),bin_op("+",int(2),int(3))),T)).

test(cond_false_term_without_canonical_form,fail):-
    derive(_,red(cond(int(1),_,var(x)),_)).


:-end_tests(infer).
