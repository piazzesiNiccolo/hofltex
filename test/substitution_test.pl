:- use_module(library(plunit)).

:- begin_tests(subst).
:- use_module("../src/substitution").

test(subst_int,[true(T = int(1))]):-
    subst(int(1),var(x),var(y),T).

test(subst_var_with_same_name,[true(T=var(y))]):-
    subst(var(x),var(x),var(y),T).

test(subst_var_with_different_name,[true(T=var(x))]):-
    subst(var(x),var(y),var(z),T).

test(subst_add,[true(T=bin_op("+",var(y),var(z)))]):-
    subst(bin_op("+",var(x),var(z)),var(x),var(y),T).

test(subst_mul,[true(T=mul(var(x),var(z)))]):-
    subst(mul(var(x),var(y)),var(y),var(z),T).

test(subst_minus,[true(T=bin_op("-",int(1),var(z)))]):-
    subst(bin_op("-",int(1),var(z)),var(x),var(y),T).

test(subst_cond,[true(T=cond(var(b),int(1),int(2)))]):-
    subst(cond(var(a),int(1),int(2)),var(a),var(b),T).

test(subst_tuple,[true(T=tuple(var(z),tuple(var(z),var(y))))]):-
    subst(tuple(var(x),tuple(var(x),var(y))),var(x),var(z),T).

test(subst_fst,[true(T=fst(tuple(var(x),var(z))))]):-
    subst(fst(tuple(var(x),var(y))),var(y),var(z),T).

test(subst_snd,[true(T=snd(tuple(var(z),var(y))))]):-
    subst(snd(tuple(var(x),var(y))),var(x),var(z),T).

test(subst_lambda,[true(T=lambda(var(a), lambda(var(b), bin_op("+",var(a), bin_op("+",var(b), int(2))))))]):-
    subst(lambda(var(x),lambda(var(y), bin_op("+",var(x), bin_op("+",var(y), int(2))))),
            var(x),
            var(y),
            T).


test(subst_rec,[true(T=rec(var(a),var(a)))]):-
    subst(rec(var(x),var(x)),var(x),var(y),T).

test(subst_apply,[true(T=apply(lambda(var(a), var(a)), int(2)))]):-
    subst(apply(lambda(var(x),var(x)),int(2)),var(x),int(2),T).

:- end_tests(subst).