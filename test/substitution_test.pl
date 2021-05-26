:- use_module(library(plunit)).

:- begin_tests(subst).
:- use_module("../src/substitution").

test(subst_int,[true(T = int(1))]):-
    subst(int(1),id(x),id(y),T).

test(subst_var_with_same_name,[true(T=id(y))]):-
    subst(id(x),id(x),id(y),T).

test(subst_var_with_different_name,[true(T=id(x))]):-
    subst(id(x),id(y),id(z),T).

test(subst_add,[true(T=bin_op("+",id(y),id(z)))]):-
    subst(bin_op("+",id(x),id(z)),id(x),id(y),T).

test(subst_mul,[true(T=mul(id(x),id(z)))]):-
    subst(mul(id(x),id(y)),id(y),id(z),T).

test(subst_minus,[true(T=bin_op("-",int(1),id(z)))]):-
    subst(bin_op("-",int(1),id(z)),id(x),id(y),T).

test(subst_cond,[true(T=cond(id(b),int(1),int(2)))]):-
    subst(cond(id(a),int(1),int(2)),id(a),id(b),T).

test(subst_tuple,[true(T=tuple(id(z),tuple(id(z),id(y))))]):-
    subst(tuple(id(x),tuple(id(x),id(y))),id(x),id(z),T).

test(subst_fst,[true(T=fst(tuple(id(x),id(z))))]):-
    subst(fst(tuple(id(x),id(y))),id(y),id(z),T).

test(subst_snd,[true(T=snd(tuple(id(z),id(y))))]):-
    subst(snd(tuple(id(x),id(y))),id(x),id(z),T).

test(subst_lambda,[true(T=lambda(id(a), lambda(id(b), bin_op("+",id(a), bin_op("+",id(b), int(2))))))]):-
    subst(lambda(id(x),lambda(id(y), bin_op("+",id(x), bin_op("+",id(y), int(2))))),
            id(x),
            id(y),
            T).


test(subst_rec,[true(T=rec(id(a),id(a)))]):-
    subst(rec(id(x),id(x)),id(x),id(y),T).

test(subst_apply,[true(T=apply(lambda(id(a), id(a)), int(2)))]):-
    subst(apply(lambda(id(x),id(x)),int(2)),id(x),int(2),T).

:- end_tests(subst).