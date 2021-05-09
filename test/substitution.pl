:- use_module(library(plunit)).

:- begin_tests(subst).
:- use_module("../src/substitution").

test(subst_int,[true(T = int(1))]):-
    subst(int(1),x,y,T).

test(subst_variable_with_same_name,[true(T=variable(y))]):-
    subst(variable(x),x,y,T).

test(subst_variable_with_different_name,[true(T=variable(x))]):-
    subst(variable(x),y,z,T).

test(subst_add,[true(T=add(variable(y),variable(z)))]):-
    subst(add(variable(x),variable(z)),x,y,T).

test(subst_mul,[true(T=mul(variable(x),variable(z)))]):-
    subst(mul(variable(x),variable(y)),y,z,T).

test(subst_minus,[true(T=minus(int(1),variable(z)))]):-
    subst(minus(int(1),variable(z)),x,y,T).

test(subst_cond,[true(T=cond(variable(b),int(1),int(2)))]):-
    subst(cond(variable(a),int(1),int(2)),a,b,T).

test(subst_tuple,[true(T=tuple(variable(z),tuple(variable(z),variable(y))))]):-
    subst(tuple(variable(x),tuple(variable(x),variable(y))),x,z,T).

test(subst_fst,[true(T=tuple(variable(x),variable(z)))]):-
    subst(tuple(variable(x),variable(y)),y,z,T).

test(subst_snd,[true(T=tuple(variable(z),variable(y)))]):-
    subst(tuple(variable(x),variable(y)),x,z,T).
    


:- end_tests(subst).