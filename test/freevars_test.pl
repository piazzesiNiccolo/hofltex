:- use_module(library(plunit)).
:-begin_tests(freevars).

:- use_module("../src/freevars").

test(freevars_int,[true(FV==[])]):-
    freevars(int(1),FV).

test(freevars_var,[true(FV==[x])]) :-
    freevars(variable(x),FV).

test(freevars_add,[true(FV==[x,y])]) :-
    freevars(add(variable(x),variable(y)),FV).

test(freevars_minus,[true(FV==[x])]) :-
    freevars(minus(variable(x),int(1)),FV).

test(freevars_mul,[true(FV==[x,y,z])]) :-
    freevars(mul(variable(x),add(variable(y),variable(z))),FV).

test(freevars_tuple,[true(FV==[])]) :-
    freevars(tuple(int(_),int(_)),FV).

test(freevars_lambda,[true(FV==[])]) :-
    T = lambda(variable(x),
            lambda(variable(y),
                add(variable(x),variable(y)))),
    freevars(T,FV).

test(freevars_rec,[true(FV==[z])]) :-
    T = rec(variable(x),
            lambda(variable(y),
                add(variable(x),add(variable(y),variable(z))))),
    freevars(T,FV).
    
test(freevars_apply,[true(FV==[y])]) :-
    F = lambda(variable(x),cond(variable(x),int(1),int(2))),
    freevars(apply(F,variable(y)),FV).