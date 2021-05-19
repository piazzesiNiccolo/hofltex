:- use_module(library(plunit)).
:-begin_tests(freevars).

:- use_module("../src/freevars").

test(freevars_int,[true(FV==[])]):-
    freevars(int(1),FV).

test(freevars_var,[true(FV==[x])]) :-
    freevars(var(x),FV).

test(freevars_add,[true(FV==[x,y])]) :-
    freevars(bin_op("+",var(x),var(y)),FV).

test(freevars_minus,[true(FV==[x])]) :-
    freevars(bin_op("-",var(x),int(1)),FV).

test(freevars_mul,[true(FV==[x,y,z])]) :-
    freevars(mul(var(x),bin_op("+",var(y),var(z))),FV).

test(freevars_tuple,[true(FV==[])]) :-
    freevars(tuple(int(_),int(_)),FV).

test(freevars_lambda,[true(FV==[])]) :-
    T = lambda(var(x),
            lambda(var(y),
                bin_op("+",var(x),var(y)))),
    freevars(T,FV).

test(freevars_rec,[true(FV==[z])]) :-
    T = rec(var(x),
            lambda(var(y),
                bin_op("+",var(x),bin_op("+",var(y),var(z))))),
    freevars(T,FV).
    
test(freevars_apply,[true(FV==[y])]) :-
    F = lambda(var(x),cond(var(x),int(1),int(2))),
    freevars(apply(F,var(y)),FV).

:- end_tests(freevars).
