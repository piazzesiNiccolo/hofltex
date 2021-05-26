:- use_module(library(plunit)).
:-begin_tests(freevars).

:- use_module("../src/freevars").

test(freevars_int,[true(FV==[])]):-
    freevars(int(1),FV).

test(freevars_var,[true(FV==[x])]) :-
    freevars(id(x),FV).

test(freevars_add,[true(FV==[x,y])]) :-
    freevars(bin_op("+",id(x),id(y)),FV).

test(freevars_minus,[true(FV==[x])]) :-
    freevars(bin_op("-",id(x),int(1)),FV).

test(freevars_mul,[true(FV==[x,y,z])]) :-
    freevars(mul(id(x),bin_op("+",id(y),id(z))),FV).

test(freevars_tuple,[true(FV==[])]) :-
    freevars(tuple(int(_),int(_)),FV).

test(freevars_lambda,[true(FV==[])]) :-
    T = lambda(id(x),
            lambda(id(y),
                bin_op("+",id(x),id(y)))),
    freevars(T,FV).

test(freevars_rec,[true(FV==[z])]) :-
    T = rec(id(x),
            lambda(id(y),
                bin_op("+",id(x),bin_op("+",id(y),id(z))))),
    freevars(T,FV).
    
test(freevars_apply,[true(FV==[y])]) :-
    F = lambda(id(x),cond(id(x),int(1),int(2))),
    freevars(apply(F,id(y)),FV).

:- end_tests(freevars).
