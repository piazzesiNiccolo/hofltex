:- use_module(library(plunit)).
:-begin_tests(repr).

:- use_module("../src/repr").


test(repr_var,[true(T=x)]):-
    repr(id(x),T).

test(repr_int,[true(T=1)]):-
    repr(int(1),T).

test(repr_add,[true(T="2 + 3")]):-
    repr(bin_op("+",int(2),int(3)),T).

test(repr_minus,[true(T="2 - 3")]):-
    repr(bin_op("-",int(2),int(3)),T).

test(repr_mul,[true(T="2 * 3")]):-
    repr(mul(int(2),int(3)),T).

test(repr_cond,[true(T="\\mbox{if } 1 \\mbox{ then } 0 \\mbox{ else } 1")]):-
    repr(cond(int(1),int(0),int(1)),T).

test(repr_tuple,[true(T="(1 , 2)")]):-
    repr(tuple(int(1),int(2)),T).

test(repr_fst,[true(T="\\mbox{fst }(1 , 2)")]):-
    repr(fst(tuple(int(1),int(2))),T).

test(repr_snd,[true(T="\\mbox{snd }(1 , 2)")]):-
    repr(snd(tuple(int(1),int(2))),T).

test(repr_lambda,[true(T="[\\lambda x.x + 2]")]):-
    repr(lambda(id(x),bin_op("+",id(x),int(2))),T).

test(repr_apply,[true(T="([\\lambda x.x + 2])@(3)")]):-
    repr(apply(lambda(id(x),bin_op("+",id(x),int(2))),int(3)),T).

test(repr_rec,[true(T="\\mbox{rec }f.[\\lambda x.\\mbox{if } x \\mbox{ then } 1 \\mbox{ else } (f)@(x - 1)]")]):-
    Term = rec(id(f)
                ,lambda(id(x),
                cond(id(x), 
                    int(1), 
                    apply(id(f),bin_op("-",id(x),int(1)))))),
    repr(Term,T).

:-end_tests(repr).