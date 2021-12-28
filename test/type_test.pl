:- use_module(library(plunit)).

:-begin_tests(infer_types).

:-use_module("../src/types").

test(type_int, []):-
    inferType(int(2), int).

test(type_bin_op_and_mul_only_int,[]):-
    inferType(bin_op("+",int(2), int(3)), int),
    inferType(bin_op("-",int(2), int(3)), int),
    inferType(mul(int(2), int(3)), int),
    \+inferType(bin_op("+", id(x), id(y)), _),
    \+inferType(bin_op("-", id(x), id(y)), _),
    \+inferType(mul(id(x), id(y)), int).

test(type_cond_only_int_if_and_same_type_branches,[]):-
    inferType(cond(int(2), int(3),int(4)),int),
    inferType(cond(int(0), lambda(id(x), fst(id(x))), lambda(id(x), snd(id(x)))),func(prod(A, A), A)),
    \+inferType(cond(id(x), int(1), int(2)), _),
    \+inferType(cond(int(0), int(3),tuple(id(x),id(y))),_).

test(type_tuple_fst_and_snd,[]):-
    Term = tuple(int(10),lambda(id(x),id(x))),
    inferType(Term, prod(int,func(A, A))),
    inferType(fst(Term), int),
    inferType(snd(Term), func(A,A)).

test(type_rec,[]):-
    Term = rec(id(fact), lambda(id(x), cond(id(x), int(1), mul(id(x), apply(id(fact), bin_op("-", id(x), int(1))))))),
    inferType(Term, func(int, int)).

test(type_lambda,[]):-
    Term = lambda(id(x), mul(id(x), int(2))),
    inferType(Term, func(int, int)),
    \+inferType(lambda(id(x),apply(id(x),id(x))),_).

test(type_apply,[]):-
    Term = lambda(id(x),fst(id(x))),
    Term2 = tuple(int(2),lambda(id(x),id(x))),
    inferType(apply(Term, Term2),int),
    \+inferType(apply(Term, int(3)),_).



