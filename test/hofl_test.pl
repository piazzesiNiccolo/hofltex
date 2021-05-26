:- use_module(library(plunit)).
:-begin_tests(hofl).


:-use_module(["../src/hofl"]).

    
test(parse_int,[true(T == int(1))]) :-
Term = "1",
    parse(Term,T).

test(parse_var,[true(T == id(x))]) :-
    Term = "x",
    parse(Term,T).

test(parse_cond,[true(T == cond(id(x),id(y),id(z)))]) :-
    Term = "if x then y else z",
    parse(Term,T).

test(parse_add,[true(T == bin_op("+",id(x),int(1)))]) :-
    Term = "x + 1",
    parse(Term,T).

test(parse_tuple,[true(T == tuple(id(x),id(y)))]) :-
    Term = "(x,y)",
    parse(Term,T).

test(parse_fst,[true(T == fst(tuple(id(x),id(y))))]) :-
    Term = "fst(x,y)",
    parse(Term,T).

test(parse_snd,[true(T == snd(tuple(id(x),id(y))))]) :-
    Term = "snd(x,y)",
    parse(Term,T).

test(parse_lambda,[true(T == lambda(id(x),id(y)))]) :-
    Term = "\\x.y",
    parse(Term,T).

test(parse_apply,[true(T == apply(lambda(id(x),id(x)),int(1)))]) :-
    Term = "(\\x.x)@1",
    parse(Term,T).
test(parse_rec,[true(T == rec(id(x),id(x)))]) :-
    Term = "rec x.x",
    parse(Term,T).

test(parse_lambda_fail_if_parameter_is_not_a_var,fail) :-
    Term = "\\(x,y).x",
    parse(Term,_).

test(parse_rec_fail_if_parameter_is_not_a_var,fail) :-
    Term = "rec 2.x",
    parse(Term,_).

test(parse_term_in_parentheses_should_have_higher_precedence,[true(T == mul(id(x),bin_op("+",id(y),id(z))))]) :-
    Term = "x * ( y + z )",
    parse(Term,T).

test(lambda_precedence,[true(T==lambda(id(x),bin_op("+",id(x),int(2))))]) :-
    Term = "\\x.x+2",
    parse(Term,T).


test(rec_precedence,[true(T==rec(id(x),bin_op("+",id(x),int(2))))]) :-
    Term = "rec x.x+2",
    parse(Term,T).

test(cond_precedence,[true(T==cond(id(x),int(1),bin_op("+",int(2),int(3))))]) :-
    Term = "if x then 1 else (2+3)",
    parse(Term,T).

:-end_tests(hofl).

