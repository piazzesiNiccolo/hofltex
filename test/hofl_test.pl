:- use_module(library(plunit)).
:-begin_tests(hofl).


:-use_module(["../src/hofl"]).

    
test(parse_int,[true(T == int(1))]) :-
Term = "1",
    parse(Term,T).

test(parse_var,[true(T == var(x))]) :-
    Term = "x",
    parse(Term,T).

test(parse_cond,[true(T == cond(var(x),var(y),var(z)))]) :-
    Term = "if x then y else z",
    parse(Term,T).

test(parse_add,[true(T == add(var(x),int(1)))]) :-
    Term = "x + 1",
    parse(Term,T).
test(parse_tuple,[true(T == tuple(var(x),var(y)))]) :-
    Term = "(x,y)",
    parse(Term,T).

test(parse_fst,[true(T == fst(tuple(var(x),var(y))))]) :-
    Term = "fst(x,y)",
    parse(Term,T).

test(parse_snd,[true(T == snd(tuple(var(x),var(y))))]) :-
    Term = "snd(x,y)",
    parse(Term,T).

test(parse_lambda,[true(T == lambda(var(x),var(y)))]) :-
    Term = "\\x.y",
    parse(Term,T).

test(parse_apply,[true(T == apply(lambda(var(x),var(x)),int(1)))]) :-
    Term = "(\\x.x)@1",
    parse(Term,T).
test(parse_rec,[true(T == rec(var(x),var(x)))]) :-
    Term = "rec x.x",
    parse(Term,T).

test(parse_lambda_fail_if_parameter_is_not_a_var,fail) :-
    Term = "\\(x,y).x",
    parse(Term,_).

test(parse_rec_fail_if_parameter_is_not_a_var,fail) :-
    Term = "rec 2.x",
    parse(Term,_).

test(parse_term_in_parentheses_should_have_higher_precedence,[true(T == mul(var(x),add(var(y),var(z))))]) :-
    Term = "x * ( y + z )",
    parse(Term,T).

test(lambda_precedence,[true(T==lambda(var(x),add(var(x),int(2))))]) :-
    Term = "\\x.x+2",
    parse(Term,T).


test(rec_precedence,[true(T==rec(var(x),add(var(x),int(2))))]) :-
    Term = "rec x.x+2",
    parse(Term,T).

test(cond_precedence,[true(T==cond(var(x),int(1),add(int(2),int(3))))]) :-
    Term = "if x then 1 else (2+3)",
    parse(Term,T).

:-end_tests(hofl).

