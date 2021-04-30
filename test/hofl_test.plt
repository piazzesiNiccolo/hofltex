:- use_module(library(plunit)).
:-begin_tests(hofl).


:-use_module(["../src/hofl"]).

    
test(parse_int,[true(T == int(1))]) :-
Term = "1",
    parse(Term,T).

test(parse_variable,[true(T == variable(x))]) :-
    Term = "x",
    parse(Term,T).

test(parse_cond,[true(T == cond(variable(x),variable(y),variable(z)))]) :-
    Term = "if x then y else z",
    parse(Term,T).

test(parse_add,[true(T == add(variable(x),int(1)))]) :-
    Term = "x + 1",
    parse(Term,T).
test(parse_tuple,[true(T == tuple(variable(x),variable(y)))]) :-
    Term = "(x,y)",
    parse(Term,T).

test(parse_fst,[true(T == fst(tuple(variable(x),variable(y))))]) :-
    Term = "fst(x,y)",
    parse(Term,T).

test(parse_snd,[true(T == snd(tuple(variable(x),variable(y))))]) :-
    Term = "snd(x,y)",
    parse(Term,T).

test(parse_lambda,[true(T == lambda(x,variable(y)))]) :-
    Term = "\\x.y",
    parse(Term,T).

test(parse_apply,[true(T == apply(lambda(x,variable(x)),int(1)))]) :-
    Term = "(\\x.x)@1",
    parse(Term,T).
test(parse_rec,[true(T == rec(x,variable(x)))]) :-
    Term = "rec x.x",
    parse(Term,T).

test(parse_lambda_fail_if_parameter_is_not_a_variable,fail) :-
    Term = "\\(x,y).x",
    parse(Term,_).

test(parse_rec_fail_if_parameter_is_not_a_variable,fail) :-
    Term = "rec 2.x",
    parse(Term,_).

test(parse_term_in_parentheses_should_have_higher_precedence,[true(T == mul(variable(x),add(variable(y),variable(z))))]) :-
    Term = "x * ( y + z )",
    parse(Term,T).

test(lambda_precedence,[true(T==lambda(x,add(variable(x),int(2))))]) :-
    Term = "\\x.x+2",
    parse(Term,T).


test(rec_precedence,[true(T==rec(x,add(variable(x),int(2))))]) :-
    Term = "rec x.x+2",
    parse(Term,T).

test(cond_precedence,[true(T==cond(variable(x),int(1),add(int(2),int(3))))]) :-
    Term = "if x then 1 else 2+3",
    parse(Term,T).

:-end_tests(hofl).

