:- use_module(library(plunit)).
:-begin_tests(hofl).


:-use_module(["../src/hofl"]).

    
test(parse_int) :-
    Term = "1",
    parse(Term,T),
    T = int(1).

test(parse_variable) :-
    Term = "x",
    parse(Term,T),
    T = variable(x).

test(parse_cond) :-
    Term = "if x then y else z",
    parse(Term,T),
    T = cond(variable(x),variable(y),variable(z)).

test(parse_add) :-
    Term = "x + 1",
    parse(Term,T),
    T = add(variable(x),int(1)).
test(parse_tuple) :-
    Term = "(x,y)",
    parse(Term,T),
    T = tuple(variable(x),variable(y)).

test(parse_fst) :-
    Term = "fst(x,y)",
    parse(Term,T),
    T = fst(tuple(variable(x),variable(y))).

test(parse_snd) :-
    Term = "snd(x,y)",
    parse(Term,T),
    T = snd(tuple(variable(x),variable(y))).

test(parse_lambda) :-
    Term = "\\x.y",
    parse(Term,T),
    T = lambda(variable(x),variable(y)).

test(parse_apply) :-
    Term = "(\\x.x 1)",
    parse(Term,T),
    T = apply(lambda(variable(x),variable(x)),int(1)).
test(parse_rec) :-
    Term = "rec x.x",
    parse(Term,T),
    T = rec(variable(x),variable(x)).

test(parse_lambda_fail_if_parameter_is_not_a_variable,fail) :-
    Term = "\\(x,y).x",
    parse(Term,_).

test(parse_rec_fail_if_parameter_is_not_a_variable,fail) :-
    Term = "rec (x,y).x",
    parse(Term,_).

test(parse_term_in_parentheses_should_have_higher_precedence) :-
    Term = "x + ( y + z )",
    parse(Term,T),
    T = add(variable(x),add(variable(y),variable(z))).
:-end_tests(hofl).

