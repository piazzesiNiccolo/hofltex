:- use_module(library(plunit)).
:-begin_tests(hofl).

:-use_module('../src/hofl').
test_hofl_parser:-
    run_tests([parse]).
    
test(parse) :-
    parse([a]).

:-end_tests(hofl).

