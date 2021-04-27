:-use_module(library(plunit)).

:-begin_tests(typecheck).

:-use_module("../src/type").

test(type_int,[true(T==int)]):-
    get_type(int(1),T).

test(type_tuple,[true(T==prod(int,int))]):-
    get_type(tuple(int(1),int(2)),T).

test(type_fst,[true(T==int)]):-
    get_type(fst(tuple(int(1),int(2))),T).

test(type_snd,[true(T==int)]):-
    get_type(snd(tuple(int(1),int(2))),T).

:-end_tests(typecheck).