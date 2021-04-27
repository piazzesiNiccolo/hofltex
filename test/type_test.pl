:-use_module(library(plunit)).

:-begin_tests(typecheck).

:-use_module("../src/type").

test(type_int,[true(T==int)]):-
    get_type(int(1),T).
:-end_tests(typecheck).