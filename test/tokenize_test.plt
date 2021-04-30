:- use_module(library(plunit)).

:- begin_tests(tokenize).

:- use_module(["../src/tokenize"]).


test(tokenize_string,[true(C==["x"])]):-
    S = "x",
    string_tokens(S,C).

test(tokenize_strings_with_whitespace,[true(C==["x","+","y"])]):-
    S = "x + y",
    string_tokens(S,C).
test(tokenize_allowed_punctuation,[true(C==["+","-","*","\\",",",".","(",")","@"])]) :-
    S = "+-*\\,.()@",
    string_tokens(S,C).


test(tokenize_disallowed_punctuation,fail):-
    S="!£$%&;:\"/=?^><_[]{}#",
    string_tokens(S,_).

test(tokenize_string_with_punctuation,[true(C==["\\","x",".","x","+","y"])]):-
    S = "\\x.x+y",
    string_tokens(S,C).
:-end_tests(tokenize).
