:- use_module(library(plunit)).

:- begin_tests(tokenize).

:- use_module(["../src/tokenize"]).


test(tokenize_string):-
    S = "x",
    string_tokens(S,C),
    C=["x"].

test(tokenize_strings_with_whitespace):-
    S = "x + y",
    string_tokens(S,C),
    C=["x","+","y"].

test(tokenize_allowed_punctuation):-
    S = "+-*\\,.()",
    string_tokens(S,C),
    C = ["+","-","*","\\",",",".","(",")"].

test(tokenize_disallowed_punctuation,fail):-
    S="!£$%&;:\"/=?^><_[]{}@#",
    string_tokens(S,_).

test(tokenize_string_with_punctuation):-
    S = "\\x.x+y",
    string_tokens(S,C),
    C = ["\\","x",".","x","+","y"].

:-end_tests(tokenize).
