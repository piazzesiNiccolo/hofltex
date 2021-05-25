:- module(hofl,[parse/2,parse_from_file/2]).

:- use_module(tokenize).
:- table pre_term//1,expr//1,basic_term//1. 
/**
The binary expressions t0 [+,-,*] t1 and function application (t0 t1) rules are left recursive. Using table allows memoization
of previous results, making the parse predicate terminate as expected (packrat parsing)
*/

parse(String,Term) :- string_tokens(String,Tokens),phrase(pre_term(Term), Tokens).

parse_from_file(File,Term) :- 
    read_file_to_string(File, String, []),
    parse(String,Term).


pre_term(X) --> ["("],pre_term(X),[")"].
pre_term(rec(var(X),Y)) -->["rec"], pre_term(var(X)),!,["."],pre_term(Y). 
pre_term(lambda(var(X),Y)) --> ["\\"] , pre_term(var(X)),!,["."],pre_term(Y).
pre_term(X) --> expr(X).

expr(X) --> basic_term(X).
expr(bin_op("+",X,Y)) --> expr(X),["+"], basic_term(Y).
expr(bin_op("-",X,Y)) --> expr(X),["-"], basic_term(Y).


basic_term(mul(X,Y)) --> basic_term(X), ["*"], fact(Y).
basic_term(X) --> fact(X).

fact(X) --> ["("],expr(X),[")"].
fact(cond(X,Y,Z)) --> ["if"],pre_term(X),["then"],pre_term(Y),["else"],pre_term(Z).
fact(tuple(X,Y)) --> ["("],pre_term(X),[","],pre_term(Y),[")"].
fact(fst(X)) --> ["fst"], pre_term(X). 
fact(snd(X)) --> ["snd"], pre_term(X).
fact(apply(X,Y)) -->  pre_term(X),["@"],!, pre_term(Y).

fact(int(X)) --> [Y],{number_string(X, Y)}.
fact(var(X)) --> [Y],{\+number_string(X,Y),atom_string(X, Y),\+member(Y,["\\",".",",","+","-","*","(",")","@"])}.




