:- module(hofl,[parse/2,parse_from_file/2]).

:- use_module(tokenize).
:- table pre_term//1,expr//1,aterm//1. 
/**
The binary expressions t0 [+,-,*] t1 and function application (t0 t1) rules are left recursive. Using table allows memoization
of previous results, making the parse predicate terminate as expected (packrat parsing)
*/

parse(String,Term) :- string_tokens(String,Tokens),phrase(pre_term(Term), Tokens).

parse_from_file(File,Term) :- 
    read_file_to_string(File, String, []),
    parse(String,Term).


pre_term(X) --> ["("],pre_term(X),[")"].
pre_term(cond(X,Y,Z)) --> ["if"],pre_term(X),["then"],pre_term(Y),["else"],pre_term(Z).
pre_term(rec(var(X),Y)) -->["rec"], pre_term(var(X)),!,["."],pre_term(Y). 
pre_term(lambda(var(X),Y)) --> ["\\"] , pre_term(var(X)),!,["."],pre_term(Y).
pre_term(apply(X,Y)) -->  pre_term(X),["@"], pre_term(Y).
pre_term(tuple(X,Y)) --> ["("],pre_term(X),[","],pre_term(Y),[")"].
pre_term(int(X)) --> [Y],{number_string(X, Y)}.
pre_term(var(X)) --> [Y],{\+number_string(X,Y),atom_string(X, Y),\+member(Y,["\\",".",",","+","-","*","(",")","@"])}.
pre_term(X) --> expr(X).

expr(X) --> aterm(X).
expr(bin_op("+",X,Y)) --> expr(X),["+"], aterm(Y).
expr(bin_op("-",X,Y)) --> expr(X),["-"], aterm(Y).

aterm(mul(X,Y)) --> aterm(X), ["*"], fact(Y).
aterm(X) --> fact(X),!.
aterm(fst(X)) --> ["fst"], pre_term(X). 
aterm(snd(X)) --> ["snd"], pre_term(X).

fact(X) --> ["("], expr(X),[")"].
fact(X) --> pre_term(X).





