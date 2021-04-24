:- module(hofl,[parse/2]).

:- table pre_term//1. 
/**
The binary expressions t0 [+,-,*] t1 and function application (t0 t1) are left recursive. Using table allows memoization
of previous results, making the parse predicate terminate as expected.
*/




pre_term(cond(X,Y,Z)) --> ["if"],pre_term(X),["then"],pre_term(Y),["else"],pre_term(Z),!.
pre_term(tuple(X,Y)) --> ["("],pre_term(X),[","],pre_term(Y),[")"],!.
pre_term(fst(X)) --> ["fst"], pre_term(X),!.
pre_term(snd(X)) --> ["snd"], pre_term(X),!.
pre_term(add(X,Y)) --> pre_term(X), ["+"], pre_term(Y),!.
pre_term(minus(X,Y)) --> pre_term(X), ["-"], pre_term(Y),!.
pre_term(mul(X,Y)) --> pre_term(X), ["*"], pre_term(Y),!.
pre_term(lambda(variable(X),Y)) --> ["fun"] , pre_term(variable(X)),["."],pre_term(Y),!.
pre_term(apply(X,Y)) --> ["("], pre_term(X), pre_term(Y),[")"],!.
pre_term(rec(variable(X),Y)) -->["rec"], pre_term(variable(X)),["."],pre_term(Y),!. 
pre_term(int(X)) --> [Y],{number_string(X, Y)},!.
pre_term(variable(X)) --> [Y],{atom_string(X, Y)},!.



parse(P,T) :- phrase(pre_term(T), P).