:- module(substitution,[subst/4]).

:- use_module(freevars).

subst(int(N),_,_,int(N)).

subst(variable(A),X,Y,variable(Y)):-
    A = X,!.

subst(variable(A),X,_,variable(A)):-
    A \= X.

subst(add(A,B),X,Y,add(C,D)):-
    subst(A,X,Y,C),
    subst(B,X,Y,D).

subst(mul(A,B),X,Y,mul(C,D)):-
    subst(A,X,Y,C),
    subst(B,X,Y,D).

subst(minus(A,B),X,Y,minus(C,D)):-
    subst(A,X,Y,C),
    subst(B,X,Y,D).

subst(cond(A,B,C),X,Y,cond(D,E,F)):-
    subst(A,X,Y,D),
    subst(B,X,Y,E),
    subst(C,X,Y,F).

subst(tuple(A,B),X,Y,tuple(C,D)):-
    subst(A,X,Y,C),
    subst(B,X,Y,D).

subst(fst(A),X,Y,fst(B)):-
    subst(A,X,Y,B).

subst(snd(A),X,Y,fst(B)):-
    subst(A,X,Y,B).

subst(apply(A,B),X,Y,apply(C,D)):-
    subst(A,X,Y,C),
    subst(B,X,Y,D).

