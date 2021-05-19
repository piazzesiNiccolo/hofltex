:- module(substitution,[subst/4]).

:- use_module(freevars).


subst(int(N),_,_,int(N)).

subst(var(A),var(X),Y,Y):-
    A = X,!.

subst(var(A),var(X),_,var(A)):-
    A \= X.

subst(bin_op(Op,A,B),var(X),Y,bin_op(Op,C,D)):-
    subst(A,var(X),Y,C),
    subst(B,var(X),Y,D).

subst(mul(A,B),var(X),Y,mul(C,D)):-
    subst(A,var(X),Y,C),
    subst(B,var(X),Y,D).


subst(cond(A,B,C),var(X),Y,cond(D,E,F)):-
    subst(A,var(X),Y,D),
    subst(B,var(X),Y,E),
    subst(C,var(X),Y,F).

subst(tuple(A,B),var(X),Y,tuple(C,D)):-
    subst(A,var(X),Y,C),
    subst(B,var(X),Y,D).

subst(fst(A),var(X),Y,fst(B)):-
    subst(A,var(X),Y,B).

subst(snd(A),var(X),Y,snd(B)):-
    subst(A,var(X),Y,B).

subst(apply(A,B),var(X),Y,apply(C,D)):-
    subst(A,var(X),Y,C),
    subst(B,var(X),Y,D).

subst(lambda(var(A),B),var(X),Y,lambda(var(Z),F)):-
    freevars(lambda(var(A),B),T1),
    freevars(Y,T2),
    union(T1,T2,T3),
    union(T3,[X],T),
    (
    char_type(Z,alpha),\+member(Z,T),!
    ),
    subst(B,var(A),var(Z),F1),
    subst(F1,var(X),Y,F).
    
        
    

subst(rec(var(A),B),var(X),Y,rec(var(Z),F)):-
    freevars(rec(var(A),B),L),
    freevars(Y,L1),
    union(L,L1,L2),
    union(L2,[X],C),
    (
        
        char_type(Z,alpha),\+member(Z,C),!
    ),
    subst(B,var(A),var(Z),F1),
    subst(F1,var(X),Y,F).

