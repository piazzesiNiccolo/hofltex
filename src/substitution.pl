:- module(substitution,[subst/4]).

:- use_module(freevars).


subst(int(N),_,_,int(N)).

subst(id(A),id(X),Y,Y):-
    A = X,!.

subst(id(A),id(X),_,id(A)):-
    A \= X.

subst(bin_op(Op,A,B),id(X),Y,bin_op(Op,C,D)):-
    subst(A,id(X),Y,C),
    subst(B,id(X),Y,D).

subst(mul(A,B),id(X),Y,mul(C,D)):-
    subst(A,id(X),Y,C),
    subst(B,id(X),Y,D).


subst(cond(A,B,C),id(X),Y,cond(D,E,F)):-
    subst(A,id(X),Y,D),
    subst(B,id(X),Y,E),
    subst(C,id(X),Y,F).

subst(tuple(A,B),id(X),Y,tuple(C,D)):-
    subst(A,id(X),Y,C),
    subst(B,id(X),Y,D).

subst(fst(A),id(X),Y,fst(B)):-
    subst(A,id(X),Y,B).

subst(snd(A),id(X),Y,snd(B)):-
    subst(A,id(X),Y,B).

subst(apply(A,B),id(X),Y,apply(C,D)):-
    subst(A,id(X),Y,C),
    subst(B,id(X),Y,D).

subst(lambda(id(A),B),id(X),Y,lambda(id(Z),F)):-
    freevars(lambda(id(A),B),T1),
    freevars(Y,T2),
    union(T1,T2,T3),
    union(T3,[X],T),
    (
    char_type(Z,lower),\+member(Z,T),!
    ),
    
    subst(B,id(A),id(Z),F1),
    subst(F1,id(X),Y,F).
    
        
    

subst(rec(id(A),B),id(X),Y,rec(id(Z),F)):-
    freevars(rec(id(A),B),L),
    freevars(Y,L1),
    union(L,L1,L2),
    union(L2,[X],C),
    (
        
        char_type(Z,lower),\+member(Z,C),!
    ),
    subst(B,id(A),id(Z),F1),
    subst(F1,id(X),Y,F).

