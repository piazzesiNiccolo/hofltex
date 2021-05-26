:- module(infer,[derive/2]).

:- use_module(freevars).
:- use_module(substitution).

/*definition of the predicates that creates the derivation tree*/
derive(D,red(int(N),int(N))):-
    D = infer(int,red(int(N),int(N)),[]).

derive(D,red(bin_op("+",A,B),int(N))):-
    derive(D1,red(A,int(N1))),
    derive(D2,red(B,int(N2))),
    N is N1+N2,
    D = infer(add,red(bin_op("+",A,B),int(N)),[D1,D2]).

derive(D,red(mul(A,B),int(N))):-
    derive(D1,red(A,int(N1))),
    derive(D2,red(B,int(N2))),
    N is N1*N2,
    D = infer(mul,red(mul(A,B),int(N)),[D1,D2]).

derive(D,red(bin_op("-",A,B),int(N))):-
    derive(D1,red(A,int(N1))),
    derive(D2,red(B,int(N2))),
    N is N1-N2,
    D = infer(minus,red(bin_op("-",A,B),int(N)),[D1,D2]).

derive(D,red(tuple(A,B),tuple(A,B))):-
    freevars(A,[]),
    freevars(B,[]),
    D = infer(tuple,red(tuple(A,B),tuple(A,B)),[]).

derive(D,red(lambda(A,B),lambda(A,B))):-
    freevars(lambda(A,B),[]),
    D = infer(lambda,red(lambda(A,B),lambda(A,B)),[]).
    
derive(D, red(cond(A,B,C),C0)):-
    derive(D1,red(A,int(0))),
    derive(D2,red(B,C0)),
    D = infer(if,red(cond(A,B,C),C0),[D1,D2]),!.

derive(D, red(cond(A,B,C),C1)):-
    derive(D1,red(A,int(N))),
    N \= 0,
    derive(D2,red(C,C1)),
    D = infer(if,red(cond(A,B,C),C1),[D1,D2]),!.

derive(D,red(fst(T),C0)):-
    derive(_,red(T,tuple(T1,_))),
    derive(D1,red(T1,C0)),
    D = infer(fst,red(fst(T),C0),[D1]).


derive(D,red(snd(T),C1)):-
    derive(_,red(T,tuple(_,T2))),
    derive(D1,red(T2,C1)),
    D = infer(snd,red(snd(T),C1),[D1]).

derive(D,red(apply(A,B),C0)):-
    derive(_,red(A,lambda(id(C),B1))),
    subst(B1,id(C),B,T),
    derive(D1,red(T,C0)),
    D = infer(apply,red(apply(A,B),C0),[D1]).

derive(D,red(rec(A,B),C)):-
    subst(B,A,rec(A,B),B1),
    derive(D1,red(B1,C)),
    D = infer(rec,red(rec(A,B),C),[D1]).