:- module(canonical, [infer/3,red/2]).

:- use_module(freevars).


red(int(N), N).

red(tuple(A,B),tuple(A,B)) :-
    freevars(A,[]),
    freevars(B,[]).

red(lamba(X,Y),lambda(X,Y)):-
    freevars(lambda(X,Y),[]).

red(add(A,B),N) :-
    red(A,N0),
    red(B,N1),
    N is N0+N1.


red(mul(A,B),N) :-
    red(A,N0),
    red(B,N1),
    N is N0*N1.


red(minus(A,B),N) :-
    red(A,N0),
    integer(N0),
    red(B,N1),
    integer(N1),
    N is N0-N1.

red(cond(A,B,C),C0) :-
    red(A,0),
    red(B,C0),!.

red(cond(A,B,C),C1) :-
    red(A,N),
    integer(N),
    N \= 0,
    red(C,C1).

red(fst(tuple(X,Y)),C0) :-
    red(X,C0).

red(snd(tuple(X,Y)),C1) :-
    red(Y,C1).
