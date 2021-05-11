:- module(infer,[derive/2]).

:- use_module(freevars).

derive(D,red(int(N),int(N))):-
    D = infer(int,red(int(N),int(N)),[]).


derive(D,red(add(A,B),N)):-
    derive(infer(R1,red(A,T1),L1),red(A,int(N1))),
    derive(infer(R2,red(B,T2),L2),red(B,int(N2))),
    N is N1+N2,
    D = infer(add,red(add(A,B),int(N)),[infer(R1,red(A,T1),L1),infer(R2,red(B,T2),L2)]).

derive(D,red(mul(A,B),N)):-
    derive(infer(R1,red(A,T1),L1),red(A,int(N1))),
    derive(infer(R2,red(B,T2),L2),red(B,int(N2))),
    N is N1*N2,
    D = infer(mul,red(mul(A,B),int(N)),[infer(R1,red(A,T1),L1),infer(R2,red(B,T2),L2)]).

derive(D,red(minus(A,B),N)):-
    derive(infer(R1,red(A,T1),L1),red(A,int(N1))),
    derive(infer(R2,red(B,T2),L2),red(B,int(N2))),
    N is N1-N2,
    D = infer(minus,red(minus(A,B),int(N)),[infer(R1,red(A,T1),L1),infer(R2,red(B,T2),L2)]).

derive(D,red(tuple(A,B),tuple(A,B))):-
    freevars(A,[]),
    freevars(B,[]),
    D = infer(tuple,red(tuple(A,B),tuple(A,B))).

derive(D,red(lambda(A,B),lambda(A,B))):-
    freevars(lambda(A,B),[]),
    D = infer(lambda,red(lambda(A,B),lambda(A,B))).
    
