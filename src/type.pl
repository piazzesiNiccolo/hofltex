:- module(type, [type/4]).

type(int(_),Env,int,Env).

type(var(A),Env,T,Nenv):-
    member((var(A),T),Env) -> Nenv = Env
    ;Nenv = [(var(A),T)|Env].

type(tuple(A,B),Env,prod(T1,T2),Nenv):-
    type(A,Env,T1,Nenv1),
    type(B,Nenv1,T2,Nenv).

type(lambda(var(A),B),Env,func(T1,T2),Nenv):-
    
    type(B,Env,T2,E1),
    type(var(A),E1,T1,Nenv).

type(bin_op(_,A,B),Env,int,Nenv):-
    type(A,Env,int,E1),
    type(B,E1,int,Nenv).


type(mul(A,B),Env,int,Nenv):-
    type(A,Env,int,E1),
    type(B,E1,int,Nenv).

type(cond(A,B,C),Env,T,Nenv):-
    type(A,Env,int,E1),
    type(B,E1,T,E2),
    type(C,E2,T,Nenv).

type(fst(A),Env,T,Nenv):-
    type(A,Env,prod(T,_),Nenv).


type(snd(A),Env,T,Nenv):-
    type(A,Env,prod(_,T),Nenv).

type(apply(A,B),Env,T,Nenv):-
    type(A,Env,func(T1,T),E1),
    type(B,E1,T1,Nenv).

type(rec(A,B),Env,T,Nenv):-
    type(B,Env,T,E1),
    type(A,E1,T,Nenv).

