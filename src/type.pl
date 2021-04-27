:- module(type,[get_type/2]).

:- use_module(hofl).

get_type(Term,Type):-
    type(Term,Type).

type(int(_), int).

type(variable(X),X).


type(tuple(A1,A2),prod(T1,T2)) :-
    type(A1,T1),type(A2,T2).

type(lambda(P,B),func(T1,T2)) :-
    type(P,T1),type(B,T2).

type(add(A,B),int) :- 
    type(A,int),type(B,int).

type(minus(A,B),int) :- 
    type(A,int),type(B,int).

type(mul(A,B),int) :- 
    type(A,int),type(B,int).

type(fst(A),T):-
    type(A,prod(T,_)).

type(snd(A),T):-
    type(A,prod(_,T)).




