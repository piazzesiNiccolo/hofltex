:- module(type,[get_type/2]).

:- use_module(hofl).

get_type(Term,Type):-
    type(Term,Type).

type(int(_), int).
type(variable(_),t).


type(tuple(T1,T2),prod(A1,A2)) :-
    type(T1,A1),type(T2,A2).

type(lambda(P,B),func(T1,T2)) :-
    type(P,T1),type(B,T2).

type(add(A,B),int) :- 
    type(A,int),type(B,int).

type(minus(A,B),int) :- 
    type(A,int),type(B,int).

type(mul(A,B),int) :- 
    type(A,int),type(B,int).




