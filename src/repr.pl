:- module(repr, [repr/2]).



repr(int(N),N).

repr(add(A,B),R):-
    repr(A,R1),
    repr(B,R2),
    string_concat(R1, "+", Rt),
    string_concat(Rt, R2,R).

repr(mul(A,B),R):-
    repr(A,R1),
    repr(B,R2),
    string_concat(R1, "*", Rt),
    string_concat(Rt, R2,R).

repr(minus(A,B),R):-
    repr(A,R1),
    repr(B,R2),
    string_concat(R1, "-", Rt),
    string_concat(Rt, R2,R).

repr(tuple(A,B),R):-
    repr(A,R1),
    repr(B,R2),
    string_concat("(", R1 ,Rt),
    string_concat(Rt, ", ",Rt2),
    string_concat(Rt2, R2, Rt3),
    string_concat(Rt3, ")", R).
repr(var(A),A).
repr(lambda(var(A),B),R):-
    string_concat("\\lambda ", A, St),
    string_concat(St, ".", St1),
    repr(B,Rb),
    string_concat(St1, Rb, R).
    
    
    