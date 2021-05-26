:- module(freevars, [freevars/2]).

/* auxiliary predicates to compute the free variables set of a term*/
freevars(int(_),[]).
freevars(id(X),[X]).
freevars(mul(X,Y),FV) :-
    freevars(X,F1),
    freevars(Y,F2),
    union(F1,F2,FV).

freevars(bin_op(_,X,Y),FV) :-
    freevars(X,F1),
    freevars(Y,F2),
    union(F1,F2,FV).
    


freevars(cond(X,Y,Z),FV) :-
    freevars(X,F1),
    freevars(Y,F2),
    freevars(Z,F3),
    union(F1,F2,F12),
    union(F12,F3,FV).

freevars(tuple(X,Y),FV) :-
    freevars(X,F1),
    freevars(Y,F2),
    union(F1,F2,FV).

freevars(apply(X,Y),FV) :-
    freevars(X,F1),
    freevars(Y,F2),
    union(F1,F2,FV).

freevars(fst(X),FV) :-
    freevars(X,FV).

freevars(snd(X),FV) :-
    freevars(X,FV).

freevars(lambda(id(X),Y),FV) :-
    freevars(Y,F1),
    subtract(F1,[X],FV).


freevars(rec(id(X),Y),FV) :-
    freevars(Y,F1),
    subtract(F1,[X],FV).
