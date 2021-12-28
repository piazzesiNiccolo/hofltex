:- module(types, [inferType/2]).


inferType(Term, Type):-
    inferType([],Term, Type),!.

inferType(_, int(N), int):-
    number(N).

inferType(Environment, bin_op(_, T1, T2), int):-
    inferType(Environment, T1, int),
    inferType(Environment, T2, int).

inferType(Environment, mul(T1, T2), int):-
    inferType(Environment, T1, int),
    inferType(Environment, T2, int).
    
inferType(Environment, cond(A, B, C), Type):-
    inferType(Environment, A, int),
    inferType(Environment, B, Type),
    inferType(Environment, C, Type).

inferType(Environment, tuple(A, B), prod(Type1, Type2)):-
    inferType(Environment, A, Type1),
    inferType(Environment, B, Type2).

inferType(Environment, fst(Tuple), Type):-
    inferType(Environment, Tuple, prod(Type,_)).

inferType(Environment, snd(Tuple), Type):-
    inferType(Environment, Tuple, prod(_,Type)).

inferType(Environment, lambda(id(Var), Body), func(Type1, Type2)):-
    inferType([(Var, Type1)| Environment], Body, Type2).

inferType(Environment, apply(Lambda, Term), Type):-
    inferType(Environment, Term, TypeBody),
    inferType(Environment, Lambda, func(TypeBody, Type)).

inferType(Environment, rec(id(Var), Body), Type):-
    inferType([(Var, Type)|Environment], Body, Type).

inferType(Environment, id(Var),Type):-
    nth0(_, Environment, (Var, Type)),
    acyclic_term(Type).  
/** if term appears as subterm of itself infinitely many times, cant assign type **/
    
    
    