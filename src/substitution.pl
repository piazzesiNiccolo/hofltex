:- module(substitution,[subst/4]).

:- use_module(freevars).

/**
 * definition of auxiliary predicates that handles capture avoiding substitution
 * 
 * 
 * The predicate subst(A, B, C, D) applies the substitution [C/B] in term A, creating the new term D.
 * For lambda abstractions and recursive definitions we first check that the names introduced are not free variables of the body.
 * */
subst(int(Number),_,_,int(Number)).

subst(id(Name),id(OldName),Term,Term):-
    Name = OldName,!.

subst(id(Name),id(OldName),_,id(Name)):-
    Name \= OldName.

subst(bin_op(Op,Term1,Term2),id(Name),Y,bin_op(Op,NewTerm1,NewTerm2)):-
    subst(Term1,id(Name),Y,NewTerm1),
    subst(Term2,id(Name),Y,NewTerm2).

subst(mul(Term1,Term2),id(Name),Y,mul(NewTerm1,NewTerm2)):-
    subst(Term1,id(Name),Y,NewTerm1),
    subst(Term2,id(Name),Y,NewTerm2).


subst(cond(Check,TrueBranch,FalseBranch),id(Name),Y,cond(NewCheck,NewTrueBranch,NewFalseBranch)):-
    subst(Check,id(Name),Y,NewCheck),
    subst(TrueBranch,id(Name),Y,NewTrueBranch),
    subst(FalseBranch,id(Name),Y,NewFalseBranch).

subst(tuple(Term1,Term2),id(Name),Y,tuple(NewTerm1,NewTerm2)):-
    subst(Term1,id(Name),Y,NewTerm1),
    subst(Term2,id(Name),Y,NewTerm2).

subst(fst(Tuple),id(Name),Y,fst(NewTuple)):-
    subst(Tuple,id(Name),Y,NewTuple).

subst(snd(Tuple),id(Name),Y,snd(NewTuple)):-
    subst(Tuple,id(Name),Y,NewTuple).

subst(apply(Function,Argument),id(Name),Y,apply(NewFunction,NewArgument)):-
    subst(Function,id(Name),Y,NewFunction),
    subst(Argument,id(Name),Y,NewArgument).

subst(lambda(id(Variable),Body),id(Name),Y,lambda(id(NewVariable),NewBody)):-
    freevars(lambda(id(Variable),Body),T1),
    freevars(Y,T2),
    union(T1,T2,T3),
    union(T3,[Name],FreeVars),
    (
    char_type(NewVariable,lower),\+member(NewVariable,FreeVars),!
    ),
    
    subst(Body,id(Variable),id(NewVariable),F1),
    subst(F1,id(Name),Y,NewBody).
    
        
    

subst(rec(id(Variable),Body),id(Name),Y,rec(id(NewVariable),NewBody)):-
    freevars(rec(id(Variable),Body),L),
    freevars(Y,L1),
    union(L,L1,L2),
    union(L2,[Name],FreeVars),
    (
        
        char_type(NewVariable,lower),\+member(NewVariable,FreeVars),!
    ),
    subst(Body,id(Variable),id(NewVariable),F1),
    subst(F1,id(Name),Y,NewBody).

