:- module(infer,[derive/2]).

:- use_module(freevars).
:- use_module(substitution).
:- use_module(types).

/*definition of the predicates that creates the derivation tree. THe predicates defined closely follow the operational semantics
with a few minor changes:
- since we assume that terms are well typed, premises that enforce a typing for terms are ignored
- in the function application (t t0) rule, the premise that enforces that t is a lambda term is checked, but it's not memorized
in the final tree to save some space
- the same reasoning is applied in the fst(t) and snd(t) rules, for the premies that checks if t is a tuple*/

derive(Derivation,red(int(N),int(N))):-
    Derivation = infer(int,red(int(N),int(N)),[]).

derive(Derivation,red(bin_op("+",A,B),int(N))):-
    derive(Derivation1,red(A,int(N1))),
    derive(Derivation2,red(B,int(N2))),
    N is N1+N2,
    Derivation = infer(add,red(bin_op("+",A,B),int(N)),[Derivation1,Derivation2]).

derive(Derivation,red(mul(A,B),int(N))):-
    derive(Derivation1,red(A,int(N1))),
    derive(Derivation2,red(B,int(N2))),
    N is N1*N2,
    Derivation = infer(mul,red(mul(A,B),int(N)),[Derivation1,Derivation2]).

derive(Derivation,red(bin_op("-",A,B),int(N))):-
    derive(Derivation1,red(A,int(N1))),
    derive(Derivation2,red(B,int(N2))),
    N is N1-N2,
    Derivation = infer(minus,red(bin_op("-",A,B),int(N)),[Derivation1,Derivation2]).

derive(Derivation,red(tuple(A,B),tuple(A,B))):-
    inferType(A,_),
    inferType(B,_),
    freevars(A,[]),
    freevars(B,[]),
    Derivation = infer(tuple,red(tuple(A,B),tuple(A,B)),[]).

derive(Derivation,red(lambda(A,B),lambda(A,B))):-
    inferType(lambda(A,B),func(_,_)),
    freevars(lambda(A,B),[]),
    Derivation = infer(lambda,red(lambda(A,B),lambda(A,B)),[]).
    
derive(Derivation, red(cond(A,B,C),CanonicalForm)):-
    derive(Derivation1,red(A,int(0))),
    derive(Derivation2,red(B,CanonicalForm)),
    Derivation = infer(if,red(cond(A,B,C),CanonicalForm),[Derivation1,Derivation2]),!.

derive(Derivation, red(cond(A,B,C),CanonicalForm)):-
    derive(Derivation1,red(A,int(N))),
    N \= 0,
    derive(Derivation2,red(C,CanonicalForm)),
    Derivation = infer(if,red(cond(A,B,C),CanonicalForm),[Derivation1,Derivation2]),!.

derive(Derivation,red(fst(T),CanonicalForm)):-
    derive(_,red(T,tuple(T1,_))),
    derive(Derivation1,red(T1,CanonicalForm)),
    Derivation = infer(fst,red(fst(T),CanonicalForm),[Derivation1]).


derive(Derivation,red(snd(T),CanonicalForm)):-
    derive(_,red(T,tuple(_,T2))),
    derive(Derivation1,red(T2,CanonicalForm)),
    Derivation = infer(snd,red(snd(T),CanonicalForm),[Derivation1]).

derive(Derivation,red(apply(A,B),CanonicalForm)):-
    derive(_,red(A,lambda(id(C),B1))), /* checks that the term applied is a function, and then ignores its tree*/
    subst(B1,id(C),B,T),
    derive(Derivation1,red(T,CanonicalForm)),
    Derivation = infer(apply,red(apply(A,B),CanonicalForm),[Derivation1]).

derive(Derivation,red(rec(A,B),CanonicalForm)):-
    subst(B,A,rec(A,B),B1),
    derive(Derivation1,red(B1,CanonicalForm)),
    Derivation = infer(rec,red(rec(A,B),CanonicalForm),[Derivation1]).