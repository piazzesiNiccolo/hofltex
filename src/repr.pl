:- module(repr, [repr/2]).

/*
Helper predicates to write AST objects as latex code
*/

repr(int(N),N).
repr(id(A),A).

/* Arithemtic operation are written in the normal conventional syntax */
repr(bin_op(Op,A,B),R):-
    repr(A,R1),
    repr(B,R2),
    swritef(R,"%w %w %w",[R1,Op,R2]).


repr(mul(A,B),R):-
    repr(A,R1),
    repr(B,R2),
    swritef(R,"%w * %w",[R1,R2]).

/* a tuple formed by a and b is written as (a, b)*/
repr(tuple(A,B),R):-
    repr(A,R1),
    repr(B,R2),
    swritef(R,"(%w , %w)",[R1,R2]).

/* lambda abstractions are represented as written in normal text, using the lambda symbol provided by latex math library*/
repr(lambda(A,B),R ):-
    repr(A,R1),
    repr(B,R2),
    swritef(Rt,"%w.%w",[R1,R2]),
    string_concat("[\\lambda ", Rt, Rt2),
    string_concat(Rt2,"]",R).

/* fst and snd terms are simply represented as they are written*/
repr(fst(T),R):-
    repr(T,R1),
    string_concat("\\mbox{fst }",R1,R).

    
repr(snd(T),R):-
    repr(T,R1),
    string_concat("\\mbox{snd }",R1,R).
   
/* the function and its arguments are separated with the @ symbol and wrapped in parentheses to distinguish between them more easily */
repr(apply(A,B),R):-
    repr(A,R1),
    repr(B,R2),
    swritef(R,"(%w)@(%w)",[R1,R2]).


/*recursive definitions are represented with the name of the recursion variable with rec as a subscript, to save space in the final pdf output.*/
repr(rec(A,_),R):-
    repr(A,R1),
    string_concat(R1,"_{rec}",R).

/* if then else constructs are represented the same way they are defined
repr(cond(A,B,C),R):-
    repr(A,R1),
    repr(B,R2),
    repr(C,R3),
    swritef(R,"\\mbox{if } %w \\mbox{ then } %w \\mbox{ else } %w",[R1,R2,R3]).