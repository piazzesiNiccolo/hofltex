:- module(repr, [repr/2]).



repr(int(N),N).
repr(var(A),A).

repr(bin_op(Op,A,B),R):-
    repr(A,R1),
    repr(B,R2),
    swritef(R,"%w %w %w",[R1,Op,R2]).


repr(mul(A,B),R):-
    repr(A,R1),
    repr(B,R2),
    swritef(R,"%w * %w",[R1,R2]).

repr(tuple(A,B),R):-
    repr(A,R1),
    repr(B,R2),
    swritef(R,"(%w , %w)",[R1,R2]).

repr(lambda(A,B),R ):-
    repr(A,R1),
    repr(B,R2),
    swritef(Rt,"%w.%w",[R1,R2]),
    string_concat("[\\lambda ", Rt, Rt2),
    string_concat(Rt2,"]",R).
    

repr(fst(T),R):-
    repr(T,R1),
    string_concat("\\mbox{fst }",R1,R).

    
repr(snd(T),R):-
    repr(T,R1),
    string_concat("\\mbox{snd }",R1,R).   
    
repr(apply(A,B),R):-
    repr(A,R1),
    repr(B,R2),
    swritef(R,"(%w)@(%w)",[R1,R2]).

repr(rec(A,B),R):-
    repr(A,R1),
    repr(B,R2),
    swritef(R3,"%w.%w",[R1,R2]),
    string_concat("\\mbox{rec }",R3,R).
repr(cond(A,B,C),R):-
    repr(A,R1),
    repr(B,R2),
    repr(C,R3),
    swritef(R,"\\mbox{if } %w \\mbox{ then } %w \\mbox{ else } %w",[R1,R2,R3]).