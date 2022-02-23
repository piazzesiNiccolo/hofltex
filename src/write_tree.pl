:- module(write_tree, [tree_to_latex/2]).
:- use_module(library(lists)).
:- use_module(repr).

tree_to_latex(Tree, String):-
    string_chars("\\begin{prooftree}\n", R1),
    write_tree_lat(Tree,R2),
    string_chars("\\end{prooftree}\n", R3),
    append([R1,R2,R3],Repr),
    string_chars(String, Repr).
    

    

write_tree_lat(infer(R,red(A,B),[]),Repr):- /** rules without premises*/
    repr(A,S),
    repr(B,S1),
    swritef(F,"\\AxiomC{}\n"),
    swritef(F1,"\\RightLabel{%w}\n",[R]),
    swritef(F2,"\\UnaryInfC{$ %w \\Rightarrow %w$}\n",[S,S1]),
    string_chars(F, R1),
    string_chars(F1, R2),
    string_chars(F2, R3),
    append([R1,R2,R3], Repr).

write_tree_lat(infer(R,red(A,B),[D1]),Repr):-
    /* predicate used for rules with a single premise,
    the premise is colored blue*/
    write_tree_lat(D1,Repr1),
    repr(A, S1),
    repr(B, S2),
    swritef(F1,"\\RightLabel{%w}\n",[R]),
    swritef(F2,"\\UnaryInfC{$ %w \\Rightarrow %w$}\n",[S1,S2]),
    string_chars(F1, Repr2),
    string_chars(F2, Repr3),
    append([Repr1,Repr2,Repr3], Repr).
    
    
    

write_tree_lat(infer(R,red(A,B),[D1,D2]),Repr):-
    /* predicates with two premises, the left one is colored red and the right one is colored green instead*/
    write_tree_lat(D1,Repr1),
    write_tree_lat(D2,Repr2),
    repr(A, S1),
    repr(B, S2),
    swritef(F1,"\\LeftLabel{%w}\n",[R]),
    swritef(F2,"\\BinaryInfC{$ %w \\Rightarrow %w$}\n",[S1,S2]),
    string_chars(F1, Repr3),
    string_chars(F2, Repr4),
    append([Repr1,Repr2,Repr3,Repr4], Repr).

