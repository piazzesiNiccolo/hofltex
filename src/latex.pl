:- module(latex,[write_to_file/2]).

:-use_module(repr).


write_to_file(Tree,File):-
    open(File, write, OS),
    writeln(OS, "\\documentclass[50pt]{article}"),
    writeln(OS,"\\usepackage{proof}"),
    writeln(OS,"\\begin{document}"),
    writeln(OS,"\\["),
    write_tree(Tree,OS),
    writeln(OS,"\\]"),
    writeln(OS,"\\end{document}").    
    
write_tree(infer(R,red(A,B),[]),OS):- /** base cases: numbers, tuples, lambdas*/
    swritef(St,"%w",[R]),
    string_concat("\\infer[",St,St1),
    string_concat(St1,"]",F),
    writeln(OS,F),
    tab(OS, 7),
    write(OS, "{"),
    repr(A,S),
    write(OS,S),
    write(OS,"\\to "),
    repr(B,S),
    write(OS,S),
    writeln(OS,"}"),
    tab(OS,7),
    writeln(OS,"{}").

write_tree(infer(R,red(A,B),[D1]),OS):-
    swritef(St,"%w",[R]),
    string_concat("\\infer[",St,St1),
    string_concat(St1,"]",F),
    writeln(OS,F),
    tab(OS, 7),
    write(OS, "{"),
    repr(A,S),
    write(OS,S),
    write(OS,"\\to "),
    repr(B,S1),
    write(OS,S1),
    writeln(OS,"}"),
    tab(OS,7),
    write(OS,"{"),
    write_tree(D1,OS),
    write(OS,"}").

write_tree(infer(R,red(A,B),[D1,D2]),OS):-
    swritef(St,"%w",[R]),
    string_concat("\\infer[",St,St1),
    string_concat(St1,"]",F),
    writeln(OS,F),
    tab(OS, 7),
    write(OS, "{"),
    repr(A,S),
    write(OS,S),
    write(OS,"\\to "),
    repr(B,S1),
    write(OS,S1),
    writeln(OS,"}"),
    tab(OS,7),
    write(OS,"{"),
    write_tree(D1,OS),
    write(OS,"&"),
    write_tree(D2,OS),
    write(OS,"}").



    
    