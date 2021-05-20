:- module(latex,[write_to_file/2]).

:-use_module(repr).


write_to_file(Tree,File):-
    open(File, write, OS),
    writeln(OS, "\\documentclass[10pt]{article}"),
    writeln(OS,"\\usepackage{proof}"),
    writeln(OS,"\\begin{document}"),
    writeln(OS,"\\pdfpageheight=11in"),
    writeln(OS,"\\pdfpagewidth=15in"),
    writeln(OS,"\\["),
    write_tree(Tree,OS,0),
    writeln(OS,"\\]"),
    writeln(OS,"\\end{document}").    
    
write_tree(infer(R,red(A,B),[]),OS,Tab):- /** base cases: numbers, tuples, lambdas*/
    swritef(F,"\\infer[%w]",[R]),
    writeln(OS,F),
    tab(OS, Tab + 7),
    write(OS, "{"),
    repr(A,S),
    write(OS,S),
    write(OS,"\\to "),
    repr(B,S),
    write(OS,S),
    writeln(OS,"}"),
    tab(OS,Tab + 7),
    writeln(OS,"{}").

write_tree(infer(R,red(A,B),[D1]),OS,Tab):-
    swritef(F,"\\infer[%w]",[R]),
    writeln(OS,F),
    tab(OS, Tab+7),
    write(OS, "{"),
    repr(A,S),
    write(OS,S),
    write(OS,"\\to "),
    repr(B,S1),
    write(OS,S1),
    writeln(OS,"}"),
    tab(OS,Tab+7),
    write(OS,"{"),
    write_tree(D1,OS,Tab+7),
    tab(OS,Tab+7),
    writeln(OS,"}").

write_tree(infer(R,red(A,B),[D1,D2]),OS,Tab):-
    swritef(F,"\\infer[%w]",[R]),
    writeln(OS,F),
    tab(OS, Tab+7),
    write(OS, "{"),
    repr(A,S),
    write(OS,S),
    write(OS,"\\to "),
    repr(B,S1),
    write(OS,S1),
    writeln(OS,"}"),
    tab(OS,Tab+7),
    write(OS,"{"),
    write_tree(D1,OS,Tab+7),
    tab(OS,Tab+7),
    write(OS,"&"),
    write_tree(D2,OS,Tab+7),
    tab(OS,Tab+7),
    writeln(OS,"}").



    
    