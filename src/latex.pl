/*predicates used to write the output to the target file
. There is a different predicate for rules with a different number of premises. I prefer to use one rule for each case
instead of recursively process the list to enforce a better indentation in the final code. This is done by using the
Tab variable, which is equal for rules at the same height in the tree*/
:- module(latex,[write_to_file/3]).

:-use_module(repr).


/* tree width is used to compute how much width is necessary to write the full tree in a pdf file*/
tree_width(infer(_,_,[]),0).
tree_width(infer(_,_,[D]),N):-
    tree_width(D,N).
tree_width(infer(_,_,[D1,D2]),N):-
    tree_width(D1,N1),
    tree_width(D2,N2),
    N is N1+N2+1.

write_to_file(infer(R,red(T,C),Tree),File,Short):-
    (
    /* If the -s option is set, ignore the derivation tree and set the page width to the default value */
    Short -> D = infer(R,red(T,C),[]),N1 is 8
    ;D = infer(R,red(T,C),Tree),tree_width(infer(R,red(T,C),Tree),N),
    N1 is 8+N
    ),
    /* set up the tex file and then write the full tree recursively*/
    open(File, write, OS),
    writeln(OS, "\\documentclass[10pt]{article}"),
    writeln(OS,"\\usepackage{proof}"),
    writeln(OS,"\\usepackage[dvipsnames]{xcolor}"),
    writeln(OS,"\\begin{document}"),
    swritef(S,"\\pdfpagewidth=%win",[N1]),
    writeln(OS,"\\pdfpageheight=11in"),
    writeln(OS,S),
    writeln(OS,"\\["),
    write_tree(D,OS,0),
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
    repr(B,S1),
    write(OS,S1),
    writeln(OS,"}"),
    tab(OS,Tab + 7),
    writeln(OS,"{}").

write_tree(infer(R,red(A,B),[D1]),OS,Tab):-
    /* predicate used for rules with a single premise,
    the premise is colored blue*/
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
    write(OS,"\\color{blue}"),
    write_tree(D1,OS,Tab+7),

    tab(OS,Tab+7),
    writeln(OS,"}").

write_tree(infer(R,red(A,B),[D1,D2]),OS,Tab):-
    /* predicates with two premises, the left one is colored red and the right one is colored green instead*/
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
    write(OS,"\\color{red}"),
    write_tree(D1,OS,Tab+7),
    tab(OS,Tab+7),
    write(OS,"&"),
    write(OS,"\\color{OliveGreen}"),
    write_tree(D2,OS,Tab+7),
    tab(OS,Tab+7),
    writeln(OS,"}").



