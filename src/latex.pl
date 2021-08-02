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
    /* If the -s option is set, ignore the derivation tree and set the page width to the default value,
    otherwise the width is set to the tree_width plus another tab of space */
    Short -> D = infer(R,red(T,C),[]),N1 is 8
    ;D = infer(R,red(T,C),Tree),tree_width(infer(R,red(T,C),Tree),N),
    N1 is 8+N
    ),
    /* set up the tex file and then write the full tree recursively*/
    open(File, write, FileStream),
    writeln(FileStream, "\\documentclass[10pt]{article}"),
    writeln(FileStream,"\\usepackage{proof}"),
    writeln(FileStream,"\\usepackage[dvipsnames]{xcolor}"),
    writeln(FileStream,"\\begin{document}"),
    swritef(S,"\\pdfpagewidth=%win",[N1]),
    writeln(FileStream,"\\pdfpageheight=11in"),
    writeln(FileStream,S),
    writeln(FileStream,"\\["),
    write_tree(D,FileStream,0),
    writeln(FileStream,"\\]"),
    writeln(FileStream,"\\end{document}").    
    
write_tree(infer(R,red(A,B),[]),FileStream,Tab):- /** rules without premises*/
    swritef(F,"\\infer[%w]",[R]),
    writeln(FileStream,F),
    tab(FileStream, Tab + 7),
    write(FileStream, "{"),
    repr(A,S),
    write(FileStream,S),
    write(FileStream,"\\to "),
    repr(B,S1),
    write(FileStream,S1),
    writeln(FileStream,"}"),
    tab(FileStream,Tab + 7),
    writeln(FileStream,"{}").

write_tree(infer(R,red(A,B),[D1]),FileStream,Tab):-
    /* predicate used for rules with a single premise,
    the premise is colored blue*/
    swritef(F,"\\infer[%w]",[R]),
    writeln(FileStream,F),
    tab(FileStream, Tab+7),
    write(FileStream, "{"),
    repr(A,S),
    write(FileStream,S),
    write(FileStream,"\\to "),
    repr(B,S1),
    write(FileStream,S1),
    writeln(FileStream,"}"),
    tab(FileStream,Tab+7), /* Tabs are used to put each tree level to a different indentation level, making the final tex source more clear to read */
    write(FileStream,"{"),
    write(FileStream,"\\color{blue}"),
    write_tree(D1,FileStream,Tab+7),

    tab(FileStream,Tab+7),
    writeln(FileStream,"}").

write_tree(infer(R,red(A,B),[D1,D2]),FileStream,Tab):-
    /* predicates with two premises, the left one is colored red and the right one is colored green instead*/
    swritef(F,"\\infer[%w]",[R]),
    writeln(FileStream,F),
    tab(FileStream, Tab+7),
    write(FileStream, "{"),
    repr(A,S),
    write(FileStream,S),
    write(FileStream,"\\to "),
    repr(B,S1),
    write(FileStream,S1),
    writeln(FileStream,"}"),
    tab(FileStream,Tab+7),
    write(FileStream,"{"),
    write(FileStream,"\\color{red}"),
    write_tree(D1,FileStream,Tab+7),
    tab(FileStream,Tab+7), /* when we have two premises, their tex source is written at the same indentation level*/
    write(FileStream,"&"),
    write(FileStream,"\\color{OliveGreen}"),
    write_tree(D2,FileStream,Tab+7),
    tab(FileStream,Tab+7),
    writeln(FileStream,"}").



