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
    
    Short-> D = infer(R,red(T,C),[]),N1 is 8
    ;tree_width(infer(R,red(T,C),Tree),N),
    (N > 50 -> D = infer(R,red(T,C),[]),N1 is 8
    ; D = infer(R,red(T,C),Tree),N1 is 8+N
    )
    ),
    /* set up the tex file and then write the full tree recursively*/
    open(File, write, FileStream),
    write_preamble(FileStream,N1),
    write_tree(D,FileStream,0),
    end_proof(FileStream),
    end_file(FileStream).
  
      

write_preamble(FileStream,PageWidth) :-
    writeln(FileStream, "\\documentclass[10pt]{article}"),
    writeln(FileStream,"\\usepackage{bussproofs}"),
    writeln(FileStream,"\\usepackage[dvipsnames]{xcolor}"),
    writeln(FileStream,"\\begin{document}"),
    swritef(S,"\\pdfpagewidth=%win",[PageWidth]),
    writeln(FileStream,"\\pdfpageheight=11in"),
    writeln(FileStream,S),
    writeln(FileStream,"\\begin{prooftree}").

end_proof(FileStream):-
    writeln(FileStream,"\\end{prooftree}").
    
end_file(FileStream):- 
writeln(FileStream,"\\end{document}"),
close(FileStream).

write_tree(infer(R,red(A,B),[]),FileStream,Tab):- /** rules without premises*/
    repr(A,S),
    repr(B,S1),
    tab(FileStream, Tab+4),
    writeln(FileStream, "\\AxiomC{}"),
    tab(FileStream,Tab+4),
    
    swritef(F1,"\\RightLabel{%w}",[R]),
    writeln(FileStream, F1),
    tab(FileStream, Tab+4),
    
    swritef(F2,"\\UnaryInfC{$ %w \\Rightarrow %w$}",[S,S1]),
    writeln(FileStream,F2).

write_tree(infer(R,red(A,B),[D1]),FileStream,Tab):-
    /* predicate used for rules with a single premise,
    the premise is colored blue*/
    write_tree(D1,FileStream,Tab),
    repr(A, S1),
    repr(B, S2),
    swritef(F1,"\\RightLabel{%w}",[R]),
    tab(FileStream,Tab+4),
    writeln(FileStream,F1),
    tab(FileStream,Tab+4),
    swritef(F2,"\\UnaryInfC{$ %w \\Rightarrow %w$}",[S1,S2]),
    writeln(FileStream,F2).

write_tree(infer(R,red(A,B),[D1,D2]),FileStream,Tab):-
    /* predicates with two premises, the left one is colored red and the right one is colored green instead*/
    write_tree(D1,FileStream,Tab),
    write_tree(D2,FileStream, Tab+8),
    repr(A, S1),
    repr(B, S2),
    tab(FileStream, Tab+4),
    swritef(F1,"\\LeftLabel{%w}",[R]),
    writeln(FileStream,F1),
    tab(FileStream, Tab+4),
    swritef(F2,"\\BinaryInfC{$ %w \\Rightarrow %w$}",[S1,S2]),
    writeln(FileStream,F2).



