/*predicates used to write the output to the target file
. There is a different predicate for rules with a different number of premises. I prefer to use one rule for each case
instead of recursively process the list to enforce a better indentation in the final code. This is done by using the
Tab variable, which is equal for rules at the same height in the tree*/
:- module(latex,[write_to_file/3]).

:-use_module(repr).
:-use_module(write_tree).


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
    writeln(FileStream,""),
    
    tree_to_latex(D,String),
    writeln(FileStream,String),
    end_file(FileStream).
  
      

write_preamble(FileStream,PageWidth) :-
    writeln(FileStream, "\\documentclass[10pt]{article}"),
    writeln(FileStream,"\\usepackage{bussproofs}"),
    writeln(FileStream,"\\usepackage[dvipsnames]{xcolor}"),
    writeln(FileStream,"\\begin{document}"),
    swritef(S,"\\pdfpagewidth=%win",[PageWidth]),
    writeln(FileStream,"\\pdfpageheight=11in"),
    writeln(FileStream,S).


    
end_file(FileStream):- 
writeln(FileStream,"\\end{document}"),
close(FileStream).







