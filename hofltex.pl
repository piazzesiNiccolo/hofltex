:-use_module(library(main)).
:-use_module(src/hofl).
:-use_module(src/infer).
:- use_module(src/latex).



:- initialization(main,main).

main(Argv) :-
    opt_spec(Spec),
    opt_parse(Spec, Argv, Opts, _),
    append(_, [Last], Argv),
    (
        member(help(true), Opts) -> show_help
        ; 
        (

            parse(Last,T) -> ( 
                derive(D,red(T,_)) ->
                    (member(output(A),Opts),write_to_file(D,A)
                    ;writeln("Could not write derivation to file "))
                ;writeln("ERROR: no canonical form for given term"))
            ; writeln("Could not parse given term")
        )
        
        
        
        
    ).

show_help:-
    opt_spec(Spec),
    opt_help(Spec, HelpText),
    write('usage: swipl hofltex.pl <options> "<hofl_term>"\n\n'),
    write(HelpText).

opt_spec([
    [opt(help),
        type(boolean),
        default(false),
        shortflags([h]),
        longflags([help]),
        help('Show help')],
    [opt(output),
        type(atom),
        default('der.tex'),
        shortflags([o]),
        longflags(['output-file']),
        help('Specify which file to save the output to (default=der.tex)')]
    ]).
    
    





    
    
      


