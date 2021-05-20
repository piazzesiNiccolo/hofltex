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
        
        ;member(file(true),Opts) -> 
            (parse_from_file(Last,T) -> ( 
                derive(D,red(T,_)) ->
                    (member(output(A),Opts),write_to_file(D,A)
                    ;writeln("Could not write derivation to file "))
                ;writeln("ERROR: no canonical form for given term"))
            ; (swritef(S,"could not parse term from file %w",[Last]),writeln(S)))

        
        ;member(file(false),Opts) ->
            (parse(Last,T) -> ( 
                derive(D,red(T,_)) ->
                    (member(output(A),Opts),write_to_file(D,A)
                    ;writeln("Could not write derivation to file "))
                ;writeln("ERROR: no canonical form for given term"))
            ; writeln("Could not parse given term"))
        
        
        
        
        
    ).


show_help:-
    opt_spec(Spec),
    opt_help(Spec, HelpText),
    write('usage: swipl hofltex.pl [-o( --output) | -f( --file )] "<hofl_term>" | file_name \n\n'),
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
        help('Specify which file to save the output to ')],
    [opt(file),
        type(boolean),
        default(false),
        shortflags([f]),
        longflags(['file']),
        help('Interpret last argument as  a file name')]

    ]).
    
    





    
    
      


