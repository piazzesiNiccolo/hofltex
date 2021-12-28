:-use_module(library(main)).
:-use_module(src/hofl).
:-use_module(src/infer).
:- use_module(src/latex).
:- use_module(src/types).


:- initialization(main,main).

main(Argv) :-
    opt_spec(Spec),
    opt_parse(Spec, Argv, Opts, _),
    (
        append(_, [Last], Argv),
        (
            /*
            Checks argument passed and choose the correct predicates to use:
            - if an input file is used, we first parse the file and then derive the canonical form
            - if no input file is passed we parse  the last argument passed to the command
            */
        (member(help(true), Opts); Opts = []) -> show_help
        
        ;member(file(true),Opts) -> 
            (parse_from_file(Last,T) -> (inferType(T,_),get_canonical_form(T,Opts))
            ;(swritef(S,"could not parse term from file %w",[Last]),writeln(S)))

        ;member(file(false),Opts) ->
            (parse(Last,T) -> ( get_canonical_form(T,Opts))
            ; writeln("Could not parse given term"))
        )
    )
    /* if no argument is passed simply shows the help*/
    ;show_help.

get_canonical_form(Term,Opts) :-
    /*
    Get the derivation tree. After that we check the output file name and if the short flag is passed and, finally, we write the derivation to the file.
    */
    derive(D,red(Term,_)) ->(
        member(output(A),Opts),
        member(short(S),Opts),
        write_to_file(D,A,S)
        ;writeln("Could not write derivation to file "))
    ;writeln("ERROR: no canonical form for given term").

show_help:-
    opt_spec(Spec),
    opt_help(Spec, HelpText),
    write('usage: swipl hofltex.pl <options>  < hofl_term | file_name > \n\n'),
    write(HelpText).

/*definition of the command line  options*/
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
        help('Interpret last argument as  a file name')],
    [opt(short),
        type(boolean),
        default(false),
        shortflags([s]),
        longflags(['short']),
        help('Write the reduction to a canonical form of a term without the full derivation tree')]

    ]).
    
    





    
    
      


