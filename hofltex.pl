:-use_module(library(main)).
:-use_module(src/hofl).
:-use_module(src/infer).
:- use_module(src/latex).



:- initialization(main,main).

main(Argv) :-
    opt_spec(Spec),
    opt_parse(Spec, Argv, Opts, _),
    (
        append(_, [Last], Argv),
        (
        (member(help(true), Opts); Opts = []) -> show_help
        
        ;member(file(true),Opts) -> 
            (parse_from_file(Last,T) -> (get_canonical_form(T,Opts))
            ;(swritef(S,"could not parse term from file %w",[Last]),writeln(S)))

        ;member(file(false),Opts) ->
            (parse(Last,T) -> ( get_canonical_form(T,Opts))
            ; writeln("Could not parse given term"))
        )
    )
    ;show_help.

get_canonical_form(Term,Opts) :-
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
        help('Write the canonical reduction of a term without the full derivation tree')]

    ]).
    
    





    
    
      


