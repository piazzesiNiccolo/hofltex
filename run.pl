:-use_module(library(main)).
:-use_module(src/hofl).
:-use_module(src/infer).



:- initialization(main,main).

main(Argv) :-
    opt_spec(Spec),
    opt_parse(Spec, Argv, Opts, _),
    append(_, [Last], Argv),
    (
        member(help(true), Opts) -> show_help
        ; parse(Last,A),derive(D,red(A,T)), writeln(D),writeln(T)
        
        
        
        
    ).

show_help:-
    opt_spec(Spec),
    opt_help(Spec, HelpText),
    write('usage: hoftex <options> f\n\n'),
    write(HelpText).

opt_spec([
    [opt(help),
        type(boolean),
        default(false),
        shortflags([h]),
        longflags([help]),
        help('Show help')],
    
    [opt(html),
        type(boolean),
        default(false),
        longflags([html]),
        help('Convert the output to html')]
    ]).
    
    





    
    
      


