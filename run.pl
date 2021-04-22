:-use_module(library(main)).
:-use_module(src/interpret).


:- initialization(main,main).

main(Argv) :-
    opt_spec(Spec),
    opt_parse(Spec, Argv, Opts, _),
    (
        member(help(true), Opts) -> show_help
        ; maplist(format('~w~n'), Opts)
        
        
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
    
    





    
    
      


