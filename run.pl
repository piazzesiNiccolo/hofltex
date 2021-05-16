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
        ; parse(Last,T),derive(D,red(T,_)),member(output(A),Opts),write_to_file(D,A)
        
        
        
        
        
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
        help('Convert the output to html')],
    [opt(output),
        type(atom),
        default('der.tex'),
        shortflags([o]),
        longflags([file]),
        help('Specify which file to save the output to')]
    ]).
    
    





    
    
      


