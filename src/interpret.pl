:- module(interpret, [interpret/0]).

interpret :- 
    set_prolog_flag(toplevel_goal, prolog),
    current_prolog_flag(argv, Argv),
    argv_options(Argv, _, Options),
    option(file(File), Options,"a.tex"),
    write_ln(Argv),
    write_ln(File).
    