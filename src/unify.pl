:- module(unify, [unify_type/2]).


subterms(int,[]).

subterms(prod(A,B),[A,B]).

subterms(func(A,B),[A,B]).
