:- module(interpret, [interpret/1]).

interpret(T) :- 
  phrase(as, T).
  

as --> [].
as --> [a],as.