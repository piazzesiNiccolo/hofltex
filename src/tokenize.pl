:- module(tokenize,[string_tokens/2]).

string_tokens(String, Tokens):-
    string_chars(String, Chars),
    phrase(tokens(Tokens), Chars).

    
    

tokens([T|Ts]) --> ws,token(T),ws,!,tokens(Ts).
tokens([]) --> [].

ws -->[W],{char_type(W, space)},ws.
ws --> [].

token(P) --> [C], { member(C,['\\','.',',','+','-','*','(',')','@']), string_chars(P, [C]) }. 
token(W) --> word(Cs), { string_chars(W, Cs) }.


word([L|Ls])      --> [L], { char_type(L, alnum) }, word_rest(Ls).
word_rest([L|Ls]) --> [L], { char_type(L, alnum) }, word_rest(Ls).
word_rest([])     --> [].
