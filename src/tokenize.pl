:- module(tokenize,[string_tokens/2]).

/*transform the input term into tokens readable by the parser*/
string_tokens(String, Tokens):-
    string_chars(String, Chars),
    phrase(tokens(Tokens), Chars).

    
    
/* ignores whitespace*/
tokens([T|Ts]) --> ws,token(T),ws,!,tokens(Ts).
tokens([]) --> [].

ws -->[W],{char_type(W, space)},ws.
ws --> [].

/* separate punctuation and alphanumeric strings*/
token(P) --> [C], { member(C,['\\','.',',','+','-','*','(',')','@']), string_chars(P, [C]) }. 
token(W) --> word(Cs), { string_chars(W, Cs) }.


/* parsing words */
word([L|Ls])      --> [L], { char_type(L, alnum) }, word_rest(Ls).
word_rest([L|Ls]) --> [L], { char_type(L, alnum) }, word_rest(Ls).
word_rest([])     --> [].
