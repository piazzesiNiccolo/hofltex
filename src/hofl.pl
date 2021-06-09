:- module(hofl,[parse/2,parse_from_file/2]).

:- use_module(tokenize).
:- table pre_term//1,expr//1,basic_term//1,fact//1. 
/**
The binary expressions t0 [+,-,*] t1 and function application (t0 t1) rules are left recursive. Using table allows memoization
of previous results, making the parse predicate terminate as expected (packrat parsing)
*/

/**
 * Produces the ast for the term. Possible terms are:
 * - lambda(A,B) -> lambda term, A is the bound variable, B the body
 * - rec(A,B) -> same structure of lambda but for recursive terms
 * - int(N) -> represent a number N
 * - id(A) -> represent the variable A
 * - bin_op("+"|"-",A,B) -> A +|- B
 * - mul(A,B) A*B. It's a different term from plus/minus so that we can better enforce operator precedence
 * - tuple(A,B)
 * - fst(A)
 * - snd(A)
 * - cond(A,B,C)  -> if A then B else C
 * - apply(A,B) ->   A@B  
 * 
 * The grammar has been modified to mantain the left associativity of binary operators. For simplicity
 * i only allow lambda  and recursive definition as expression if they are inside parentheses. This allowed for better handling of parsing precedence
 * without having any significant change on the semantics. 
 *
 * */
parse(String,Term) :- string_tokens(String,Tokens),phrase(pre_term(Term), Tokens).

parse_from_file(File,Term) :- 
    read_file_to_string(File, String, []),
    parse(String,Term).


pre_term(X) --> ["("],pre_term(X),[")"].
pre_term(rec(id(X),Y)) -->["rec"], pre_term(id(X)),!,["."],pre_term(Y). 
pre_term(lambda(id(X),Y)) --> ["\\"] , pre_term(id(X)),!,["."],pre_term(Y).
pre_term(X) --> expr(X).

expr(X) --> basic_term(X).
expr(bin_op("+",X,Y)) --> expr(X),["+"], basic_term(Y).
expr(bin_op("-",X,Y)) --> expr(X),["-"], basic_term(Y).


basic_term(mul(X,Y)) --> basic_term(X), ["*"], fact(Y).
basic_term(X) --> fact(X).
fact(X) --> ["("],pre_term(X),[")"].
fact(apply(X,Y)) -->  pre_term(X),["@"], pre_term(Y).
fact(cond(X,Y,Z))--> ["if"],pre_term(X),["then"],pre_term(Y),["else"],pre_term(Z).
fact(tuple(X,Y)) --> ["("],pre_term(X),[","],pre_term(Y),[")"].
fact(fst(X)) --> ["fst"], pre_term(X). 
fact(snd(X)) --> ["snd"], pre_term(X).


fact(int(X)) --> [Y],{number_string(X, Y)}.

fact(id(X)) --> [Y],{\+number_string(X,Y),atom_string(X, Y),\+member(Y,["\\",".",",","+","-","*","(",")","@"])}.




