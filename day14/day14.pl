#!/usr/bin/env swipl

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

:- initialization main.

char(C) --> [X], {between(0'A, 0'Z, X), char_code(C, X)}.
template(T) --> sequence(char, T), "\n".
rule((X, Z, Y)) --> char(X), char(Y), " -> ", char(Z), "\n".
input(Template, Rules) --> template(Template), "\n", sequence(rule, Rules).

parse_input(Template, Rules) :- phrase_from_file(input(Template, Rules), 'input.txt').

coalesce([X-N0, X-N1|T], L) :-
    !, N #= N0 + N1,
    coalesce([X-N|T], L).
coalesce([H|T0], [H|T]) :- coalesce(T0, T).
coalesce([], []).

pairs_from_template(Templ, Pairs) :-
    findall((X, Y), nextto(X, Y, Templ), Ps),
    msort(Ps, Psort),
    clumped(Psort, Pairs).

form_pair(Rules, Pairs, (X, Y), N) :- 
    member((X, Y, Z), Rules),
    member((X, Z)-N, Pairs).
form_pair(Rules, Pairs, (X, Y), N) :- 
    member((Z, X, Y), Rules),
    member((Z, Y)-N, Pairs).

step(Rules, Pairs0, Pairs) :-
    findall(P-N, form_pair(Rules, Pairs0, P, N), Ps),
    msort(Ps, PairsDups),
    coalesce(PairsDups, Pairs).

step_n(_, Pairs, 0, Pairs).
step_n(Rules, Pairs0, N, Pairs) :-
    N #> 0,
    N1 #= N - 1,
    step(Rules, Pairs0, Pairs1),
    step_n(Rules, Pairs1, N1, Pairs).

fst((X, _)-N, X-N).

solve(Template, Rules, Iters, N) :-
    last(Template, Last),
    pairs_from_template(Template, Pairs0),
    step_n(Rules, Pairs0, Iters, Pairs),
    maplist(fst, Pairs, Chars),
    msort([Last-1|Chars], CharsS),
    coalesce(CharsS, CharsC),
    pairs_values(CharsC, Vals),
    sort(Vals, Vsort),
    min_list(Vsort, Min),
    max_list(Vsort, Max),
    N #= Max - Min.

main :-
    parse_input(Template, Rules),
    solve(Template, Rules, 10, N1),
    solve(Template, Rules, 40, N2),
    print(N1), nl,
    print(N2), nl,
    halt(0).
