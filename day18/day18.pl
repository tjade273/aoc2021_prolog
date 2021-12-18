#!/usr/bin/env swipl

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

:- initialization main.

% Input Parsing

snail_number(N) --> integer(N).
snail_number([X, Y]) --> "[", snail_number(X), ",", snail_number(Y), "]".
input_line(L) --> snail_number(L), "\n".
input(Ls) --> sequence(input_line, Ls).

parse_input(File, Ls) :-
    phrase_from_file(input(Ls), File).


% Part 1

split(N, [N0, N1]) :-
    integer(N),
    N #> 9,
    N0 #= N // 2,
    N1 #= N // 2 + N mod 2.

split([L, R], [L1, R]) :- split(L, L1), !.
split([L, R], [L, R1]) :- split(R, R1).

add_left(X, N, Y) :-
    integer(X),
    Y #= X + N.

add_left([L, R], N, [L1, R]) :-
    add_left(L, N, L1).

add_right(X, N, Y) :-
    integer(X),
    Y #= X + N.

add_right([L, R], N, [L, R1]) :-
    add_right(R, N, R1).

explode([L, R], 0, 0, [L, R]).
explode([L, R], Height, [L1, R1], [NL, 0]) :-
    Height #> 0,
    H1 #= Height - 1,
    explode(L, H1, L1, [NL, NR]),
    add_left(R, NR, R1), !.

explode([L, R], Height, [L1, R1], [0, NR]) :-
    Height #> 0,
    H1 #= Height - 1,
    explode(R, H1, R1, [NL, NR]),
    add_right(L, NL, L1).

explode(X, Y) :-
    explode(X, 4, Y, _).

reduce_step(X, Y) :- explode(X, Y), !.
reduce_step(X, Y) :-split(X, Y).

reduce(X, Y) :- reduce_step(X, Y1), !, reduce(Y1, Y).
reduce(X, X).

add(X, Y, Z) :-
    reduce([X, Y], Z).

sum([H|T], X) :-
    foldl([X, V0, V] >> add(V0, X, V), T, H, X). 

magnitude(X, X) :- integer(X), !.
magnitude([L, R], X) :- 
    magnitude(L, XL), 
    magnitude(R, XR), 
    X #= 3 * XL + 2 * XR.


% Part 2

pair_magnitude(L, M) :-
    member(X, L),
    member(Y, L),
    X \= Y,
    add(X, Y, Z),
    magnitude(Z, M).

max_pair_magnitude(Ls, Max) :-
    findall(M, pair_magnitude(Ls, M), Ms),
    max_list(Ms, Max).

main :- 
    parse_input('input.txt', Ls),
    sum(Ls, S),
    magnitude(S, N1),
    print(N1), nl,
    max_pair_magnitude(Ls, N2),
    print(N2), nl,
    halt(0).
