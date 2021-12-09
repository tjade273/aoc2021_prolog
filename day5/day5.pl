#!/usr/bin/env swipl
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main).

point((X, Y)) --> integer(X), ",", integer(Y).
line((P1, P2)) --> point(P1), " -> ", point(P2), "\n".
input(Lines) --> sequence(line, Lines).

parse_input(Lines) :- phrase_from_file(input(Lines), "input.txt").

in_bounds(N, N0, N1) :-
   ( N0 #=< N1
   -> N in N0..N1
  ; N in N1..N0 ). 

on_line((X, Y0), ((X, Y1), (X, Y2))) :- in_bounds(Y0, Y1, Y2).
on_line((X0, Y), ((X1, Y), (X2, Y))) :- in_bounds(X0, X1, X2).

on_line((X0, Y0), ((X1, Y1), (X2, Y2))) :-
    X2 - X1 #= Y2 - Y1,
    X0 - X1 #= Y0 - Y1,
    in_bounds(X0, X1, X2),
    in_bounds(Y0, Y1, Y2).

on_line((X0, Y0), ((X1, Y1), (X2, Y2))) :-
    X2 - X1 #= Y1 - Y2,
    X0 - X1 #= Y1 - Y0,
    in_bounds(X0, X1, X2),
    in_bounds(Y0, Y1, Y2).

in_intersection(P, L1, L2) :-
    L1 \= L2, on_line(P, L1), on_line(P, L2).

intersection(L1, L2, Ps) :-
    in_intersection((X, Y), L1, L2),
    findall((X, Y), label([X, Y]), Ps).

on_some_line(P, [L | _]) :- on_line(P, L).
on_some_line(P, [_ | Ls]) :- on_some_line(P, Ls).

in_some_intersection((X, Y), [L | Ls]) :- 
    on_line((X, Y), L), on_some_line((X, Y), Ls), label([X, Y]).
in_some_intersection(P, [_ | Ls]) :- 
    in_some_intersection(P, Ls).

all_intersections(Lines, Ints) :-
    findall(P, in_some_intersection(P, Lines), Ints).
    
count_intersections(Lines, N) :-
    all_intersections(Lines, Ints),
    sort(Ints, Sorted),
    length(Sorted, N).

is_aligned(((X, _), (X, _))).
is_aligned(((_, Y), (_, Y))).

main :- 
    parse_input(Lines),
    include(is_aligned, Lines, Aligned),
    count_intersections(Aligned, Part1),
    count_intersections(Lines, Part2),
    print(Part1),nl,
    print(Part2), nl,
    halt(0).





