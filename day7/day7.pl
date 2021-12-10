#!/usr/bin/env swipl

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

:- initialization main.

crabs(Ns) --> sequence(integer, ",", Ns), "\n".

norm1(X, Y, N) :- N #= abs(X - Y).
norm2(X, Y, N) :- N #= (X - Y) ^ 2.

% CLP(FD) Based Part1
% Slow, brute-force. Seeking improvements
cost_at_position(Crabs, Pos, Cost) :-
    max_list(Crabs, MaxPos),
    min_list(Crabs, MinPos),
    Pos in MinPos..MaxPos,
    maplist(norm1(Pos), Crabs, Costs),
    sum(Costs, #=, Cost).

lowest_cost_part1_clpfd(Crabs, Cost) :- 
    cost_at_position(Crabs, Pos, Cost),
    once(labeling([enum, min(Cost)], [Pos, Cost])).

% Manual Part 1
% There are better (O(n)) ways of doing this, but sorting is fast enough
median(Crabs, Median) :-
    msort(Crabs, Sorted),
    length(Crabs, N),
    M #= N // 2,
    nth0(M, Sorted, Median).

lowest_cost_part1(Crabs, MinCost) :-
    median(Crabs, M),
    maplist(norm1(M), Crabs, Norms),
    sum_list(Norms, MinCost).

% Manual Part2 
mean(Crabs, Mean) :-
    sum_list(Crabs, S),
    length(Crabs, N),
    Mean #= S // N.

total_norm2(Crabs, Pos, Cost) :-
    maplist(norm1(Pos), Crabs, Norms1),
    maplist(norm2(Pos), Crabs, Norms2),
    sum_list(Norms1, N1),
    sum_list(Norms2, N2),
    Cost #= N1 + N2.

lowest_cost_part2(Crabs, MinCost) :-
    mean(Crabs, M1),
    total_norm2(Crabs, M1, Cost1),
    M2 #= M1 + 1,
    total_norm2(Crabs, M2, Cost2),
    MinCost #= min(Cost1, Cost2) // 2.


main :-
    phrase_from_file(crabs(Crabs), 'input.txt'),
    lowest_cost_part1(Crabs, MinCost1),
    lowest_cost_part2(Crabs, MinCost2),
    print(MinCost1), nl,
    print(MinCost2), nl,
    halt(0).
