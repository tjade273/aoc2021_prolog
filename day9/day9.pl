#!/usr/bin/env swipl

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

:- initialization main.

% Input Parsing

depth(D) --> digit(Dc), {number_codes(D, [Dc])}.
row(R) --> sequence(depth, R), "\n".
grid(G) --> sequence(row, G).

parse_input(Grid) :- phrase_from_file(grid(Grid), 'input.txt').

% Part 1

% Convert the grid to an association list of index -> value mappings. 
% Tail-recursive for funzies
grid_to_assoc_([], _, _, Assoc, Assoc).

grid_to_assoc_([[]|Rows], _, J, AssocAcc, Assoc) :- 
    J1 #= J + 1,
    grid_to_assoc_(Rows, 0, J1, AssocAcc, Assoc).

grid_to_assoc_([[Depth|Depths]|Rows] , I, J, AssocAcc, Assoc) :-
    I1 #= I + 1, 
    put_assoc((I,J), AssocAcc, Depth, AssocAcc1),
    grid_to_assoc_([Depths|Rows], I1, J, AssocAcc1, Assoc).

grid_to_assoc(Grid, Assoc) :- 
    empty_assoc(Empty),
    grid_to_assoc_(Grid, 0, 0, Empty, Assoc).

neighbor((I, J0), (I, J1)) :-
    abs(J1 - J0) #= 1.
neighbor((I0, J), (I1, J)) :-
    abs(I1 - I0) #= 1.

lower_neighbor(Grid, Index, (I1, J1)) :-
    get_assoc(Index, Grid, Self),
    neighbor(Index, (I1, J1)),
    label([I1, J1]),
    get_assoc((I1, J1), Grid, Neighbor),
    Neighbor #< Self.
    
local_minimum(Grid, Minimum) :-
    gen_assoc(Index, Grid, Minimum),
    \+ lower_neighbor(Grid, Index, _).

part1(Grid, N) :-
    findall(Minimum, local_minimum(Grid, Minimum), Mins),
    length(Mins, L),
    sum_list(Mins, S),
    N #= L + S.


% Part 2

:- table flows_into/3.
flows_into(Grid, Index, Index) :- get_assoc(Index, Grid, 9), !.
flows_into(Grid, Index, LocalMin) :-
    lower_neighbor(Grid, Index, IndexLower), !,
    flows_into(Grid, IndexLower, LocalMin).
flows_into(_, LocalMin, LocalMin).


swap(X-Y, Y-X).

part2(Grid, N) :-
    assoc_to_keys(Grid, Ids),
    maplist(flows_into(Grid), Ids, Minima),
    msort(Minima, Sorted),
    clumped(Sorted, Clumped), 
    maplist(swap, Clumped, Swapped),
    msort(Swapped, SwapSort),
    reverse(SwapSort, [X1-_, X2-_, X3-_ | _]),
    N #= X1 * X2 * X3.

main :-
    parse_input(Grid),
    grid_to_assoc(Grid, Assoc),
    part1(Assoc, N1),
    part2(Assoc, N2),
    print(N1), nl,
    print(N2), nl,
    halt(0).
