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
%parse_input(Grid) :- phrase_from_file(grid(Grid), 'test.txt').

% Part 1

% Convert the grid to an association list of index -> value mappings. 
% This speeds the overall runtime from O(N^2) to O(NlogN).

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

has_low_neighbor(Grid, Index) :-
    get_assoc(Index, Grid, Self), 
    neighbor(Index, (I1, J1)),
    label([I1, J1]),
    get_assoc((I1, J1), Grid, Neighbor),
    Self #>= Neighbor.
    
local_minimum(Grid, Minimum) :-
    gen_assoc(Index, Grid, Minimum),
    \+ has_low_neighbor(Grid, Index).

part1(Grid, N) :-
    findall(Minimum, local_minimum(Grid, Minimum), Mins),
    length(Mins, L),
    sum_list(Mins, S),
    N #= L + S.

main :-
    parse_input(Grid),
    grid_to_assoc(Grid, Assoc),
    part1(Assoc, N1),
    print(N1), nl,
    halt(0).
