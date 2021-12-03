#!/usr/bin/env swipl

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

:- initialization main.

bit(0) --> "0".
bit(1) --> "1".

input_line(L) --> sequence(bit, L), "\n".
input(Ls) --> sequence(input_line, Ls).

majority(Bs, 1) :- length(Bs, L), sum(Bs, #=, X), 2 * X #>= L.
majority(Bs, 0) :- length(Bs, L), sum(Bs, #=, X), 2 * X #< L.

gamma(Bits, Gamma) :- 
    transpose(Bits, Bt),
    maplist(majority, Bt, Gamma).

neg([], []).
neg([1|T0], [0|T]) :- neg(T0, T).
neg([0|T0], [1|T]) :- neg(T0, T).

% https://stackoverflow.com/a/28442760
binary_number(Bs, N) :-
   binary_number_min(Bs, 0,N, N).

binary_number_min([], N,N, _M).
binary_number_min([B|Bs], N0,N, M) :-
   B in 0..1,
   N1 #= B+2*N0,
   M #>= N1,
   binary_number_min(Bs, N1,N, M).

gas_filter([], _, _, []).
gas_filter([H|T], I, X, [H|T0]) :- 
    nth0(I, H, X), gas_filter(T, I, X, T0).
gas_filter([H|T], I, X, T0) :-
    nth0(I, H, Y), Y #\= X, gas_filter(T, I, X, T0).
   
oxygen_rating(_, [N], N) :- !.
oxygen_rating(I, Ns, N) :-
    gamma(Ns, Gamma),
    nth0(I, Gamma, X),
    gas_filter(Ns, I, X, Ns1),
    I1 #= I + 1,
    oxygen_rating(I1, Ns1, N).

co2_rating(_, [N], N) :- !.
co2_rating(I, Ns, N) :-
    gamma(Ns, Gamma),
    nth0(I, Gamma, X),
    Y #= 1 - X,
    gas_filter(Ns, I, Y, Ns1),
    I1 #= I + 1,
    co2_rating(I1, Ns1, N).

main :-
    phrase_from_file(input(Bits), 'input.txt'),
    gamma(Bits, GammaB),
    neg(GammaB, EpsilonB),
    binary_number(GammaB, Gamma),
    binary_number(EpsilonB, Epsilon),
    Part1 #= Gamma * Epsilon,
    print(Part1), nl,

    oxygen_rating(0, Bits, OxygenB),
    co2_rating(0, Bits, CO2B),
    binary_number(OxygenB, Oxygen),
    binary_number(CO2B, CO2),
    Part2 #= Oxygen * CO2,
    print(Part2), nl,
    halt(0).
