#!/usr/bin/env swipl

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

:- initialization main.

% Input Parsing

% Convert between number an 9-bit padded list
binary_number(Bs, N) :-
    format(chars(X), "~`0t~2r~9+", [N]), maplist(atom_number, X, Bs).

pixel_char(0) --> ".".
pixel_char(1) --> "#".

pixel(Index), [PixelSet] --> 
    [PixelSet0],
    pixel_char(X),
    {put_assoc(Index, PixelSet0, X, PixelSet)}.

binary_index(X) :- 
    between(0, 511, N), 
    binary_number(X, N).

empty_set, [X] --> {empty_assoc(X)}, []. 
enhancement_alg(Pixels) -->
    empty_set,
    foreach(binary_index(X), pixel(X)),
    [Pixels], "\n".
    
zero_index, [(0,0)] --> [].
indexed_pixel(_), [(X, Y)] --> [(X0, Y)], {X #= X0 + 1}, pixel((X0, Y)).
indexed_pixel(_), [(0, Y), Ps] --> [(_, Y0), Ps], {Y #= Y0 + 1}, "\n".

image(Pixels) --> 
    empty_set, zero_index,
    sequence(indexed_pixel, _),
    [_, Pixels].

input(Enhance, Image) -->
    enhancement_alg(Enhance), "\n",
    image(Image).

parse_input(File, Enhance, Image) :- phrase_from_file(input(Enhance, Image), File).
parse_input(Enhance, Image) :- parse_input('input.txt', Enhance, Image).
parse_test(Enhance, Image) :- parse_input('test.txt', Enhance, Image).

neighbor((X, Y), (Nx, Ny)) :-
    Xd #= X - Nx,
    Yd #= Y - Ny,
    [Xd, Yd] ins -1..1.

neighbors(X, Ns) :-
    neighbor(X, (Nx, Ny)),
    findall((Nx, Ny), label([Ny, Nx]), Ns).

index_check(Image, _, Index, X) :- get_assoc(Index, Image, X), !.
index_check(_, Default, _, Default).

zip([], [], []).
zip([X|Xs], [Y|Ys], [(X, Y)|XYs]) :- zip(Xs, Ys, XYs).

min_max_bound(L, X) :-
    min_list(L, Min),
    max_list(L, Max),
    X #=< Max + 1,
    X #>= Min - 1.


enhance(Enhancement, Image, Default, (X, Y), V) :-
    assoc_to_keys(Image, Keys),
    zip(Xs, Ys, Keys),
    min_max_bound(Xs, X),
    min_max_bound(Ys, Y),
    label([X, Y]),
    neighbors((X, Y), Ns),
    maplist(index_check(Image, Default), Ns, Bits),
    get_assoc(Bits, Enhancement, V).

enhanced(Enhancement, Image0, Default0, Image, Default) :-
    findall(Idx-V, enhance(Enhancement, Image0, Default0, Idx, V), Pairs),
    list_to_assoc(Pairs, Image),
    min_assoc(Enhancement, _, V),
    Default #= Default0 xor V.


enhanced_n(_, 0, Image, Default, Image, Default).
enhanced_n(Enhancement, N, Image0, Default0, Image, Default) :-
    N #> 0,
    N1 #= N - 1,
    enhanced(Enhancement, Image0, Default0, Image1, Default1),
    enhanced_n(Enhancement, N1, Image1, Default1, Image, Default).

enhanced_n(Enhancement, N, Image0, Image) :-
    enhanced_n(Enhancement, N, Image0, 0, Image, _).

challenge(E, I, N0, N) :-
    enhanced_n(E, N0, I, I1), 
    assoc_to_list(I1, L), 
    include([X]>>(X = _-1), L, L1), 
    length(L1, N).
    
main :-
    parse_input(E, I),
    challenge(E, I, 2, N1),
    print(N1), nl,
    challenge(E, I, 50, N2),
    print(N2), nl,
    halt(0).
