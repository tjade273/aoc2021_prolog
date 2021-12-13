#!/usr/bin/env swipl

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

:- initialization main.

point((X, Y)) --> integer(X), ",", integer(Y), "\n".
fold(x(X)) --> "fold along x=", integer(X), "\n".
fold(y(Y)) --> "fold along y=", integer(Y), "\n".
input((Ps, Fs)) --> sequence(point, Ps), "\n", sequence(fold, Fs).

parse_input(Ps, Fs) :- phrase_from_file(input((Ps, Fs)), 'input.txt').

fold_point(x(Xr), (X0, Y), (X, Y)) :-
    X #= Xr - abs(Xr - X0).
fold_point(y(Yr), (X, Y0), (X, Y)) :-
    Y #= Yr - abs(Yr - Y0).

fold_points(Rs, Ps0, Ps) :-
    foldl([R]>>maplist(fold_point(R)), Rs, Ps0, Ps).

part1(Ps, [F|_]) :-
    fold_points([F], Ps, Ps1),
    sort(Ps1, Sorted),
    length(Sorted, N),
    print(N), nl.

out_point(Ps, X, Y) --> {member((X, Y), Ps)}, !, "#".
out_point(_, _, _) --> " ".
out_line(Ps, XMax, Y) --> foreach(between(0, XMax, X), out_point(Ps, X, Y)), "\n".
out_grid(Ps, XMax, YMax) --> foreach(between(0, YMax, Y), out_line(Ps, XMax, Y)).

part2(Ps0, Fs) :-
    fold_points(Fs, Ps0, Ps),
    phrase(out_grid(Ps, 100, 6), Grid),
    string_codes(GridS, Grid),
    write(GridS).

main :- 
    parse_input(Ps, Fs),
    part1(Ps, Fs),
    part2(Ps, Fs).
