#!/usr/bin/env swipl

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

:- initialization main.

measurements([H|T]) --> digits(H), "\n", measurements(T).
measurements([]) --> [].

increases(X, [H1,H2|T]) :- H2 #> H1, increases(Y, [H2|T]), X #= Y + 1, !.
increases(X, [_,H2|T]) :- increases(X, [H2|T]).
increases(0, [_]).
increases(0, []).

window_sum([X|Y], [H1,H2,H3|T]) :- X #= H1 + H2 + H3, window_sum(Y, [H2,H3|T]), !.
window_sum([], _).

main :- 
    phrase_from_file(measurements(InCodes), 'input.txt'),
    maplist(number_codes, Depths, InCodes),
    increases(Incrs1, Depths),
    print(Incrs1), nl,
    window_sum(Windows, Depths),
    increases(Incrs2, Windows),
    print(Incrs2), nl,
    halt(0).

