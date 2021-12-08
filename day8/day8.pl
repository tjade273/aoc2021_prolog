#!/usr/bin/env swipl

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

:- initialization main.

signal(S) --> {member(C, `abcdefg`), char_code(S, C)}, [C].

signals(Signals) --> sequence(signal, Signals).

input_line((TestDigits, Outputs)) --> 
    {length(TestDigits, 10)}, sequence(signals, " ", TestDigits), " | ", 
    {length(Outputs, 4)}, sequence(signals, " ", Outputs), "\n".

inputs(Lines) --> sequence(input_line, Lines).

parse_input(Lines) :- phrase_from_file(inputs(Lines), 'input.txt').


unique_length(L) :- length(L, X), member(X, [2,3,4,8]).
unique_outputs((_, Outputs), N) :- include(unique_length, Outputs, Uniques), length(Uniques, N).
part1(Lines, Count) :- maplist(unique_outputs, Lines, Ns), sum_list(Ns, Count).


mem_of_len(Ds, N, X) :- member(L, Ds), length(L, N), member(X, L).

% X is a member of all digits of length Len
mem_of_all([], _, _).
mem_of_all([D|Ds], Len, X) :- length(D, Len), member(X, D), mem_of_all(Ds, Len, X).
mem_of_all([D|Ds], Len, X) :- length(D, L), L #\= Len, mem_of_all(Ds, Len, X).

% X is a member of no digits of length Len
mem_of_none([], _, _).
mem_of_none([D|Ds], Len, X) :- length(D, Len), \+ member(X, D), mem_of_none(Ds, Len, X).
mem_of_none([D|Ds], Len, X) :- length(D, L), L #\= Len, mem_of_none(Ds, Len, X).

translate(Ds, X, a) :- mem_of_all(Ds, 3, X), mem_of_none(Ds, 2, X).
translate(Ds, X, b) :- mem_of_all(Ds, 4, X), mem_of_none(Ds, 2, X), mem_of_all(Ds, 6, X).
translate(Ds, X, c) :- mem_of_all(Ds, 2, X), \+ mem_of_all(Ds, 6, X).
translate(Ds, X, d) :- mem_of_all(Ds, 4, X), mem_of_none(Ds, 2, X), \+ mem_of_all(Ds, 6, X).
translate(Ds, X, e) :- mem_of_none(Ds, 3, X), mem_of_none(Ds, 4, X), \+ mem_of_all(Ds, 6, X).
translate(Ds, X, f) :- mem_of_all(Ds, 2, X), mem_of_all(Ds, 6, X).
translate(Ds, X, g) :- mem_of_none(Ds, 3, X), mem_of_none(Ds, 4, X), mem_of_all(Ds, 6, X).

translate_digit(TestOuts, Segments, N) :- 
    maplist(translate(TestOuts), Segments, RealSegs),
    sort(RealSegs, Sorted),
    segments(Sorted, N).

digits_number_rev([], 0).
digits_number_rev([D|Ds], N) :-
    D in 0..9,
    digits_number_rev(Ds, N1),
    N #= 10 * N1 + D.

digits_number(Ds, N) :-
    reverse(Ds, DsRev),
    digits_number_rev(DsRev, N).

translate_number((TestOuts, SegList), N) :- 
    maplist(translate_digit(TestOuts), SegList, Ns),
    digits_number(Ns, N).

segments([a,b,c,e,f,g], 0).
segments([c, f], 1).
segments([a,c,d,e,g], 2).
segments([a,c,d,f,g], 3).
segments([b, c, d, f], 4).
segments([a,b,d,f,g], 5).
segments([a,b,d,e,f,g], 6).
segments([a,c,f], 7).
segments([a,b,c,d,e,f,g], 8).
segments([a,b,c,d,f,g], 9).


part2(Lines, N) :-
    maplist(translate_number, Lines, Ns),
    sum_list(Ns, N).

main :- 
    parse_input(Lines), 
    part1(Lines, Soln1),
    print(Soln1), nl,
    part2(Lines, Soln2),
    print(Soln2), nl,
    halt(0).
