#!/usr/bin/env swipl

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

:- initialization main.

input_char(X) --> [C], {member(C, `()[]{}<>`), char_code(X, C)}.
input_line(L) --> sequence(input_char, L), "\n".
input(Ls) --> sequence(input_line, Ls).
parse_input(Ls) :- phrase_from_file(input(Ls), 'input.txt').

matching('(', ')').
matching('[', ']').
matching('{', '}').
matching('<', '>').

points1(')', 3).
points1(']', 57).
points1('}', 1197).
points1('>', 25137).

first_error_([Open|Opens], [Close|Remaining], Err) :- matching(Open, Close), !, first_error_(Opens, Remaining, Err).
first_error_(Opens, [Open|Remaining], Err) :- matching(Open, _), !, first_error_([Open|Opens], Remaining, Err).
first_error_(_, [Close|_], Err) :- points1(Close, Err). 

first_error(Line, Err) :- first_error_([], Line, Err).

part1(Ls, Res) :- 
    convlist(first_error, Ls, Errs),
    sum_list(Errs, Res).


points2('(', 1).
points2('[', 2).
points2('{', 3).
points2('<', 4).


add_score(X, Acc0, Acc) :- points2(X, P), Acc #= 5*Acc0 + P.
complete_score(Opens, [], Score) :- foldl(add_score, Opens, 0, Score).
complete_score(Opens, [Open|Rest], Score) :- matching(Open, _), complete_score([Open|Opens], Rest, Score), !.
complete_score([Open|Opens], [Close|Rest], Score) :- matching(Open, Close), complete_score(Opens, Rest, Score).

complete_score(Chars, Score) :- complete_score([], Chars, Score).


part2(Ls, Res) :-
    convlist(complete_score, Ls, Scores),
    msort(Scores, Sorted),
    length(Scores, N),
    M #= N // 2,
    nth0(M, Sorted, Res).

main :-
    parse_input(Ls),
    part1(Ls, Part1),
    part2(Ls, Part2),
    print(Part1), nl,
    print(Part2), nl,
    halt(0).
