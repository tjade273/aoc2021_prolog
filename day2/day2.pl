#!/usr/bin/env swipl

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

:- initialization main.

direction(forward) --> "forward".
direction(up) --> "up".
direction(down) --> "down".
command(D, N) --> direction(D), " ", digits(NCodes), {number_codes(N, NCodes)}, "\n".

commands([command(D, N)|T]) --> command(D, N), commands(T).
commands([]) --> [].

execute_part1(command(forward, N), position(X_in, Y), position(X_out, Y)) :- X_out #= X_in + N.
execute_part1(command(up, N), position(X, Y_in), position(X, Y_out)) :- Y_out #= Y_in - N.
execute_part1(command(down, N), position(X, Y_in), position(X, Y_out)) :- Y_out #= Y_in + N.


execute_part2(command(forward, N), position(X_in, Y_in, Aim), position(X_out, Y_out, Aim)) :- X_out #= X_in + N, Y_out #= Y_in + N * Aim.
execute_part2(command(up, N), position(X, Y, Aim_in), position(X, Y, Aim_out)) :- Aim_out #= Aim_in - N.
execute_part2(command(down, N), position(X, Y, Aim_in), position(X, Y, Aim_out)) :- Aim_out #= Aim_in + N.

main :-
    phrase_from_file(commands(Commands), 'input.txt'),
    foldl(execute_part1, Commands, position(0, 0), position(X1, Y1)),
    foldl(execute_part2, Commands, position(0, 0, 0), position(X2, Y2, _)),
    Res1 #= X1 * Y1,
    Res2 #= X2 * Y2,
    print(Res1), nl,
    print(Res2), nl,
    halt(0).



