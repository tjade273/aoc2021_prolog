#!/usr/bin/env swipl

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

:- initialization main.

% (Hex) Input Parsing

input(N) --> xinteger(N), "\n".
parse_input(File, S) :-
    phrase_from_file(input(N), File),
    format(codes(S), "~2r", [N]).

% Packet Parsing

binary_number(Bs, N) :-
   binary_number_min(Bs, 0,N, N).

binary_number_min([], N,N, _M).
binary_number_min([B|Bs], N0,N, M) :-
   B in 0..1,
   N1 #= B+2*N0,
   M #>= N1,
   binary_number_min(Bs, N1,N, M).

bit(0) --> `0`.
bit(1) --> `1`.
bit_n(X) --> {member(X, [0, 1])}, [X].
binary_number(N, Len) --> {length(Bits, Len)}, sequence(bit, Bits), {binary_number(Bits, N)}.

packet_header(Version, TypeID) --> binary_number(Version, 3), binary_number(TypeID, 3). 

lstop, [lstop] --> [].
literal, Bits --> bit(0), {length(Bits, 4)}, sequence(bit, Bits), lstop.
literal, Bits --> bit(1), {length(Bits, 4)}, sequence(bit, Bits), literal.
literal(N) --> literal, sequence(bit_n, Bits), [lstop], {binary_number(Bits, N)}.


stopat(0), [stopat] --> [].
stopat(N), [X] --> [X], {N #> 0, N1 #= N - 1}, stopat(N1).

subpackets(0, Packets) --> binary_number(PacketLen, 15), stopat(PacketLen), sequence(packet, Packets), [stopat].
subpackets(1, Packets) --> binary_number(PacketLen, 11), {length(Packets, PacketLen)}, sequence(packet, Packets).

packet_body(4, [N]) --> literal(N).
packet_body(_, Packets) --> bit(LType), subpackets(LType, Packets).

packet((Version, TypeID, Body)) --> packet_header(Version, TypeID), packet_body(TypeID, Body).

zero(_) --> `0`.
transmission(Packet) --> packet(Packet), sequence(zero, _).


% Part 1
part1((Version, 4, _), Version) :- !.
part1((Version, _, Packets), N) :- 
    maplist(part1, Packets, Ns),
    sum_list(Ns, N0),
    N #= N0 + Version.

% Part 2

lop(0, L, X) :- sum_list(L, X).
lop(1, [X], X).
lop(1, [H|T], X) :- lop(1, T, X1), X #= H * X1.
lop(2, L, X) :- min_list(L, X).
lop(3, L, X) :- max_list(L, X).
lop(4, [X], X).
lop(5, [A, B], 1) :- A #> B.
lop(5, [A, B], 0) :- A #=< B.
lop(6, [A, B], X) :- lop(5, [B, A], X).
lop(7, [A, B], 1) :- A #= B.
lop(7, [A, B], 0) :- A #\= B.

eval(N, N) :- integer(N).

eval((_, TypeID, Body), N) :- 
    maplist(eval, Body, Results), 
    lop(TypeID, Results, N).

main :-
    parse_input('input.txt', Input),
    phrase(transmission(T), Input),
    part1(T, N1),
    print(N1), nl,
    eval(T, N2),
    print(N2), nl,
    halt(0).

