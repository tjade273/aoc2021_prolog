#!/usr/bin/env swipl

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

%:- initialization main.

capital(X) --> {between(0'A, 0'Z, X)}, [X].
lowercase(X) --> {between(0'a, 0'z, X)}, [X].
cave(big(S)) --> sequence(capital, X), {string_codes(S, X)}.
cave(small(S)) --> sequence(lowercase, X), {string_codes(S, X)}.


edge(C1-C2) --> cave(C1), "-", cave(C2), "\n".
edges(L) --> sequence(edge, L).

parse_input(L) :- phrase_from_file(edges(L), 'input.txt').
parse_test(L) :- phrase_from_file(edges(L), 'test.txt').

connected(Graph, V1, V2) :- 
    member(V1-V2, Graph);
    member(V2-V1, Graph).

remove([], _, []).
remove([small(V)-_|T], small(V), Graph) :- remove(T, small(V), Graph), !.
remove([_-small(V)|T], small(V), Graph) :- remove(T, small(V), Graph), !.
remove([H|T], V, [H|Graph]) :- remove(T, V, Graph).

cons(X, Y, [X|Y]).

paths_from(_, small("end"), 1) :- !.
paths_from(Graph, V, N) :-
    findall(V2, connected(Graph, V, V2), V2s),
    remove(Graph, V, Graph1),  
    maplist(paths_from(Graph1), V2s, Ns),
    sum_list(Ns, N).


