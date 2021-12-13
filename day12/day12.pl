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

parse_input(L) :- phrase_from_file(edges(L), 'test2.txt').
%parse_input(L) :- phrase_from_file(edges(L), 'input.txt').

connected(Graph, V1, V2) :- 
    member(V1-V2, Graph);
    member(V2-V1, Graph).

remove_v([], _, []).
remove_v([V-_|T], V, Graph) :- remove_v(T, V, Graph), !.
remove_v([_-V|T], V, Graph) :- remove_v(T, V, Graph), !.
remove_v([H|T], V, [H|Graph]) :- remove_v(T, V, Graph).

remove_dups([], []).
remove_dups([dup(_)-_|T], Graph) :- remove_dups(T, Graph), !.
remove_dups([_-dup(_)|T], Graph) :- remove_dups(T, Graph), !.
remove_dups([H|T], [H|Graph]) :- remove_dups(T, Graph).

remove(Graph, big(_), Graph).
remove(Graph0, small(V), Graph) :- remove_v(Graph0, small(V), Graph).
remove(Graph0, dup(V), Graph) :- 
    remove_dups(Graph0, Graph1), 
    remove_v(Graph1, small(V), Graph).

cons(X, Y, [X|Y]).

paths_from([], _, []).
paths_from(_, small("end"), [[small("end")]]) :- !.
paths_from(Graph, V, Paths) :-
    findall(V2, connected(Graph, V, V2), V2s),
    remove(Graph, V, Graph1),  
    maplist(paths_from(Graph1), V2s, Paths0),
    append(Paths0, Paths1),
    maplist(cons(V), Paths1, Paths).

normalize_v(dup(V), small(V)) :- !.
normalize_v(V, V).

normalize_path(P0, P) :- maplist(normalize_v, P0, P).

path_count(Graph, N) :-
    paths_from(Graph, small("start"), Paths),
    maplist(normalize_path, Paths, PNorm),
    sort(PNorm, PUniq),
    length(PUniq, N).

duplicate_graph([small(V1)-V2|T], [dup(V1)-V2, small(V1)-V2|T1]) :- 
    V1 \= "start", 
    V1 \= "end", !, 
    duplicate_graph(T, T1).
duplicate_graph([V2-small(V1)|T], [V2-dup(V1), V2-small(V1)|T1]) :- 
    V1 \= "start", 
    V1 \= "end", !, 
    duplicate_graph(T, T1).
duplicate_graph([H|T], [H|T1]) :- duplicate_graph(T, T1).
duplicate_graph([], []).


