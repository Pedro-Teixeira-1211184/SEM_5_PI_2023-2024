:- module(path_finding, [find_path_handler/1]).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dispatch)).

% Define the HTTP handler
:- http_handler('/maps/pathBetweenFloors/:origin/:destination', find_path_handler, []).

% Adapted DFS predicates using the campus graph representation
find_path_handler(Request) :-
    http_parameters(Request, [origin(Org), destination(Dest)]),
    
    parse_location(Org, Origin),
    parse_location(Dest, Destination),
    
    dfs2_user_input(Origin, Destination, Cam),
    
    reply_json(json{path: Cam}).

% Predicate to parse the location string
parse_location(Location, (BuildingCode, Floor, Col, Lin)) :-
    atomic_list_concat([FloorCode, ColStr, LinStr], '-', Location),
    atom_number(ColStr, Col),
    atom_number(LinStr, Lin),
    BuildingCode = planta(BuildingCode, FloorCode, _, _, _, _).

dfs2_user_input(Dest, Dest, Cam) :-
    reverse(Cam, [_ | ReverseCam]),
    reverse(ReverseCam, Cam).

dfs2_user_input(Act, Dest, Cam) :-
    connected_user_input(Act, X),
    \+ member(X, Cam),
    dfs2_user_input(X, Dest, [X | Cam]).


:- consult('campus_graph.pl').

dfs(Orig, Dest, Cam) :-
    dfs2(Orig, Dest, [Orig], Cam).

dfs2(Dest, Dest, LA, Cam) :-
    reverse(LA, Cam).

dfs2(Act, Dest, LA, Cam) :-
    ligacel(Act, X),
    \+ member(X, LA),
    dfs2(X, Dest, [X | LA], Cam).

all_dfs(Orig, Dest, LCam) :-
    findall(Cam, dfs(Orig, Dest, Cam), LCam).

better_dfs(Orig, Dest, Cam) :-
    all_dfs(Orig, Dest, LCam),
    shortlist(LCam, Cam, _).

shortlist([L], L, N) :- !, length(L, N).
shortlist([L | LL], Lm, Nm) :- shortlist(LL, Lm1, Nm1),
    length(L, NL),
    ((NL < Nm1, !, Lm = L, Nm is NL); (Lm = Lm1, Nm is Nm1)).

bfs(Orig, Dest, Cam) :- bfs2(Dest, [[Orig]], Cam).

bfs2(Dest, [[Dest | T] | _], Cam) :-
    reverse([Dest | T], Cam).

bfs2(Dest, [LA | Outros], Cam) :-
    LA = [Act | _],
    findall([X | LA],
        (Dest \== Act, ligacel(Act, X), \+ member(X, LA)),
        Novos),
    append(Outros, Novos, Todos),
    bfs2(Dest, Todos, Cam).
