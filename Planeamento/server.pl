:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/thread_httpd)).


% Include other files
:- [getPlants].
:- [getElevators].
:- [getPassageways].
:- [graph_handling].
:- [campus_graph].
:- [path_finding].

% Server management
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    asserta(port(Port)).

stop_server :-
    retract(port(Port)),
    http_stop_server(Port, _).

% HTTP request handling
:- http_handler('/maps', get_all_maps, []).
get_all_maps(Request) :-
    cors_enable(Request, [methods([get])]),
    findall(Map, map(Map), Maps),
    reply_json(Maps).

:- http_handler('/maps/path/:origin/:destination', path_between_floors_handler, []).
path_between_floors_handler(Request) :-
    cors_enable(Request, [methods([get])]),
    memberchk(origin=Origin, Request),
    memberchk(destination=Destination, Request),
    path_between_floors(BuildingCode, Origin, Destination, Path),
    prolog_to_json(Path, JSONObject),
    reply_json(JSONObject, [json_object(dict)]).

:- http_handler('/buildings/elevators', get_all_elevators, []).
get_all_elevators(Request) :-
    cors_enable(Request, [methods([get])]),
    findall(Elevator, elevator(Elevator), Elevators),
    reply_json(Elevators).

:- http_handler('/passageways', get_all_passageways, []).
get_all_passageways(Request) :-
    cors_enable(Request, [methods([get])]),
    findall(Passageway, passageway(Passageway), Passageways),
    reply_json(Passageways).

% Initialization
initialize_server(Port) :-
    start_server(Port),
    initialize_system.

initialize_system :-
    consult("getPlants.pl"),
    consult("getPassageways.pl"),
    consult("getElevators.pl"),
    consult("graph_handling.pl"),
    consult("campus_graph.pl"),
    consult("path_finding.pl"),
    fetch_and_export_plant_map_data,
    fetch_and_export_passageway_data,
    fetch_and_export_elevator_data,
    create_graph_for_each_plant,
    connect_floors_with_elevators,
    connect_floors_with_passageways.

% Example usage:
% To start the server on port 5000, call initialize_server(5000).