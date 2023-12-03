:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/thread_httpd)).
:- use_module(getPlants,[fetch_and_export_plant_map_data/0]).
:- use_module(getElevators,[fetch_and_export_elevator_data/0]).
:- use_module(getPassageways,[fetch_and_export_passageway_data/0]).
:- use_module(graph_handling,[create_graph_for_each_plant/0]).
:- use_module(campus_graph,[connect_floors_with_elevators/0, connect_floors_with_passageways/0]).
:- use_module(path_finding,[find_path_handler/1]).

% Server management
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]),

stop_server :-
    retract(port(Port)),
    http_stop_server(Port, _).

% HTTP request handling

:- http_handler('/maps/path/:origin/:destination', path_between_floors_handler, []).
path_between_floors_handler(Request) :-
    cors_enable(Request, [methods([get])]),
    memberchk(origin=Origin, Request),
    memberchk(destination=Destination, Request),
    path_between_floors(BuildingCode, Origin, Destination, Path),
    prolog_to_json(Path, JSONObject),
    reply_json(JSONObject, [json_object(dict)]).

% Initialization
initialize_server(Port) :-
    start_server(Port),
    initialize_system.

initialize_system :-
    fetch_and_export_plant_map_data([]),
    consult('plants_map.pl').
    fetch_and_export_elevator_data([]),
    consult('elevators.pl').
    fetch_and_export_passageway_data([]),
    consult('passageways.pl').
    create_graph_for_each_plant/0.
    connect_floors_with_elevators/0.
    connect_floors_with_passageways/0.


