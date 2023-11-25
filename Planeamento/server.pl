% server.pl

% Include other files
:- [getElevators].
:- [getPassageways].
:- [getPlants].
:- [graph_handling].
:- [path_finding].
:- [campus_graph].

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
    prolog_to_json(Maps, JSONObject),
    reply_json(JSONObject, [json_object(dict)]).

:- http_handler('/maps/path/:origin/:destination', path_between_floors_handler, []).
path_between_floors_handler(Request) :-
    cors_enable(Request, [methods([get])]),
    memberchk(origin=Origin, Request),
    memberchk(destination=Destination, Request),
    path_between_floors(BuildingCode, Origin, Destination, Path),
    prolog_to_json(Path, JSONObject),
    reply_json(JSONObject, [json_object(dict)]).

:-http_handler('/buildings/elevators', get_all_elevators, []).
get_all_elevators(Request) :-
    cors_enable(Request, [methods([get])]),
    findall(Elevator, elevator(Elevator), Elevators),
    prolog_to_json(Elevators, JSONObject),
    reply_json(JSONObject, [json_object(dict)]).

:- http_handler('/passageways', get_all_passageways, []).
get_all_passageways(Request) :-
    cors_enable(Request, [methods([get])]),
    findall(Passageway, passageway(Passageway), Passageways),
    prolog_to_json(Passageways, JSONObject),
    reply_json(JSONObject, [json_object(dict)]).


% Initialization
initialize_server(Port) :-
    start_server(Port),
    initialize_system.

initialize_system :-
    % Additional initialization steps for the system, if needed.
    true.

% Example usage:
% To start the server on port 5000, call initialize_server(5000).
    