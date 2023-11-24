% server.pl

% Include other files
:- [base_conhecimento].
:- [graph_handling].
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
    prolog_to_json(Maps, JSONObject),
    reply_json(JSONObject, [json_object(dict)]).

:- http_handler('/maps/:buildingCode/:floorNumber', load_map, []).
load_map(Request) :-
    cors_enable(Request, [methods([get])]),
    memberchk(buildingCode=BuildingCode, Request),
    memberchk(floorNumber=FloorNumber, Request),
    map(BuildingCode, FloorNumber, Map),
    prolog_to_json(Map, JSONObject),
    reply_json(JSONObject, [json_object(dict)]).

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
    % Additional initialization steps for the system, if needed.
    true.

% Example usage:
% To start the server on port 5000, call initialize_server(5000).
