:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:- use_module(utils, [format_floorPath/2, format_path2/2]).
:- use_module(graph_handling, [
    data/0,
    building_data/0,
    floors_data/0,
    passageways_data/0,
    elevators_data/0,
    cria_grafo/1,
    clear/0,
    create_matrices/0,
    create_graph/0
]).
:- use_module(search_algorithms, [
    path_between_floors/4,
    best_path/3,
    searchByDfsAlgorithm/3,
    searchByBestDfsAlgorithm/3,
    searchByBestBfsAlgorithm/3
]).

% Corrected HTTP handlers
:- http_handler('/path', searchPathHandler, [method(get)]).
:- http_handler('/pathLessElevators', searchPathLessElevators, [method(get)]).
:- http_handler('/pathPerFloorsDFS', searchPathPerFloor, [method(post)]).
:- http_handler('/pathPerFloorsBestDFS', searchPathPerFloorBest, [method(post)]).
:- http_handler('/pathPerFloorsBFS', searchPathPerFloorBFS, [method(post)]).
:- http_handler('/pathPerFloorsAStar', searchPathPerFloorAStar, [method(post)]).

% Server management
start_server(Port) :-
    data(), % Load the data
    create_matrices(),
    create_graph(),
    http_server(http_dispatch, [port(Port)]).

% Request handling
searchPathHandler(Request) :- 
    http_parameters(Request, [floorA(FloorA, [default('')]), floorB(FloorB, [default('')])]),
    path_between_floors(FloorA, FloorB, Buildings, Path), % Change this line
    format_path2(Path, JsonObjects),
    Msg1 = json{buildings:Buildings, paths:JsonObjects},
    reply_json(Msg1),
    clear().


searchPath(FloorA, FloorB) :-
    format('Content-type: application/json~n~n'),
    format('Received request: FloorA = ~w, FloorB = ~w~n', [FloorA, FloorB]), % Debugging line
     format('Calling path_between_floors/4 predicate...~n'), % Debugging line
    (FloorA \= '', FloorB \= '' ->
        format('Processing request with non-empty parameters~n'), % Debugging line

        (path_between_floors(FloorA, FloorB, Buildings, Path) ->
            format_path2(Path, JsonObjects),
            Msg1 = json{buildings:Buildings, paths:JsonObjects},
            reply_json(Msg1),
            clear()
        ;
            format('No path found between the specified floors~n'),
            reply_json(json{message:"No path found between the specified floors"})
        )
    ;
        format('Invalid or missing parameters~n'),
        reply_json(json{message:"Invalid or missing parameters"})
    ),
    format('End of searchPath/2~n'). % Debugging line

searchPathLessElevators(Request) :-
    data(),
    http_parameters(Request, [floorA([FloorA|_]), floorB([FloorB|_])]),
    (path_between_floors_less_elevators(FloorA, FloorB, Buildings, Path) ->
        format_path2(Path, JsonObjects),
        Msg1 = json{buildings:Buildings, paths:JsonObjects},
        reply_json(Msg1),
        clear()
    ;   format('Content-type: application/json~n~n'),
        format('\nPath not found'), clear()
    ).


searchPathPerFloor(Request) :-
    http_read_json_dict(Request, Data),
    Data = json{matriz:Matriz, limits:Limits, origin:Origin, destination:Destination},
    parse_and_assert_matrix(Matriz),
    cria_grafo(Limits),
    (searchByDfsAlgorithm(Origin, Destination, Path) ->
        format_floorPath(Path, JsonObjects),
        Msg1 = json{cells:JsonObjects},
        reply_json(Msg1),
        clear()
    ;   format('Content-type: application/json~n~n'),
        format('Path not found'), clear()
    ).

searchPathPerFloorBest(Request) :-
    http_read_json_dict(Request, Data),
    Data = json{matriz:Matriz, limits:Limits, origin:Origin, destination:Destination},
    parse_and_assert_matrix(Matriz),
    cria_grafo(Limits),
    (searchByBestDfsAlgorithm(Origin, Destination, Path) ->
        format_floorPath(Path, JsonObjects),
        Msg1 = json{cells:JsonObjects},
        reply_json(Msg1),
        clear()
    ;   format('Content-type: application/json~n~n'),
        format('Path not found'), clear()
    ).

searchPathPerFloorBFS(Request) :-
    http_read_json_dict(Request, Data),
    Data = json{matriz:Matriz, limits:Limits, origin:Origin, destination:Destination},
    parse_and_assert_matrix(Matriz),
    cria_grafo(Limits),
    (searchByBestBfsAlgorithm(Origin, Destination, Path) ->
        format_floorPath(Path, JsonObjects),
        Msg1 = json{cells:JsonObjects},
        reply_json(Msg1),
        clear()
    ;   format('Content-type: application/json~n~n'),
        format('Path not found'), clear()
    ).
