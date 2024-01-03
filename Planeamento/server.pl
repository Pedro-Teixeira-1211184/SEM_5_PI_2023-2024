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
    create_graph/0,
    parse_and_assert_matrix/2

]).
:- use_module(search_algorithms, [
    path_between_floors/4,
    best_path/3,
    searchByDfsAlgorithm/3,
    searchByBestDfsAlgorithm/3,
    searchByBestBfsAlgorithm/3,
    melhor_brute_force/1,
    searchSequencebyGeneticAlgorithm/5
]).

% Corrected HTTP handlers
:- http_handler('/path', searchPathHandler, [method(get)]).
:- http_handler('/pathLessElevators', searchPathLessElevators, [method(get)]).
:- http_handler('/pathPerFloorsDFS', searchPathPerFloor, [method(post)]).
:- http_handler('/pathPerFloorsBestDFS', searchPathPerFloorBest, [method(post)]).
:- http_handler('/pathPerFloorsBFS', searchPathPerFloorBFS, [method(post)]).
:- http_handler('/pathPerFloorsAStar', searchPathPerFloorAStar, [method(post)]).
:- http_handler('/bestSequenceAllPathsAlgorithm', bestSequenceAllPathsAlgorithm, [methods([get])]).
:- http_handler('/bestSequenceGeneticAlgorithm', bestSequenceGeneticAlgorithm, [methods([get])]).

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


% Updated json_to_prolog to handle nested lists
json_to_prolog_list([], []).
json_to_prolog_list([H|T], [H1|T1]) :-
    json_to_prolog(H, H1),
    json_to_prolog_list(T, T1).

json_to_prolog_list(Json, Prolog) :-
    is_list(Json),
    maplist(json_to_prolog, Json, Prolog).

% Updated json_to_prolog to handle nested lists
json_to_prolog(Json, Prolog) :-
    is_list(Json),
    json_to_prolog_list(Json, Prolog).

json_to_prolog(Json, Prolog) :-
    term_string(Prolog, Json).  % Convert JSON term to Prolog term

json_to_prolog(Json, Prolog) :-
    json_read(string(Json), Prolog).  % Parse JSON arrays using json_read/2


% Modified HTTP handler to use the updated json_to_prolog/2 predicate
searchPathPerFloor(Request) :-
    http_read_json_dict(Request, Data),
    (   is_dict(Data),
        get_dict(matriz, Data, Matrix),
        get_dict(floorNumber, Data, FloorNumber),
        get_dict(limits, Data, Limits),
        get_dict(origin, Data, Origin),
        get_dict(destination, Data, Destination),
        
        % Validate the parameters
        is_valid_data(Matrix, FloorNumber, Limits, Origin, Destination),
        
        % Extracting matrix values and converting to Prolog terms
        parse_and_assert_matrix(Matrix, FloorNumber),
        cria_grafo(Limits),
        
        (   searchByDfsAlgorithm(Origin, Destination, Path) ->
                format_floorPath(Path, JsonObjects),
                Msg1 = json{cells:JsonObjects},
                reply_json(Msg1),
                clear()
            ;   reply_json(json{code: 404, message: "Path not found"})
        )
    ;   reply_json(json{code: 400, message: "Invalid or missing parameters"})
    ).

% Additional predicate to check the validity of parameters
is_valid_data(Matrix, FloorNumber, Limits, Origin, Destination) :-
    is_list(Matrix),
    is_integer(FloorNumber),
    is_list(Limits),
    is_list(Origin),
    is_list(Destination).

% Predicate to check if a term is an integer
is_integer(X) :-
    integer(X).


% Additional predicate to check if Matrix is a list of lists
is_matrix(Matrix) :-
    is_list(Matrix),
    maplist(is_list, Matrix).


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


bestSequenceAllPathsAlgorithm(Request) :-
    tasks_data(),
    melhor_brute_force(Result),
    Msg1 = json{tarefas: Result},
    reply_json(Msg1).

bestSequenceGeneticAlgorithm(Request) :-
    tasks_data(),
    searchSequencebyGeneticAlgorithm(10,3,80,10,[Sequence*Time|Rest]),
    Msg1 = json{tarefas: Sequence},
    reply_json(Msg1).
    
  