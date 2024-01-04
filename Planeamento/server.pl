:- module(server, [bestSequenceAllPathsAlgorithm/1,
    searchPathHandler/1,
    searchPathLessElevators/1,
    searchPathPerFloor/1,
    searchPathPerFloorBest/1,
    searchPathPerFloorBFS/1,
    searchPathPerFloorAStar/1]).

:- use_module(graph_handling,[tasks_data/0,data/0,
    clear/0,
    parse_and_assert_matrix/1,
    cria_grafo/1,
    parse_and_assert_matrix_Astar/1,
    cria_grafo_Astar/1]).

:- use_module(utils,[format_insideFloorPath/2,format_caminho2/2]).
:- use_module(search_algorithms,[melhor_sequencia_forca_bruta/1,searchSequencebyGeneticAlgorithm/5,caminho_pisos/4,melhor_caminho_pisos/3,searchByDfsAlgorithm/3,searchByBestDfsAlgorithm/3,searchByBfsAlgorithm/3,searchByAstarAlgorithm/4]).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:- http_handler('/path', searchPathHandler, [method(get)]).
:- http_handler('/pathLessElevators', searchPathLessElevators, [method(get)]). 
:- http_handler('/pathPerFloorsDFS', searchPathPerFloor, [method(post)]).
:- http_handler('/pathPerFloorsBestDFS', searchPathPerFloorBest, [method(post)]).
:- http_handler('/pathPerFloorsBFS', searchPathPerFloorBFS, [method(post)]).
:- http_handler('/pathPerFloorsAStar', searchPathPerFloorAStar, [method(post)]).

:- http_handler('/bestSequenceAllPathsAlgorithm', bestSequenceAllPathsAlgorithm, [methods([get])]).
:- http_handler('/bestSequenceGeneticAlgorithm', bestSequenceGeneticAlgorithm, [methods([get])]).


% Start the HTTP server
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).


% Handle HTTPRequests
searchPathHandler(Request) :-
    
    data(),
    http_parameters(Request, [floorA(FloorA, []), floorB(FloorB, [])]),
    (caminho_pisos(FloorA,FloorB,Edificios,Caminho2) ->
    (format_caminho2(Caminho2,JsonObjects),
    Msg1=json([buildings=Edificios,paths=JsonObjects]),
    reply_json(Msg1)),
    clear();
    format('Content-type: application/json~n~n'),
    format('\nPath not found'),clear()).


%Path between buildings
searchPathLessElevators(Request) :-
    data(),
    http_parameters(Request, [floorA(FloorA, []), floorB(FloorB, [])]),
    (melhor_caminho_pisos(FloorA,FloorB,Caminho) ->
    (format_caminho2(Caminho,JsonObjects),
    Msg1=json([paths=JsonObjects]),
    reply_json(Msg1)),
    clear();
    format('Content-type: application/json~n~n'),
    format('Path not found'),clear()).

%Path inside a floor - DFS
searchPathPerFloor(Request):-
    http_read_data(Request, Dados, []),
    Dados = json(RequestProperties), % Extracting the properties of the JSON object
    member(matriz=Matriz, RequestProperties), % Extracting '
    member(limits=Limits, RequestProperties),
    member(origin=Origin, RequestProperties),
    member(destination=Destination, RequestProperties),
    parse_and_assert_matrix(Matriz),
    cria_grafo(Limits),
    (searchByDfsAlgorithm(Origin,Destination,Caminho) ->
    (format_insideFloorPath(Caminho,JsonObjects),
    Msg1=json([celulas=JsonObjects]),
    reply_json(Msg1)),
    clear();
    format('Content-type: application/json~n~n'),
    format('Path not found'),clear()).

%Path inside a floor - better DFS
searchPathPerFloorBest(Request):-
    http_read_data(Request, Dados, []),
    Dados = json(RequestProperties), % Extracting the properties of the JSON object
    member(matriz=Matriz, RequestProperties), % Extracting '
    member(limits=Limits, RequestProperties),
    member(origin=Origin, RequestProperties),
    member(destination=Destination, RequestProperties),
    parse_and_assert_matrix(Matriz),
    cria_grafo(Limits),
    (searchByBestDfsAlgorithm(Origin,Destination,Caminho) ->
    (format_insideFloorPath(Caminho,JsonObjects),
    Msg1=json([celulas=JsonObjects]),
    reply_json(Msg1)),
    clear();
    format('Content-type: application/json~n~n'),
    format('Path not found'),clear()).

%Path inside a floor - BFS
searchPathPerFloorBFS(Request):-
    http_read_data(Request, Dados, []),
    Dados = json(RequestProperties), % Extracting the properties of the JSON object
    member(matriz=Matriz, RequestProperties), % Extracting '
    member(limits=Limits, RequestProperties),
    member(origin=Origin, RequestProperties),
    member(destination=Destination, RequestProperties),
    parse_and_assert_matrix(Matriz),
    cria_grafo(Limits),
    (searchByBfsAlgorithm(Origin,Destination,Caminho) ->
    (format_insideFloorPath(Caminho,JsonObjects),
    Msg1=json([celulas=JsonObjects]),
    reply_json(Msg1)),
    clear();
    format('Content-type: application/json~n~n'),
    format('Path not found'),clear()).

%Path inside a floor - A*
searchPathPerFloorAStar(Request):-
    http_read_data(Request, Dados, []),
    Dados = json(RequestProperties), % Extracting the properties of the JSON object
    member(matriz=Matriz, RequestProperties), % Extracting '
    member(limits=Limits, RequestProperties),
    member(origin=Origin, RequestProperties),
    member(destination=Destination, RequestProperties),
    parse_and_assert_matrix_Astar(Matriz),
    cria_grafo_Astar(Limits),
    (searchByAstarAlgorithm(Origin,Destination,Caminho,Custo) ->
    (format_insideFloorPath(Caminho,JsonObjects),
    Msg1=json([celulas=JsonObjects]),
    reply_json(Msg1)),
    clear();
    format('Content-type: application/json~n~n'),
    format('Path not found'),clear()).

%Tasks
bestSequenceAllPathsAlgorithm(Request) :-
    tasks_data(),
    melhor_sequencia_forca_bruta(Result),
    Msg1 = json{tarefas: Result},
    reply_json(Msg1).
    

bestSequenceGeneticAlgorithm(Request) :-
    tasks_data(),
    searchSequencebyGeneticAlgorithm(10,3,80,10,[Sequence*Time|Rest]),
    Msg1 = json{tarefas: Sequence},
    reply_json(Msg1).
    

