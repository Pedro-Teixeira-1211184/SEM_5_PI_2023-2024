:-module(graph_handling,[tasks_data/0,data/0,building/1,floors/2,passageways/4,elevators/2,connections/2,parse_and_assert_matrix/1,cria_grafo/1,parse_and_assert_matrix_Astar/1,cria_grafo_Astar/1]).
:- use_module(utils,[is_member/2]).


% Include necessary library
:- use_module(library(http/json)).
:- use_module(library(http/http_client)).  % Include the HTTP client library
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).



% Knowledge base to store building data

:- dynamic building/1. 
:- dynamic floors/2.  
:- dynamic passageways/4. 
:- dynamic elevators/2.
:- dynamic connections/2.
:- dynamic ligacel/2.
%:- dynamic map/3.
:- dynamic m/3.
:- dynamic node/3.
:- dynamic edge/3.
:- dynamic timeBetweenTasks/3.
:- dynamic task/1.



data():-
    building_data(),
    floors_data(),
    passageways_data(),
    elevators_data().
    %maps_data().


% Buildings
building_data() :-
    catch(
        http_open('http://localhost:5050/buildings', Stream, []),
        Error,
        handle_http_error(Error)
    ),
    % writeln('Building data retrieved.'),
    json_read(Stream, Term, Options),
    update_knowledge_base_building(Term).

update_knowledge_base_building([]).
update_knowledge_base_building([Building | Rest]) :-
    assert_building(Building),
    update_knowledge_base_building(Rest).

assert_building(Building) :-
    extract_building_code(Building, BuildingCode),
    assert(building(BuildingCode)).
    %format('Asserted building: ~w~n', [BuildingCode]).

extract_building_code(JsonString, BuildingCode) :-
    JsonString = json(BuildingProperties),
    member(code = BuildingCode, BuildingProperties).


% Floors
floors_data():-
    http_open('http://localhost:5050/buildings/floors', Stream, []),
    catch(
        json_read(Stream, Floors, [null(null)]), % Fix: Add option to handle null values
        Error,
        handle_json_error(Error)
    ),
    % writeln('Floor data retrieved.'),
    update_knowledge_base_floor(Floors).

handle_json_error(Error) :-
    format('Error reading JSON: ~w~n', [Error]),
    fail.

update_knowledge_base_floor([]).
update_knowledge_base_floor(Floors) :-
    maplist(assert_floor, Floors).

assert_floor(Floor) :-
    (   extract_floor_properties(Floor, FloorCode, BuildingCode)
    ->  %format('Asserting: ~w~nBuilding Code: ~w~n', [Floor, BuildingCode]),
        (   clause(floors(BuildingCode, OldList), true)
        ->  retract(floors(BuildingCode, OldList)),
            append(OldList, [FloorCode], NewList),
            assert(floors(BuildingCode, NewList))
        ;   assert(floors(BuildingCode, [FloorCode]))
        )
       %format('Failed to extract floor properties: ~w~n', [Floor])
    ).

extract_floor_properties(JsonString, FloorCode, BuildingCode) :-
    JsonString = json([id=_Id, buildingCode=BuildingCode, number=_Number, code=FloorCode, description=_Description]).


% Passageways
extract_passageways_properties(JsonString, PassageCode, FloorsCodes) :-
    JsonString = json([
        id=PassageCode,
        floorCode1=FloorCode1,
        floorCode2=FloorCode2
    ]),
    FloorsCodes = [FloorCode1, FloorCode2].

passageways_data() :-
    http_open('http://localhost:5050/passageways', Passageways, []),
    % writeln('Passageway data retrieved.'),
    json_read(Passageways, Term, Options),
    update_passageways(Term).

update_passageways([]).
update_passageways([H|T]) :-
    assert_passageways(H),
    update_passageways(T).

assert_passageways(H) :-
    extract_passageways_properties(H, PassageCode, [FloorCode1, FloorCode2]),
    assert_connections([FloorCode1, FloorCode2], Building1, Building2),
    assert(passageways(Building1, Building2, FloorCode1, FloorCode2)).
    %format('Asserted passageway: ~w between floors ~w and ~w in buildings ~w and ~w~n', [PassageCode, FloorCode1, FloorCode2, Building1, Building2]).

assert_connections([P1, P2], Building1, Building2) :-
    (find_connections(P1, Building1) ; true),
    (find_connections(P2, Building2) ; true),
    assert_connection(Building1, Building2).
    %format('Asserted connection between buildings: ~w and ~w~n', [Building1, Building2]).

find_connections(P, Building) :-
    floors(Building, FloorsList),
    is_member(P, FloorsList).

assert_connection(Building1, Building2) :-
    ((clause(connections(Building1, Building2), true) ; clause(connections(Building2, Building1), true)) -> true ; assert(connections(Building1, Building2))).

% Elevators
% Extract elevator properties
extract_elevator_properties(JsonString, ElevatorCode, FloorsCodes) :-
    JsonString = json([
        id=_, % Assuming you don't need the ID
        buildingCode=ElevatorCode,
        floorNumbers=FloorsList
    ]),
    split_string(FloorsList, ",", "", FloorsCodes).

elevators_data() :-
    http_open('http://localhost:5050/buildings/elevators', Stream, []),
    json_read(Stream, Term, Options),
    update_knowledge_base_elevators(Term).
    % writeln('Elevator data retrieved.'),

update_knowledge_base_elevators([]).
update_knowledge_base_elevators([Elevator|Rest]) :-
    assert_elevators(Elevator),
    update_knowledge_base_elevators(Rest).

assert_elevators(Elevator) :-
    extract_elevator_properties(Elevator, ElevatorCode, FloorsCodes),
    assert(elevators(ElevatorCode, FloorsCodes)).
    %format('Asserted elevator: ~w with floors ~w~n', [ElevatorCode, FloorsCodes]).


% Map
maps_data() :-
    http_open('http://localhost:5050/maps', Stream, []),
    catch(
        json_read(Stream, Maps, [null(null)]),
        Error,
        handle_json_error(Error)
    ),
    %writeln('Received JSON data:'),
    %writeln(Maps),  % Add this line for debugging
    assert_maps_data(Maps).

assert_maps_data(Maps) :-
    %writeln('Asserting maps data:'), % Debug
    assert_maps_data(Maps, 0).

assert_maps_data([], _).
assert_maps_data([Map | Rest], Index) :-
    assert_map_data(Map, Index),
    Index1 is Index + 1,
    assert_maps_data(Rest, Index1).

assert_map_data(Map, Index) :-
    extract_map_properties(Map, BuildingCode, FloorNumber, MapData),
    format('Asserting map for Building Code: ~w, Floor Number: ~w~n', [BuildingCode, FloorNumber]),
    assert(map(BuildingCode, FloorNumber, MapData, Index)),
    parse_and_assert_matrix(MapData).

extract_map_properties(Map, BuildingCode, FloorNumber, MapData) :-
    Map = json([
        id=_,
        buildingCode=BuildingCode,
        floorNumber=FloorNumber,
        size=json([height=_Height, width=_Width]),
        map=MapData,
        rooms=_Rooms,  % Ignore rooms for now
        passageways=_Passageways,  % Ignore passageways for now
        elevator=_Elevator  % Ignore elevator for now
    ]).
    % Add more debugging information
    %writeln('Extracted map properties:'), % Debug
    %writeln(BuildingCode), % Debug
    %writeln(FloorNumber), % Debug
    %writeln(MapData). % Debug



%Graph
cria_grafo([X,Y]):- cria_grafo(X,Y).
cria_grafo(_, -1):- !.
cria_grafo(Col, Lin):-
    cria_grafo_lin(Col, Lin),
    Lin1 is Lin - 1,
    cria_grafo(Col, Lin1).

cria_grafo_lin(-1, _):- !.
cria_grafo_lin(Col, Lin):-
    m(Col, Lin, 0),
    !,
    ColS is Col + 1, ColA is Col - 1, LinS is Lin + 1, LinA is Lin - 1,

    ((m(ColS, Lin, 0), assertz(ligacel(cel(Col, Lin), cel(ColS, Lin))); true)),
    ((m(ColA, Lin, 0), assertz(ligacel(cel(Col, Lin), cel(ColA, Lin))); true)),
    ((m(Col, LinS, 0), assertz(ligacel(cel(Col, Lin), cel(Col, LinS))); true)),
    ((m(Col, LinA, 0), assertz(ligacel(cel(Col, Lin), cel(Col, LinA))); true)),
    % Diagonal connections
   % ((m(ColS, LinS, 0), assertz(ligacel(cel(Col, Lin), cel(ColS, LinS))); true)),
   % ((m(ColA, LinS, 0), assertz(ligacel(cel(Col, Lin), cel(ColA, LinS))); true)),
   % ((m(ColS, LinA, 0), assertz(ligacel(cel(Col, Lin), cel(ColS, LinA))); true)),
   % ((m(ColA, LinA, 0), assertz(ligacel(cel(Col, Lin), cel(ColA, LinA))); true)),

    Col1 is Col - 1,
    cria_grafo_lin(Col1, Lin).
    
cria_grafo_lin(Col, Lin):-
    Col1 is Col - 1,
    cria_grafo_lin(Col1, Lin).

%Matrix
parse_and_assert_matrix(Matrix) :-
    parse_and_assert_matrix_rows(Matrix, 0).

    parse_and_assert_matrix_rows([], _).
parse_and_assert_matrix_rows([Row|Rest], Y) :-
    parse_and_assert_matrix_row(Row, 0, Y),
    Y1 is Y + 1,
    parse_and_assert_matrix_rows(Rest, Y1).

parse_and_assert_matrix_row([], _, _).
parse_and_assert_matrix_row([Cell|Rest], X, Y) :-
    assert(m(X, Y, Cell)), 
    X1 is X + 1,
    parse_and_assert_matrix_row(Rest, X1, Y).


%AStar Matrix
parse_and_assert_matrix_Astar(Matrix) :-
    parse_and_assert_matrix_rows_Astar(Matrix, 0).

    parse_and_assert_matrix_rows_Astar([], _).
parse_and_assert_matrix_rows_Astar([Row|Rest], Y) :-
    parse_and_assert_matrix_row_Astar(Row, 0, Y),
    Y1 is Y + 1,
    parse_and_assert_matrix_rows_Astar(Rest, Y1).

parse_and_assert_matrix_row_Astar([], _, _).
parse_and_assert_matrix_row_Astar([0|Rest], X, Y) :-
    assert(node(cel(X,Y), X, Y)), 
    X1 is X + 1,
    parse_and_assert_matrix_row_Astar(Rest, X1, Y).

    parse_and_assert_matrix_row_Astar([], _, _).
parse_and_assert_matrix_row_Astar([1|Rest], X, Y) :- 
    X1 is X + 1,
    parse_and_assert_matrix_row_Astar(Rest, X1, Y).

%AStar Graph
    cria_grafo_Astar([X,Y]):- cria_grafo_Astar(X,Y).
cria_grafo_Astar(_, -1):- !.
cria_grafo_Astar(Col, Lin):-
    cria_grafo_lin_Astar(Col, Lin),
    Lin1 is Lin - 1,
    cria_grafo_Astar(Col, Lin1).

cria_grafo_lin_Astar(-1, _):- !.
cria_grafo_lin_Astar(Col, Lin):-
    node(cel(Col,Lin),Col,Lin),
    !,
    ColS is Col + 1, ColA is Col - 1, LinS is Lin + 1, LinA is Lin - 1,

    ((node(cel(ColS,Lin),ColS,Lin), assertz(edge(cel(Col, Lin), cel(ColS, Lin),1)); true)),
    ((node(cel(ColA,Lin),ColA,Lin), assertz(edge(cel(Col, Lin), cel(ColA, Lin),1)); true)),
    ((node(cel(Col,LinS),Col,LinS), assertz(edge(cel(Col, Lin), cel(Col, LinS),1)); true)),
    ((node(cel(Col,LinA),Col,LinA), assertz(edge(cel(Col, Lin), cel(Col, LinA),1)); true)),
    % Diagonal connectionsl
  %  ((node(cel(ColS,LinS),ColS,LinS), assertz(edge(cel(Col, Lin), cel(ColS, LinS),1.4)); true)),
  %  ((node(cel(ColA,LinS),ColA,LinS), assertz(edge(cel(Col, Lin), cel(ColA, LinS),1.4)); true)),
  %  ((node(cel(ColS,LinA),ColS,LinA), assertz(edge(cel(Col, Lin), cel(ColS, LinA),1.4)); true)),
  %  ((node(cel(ColA,LinA),ColA,LinA), assertz(edge(cel(Col, Lin), cel(ColA, LinA),1.4)); true)),

    Col1 is Col - 1,
    cria_grafo_lin_Astar(Col1, Lin).
    
cria_grafo_lin_Astar(Col, Lin):-
    Col1 is Col - 1,
    cria_grafo_lin_Astar(Col1, Lin).



%----------------------------------------------------------------------------


tasks_data():-
    %write("adding building data"),
    http_open('http://localhost:5050/tasks', Stream, []),
    json_read(Stream, Term,Options),
    update_knowledge_base_tasks(Term).

update_knowledge_base_tasks([]).
update_knowledge_base_tasks([Tasks|Rest]) :-
    assert_tasks(Tasks),
    update_knowledge_base_tasks(Rest).

task_already_exists(Task) :-
    task(Task).


    assert_tasks(Tasks):-
    extract_task_code(Tasks,InicialTask,NextTask,Time),
    assert(timeBetweenTasks(InicialTask,NextTask,Time)),
    (task_already_exists(InicialTask);assert(task(InicialTask))),
    (task_already_exists(NextTask);assert(task(NextTask))).


extract_task_code(JsonString,InicialTask,NextTask,Time) :-
    JsonString = json(TasksProperties), 
    member(inicialTask=InicialTask, TasksProperties),
    member(nextTask=NextTask, TasksProperties),
    member(time=Time, TasksProperties).

assert_taskCodes([]).
assert_taskCodes(TaskCode|Rest):-
    assert(task(TaskCode)),
    assert_taskCodes(Rest).

assert_timeBetweenTasks([]).
assert_timeBetweenTasks([task1,task2,time]|Rest):-
    assert(timeBetweenTasks(task1,task2,time)),
    assert_timeBetweenTasks(Rest).


%-------------------------------------------------------------------------------

clear:-
    retractall(building(_)),
    retractall(floors(_, _)),
    retractall(passageways(_, _, _, _)),
    retractall(elevators(_, _)),
    retractall(connections(_, _)),
    retractall(m(_, _,_)),
    retractall(ligacel(_, _)),
    retractall(node(_,_, _)),
    retractall(edge(_,_,_)).

