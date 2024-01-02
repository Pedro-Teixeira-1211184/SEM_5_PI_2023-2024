:- module(graph_handling, [
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

:- use_module(utils, [is_member/2]).
:- use_module(library(http/json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).

% Knowledge base
:- dynamic building/1.
:- dynamic floors/2.
:- dynamic passageways/4.
:- dynamic elevators/2.
:- dynamic map/3.
:- dynamic connections/2.
:- dynamic ligacel/2.
:- dynamic x/5.

% Data retrieval
data() :-
    building_data(),
    floors_data(),
    passageways_data(),
    elevators_data(),
    maps_data(),
    assert_x_facts(). % Assert x/5 facts after data retrieval.

assert_x_facts() :-
    forall(x(BuildingCode, FloorNumber, X, Y, Value), assert(x(BuildingCode, FloorNumber, X, Y, Value))).

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
    assert(building(BuildingCode)),
    format('Asserted building: ~w~n', [BuildingCode]).

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
    ->  format('Asserting: ~w~nBuilding Code: ~w~n', [Floor, BuildingCode]),
        (   clause(floors(BuildingCode, OldList), true)
        ->  retract(floors(BuildingCode, OldList)),
            append(OldList, [FloorCode], NewList),
            assert(floors(BuildingCode, NewList))
        ;   assert(floors(BuildingCode, [FloorCode]))
        )
    ;   format('Failed to extract floor properties: ~w~n', [Floor])
    ).

extract_floor_properties(JsonString, FloorCode, BuildingCode) :-
    JsonString = json([id=_Id, buildingCode=BuildingCode, number=_Number, code=FloorCode, description=_Description]).


% Passageways
extract_passages_properties(JsonString, PassageCode, FloorsCodes) :-
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
    extract_passages_properties(H, PassageCode, [FloorCode1, FloorCode2]),
    assert_connections([FloorCode1, FloorCode2], Building1, Building2),
    assert(passageways(Building1, Building2, FloorCode1, FloorCode2)),
    format('Asserted passageway: ~w between floors ~w and ~w in buildings ~w and ~w~n', [PassageCode, FloorCode1, FloorCode2, Building1, Building2]).

assert_connections([P1, P2], Building1, Building2) :-
    (find_connections(P1, Building1) ; true),
    (find_connections(P2, Building2) ; true),
    assert_connection(Building1, Building2),
    format('Asserted connection between buildings: ~w and ~w~n', [Building1, Building2]).

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
    assert(elevators(ElevatorCode, FloorsCodes)),
    format('Asserted elevator: ~w with floors ~w~n', [ElevatorCode, FloorsCodes]).


% Map
maps_data() :-
    http_open('http://localhost:5050/maps', Stream, []),
    catch(
        json_read(Stream, Maps, [null(null)]),
        Error,
        handle_json_error(Error)
    ),
    writeln('Received JSON data:'),
    writeln(Maps),  % Add this line for debugging
    assert_maps_data(Maps).

assert_maps_data(Maps) :-
    writeln('Asserting maps data:'), % Debug
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
    parse_and_assert_matrix(MapData, FloorNumber).

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
    ]),
    % Add more debugging information
    writeln('Extracted map properties:'), % Debug
    writeln(BuildingCode), % Debug
    writeln(FloorNumber), % Debug
    writeln(MapData). % Debug



% Graph creation
create_graph() :-
    forall(floors(BuildingCode, _), cria_grafo(BuildingCode)).

cria_grafo(BuildingCode) :-
    floors(BuildingCode, FloorsList),
    member(Floor, FloorsList),
    writeln('Creating graph for building:'), %% Debug
    writeln(BuildingCode), %% Debug
    writeln('Creating graph for floor:'), %% Debug
    writeln(Floor), %% Debug
    cria_grafo_floor(BuildingCode, Floor).

cria_grafo_floor(BuildingCode, Floor) :-
    writeln('Creating graph for floor:'), %% Debug
    writeln(Floor), %% Debug
    forall(x(BuildingCode, Floor, X, Y, _), assert(ligacel(cel(BuildingCode, Floor, X, Y), cel(BuildingCode, Floor, X, Y)))),
    writeln('Asserted ligacel for floor:'), %% Debug
    writeln(Floor). %% Debug

create_connections(BuildingCode, Floor, X, Y) :-
    X1 is X + 1,
    X_ is X - 1,
    Y1 is Y + 1,
    Y_ is Y - 1,
    connect_cells(BuildingCode, Floor, X, Y, X1, Y),
    connect_cells(BuildingCode, Floor, X, Y, X_, Y),
    connect_cells(BuildingCode, Floor, X, Y, X, Y1),
    connect_cells(BuildingCode, Floor, X, Y, X, Y_),
    % Diagonal connections
    connect_cells(BuildingCode, Floor, X, Y, X1, Y1),
    connect_cells(BuildingCode, Floor, X, Y, X_, Y1),
    connect_cells(BuildingCode, Floor, X, Y, X1, Y_),
    connect_cells(BuildingCode, Floor, X, Y, X_, Y_).

connect_cells(BuildingCode, Floor, X, Y, X1, Y1) :-
    (x(X1, Y1, _), assertz(ligacel(cel(BuildingCode, Floor, X, Y), cel(BuildingCode, Floor, X1, Y1)))),
    true.

cria_grafo([X,Y]) :-
    cria_grafo(X,Y).
cria_grafo(_, -1) :- !.
cria_grafo(Col, Lin) :-
    cria_grafo_lin(Col, Lin),
    Lin1 is Lin - 1,
    cria_grafo(Col, Lin1).

cria_grafo_lin(-1, _) :- !.
cria_grafo_lin(Col, Lin) :-
    x(Col, Lin, 0),
    assertz(ligacel(cel(Col, Lin), cel(Col, Lin))), % Added this line
    writeln('Asserted connection for cell: '),
    writeln(cel(Col, Lin)),
    ColS is Col + 1,
    ColA is Col - 1,
    LinS is Lin + 1,
    LinA is Lin - 1,

    ((x(ColS, Lin, 0), assertz(ligacel(cel(Col, Lin), cel(ColS, Lin)))) ; true),
    ((x(ColA, Lin, 0), assertz(ligacel(cel(Col, Lin), cel(ColA, Lin)))) ; true),
    ((x(Col, LinS, 0), assertz(ligacel(cel(Col, Lin), cel(Col, LinS)))) ; true),
    ((x(Col, LinA, 0), assertz(ligacel(cel(Col, Lin), cel(Col, LinA)))) ; true),
    % Diagonal connections
    ((x(ColS, LinS, 0), assertz(ligacel(cel(Col, Lin), cel(ColS, LinS)))) ; true),
    ((x(ColA, LinS, 0), assertz(ligacel(cel(Col, Lin), cel(ColA, LinS)))) ; true),
    ((x(ColS, LinA, 0), assertz(ligacel(cel(Col, Lin), cel(ColS, LinA)))) ; true),
    ((x(ColA, LinA, 0), assertz(ligacel(cel(Col, Lin), cel(ColA, LinA)))) ; true),

    Col1 is Col - 1,
    cria_grafo_lin(Col1, Lin).

cria_grafo_lin(Col, Lin) :-
    Col1 is Col - 1,
    cria_grafo_lin(Col1, Lin).


% Matrix parsing and assertion
create_matrices() :-
    findall([BuildingCode, FloorNumber, MapData], map(BuildingCode, FloorNumber, MapData), Maps),
    writeln('Found maps:'), %% Debug
    writeln(Maps), %% Debug
    forall(member([BuildingCode, FloorNumber, MapData], Maps),
           (format('Creating matrix for Building Code: ~w, Floor Number: ~w~n', [BuildingCode, FloorNumber]), %% Debug
            parse_and_assert_matrix(MapData, FloorNumber))).

parse_and_assert_matrix(Matrix, FloorNumber) :-
    writeln('Parsing and asserting matrix:'), %% Debug
    writeln(Matrix), %% Debug
    parse_and_assert_matrix_rows(Matrix, 0, FloorNumber).

parse_and_assert_matrix_rows([], _, _).
parse_and_assert_matrix_rows([Row|Rest], X, Y) :-
    parse_and_assert_matrix_row(Row, X, Y),
    X1 is X + 1,
    parse_and_assert_matrix_rows(Rest, X1, Y).

parse_and_assert_matrix_row([], _, _).
parse_and_assert_matrix_row([Cell|Rest], X, Y) :-
    assert(x(BuildingCode, FloorNumber, X, Y, Cell)), % Change this line
    writeln('Asserted x for cell:'), %% Debug
    writeln(cel(BuildingCode, FloorNumber, X, Y, Cell)), %% Debug
    X1 is X + 1,
    parse_and_assert_matrix_row(Rest, X1, Y).

% Clear the knowledge base
clear() :-
    retractall(building(_)),
    retractall(floors(_, _)),
    retractall(passageways(_, _)),
    retractall(elevators(_, _)),
    retractall(connections(_, _)),
    retractall(ligacel(_, _)),
    retractall(x(_, _, _)).