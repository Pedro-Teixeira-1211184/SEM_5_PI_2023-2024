:-module(graph_handling, [data/0,building/1,floors/2,passageways/4,elevators/2,connections/2,parse_and_assert_matrix/1,cria_grafo/1]).
:- use_module(utils, [is_member/2]).

:- use_module(library(http/json)).
:- use_module(library(http/http_client)).  
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).

%Knowledge base
:- dynamic building/1. 
:- dynamic floors/2.  
:- dynamic passagewayss/4. 
:- dynamic elevators/2.
:- dynamic connections/2.
:- dynamic ligacel/2.
:- dynamic m/3.

data():-
    building_data(),
    floors_data(),
    passageways_data(),
    elevators_data(),

%buildings
building_data():-
    http_open('http://localhost:5050/buildings', Buildings, []),
    json_read(Buildings, Term, Options),
    update_building(Term).

update_building([]).
update_building([H|T]):-
    assert_building(H),
    update_building(T).    

assert_building(H):-
    format_code(H, Code),
    assert(building(Code)).

format_code(JsonString, Code):-
    JsonString = json(BuildingProperties),
    member(code=Code, BuildingProperties).

%floors
extract_floor_properties(JsonString,FloorCode,BuildingCode) :-
    JsonString = json(FloorProperties),
    member(buildingCode=BuildingCode, FloorProperties),
    member(floorCode=FloorCode, FloorProperties). 

floors_data():-
    http_open('http://localhost:5050/floors', Floors, []),
    json_read(Floors, Term, Options),
    update_floors(Term).    

update_floors([]).
update_floors([H|T]):-
    assert_floors(H),
    update_floors(T).

assert_floors(H):-
    format_floor(H, Code, Number),
    assert(floors(Code, Number)).

%passageways 
extract_passageways_properties(JsonString,PassagewayCode,FloorsCodes) :-
    JsonString = json(PassagewayProperties), 
    member(floorsCodes=FloorsCodes, PassagewayProperties). 

passageways_data():-
    http_open('http://localhost:5050/passageways', Passageways, []),
    json_read(Passageways, Term, Options),
    update_passageways(Term).

update_passageways([]).
update_passageways([H|T]):-
    assert_passageways(H),
    update_passageways(T).

assert_passageways(H):- 
    format_passageways(H, Code, Floor, [FloorCode1, FloorCode2]),
    assert_connections(FloorCode1, FloorCode2, Building1, Building2),
    assert(passageways(FloorCode1, FloorCode2)).

assert_connections([P1, P2], Building1, Building2):-
    (search_connections(P1, Building1) ; true),
    (search_connections(P2, Building2) ; true).
    assert_connection(Building1, Building2).

search_connections(P, Building):-
    floors(Building, Floors),
    is_member(P, Floors).    

assert_connection(Building1, Building2):-
        ((clause(connections(Building1,Building2), true) ; clause(connections(Building1,Building2), true)) -> true ; assert(connections(Building1,Building2))).


%elevators
extract_elevators_properties(JsonString,ElevatorCode,FloorsCodes) :-
    JsonString = json(ElevatorProperties), 
    member(BuildingCodes=BuildingCodes, ElevatorProperties).
    member(FloorNrs=FloorNrs, ElevatorProperties).


elevators_data():-
    http_open('http://localhost:5050/elevators', Elevators, []),
    json_read(Elevators, Term, Options),
    update_elevators(Term).

update_elevators([]).
update_elevators([H|T]):-
    assert_elevators(H),
    update_elevators(T).


assert_elevators(H):-
    format_elevators(H, BuildingCode, FloorNrs),
    assert(elevators(BuildingCode, FloorNrs)).

%Graph - apoio slides moodle ALGAV    
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
    ((m(ColS, LinS, 0), assertz(ligacel(cel(Col, Lin), cel(ColS, LinS))); true)),
    ((m(ColA, LinS, 0), assertz(ligacel(cel(Col, Lin), cel(ColA, LinS))); true)),
    ((m(ColS, LinA, 0), assertz(ligacel(cel(Col, Lin), cel(ColS, LinA))); true)),
    ((m(ColA, LinA, 0), assertz(ligacel(cel(Col, Lin), cel(ColA, LinA))); true)),

    Col1 is Col - 1,
    cria_grafo_lin(Col1, Lin).
    
cria_grafo_lin(Col, Lin):-
    Col1 is Col - 1,
    cria_grafo_lin(Col1, Lin).

%parse_and_assert_matrix - m(x, y, z) - x = coluna, y = linha, z = 0/1

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

%clear
clear():-
    retractall(building(_)),
    retractall(floors(_,_)),
    retractall(passageways(_, _)),
    retractall(elevators(_, _)),
    retractall(connections(_, _)),
    retractall(ligacel(_, _)),
    retractall(m(_, _, _)).    