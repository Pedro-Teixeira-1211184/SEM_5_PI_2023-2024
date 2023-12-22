:- module(campus_graph, [connect_floors_with_elevators/0, connect_floors_with_passageways/0]).

:- dynamic elevador/2.  
:- dynamic corredor/4.  

% Predicate to consult elevator data
consult_elevators :-
    consult('elevators.pl').

% Predicate to consult passageway data
consult_passageways :-
    consult('passageways.pl').


% Predicate to connect floors through elevators
connect_floors_with_elevators :-
    forall(elevador(BuildingCode, Floors),
           connect_floors(BuildingCode, Floors, 2)).

% Predicate to connect floors through passageways
connect_floors_with_passageways :-
    forall(corredor(BuildingCodeFrom, BuildingCodeTo, FloorsFrom, FloorsTo),
           connect_floors(BuildingCodeFrom, FloorsFrom, BuildingCodeTo, FloorsTo, 1)).

% Predicate to connect floors based on elevators or passageways
connect_floors(BuildingCode, Floors, Type) :-
    forall(member(Floor, Floors),
           connect_plants(BuildingCode, Floor, Type)).

connect_floors(BuildingCodeFrom, FloorsFrom, BuildingCodeTo, FloorsTo, Type) :-
    forall((member(FloorFrom, FloorsFrom), member(FloorTo, FloorsTo)),
           connect_plants(BuildingCodeFrom, FloorFrom, BuildingCodeTo, FloorTo, Type)).

% Predicate to connect plants based on elevators or passageways
connect_plants(BuildingCode, Floor, Type) :-
    plants_map(Width, Length, _),  
    connect_cells(BuildingCode, Floor, 1, 1, Width, Length, Type).

connect_cells(_, _, Col, Lin, _, 0, _Type) :- !.
connect_cells(BuildingCode, Floor, Col, Lin, Width, Height, Type) :-m(Col, Lin, Value, _),  
    update_cell(BuildingCode, Floor, Col, Lin, Value, Type),
    Col1 is Col + 1,
    connect_cells(BuildingCode, Floor, Col1, Lin, Width, Height, Type).

update_cell(BuildingCode, Floor, Col, Lin, Value, Type) :-
    assertz(planta(BuildingCode, Floor, Col, Lin, Value, Type)).

:- consult_elevators.
:- consult_passageways.

:- connect_floors_with_elevators.
:- connect_floors_with_passageways.
