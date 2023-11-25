% Load plant map data from the file
:- consult('plants_map.pl').

% Predicate to create a graph for each plant map
create_graph_for_each_plant :-
    findall(FloorCode, m(_, _, _, _, FloorCode), FloorCodes),
    create_graphs(FloorCodes).

% Predicate to create graphs for each plant map
create_graphs([]) :- !.
create_graphs([FloorCode | Rest]) :-
    plant_dimensions(FloorCode, Col, Lin),
    write('Creating graph for plant: '), write(FloorCode), write(', Col: '), write(Col), write(', Lin: '), write(Lin), nl,
    cria_grafo(Col, Lin, FloorCode),
    create_graphs(Rest).

% Adapted algorithm to create a 2D grid/graph
cria_grafo(Col, Lin, FloorCode) :-
    cria_grafo_lin(Col, Lin, FloorCode).

cria_grafo(_, 0, _) :- !.

cria_grafo(Col, Lin, FloorCode) :-
    cria_grafo_lin(Col, Lin, FloorCode),
    Lin1 is Lin - 1,
    cria_grafo(Col, Lin1, FloorCode).

cria_grafo_lin(0, _, _) :- !.

cria_grafo_lin(Col, Lin, FloorCode) :-
    m(Col, Lin, Valor, Tipo, FloorCode),
    assertz(celula(FloorCode, Col, Lin, Valor, Tipo)),
    ColS is Col + 1, ColA is Col - 1, LinS is Lin + 1, LinA is Lin - 1,

    ((m(ColS, Lin, _, _, FloorCode), assertz(ligacel(cel(Col, Lin, FloorCode), cel(ColS, Lin, FloorCode))); true)),
    ((m(ColA, Lin, _, _, FloorCode), assertz(ligacel(cel(Col, Lin, FloorCode), cel(ColA, Lin, FloorCode))); true)),
    ((m(Col, LinS, _, _, FloorCode), assertz(ligacel(cel(Col, Lin, FloorCode), cel(Col, LinS, FloorCode))); true)),
    ((m(Col, LinA, _, _, FloorCode), assertz(ligacel(cel(Col, Lin, FloorCode), cel(Col, LinA, FloorCode))); true)),

    Col1 is Col - 1,
    cria_grafo_lin(Col1, Lin, FloorCode).

% Predicate to get plant dimensions (Col, Lin) based on FloorCode
plant_dimensions(FloorCode, Col, Lin) :-
    findall(X, m(X, _, _, _, FloorCode), Cols),
    findall(Y, m(_, Y, _, _, FloorCode), Lins),
    max_list(Cols, Col),
    max_list(Lins, Lin).

% Example Usage:
% Call create_graph_for_each_plant/0 to create separate graphs for each plant map
