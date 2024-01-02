:- module(search_algorithms, [
    path_between_floors/4,
    best_path/3,
    searchByDfsAlgorithm/3,
    searchByBestDfsAlgorithm/3,
    searchByBestBfsAlgorithm/3
]).

:- use_module(graph_handling, [data/0]).

% path_between_floors(+StartFloor, +EndFloor, +ListaEdificiosCaminho, +ListaLigacoes)
% Devolve uma lista com os edificios que fazem parte do caminho entre dois pisos
path_between_floors(StartFloor, EndFloor, LEdCam, LLig) :-
    writeln('Starting path_between_floors...'), % Debug message
    graph_handling:floors(EdOr, _),
    member(StartFloor, EdOr),
    graph_handling:floors(EdDest, _),
    member(EndFloor, EdDest),
    path_between_buildings(EdOr, EdDest, LEdCam),
    segue_pisos(StartFloor, EndFloor, LEdCam, LLig),
    writeln('Ending path_between_floors.'). % Debug message

segue_pisos(StartFloor, EndFloor, _, []) :-
    StartFloor = EndFloor,
    writeln('Reached the same floor in segue_pisos.'). %Debug message

segue_pisos(StartFloor1, EndFloor, [EdDest], [elevator(StartFloor1, EndFloor)]) :-
    StartFloor1 \= EndFloor,
    graph_handling:elevators(EndFloor, LPisos),
    member(StartFloor1, LPisos),
    member(EndFloor, LPisos),
    writeln('Constructed elevator connection in segue_pisos.'). %Debug message

segue_pisos(PisoAct, EndFloor, [EdAct, EdDest|LEdCam], [passageway(PisoAct, PisoSeg)|LOutrasLig]) :-
    (graph_handling:passageways(EdAct, EdDest, PisoAct, PisoSeg)
     -> writeln('Constructed passageway connection in segue_pisos.') % Remove the extra semicolon
     ; graph_handling:passageways(EdSeg, EdAct, PisoSeg, PisoAct),
       writeln('Constructed reverse passageway connection in segue_pisos.')
    ),
    segue_pisos(PisoSeg, EndFloor, [EdSeg|LOutrosEd], LOutrasLig),
    writeln('Constructed passageway connection in segue_pisos.').


segue_pisos(PisoAct, EndFloor, [EdAct, EdDest|LEdCam], [elevator(PisoAct, PisoAct1), passageway(PisoAct1, PisoSeg) | LOutrasLig]) :-
    (graph_handling:passageways(EdAct, EdSeg, PisoAct1, PisoSeg);
    graph_handling:passageways(EdSeg, EdAct, PisoSeg, PisoAct1)),
    PisoAct1 \== PisoAct,
    graph_handling:elevators(EdAct, LPisos),
    member(PisoAct, LPisos),
    member(PisoAct1, LPisos),
    segue_pisos(PisoSeg, EndFloor, [EdSeg|LOutrosEd], LOutrasLig).
    writeln('Constructed elevator and passageway connection in segue_pisos.'). %Debug message

path_between_buildings(EdOr, EdDest, LEdCam) :-
    writeln('Starting path_between_buildings...'), % Debug message
    path_between_buildings2(EdOr, EdDest, [EdOr], LEdCam),
    writeln('Ending path_between_buildings.'). % Debug message

path_between_buildings2(Dest, Dest, LA, Cam) :- !,
    reverse(LA, Cam),
    writeln('Reached destination in path_between_buildings2.'). % Debug message
path_between_buildings2(EdAct, EdDest, LEdPassed, LEdCam) :-
    (graph_handling:connections(EdAct, EdInt); graph_handling:connections(EdInt, EdAct)),
    \+ member(EdInt, LEdPassed),
    path_between_buildings2(EdInt, EdDest, [EdInt|LEdPassed], LEdCam),
    writeln('Continuing path_between_buildings2...'). % Debug message

%-----------------------------------------------------
% Predicado para calcular o numero de utilizacoes de elevador em cada path
elevator_sections(Path, Num) :-
    aggregate_all(count, (member(Step, Path), elevator(Step, _)), Num).

% Predicado para selecionar o path com o menor numero de utilizacoes de elevador
best_path(Origin, Destination, Path) :-
    % Gerar todos os paths desde origin até destination
    all_dfs(Origin, Destination, Paths),
    % Iterar sobre estes paths
    best_path(Paths, [], Path).

% Predicado auxiliar
best_path([], BestPath, BestPath).
best_path([Path|Paths], BestPath, BestPath) :-
    % Calcular o numero de utilizacoes de elevador no current path
    elevator_sections(Path, Num),
    % Se o numero de utilizacoes de elevador for menor do que no melhor path até agora,
    % atualiza o best path
    (  Num < BestNum
    -> best_path(Paths, Path, BestPath)
    ;  best_path(Paths, BestPath, BestPath)
    ).

% DFS
searchByDfsAlgorithm([Ox,Oy],[Dx,Dy],Result) :-
    dfs(cel(Ox,Oy),cel(Dx,Dy),Result).

dfs(Orig,Dest,Cam) :-
    dfs2(Orig,Dest,[Orig],Cam).

dfs2(Dest,Dest,LA,Cam) :-
    reverse(LA,Cam).

dfs2(Dest,[Act|T],LA,Cam) :-
    graph_handling:ligacel(Act,X),
    \+ member(X,LA),
    dfs2(Dest,X,[X|LA],Cam).

all_dfs(Orig,Dest,LCam) :-
    findall(Cam,dfs(Orig,Dest,Cam),LCam).

% Best DFS
searchByBestDfsAlgorithm([Ox,Oy],[Dx,Dy],Result) :-
    better_dfs(cel(Ox,Oy),cel(Dx,Dy),Result).

better_dfs(Orig,Dest,Cam) :-
    all_dfs(Orig,Dest,LCam),
    shortlist(LCam,Cam,_).

shortlist([L],L,N) :-
    length(L,N).

shortlist([L|LL],Lm,Nm) :-
    shortlist(LL,Lm1,Nm1),
    length(L,NL),
    ((NL<Nm1,!,Lm=L,Nm is NL);(Lm=Lm1,Nm is Nm1)).

% Best BFS
searchByBestBfsAlgorithm([Ox,Oy],[Dx,Dy],Result) :-
    bfs(cel(Ox,Oy),cel(Dx,Dy),Result).

bfs(Orig,Dest,Cam) :-
    bfs2(Dest,[[Orig]],Cam).

bfs2(Dest,[[Dest|T]|_],Cam) :-
    reverse([Dest|T],Cam).

bfs2(Dest,[LA|Outros],Cam) :-
    LA=[Act|_],
    findall([X|LA],
    (Dest\==Act,graph_handling:ligacel(Act,X),\+ member(X,LA)),Novos),
    append(Outros,Novos,Todos),
    bfs2(Dest,Todos,Cam).

% A*
aStar(Orig, Dest, Cam, Custo) :-
    aStar2(Dest, [(_, 0, [Orig])], Cam, Custo).

aStar2(Dest, [(_, Custo, [Dest | T]) | _], Cam, Custo) :-
    reverse([Dest | T], Cam).

aStar2(Dest, [(_, Ca, LA) | Outros], Cam, Custo) :-
    LA = [Act | _],
    findall((CEX, CaX, [X | LA]),
            (Dest \== Act,
             graph_handling:ligacel(Act, X),
             \+ member(X, LA),
             graph_handling:x(X, X1, Y1),
             graph_handling:x(Dest, X2, Y2),
             EstX is sqrt((X1 - X2)**2 + (Y1 - Y2)**2),
             CaX is Ca + 1,
             CEX is CaX + EstX),
            Novos),
    append(Outros, Novos, Todos),
    sort(Todos, TodosOrd),
    aStar2(Dest, TodosOrd, Cam, Custo).


% Estimativa heurística para o A*
estimativa(Nodo1,Nodo2,Estimativa) :-
    graph_handling:x(Nodo1,X1,Y1),
    graph_handling:x(Nodo2,X2,Y2),
    Estimativa is sqrt((X1-X2)^2+(Y1-Y2)^2).
