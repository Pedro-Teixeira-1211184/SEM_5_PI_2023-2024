:-module(search_algorithms, [path_between_floors/4, best_path_per_floors/3, searchByDfsAlgorithm/3, searchByBestDfsAlgorithm/3, searchByBfsAlgorithm/3]).
:- use_module(graph_handling, data/0).

% path_between_floors(+StartFloor, +EndFloor, +ListaEdificiosCaminho, +ListaLigacoes)
% Devolve uma lista com os edificios que fazem parte do caminho entre dois pisos
path_between_floors(StartFloor, EndFloor, LEdCam, LLig) :-
    graph_handling:floors(EdOr, LPisosOr),
    member(StartFloor, LPisosOr),
    graph_handling:floors(EdDest, LPisosDest),
    member(EndFloor, LPisosDest),
    path_between_buildings(EdOr, EdDest, LEdCam).
    segue_pisos(StartFloor, EndFloor, LEdCam, LLig).

segue_pisos(StartFloor, EndFloor, _, []).
segue_pisos(StartFloor1, EndFloor, [EdDest], [elevator(StartFloor1, EndFloor)]) :-
    StartFloor1 \= EndFloor,
    graph_handling:elevators(EndFloor, LPisos),
    member(StartFloor1, LPisos),
    member(EndFloor, LPisos).

segue_pisos(PisoAct, EndFloor, [EdAct, EdDest|LEdCam], [passageway(PisoAct, PisoSeg)|LOutrasLig]) :-
    (graph_handling:passageways(EdAct, EdDest, PisoAct, PisoSeg);
    graph_handling:passageways(EdSeg, EdAct, PisoSeg, PisoAct)),
    segue_pisos(PisoSeg, EndFloor, [EdSeg|LOutrosEd], LOutrasLig).

segue_pisos(PisoAct, EndFloor, [EdAct, EdDest|LEdCam], [elevator(PisoAct, PisoAct1), passageway(PisoAct1, PisoSeg) | LOutrasLig]) :-
    (graph_handling:passageways(EdAct, EdSeg, PisoAct1, PisoSeg);
    graph_handling:passageways(EdSeg, EdAct, PisoSeg, PisoAct1)),
    PisoAct1 \== PisoAct,
    graph_handling:elevators(EdAct, LPisos), member(PisoAct, LPisos),member(PisoAct1, LPisos),
    segue_pisos(PisoSeg, EndFloor, [EdSeg|LOutrosEd], LOutrasLig).

path_between_buildings(EdOr, EdDest, LEdCam) :-
    path_between_buildings2(EdOr, EdDest, [EdOr], LEdCam).
    path_between_buildings2(EdX, EdX, LEdInv, LEdCam) :- !, reverse(LEdInv, LEdCam).
    path_between_buildings2(EdAct, EdDest, LEdPassed, LEdCam) :-
       (graph_handling:connections(EdAct, EdInt);
       graph_handling:connections(EdInt, EdAct)),
       \+ member(EdInt, LEdPassed),
         path_between_buildings2(EdInt, EdDest, [EdInt|LEdPassed], LEdCam).

%-----------------------------------------------------
% Predicado para calcular o numero de utilizacoes de elevador em cada path
elevator_sections(Path, Num) :-
 aggregate_all(count, (member(Step, Path), elev(Step, _)), Num).

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

%DFS------------------------------------------------------------------------------------------------
searchByDfsAlgorithm([Ox,Oy],[Dx,Dy],Result):-
    dfs(cel(Ox,Oy),cel(Dx,Dy),Result).

dfs(Orig,Dest,Cam):-
    dfs2(Orig,Dest,[Orig],Cam).

dfs2(Dest,Dest,LA,Cam):-
    reverse(LA,Cam).

dfs2(Act,Dest,LA,Cam):-
    graph_handling:ligacel(Act,X),
    \+ member(X,LA),
    dfs2(X,Dest,[X|LA],Cam).

all_dfs(Orig,Dest,LCam):-
    findall(Cam,dfs(Orig,Dest,Cam),LCam).


%Best DFS------------------------------------------------------------------------------------------------
searchByBestDfsAlgorithm([Ox,Oy],[Dx,Dy],Result):-
    better_dfs(cel(Ox,Oy),cel(Dx,Dy),Result).

better_dfs(Orig,Dest,Cam):-
    all_dfs(Orig,Dest,LCam),
    shortlist(LCam,Cam,_).

shortlist([L],L,N):-
    !,
    length(L,N).

shortlist([L|LL],Lm,Nm):-
    shortlist(LL,Lm1,Nm1),
    length(L,NL),
    ((NL<Nm1,!,Lm=L,Nm is NL);(Lm=Lm1,Nm is Nm1)).

%Best BFS------------------------------------------------------------------------------------------------
searchByBestBfsAlgorithm([Ox,Oy],[Dx,Dy],Result):-
    bfs(cel(Ox,Oy),cel(Dx,Dy),Result).

bfs(Orig,Dest,Cam):-
    bfs2(Dest,[[Orig]],Cam).
bfs2(Dest,[[Dest|T]|_],Cam):-
    reverse([Dest|T],Cam).
bfs2(Dest,[LA|Outros],Cam):-
    LA=[Act|_],
    findall([X|LA],
    (Dest\==Act,graph_handling:ligacel(Act,X),\+ member(X,LA)),Novos),
    append(Outros,Novos,Todos),
    bfs2(Dest,Todos,Cam).

%A*------------------------------------------------------------------------------------------------
aStar(Orig,Dest,Cam,Custo):-
    aStar2(Dest,[(_,0,[Orig])],Cam,Custo).

aStar2(Dest,[(_,Custo,[Dest|T])|_],Cam,Custo):-
    reverse([Dest|T],Cam).

aStar2(Dest,[(_,Ca,LA)|Outros],Cam,Custo):-
    LA=[Act|_],
    findall((CEX,CaX,[X|LA]),
    (Dest\==Act,edge(Act,X,CustoX),\+ member(X,LA),CaX is CustoX + Ca, estimativa(X,Dest,EstX),
    CEX is CaX +EstX),Novos),
    append(Outros,Novos,Todos),
    sort(Todos,TodosOrd),
    aStar2(Dest,TodosOrd,Cam,Custo).
estimativa(Nodo1,Nodo2,Estimativa):-
    node(Nodo1,X1,Y1),
    node(Nodo2,X2,Y2),
    Estimativa is sqrt((X1-X2)^2+(Y1-Y2)^2).








