:- module(search_algorithms, [
    path_between_floors/4,
    best_path/3,
    searchByDfsAlgorithm/3,
    searchByBestDfsAlgorithm/3,
    searchByBestBfsAlgorithm/3
]).

:- use_module(graph_handling, [data/0]).

% path_between_floors(+StartFloor, +EndFloor, +listEdificiosCaminho, +listLigacoes)
% Devolve uma list com os edificios que fazem parte do caminho entre dois pisos
path_between_floors(StartFloor, StartFloor, [], []).
path_between_floors(StartFloor, EndFloor, Buildings, Path) :-
    StartFloor \= EndFloor,
    path_between_floors_aux(StartFloor, EndFloor, Buildings, Path).

path_between_floors_aux(StartFloor, EndFloor, Buildings, Path) :-
    graph_handling:floors(Building, _),
    path_between_buildings(Building, Building, Buildings),
    path_between_floors_aux2(StartFloor, EndFloor, Building, Path).

path_between_floors_aux2(StartFloor, EndFloor, Building, Path) :-
    path_between_floors_aux3(StartFloor, EndFloor, Building, Path),
    reverse(Path, PathReversed),
    format('Found path between floors ~w and ~w: ~w~n', [StartFloor, EndFloor, PathReversed]).

path_between_floors_aux3(StartFloor, EndFloor, Building, Path) :-
    (graph_handling:elevators(Building, _) ->
        best_path(StartFloor, EndFloor, Path)
    ;   searchByBestDfsAlgorithm([StartFloor, Building], [EndFloor, Building], Path)
    ).

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


% Predicate to find all tasks
find_tasks(Tasks) :-
    findall(Task, dataService:task(Task), Tasks).

% Predicate to count the number of tasks
count_tasks(Count) :-
    find_tasks(Tasks),
    length(Tasks, Count).
% tasks(Ntasks).

tasks(Count):-
    count_tasks(Count).

% parameterização
inicializa(NG,DP,P1,P2):-
    (retract(geracoes(_));true), asserta(geracoes(NG)),
    (retract(populacao(_));true), asserta(populacao(DP)),
    PC is P1/100,
    (retract(prob_cruzamento(_));true), asserta(prob_cruzamento(PC)),
    PM is P2/100,
    (retract(prob_mutacao(_));true), asserta(prob_mutacao(PM)).

%pc-PROB.cruzamento,pM-PROB.MUTAÇAO,dp-dimensao da populaçao,NG-numero-geraçoes
searchSequencebyGeneticAlgorithm(NG,DP,PC,PM,PopOrd):-
    inicializa(NG,DP,PC,PM),
    gera_populacao(Pop),
    avalia_populacao(Pop,PopAv),
    ordena_populacao(PopAv,PopOrd),
    geracoes(NG),
    gera_geracao(0,NG,PopOrd).

gera_populacao(Pop):-
    populacao(TamPop),
    tasks(NumT),
    findall(Tarefa,dataService:task(Tarefa),listtasks),
    gera_populacao(TamPop,listtasks,NumT,Pop).
    gera_populacao(0,_,_,[]):-!.

gera_populacao(TamPop,listtasks,NumT,[Ind|Resto]):-
    TamPop1 is TamPop-1,
    gera_populacao(TamPop1,listtasks,NumT,Resto),
    gera_individuo(listtasks,NumT,Ind),
    not(member(Ind,Resto)).

gera_populacao(TamPop,listtasks,NumT,L):-
    gera_populacao(TamPop,listtasks,NumT,L).


gera_individuo([G],1,[G]):-!.
gera_individuo(listtasks,NumT,[G|Resto]):-
    NumTemp is NumT + 1, % para usar com random
    random(1,NumTemp,N),
    retira(N,listtasks,G,Novalist),
    NumT1 is NumT-1,
    gera_individuo(Novalist,NumT1,Resto).
    retira(1,[G|Resto],G,Resto).

retira(N,[G1|Resto],G,[G1|Resto1]):-
    N1 is N-1,
    retira(N1,Resto,G,Resto1).


avalia_populacao([],[]).
avalia_populacao([Ind|Resto],[Ind*V|Resto1]):-
    avalia(Ind,V),
    avalia_populacao(Resto,Resto1).

avalia(Seq,V):- 
    avalia(Seq,0,V).

avalia([ ],Inst,Inst).

avalia([T1|[]],Inst,Inst):-!.

avalia([T1,T2|Resto],Inst,V):-
    dataService:timeBetweenTasks(T1,T2,Tempo),
    InstFinal is Inst + Tempo,
    avalia([T2|Resto],InstFinal,V).


ordena_populacao(PopAv,PopAvOrd):-
    bsort(PopAv,PopAvOrd).
bsort([X],[X]):-!.

bsort([X|Xs],Ys):-
    bsort(Xs,Zs),
    btroca([X|Zs],Ys).

btroca([X],[X]):-!.

btroca([X*VX,Y*VY|L1],[Y*VY|L2]):-
    VX>VY,!,
    btroca([X*VX|L1],L2).

btroca([X|L1],[X|L2]):-btroca(L1,L2).


selecao([X1,X2|Rest], Len,[X,Y|Rest1]):-
    X=X1,
    Y=X2,
    random_permutation(Rest,LRP),
    selecaoRandom(LRP,Len,Rest1).


selecaoRandom(_,0,[]):-!.

selecaoRandom([X1|Rest], Len,[X|Rest1]):-
    X=X1,
    NewLen is Len - 1,
    selecaoRandom(Rest, NewLen, Rest1).

gera_geracao(G,G,Pop):-!.

gera_geracao(N,G,[F|Pop]):-
random_permutation(Pop,LRP),
cruzamento([F|LRP],NPop1),
mutacao(NPop1,NPop),
avalia_populacao(NPop,NPopAv),
ordena_populacao(NPopAv,NPopOrd),
length([F|Pop],Len),
selecao(NPopOrd,Len,NPopSel),
N1 is N+1,
gera_geracao(N1,G,NPopSel).

gerar_pontos_cruzamento(P1,P2):- 
    gerar_pontos_cruzamento1(P1,P2).

gerar_pontos_cruzamento1(P1,P2):-
    tasks(N),
    NTemp is N+1,
    random(1,NTemp,P11),
    random(1,NTemp,P21),
    P11\==P21,!,
    ((P11<P21,!,P1=P11,P2=P21);P1=P21,P2=P11).
    gerar_pontos_cruzamento1(P1,P2):-
    gerar_pontos_cruzamento1(P1,P2).


cruzamento([ ],[ ]).
cruzamento([Ind*_],[Ind]).
cruzamento([Ind1*_,Ind2*_|Resto],[NInd1,NInd2,Ind1,Ind2|Resto1]):-
    gerar_pontos_cruzamento(P1,P2),
    prob_cruzamento(Pcruz),random(0.0,1.0,Pc),
    ((Pc =< Pcruz,!,
    cruzar(Ind1,Ind2,P1,P2,NInd1),
    cruzar(Ind2,Ind1,P1,P2,NInd2))
    ;
    (NInd1=Ind1,NInd2=Ind2)),
    cruzamento(Resto,Resto1).
    preencheh([ ],[ ]).

preencheh([_|R1],[h|R2]):-
    preencheh(R1,R2).

sublist(L1,I1,I2,L):-I1 < I2,!,
    sublist1(L1,I1,I2,L).

sublist(L1,I1,I2,L):-
    sublist1(L1,I2,I1,L).

sublist1([X|R1],1,1,[X|H]):-!, preencheh(R1,H).

sublist1([X|R1],1,N2,[X|R2]):-!,N3 is N2 - 1,
    sublist1(R1,1,N3,R2).
    
sublist1([_|R1],N1,N2,[h|R2]):-N3 is N1 - 1,
    N4 is N2 - 1,
    sublist1(R1,N3,N4,R2).

rotate_right(L,K,L1):- tasks(N),
    T is N - K,
    rr(T,L,L1).
    rr(0,L,L):-!.

rr(N,[X|R],R2):- N1 is N - 1,
    append(R,[X],R1),
    rr(N1,R1,R2).


elimina([],_,[]):-!.
elimina([X|R1],L,[X|R2]):- not(member(X,L)),!,
    elimina(R1,L,R2).

elimina([_|R1],L,R2):-
    elimina(R1,L,R2).
insere([],L,_,L):-!.

insere([X|R],L,N,L2):-
    tasks(T),
    ((N>T,!,N1 is N mod T);N1 = N),
    insere1(X,N1,L,L1),
    N2 is N + 1,
    insere(R,L1,N2,L2).

insere1(X,1,L,[X|L]):-!.
    insere1(X,N,[Y|L],[Y|L1]):-
    N1 is N-1,
    insere1(X,N1,L,L1).


cruzar(Ind1,Ind2,P1,P2,NInd11):-
    sublist(Ind1,P1,P2,Sub1),
    tasks(NumT),
    R is NumT-P2,
    rotate_right(Ind2,R,Ind21),
    elimina(Ind21,Sub1,Sub2),
    P3 is P2 + 1,
    insere(Sub2,Sub1,P3,NInd1),
    eliminah(NInd1,NInd11).
    eliminah([],[]).

eliminah([h|R1],R2):-!,
    eliminah(R1,R2).
eliminah([X|R1],[X|R2]):-
    eliminah(R1,R2).

    mutacao([],[]).
mutacao([Ind|Rest],[NInd|Rest1]):-
    prob_mutacao(Pmut),
    random(0.0,1.0,Pm),
    ((Pm < Pmut,!,mutacao1(Ind,NInd));NInd = Ind),
    mutacao(Rest,Rest1).

mutacao1(Ind,NInd):-
    gerar_pontos_cruzamento(P1,P2),
    mutacao22(Ind,P1,P2,NInd).

mutacao22([G1|Ind],1,P2,[G2|NInd]):-
    !, P21 is P2-1,
    mutacao23(G1,P21,Ind,G2,NInd).
mutacao22([G|Ind],P1,P2,[G|NInd]):-
    P11 is P1-1, P21 is P2-1,
    mutacao22(Ind,P11,P21,NInd).

mutacao23(G1,1,[G2|Ind],G2,[G1|Ind]):-!.

mutacao23(G1,P,[G|Ind],G2,[G|NInd]):-
    P1 is P-1,
    mutacao23(G1,P1,Ind,G2,NInd).

% Exemplo de função para avaliar a pontuação de uma sequência
evaluate(Sequencia, Pontuacao) :-
    avalia(Sequencia,Pontuacao).

% Exemplo de como encontrar a melhor sequência entre várias
avaliarSequencias([],MelhorSequencia,MelhorTempo,MelhorSequencia).
avaliarSequencias([Sequencia|Resto],MelhorSequencia,MelhorTempo,Result) :-
    evaluate(Sequencia,Tempo),
    ((MelhorTempo = Tempo,NovoMelhorTempo = MelhorTempo , MelhorSequencia=Sequencia,NovaMelhorSequencia = MelhorSequencia) ;
    (Tempo<MelhorTempo -> NovoMelhorTempo = Tempo,NovaMelhorSequencia = Sequencia);true),
    avaliarSequencias(Resto,NovaMelhorSequencia,NovoMelhorTempo,Result).


melhor_brute_force(Result) :-
    findall(Tarefa,dataService:task(Tarefa),listOriginal),
    gera_permutacoes(listOriginal,Permutacoes),
    avaliarSequencias(Permutacoes, MelhorSeq,MelhorTempo,Result).

gera_permutacoes(list, Permutacoes) :-
    findall(Perm, permutation(list, Perm), Permutacoes).    