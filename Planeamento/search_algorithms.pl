:-module(search_algorithms,[melhor_sequencia_forca_bruta/1,searchSequencebyGeneticAlgorithm/5,caminho_pisos/4,melhor_caminho_pisos/3,searchByDfsAlgorithm/3,searchByBestDfsAlgorithm/3,searchByBfsAlgorithm/3,searchByAstarAlgorithm/4]).
:-use_module(graph_handling,data/0).

%DFS, BFS, AStar e AG -> moodle ALGAV

caminho_pisos(PisoOr,PisoDest,LEdCam,LLig):-
       
    graph_handling:floors(EdOr,LPisosOr),
    member(PisoOr,LPisosOr),
    graph_handling:floors(EdDest,LPisosDest),
    member(PisoDest,LPisosDest),
    caminho_edificios(EdOr,EdDest,LEdCam),
    segue_pisos(PisoOr,PisoDest,LEdCam,LLig).  


segue_pisos(PisoDest,PisoDest,_,[]).

segue_pisos(PisoDest1,PisoDest,[EdDest],[elev(PisoDest1,PisoDest)]):-
    
    PisoDest\==PisoDest1,
    graph_handling:elevators(EdDest,LPisos),
    member(PisoDest1,LPisos), 
    member(PisoDest,LPisos).

segue_pisos(PisoAct,PisoDest,[EdAct,EdSeg|LOutrosEd],[cor(PisoAct,PisoSeg)|LOutrasLig]):-
    
    (graph_handling:passageways(EdAct,EdSeg,PisoAct,PisoSeg);graph_handling:passageways(EdSeg,EdAct,PisoSeg,PisoAct)),
    segue_pisos(PisoSeg,PisoDest,[EdSeg|LOutrosEd],LOutrasLig).


segue_pisos(PisoAct,PisoDest,[EdAct,EdSeg|LOutrosEd],[elev(PisoAct,PisoAct1),cor(PisoAct1,PisoSeg)|LOutrasLig]):-
    
    (graph_handling:passageways(EdAct,EdSeg,PisoAct1,PisoSeg);graph_handling:passageways(EdSeg,EdAct,PisoSeg,PisoAct1)),
    PisoAct1\==PisoAct,
    graph_handling:elevators(EdAct,LPisos),member(PisoAct,LPisos),member(PisoAct1,LPisos),
    segue_pisos(PisoSeg,PisoDest,[EdSeg|LOutrosEd],LOutrasLig).


caminho_edificios(EdOr,EdDest,Result):-
    findall(LEdCam,caminho_edificios2(EdOr,EdDest,[EdOr],LEdCam),AllPaths),
    leastBuildings(AllPaths,Result).

caminho_edificios2(EdX,EdX,LEdInv,LEdCam):-!,
    reverse(LEdInv,LEdCam).

caminho_edificios2(EdAct,EdDest,LEdPassou,LEdCam):-
    (graph_handling:connections(EdAct,EdInt);graph_handling:connections(EdInt,EdAct)),
    \+member(EdInt,LEdPassou),
    caminho_edificios2(EdInt,EdDest,[EdInt|LEdPassou],LEdCam).


%Limitar o numero de buildings
leastBuildings([Array], Array).
leastBuildings([Array1, Array2 | Rest], MinArray) :-
    length(Array1, Len1),
    length(Array2, Len2),
    Len1 =< Len2,
    leastBuildings([Array1 | Rest], MinArray).
leastBuildings([Array1, Array2 | Rest], MinArray) :-
    length(Array1, Len1),
    length(Array2, Len2),
    Len1 > Len2,
    leastBuildings([Array2 | Rest], MinArray).


%Limitar o numero de elevadores
melhor_caminho_pisos(PisoOr,PisoDest,LLigMelhor):-
    findall(LLig,caminho_pisos(PisoOr,PisoDest,_,LLig),LLLig),
    menos_elevadores(LLLig,LLigMelhor,_,_).

menos_elevadores([LLig],LLig,NElev,NCor):-
    conta(LLig,NElev,NCor).
menos_elevadores([LLig|OutrosLLig],LLigR,NElevR,NCorR):-
    menos_elevadores(OutrosLLig,LLigM,NElev,NCor),
    conta(LLig,NElev1,NCor1),
    (((NElev1<NElev ; (NElev1==NElev,NCor1<NCor)),
    !,
    NElevR is NElev1, NCorR is NCor1,LLigR=LLig);
    (NElevR is NElev,NCorR is NCor,LLigR=LLigM)).
    
    
    
conta([],0,0).
conta([elev(_,_)|L],NElev,NCor):-
    conta(L,NElevL,NCor),
    NElev is NElevL+1.
conta([cor(_,_)|L],NElev,NCor):-
    conta(L,NElev,NCorL),
    NCor is NCorL+1.



%DFS
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



%Better DFS
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



%BFS
searchByBfsAlgorithm([Ox,Oy],[Dx,Dy],Result):-
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


%AStar
searchByAstarAlgorithm([Ox,Oy],[Dx,Dy],Result,Custo):-
    aStar(cel(Ox,Oy),cel(Dx,Dy),Result,Custo).

aStar(Orig,Dest,Cam,Custo):-
    aStar2(Dest,[(_,0,[Orig])],Cam,Custo).

aStar2(Dest,[(_,Custo,[Dest|T])|_],Cam,Custo):-
    reverse([Dest|T],Cam).

aStar2(Dest,[(_,Ca,LA)|Outros],Cam,Custo):-
    LA=[Act|_],
    findall((CEX,CaX,[X|LA]),
    (Dest\==Act,graph_handling:edge(Act,X,CustoX),\+ member(X,LA),CaX is CustoX + Ca, estimativa(X,Dest,EstX),
    CEX is CaX +EstX),Novos),
    append(Outros,Novos,Todos),
    sort(Todos,TodosOrd),
    aStar2(Dest,TodosOrd,Cam,Custo).
estimativa(Nodo1,Nodo2,Estimativa):-
    graph_handling:node(Nodo1,X1,Y1),
    graph_handling:node(Nodo2,X2,Y2),
    Estimativa is sqrt((X1-X2)^2+(Y1-Y2)^2).


% Predicate to find all tasks
all_tasks(Tasks) :-
    findall(Task, graph_handling:task(Task), Tasks).

% Predicate to count the number of tasks
count_tasks(Count) :-
    all_tasks(Tasks),
    length(Tasks, Count).
% tarefas(NTarefas).

tarefas(Count):-
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
    tarefas(NumT),
    findall(Tarefa,graph_handling:task(Tarefa),ListaTarefas),
    gera_populacao(TamPop,ListaTarefas,NumT,Pop).
    gera_populacao(0,_,_,[]):-!.

gera_populacao(TamPop,ListaTarefas,NumT,[Ind|Resto]):-
    TamPop1 is TamPop-1,
    gera_populacao(TamPop1,ListaTarefas,NumT,Resto),
    gera_individuo(ListaTarefas,NumT,Ind),
    not(member(Ind,Resto)).

gera_populacao(TamPop,ListaTarefas,NumT,L):-
    gera_populacao(TamPop,ListaTarefas,NumT,L).


gera_individuo([G],1,[G]):-!.
gera_individuo(ListaTarefas,NumT,[G|Resto]):-
    NumTemp is NumT + 1, % para usar com random
    random(1,NumTemp,N),
    retira(N,ListaTarefas,G,NovaLista),
    NumT1 is NumT-1,
    gera_individuo(NovaLista,NumT1,Resto).
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
    graph_handling:timeBetweenTasks(T1,T2,Tempo),
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
    tarefas(N),
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

sublista(L1,I1,I2,L):-I1 < I2,!,
    sublista1(L1,I1,I2,L).

sublista(L1,I1,I2,L):-
    sublista1(L1,I2,I1,L).

sublista1([X|R1],1,1,[X|H]):-!, preencheh(R1,H).

sublista1([X|R1],1,N2,[X|R2]):-!,N3 is N2 - 1,
    sublista1(R1,1,N3,R2).
    
sublista1([_|R1],N1,N2,[h|R2]):-N3 is N1 - 1,
    N4 is N2 - 1,
    sublista1(R1,N3,N4,R2).

rotate_right(L,K,L1):- tarefas(N),
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
    tarefas(T),
    ((N>T,!,N1 is N mod T);N1 = N),
    insere1(X,N1,L,L1),
    N2 is N + 1,
    insere(R,L1,N2,L2).

insere1(X,1,L,[X|L]):-!.
    insere1(X,N,[Y|L],[Y|L1]):-
    N1 is N-1,
    insere1(X,N1,L,L1).


cruzar(Ind1,Ind2,P1,P2,NInd11):-
    sublista(Ind1,P1,P2,Sub1),
    tarefas(NumT),
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
avalia_sequencia(Sequencia, Pontuacao) :-
    avalia(Sequencia,Pontuacao).

% Exemplo de como encontrar a melhor sequência entre várias
avaliarSequencias([],MelhorSequencia,MelhorTempo,MelhorSequencia).
avaliarSequencias([Sequencia|Resto],MelhorSequencia,MelhorTempo,Result) :-
    avalia_sequencia(Sequencia,Tempo),
    ((MelhorTempo = Tempo,NovoMelhorTempo = MelhorTempo , MelhorSequencia=Sequencia,NovaMelhorSequencia = MelhorSequencia) ;
    (Tempo<MelhorTempo -> NovoMelhorTempo = Tempo,NovaMelhorSequencia = Sequencia);true),
    avaliarSequencias(Resto,NovaMelhorSequencia,NovoMelhorTempo,Result).


melhor_sequencia_forca_bruta(Result) :-
    findall(Tarefa,graph_handling:task(Tarefa),ListaOriginal),
    gera_permutacoes(ListaOriginal,Permutacoes),
    avaliarSequencias(Permutacoes, MelhorSeq,MelhorTempo,Result).

gera_permutacoes(Lista, Permutacoes) :-
    findall(Perm, permutation(Lista, Perm), Permutacoes).