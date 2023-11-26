:- consult(path_finding).

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
