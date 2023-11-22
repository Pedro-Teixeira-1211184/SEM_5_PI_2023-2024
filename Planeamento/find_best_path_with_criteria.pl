
path(a1, b3, [elev(j2, j3), cor(j3, i3), cor(i3, b3), cor(b3, g3), elev(g3, g4)]).
path(a1, b3, [elev(j2, j3), cor(j3, b3), cor(b3, g3), elev(g3, g4)]).
path(a1, b3, [elev(j2, j3), cor(j3, i3), cor(i3, b3), cor(b3, g3), elev(g3, g4), cor(g3, h3)]).


% Define predicado para calcular o numero de ocorrencias de elevador em cada path
elevator_sections(Path, Num) :-
	% Remove todos os elementos que nao sao elevador do path
	select(elev(_, _), Path, ElevatorSections),
	% Conta os elementos restantes
	length(ElevatorSections, Num).


% Define predicado para selecionar o path com o menor numero de utilizacoes de elevador
best_path(Origin, Destination, Path) :-
	% Chama o predicado auxiliar com os valores iniciais para o numero minimo de ocorrencias de elevador e
	% o path com o menor numero de elementos elevador
	best_path(Origin, Destination, Path, 9999, []).


% Define predicado auxiliar
best_path(_, _, Path, _, Path) :- !. % Se o current path fôr o path com o menor numero de ocorrencias elevador, pára
	best_path(Origin, Destination, Path, Min, _) :-
		% Obtem path desde origin para destination
		path(Origin, Destination, P),
	% Calcular o numero de utilizaçoes de elevador no path
	elevator_sections(P, Num),
	% Se o numero de ocorrencias de elevador fôr menos do que o minimo atual, 
	% atualiza o minimo e o path com o menor numero de utilizações de elevador
	Num < Min,
	best_path(Origin, Destination, P, Num, P).
	
best_path(Origin, Destination, Path, Min, _) :-
	% Obter path desde origem até destino
	path(Origin, Destination, P),
	% Calcula o numero de utilizações de elevador no path
	elevator_sections(P, Num),
	% Se o numero de utilizações de elevador fôr maior ou igual do que o minimo atual, fail 
	% e faz backtrack para a chamada anterior com o proximo path
	Num >= Min,
	fail.
