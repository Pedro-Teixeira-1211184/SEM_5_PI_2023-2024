:-consult('base_conhecimento.pl').

% O algoritmo cria_grafo cria um 2D grid/grafo com as dimensoes Col/Lin.

% Caso base da recursao. Define que o algoritmo pára quando lin fôr igual a 0

cria_grafo(_,0):-!.

% Esta regra é chamada recursivamente para cada linha
% Cria as celulas na linha atual
% Decrementa Lin de 1 e invoca-se a si mesmo com o novo valor de Lin

cria_grafo(Col,Lin):-cria_grafo_lin(Col,Lin),Lin1 is Lin-1,cria_grafo(Col,Lin1).

% Caso base da recursao (colunas)
% Quando col == 0, a recursão pára

cria_grafo_lin(0,_):-!.

% Esta regra é chamada recursivamente para cada coluna
% Verifica se a celula atual == 0, se sim conecta-a às celulas vizinhas
% ColX e LinY sao as coordenadas da celula vizinha

cria_grafo_lin(Col,Lin):-m(Col,Lin,0),!,ColS is Col+1, ColA is Col-1, LinS is Lin+1,LinA is Lin-1,
    ((m(ColS,Lin,0),assertz(ligacel(cel(Col,Lin),cel(ColS,Lin)));true)),
    ((m(ColA,Lin,0),assertz(ligacel(cel(Col,Lin),cel(ColA,Lin)));true)),
    ((m(Col,LinS,0),assertz(ligacel(cel(Col,Lin),cel(Col,LinS)));true)),
    ((m(Col,LinA,0),assertz(ligacel(cel(Col,Lin),cel(Col,LinA)));true)),
    
	% Decrementa Col
	
	Col1 is Col - 1,
	
	% Chama-se recursivamente com novo valor de Col
    
	cria_grafo_lin(Col1,Lin).
	
% Se a celula atual nao estiver vazia,
% Decrementa Col de 1 e chama-se recursivamente com Col atualizado

cria_grafo_lin(Col,Lin):-Col1 is Col-1,cria_grafo_lin(Col1,Lin).