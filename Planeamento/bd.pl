:-dynamic ligacel/2.

% Definicao pisos cada edificio

pisos(b,[b1,b2,b3,b4]).
pisos(g,[g2,g3,g4]).
pisos(i,[i1,i2,i3,i4]).

%Definicao elevador em cada edificio

elevador(b,[b1,b2,b3,b4]).
elevador(g,[g2,g3,g4]).
elevador(i,[i1,i2,i3,i4]).

%Definicao pontes entre edificios

corredor(b,g,b2,g2).
corredor(b,g,b3,g3).
corredor(b,i,b3,i3).


% Exemplo Piso X Edificio Y

%linha 1:1,1,1,1,1,1,1,1
%linha 2:0,0,0,0,0,0,0,1
%linha 3:0,0,0,0,0,0,0,1
%linha 4:0,0,0,0,0,0,0,1
%linha 5:1,1,1,1,0,0,0,1
%linha 6:1,1,1,1,0,0,0,1
%linha 7:1,1,1,1,0,0,0,1
%coluna :1,2,3,4,5,6,7,8
%
%
%
%m(col,lin,valor)
m(1,1,1).
m(2,1,1).
m(3,1,1).
m(4,1,1).
m(5,1,1).
m(6,1,1).
m(7,1,1).
m(8,1,1).

m(1,2,0).
m(2,2,0).
m(3,2,0).
m(4,2,0).
m(5,2,0).
m(6,2,0).
m(7,2,0).
m(8,2,1).

m(1,3,0).
m(2,3,0).
m(3,3,0).
m(4,3,0).
m(5,3,0).
m(6,3,0).
m(7,3,0).
m(8,3,1).

m(1,4,0).
m(2,4,0).
m(3,4,0).
m(4,4,0).
m(5,4,0).
m(6,4,0).
m(7,4,0).
m(8,4,1).

m(1,5,1).
m(2,5,1).
m(3,5,1).
m(4,5,1).
m(5,5,0).
m(6,5,0).
m(7,5,0).
m(8,5,1).

m(1,6,1).
m(2,6,1).
m(3,6,1).
m(4,6,1).
m(5,6,0).
m(6,6,0).
m(7,6,0).
m(8,6,1).

m(1,7,1).
m(2,7,1).
m(3,7,1).
m(4,7,1).
m(5,7,0).
m(6,7,0).
m(7,7,0).
m(8,7,1).





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

% ----------------------------------------------------------------

% Algoritmo Pesquisa Primeiro em Profundidade para encontrar caminho
% Chama dfs2 com o nodo origem (Orig), o nodo destino (Dest), 
% Lista que contem apenas nodo origem e uma lista vazia (Cam) para guardar o caminho

dfs(Orig,Dest,Cam):-
	dfs2(Orig,Dest,[Orig],Cam).

% Caso base da recursao.
% Quando o nó atual (Act) é o nodo destino, inverte a lista LA
%(Que contém o caminho desde o nodo origem até ao nodo destino) e unifica-a com Cam

dfs2(Dest,Dest,LA,Cam):-
	reverse(LA,Cam).

% Esta regra é chamada recursivamente para cada nodo.
% Primeiro verifica se existe uma ligação (ligacel) do nodo atual para outro nodo X
% Verifica se X já não está no caminho LA. 
% Se as condicoes se verificarem, chama dfs2 com 
% X como nodo atual, nodo destino, nova lista que contem X e todos os nodos anteriores e Cam

dfs2(Act,Dest,LA,Cam):-
	ligacel(Act,X),
        \+ member(X,LA),
	dfs2(X,Dest,[X|LA],Cam).

% --------------------------------------------------------------------

all_dfs(Orig,Dest,LCam):-findall(Cam,dfs(Orig,Dest,Cam),LCam).


better_dfs(Orig,Dest,Cam):-all_dfs(Orig,Dest,LCam), shortlist(LCam,Cam,_).


shortlist([L],L,N):-!,length(L,N).
shortlist([L|LL],Lm,Nm):-shortlist(LL,Lm1,Nm1),
				length(L,NL),
			((NL<Nm1,!,Lm=L,Nm is NL);(Lm=Lm1,Nm is Nm1)).


% ------------------------------------------------------------------

bfs(Orig,Dest,Cam):-bfs2(Dest,[[Orig]],Cam).

bfs2(Dest,[[Dest|T]|_],Cam):-
	reverse([Dest|T],Cam).

bfs2(Dest,[LA|Outros],Cam):-
	LA=[Act|_],  
	findall([X|LA],
		(Dest\==Act,ligacel(Act,X),\+ member(X,LA)),
		Novos),
	append(Outros,Novos,Todos),
	bfs2(Dest,Todos,Cam).


% Algoritmo para minimizar o número de utilizações do elevador




% Algoritmo para minimizar o número de edificios percorridos