:-consult('base_conhecimento.pl').

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

all_dfs(Orig,Dest,LCam):-findav ll(Cam,dfs(Orig,Dest,Cam),LCam).


better_dfs(Orig,Dest,Cam):-all_dfs(Orig,Dest,LCam), shortlist(LCam,Cam,_).


shortlist([L],L,N):-!,length(L,N).
shortlist([L|LL],Lm,Nm):-shortlist(LL,Lm1,Nm1),
				length(L,NL),
			((NL<Nm1,!,Lm=L,Nm is NL);(Lm=Lm1,Nm is Nm1)).


% ------------------------------------------------------------------


% O algoritmo Pesquisa Primeiro em Largura para encontrar caminho
% Chama bfs2 com o nodo origem (Orig), o nodo destino (Dest),
% Lista que contem apenas nodo origem e uma lista vazia (Cam) para guardar o caminho

bfs(Orig,Dest,Cam):-bfs2(Dest,[[Orig]],Cam).

% Caso base da recursao.
% Quando o caminho atual termina com Dest, inverte o caminho
% Obtém o caminho final (Cam)

bfs2(Dest,[[Dest|T]|_],Cam):-
	reverse([Dest|T],Cam).

% Esta regra é chamada recursivamente para cada caminho.
% Primeiro passa a cabeça da lista de caminhos para LA
% Usa finall para encontrar extensões do caminho atual, com a verificação 
% de que o nodo destino não está no caminho e que o nodo atual tem ligação com o próximo (ligacel)
% verificando se este não está já presente no caminho
% Adiciona as novas extensões à lista de caminhos (Todos) - append
% Chama-se recursivamente com a lista de caminhos atualizada

bfs2(Dest,[LA|Outros],Cam):-
	LA=[Act|_],  
	findall([X|LA],
		(Dest\==Act,ligacel(Act,X),\+ member(X,LA)),
		Novos),
	append(Outros,Novos,Todos),
	bfs2(Dest,Todos,Cam).


% Algoritmo para minimizar o número de utilizações do elevador




% Algoritmo para minimizar o número de edificios percorridos