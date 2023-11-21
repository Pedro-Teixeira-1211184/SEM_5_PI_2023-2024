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
%m(col,lin,valor, tipo) - valor: 1 se for obstáculo, 0 se for livre, 
% tipo: 2 se for elevador, 3 se for passagem, 4 se for porta, 5 se for parede,0 se for livre
m(1,1,1,5).
m(2,1,1,5).
m(3,1,1,5).
m(4,1,1,5).
m(5,1,1,5).
m(6,1,1,5).
m(7,1,1,5).
m(8,1,1,5).

m(1,2,0,0).
m(2,2,0,0).
m(3,2,0,0).
m(4,2,0,0).
m(5,2,0,0).
m(6,2,0,0).
m(7,2,0,0).
m(8,2,1,5).

m(1,3,0,2).
m(2,3,0,0).
m(3,3,0,0).
m(4,3,0,0).
m(5,3,0,0).
m(6,3,0,0).
m(7,3,0,0).
m(8,3,1,5).

m(1,4,0,0).
m(2,4,0,0).
m(3,4,0,0).
m(4,4,0,0).
m(5,4,0,0).
m(6,4,0,0).
m(7,4,0,0).
m(8,4,1,5).

m(1,5,1,5).
m(2,5,1,5).
m(3,5,1,4).
m(4,5,1,5).
m(5,5,0,0).
m(6,5,0,0).
m(7,5,0,0).
m(8,5,1,5).

m(1,6,1,5).
m(2,6,1,5).
m(3,6,1,5).
m(4,6,1,5).
m(5,6,0,0).
m(6,6,0,0).
m(7,6,0,0).
m(8,6,1,5).

m(1,7,1,5).
m(2,7,1,5).
m(3,7,1,5).
m(4,7,1,5).
m(5,7,0,3).
m(6,7,0,3).
m(7,7,0,3).
m(8,7,1,5).


