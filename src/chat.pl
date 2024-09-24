:- module(chat, [rutina/0]).
:- use_module(gramatica).

mensaje(1, 'Hola bienvenido a NutriTec, ¿en que le puedo ayudar? \n').
rutina():- mensaje(1,X), write(X), read(A), elementos(A,B), write(B).

elementos(O,C):- split_string(O,"."," ",LO), elementos(LO,[],C). % Separar oraciones por el simbolo del punto(.)
elementos([X|R],C,CF):- split_string(X," ","",LP),listado(LP,PS),elementos(R,[PS|C],CF). % Separar elementos/palabras separadas por espacio
elementos([],C,C).

listado(LP,L):- listado(LP,[],[],L).
listado([X|R],[],L1,L2):- split_string(X,","," ",P), listado(R,P,L1,L2).
listado(LP,[N|P],L1,L2):- N \== "", listado(LP,P,[N|L1],L2).
listado(LP,[N|P],L1,L2):- N == "", listado(LP,P,L1,L2).
listado([],[],L1,L2):- listado([],L1,L2).
listado([],L,L).

print([X|_]):- write(X).