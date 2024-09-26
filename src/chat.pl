:- module(chat, [rutina/0]).
:- use_module(gramatica).
:- use_module(misc).

mensaje(1, 'Hola bienvenido a NutriTec, ¿en que le puedo ayudar? \n').
rutina():- mensaje(1,X), write(X), read(A), string_lower(A,B), elementos(B,C) , validar_estructura(C).

elementos(O,C):- split_string(O,"."," ",LO), elementos(LO,[],C). % Separar oraciones por el simbolo del punto(.)
elementos([X|R],C,CF):- split_string(X," ","",LP),listado(LP,PS),elementos(R,[PS|C],CF). % Separar elementos/palabras separadas por espacio
elementos([],C,CF):- inversa(C,CF).

listado(LP,L):- listado(LP,[],[],L).
listado([X|R],[],L1,L2):- split_string(X,","," ",P), listado(R,P,L1,L2).
listado(LP,[N|P],L1,L2):- N \== "", listado(LP,P,[N|L1],L2).
listado(LP,[N|P],L1,L2):- N == "", listado(LP,P,L1,L2).
listado([],[],L1,L2):- listado([],L1,L2).
listado([],L1,L2):- inversa(L1,L2).

validar_estructura([X|In]):- print(X), descomponer(X,A,[B,C]), write("\n"), write([A,B,C]), oracion(A,B,C),
                                write("\n>> La oracion es valida\n"), validar_estructura(In).
validar_estructura([]).