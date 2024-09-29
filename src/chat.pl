:- module(chat, [rutina/0]).
:- use_module(gramatica).
:- use_module(misc).

mensaje(1, 'Hola bienvenido a NutriTec, ¿en que le puedo ayudar? \n').
rutina:- mensaje(1,X), write(X), read_line_to_string(user_input,A), string_lower(A,B), segmentos(B,C), print(C), validar_estructura(C).

segmentos(S,L):- split_string(S,"."," ",S1), segmentos(S1,[],L).
segmentos([X|R],L1,L2):- X \== "", split_string(X," ", ",",X1), palabras(X1,P), segmentos(R,[P|L1],L2).
segmentos([X|R],L1,L2):- X == "", segmentos(R,L1,L2).
segmentos([],L1,L2):- inversa(L1,L2).

palabras(P,LP):- palabras(P,[],[],LP).
palabras([X|R],[],L2, LP):- atom_string(Atom,X), atomic_list_concat(X1,',',Atom), palabras(R,X1,L2,LP).
palabras(R,[X|L1],L2,LP):- X \== '', palabras(R,L1,[X|L2],LP).
palabras(R,[X|L1],L2,LP):- X == '', palabras(R,L1,L2,LP).
palabras([],[],L2,LP):- palabras([],L2,LP).
palabras([],P,LP):- inversa(P,LP).

validar_estructura([X|In]):- print(X), descomponer(X,A,[B,C]), write("\n"), write([A,B,C]), oracion(A,B,C),
                                write("\n>> La oracion es valida\n"), validar_estructura(In).
validar_estructura([]).