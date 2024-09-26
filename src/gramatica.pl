:- module(gramatica, [oracion_valida/1, descomponer/3, oracion/3]).
:- use_module(diccionario).
:- use_module(misc).

% -------------------------------[ Composicion de oraciones ]-------------------------------
% >> Sintagmas verbales
sintagma_verbal([V], Q):- verbo(V, Q).
sintagma_verbal([V,SN], Q):- verbo(V, Q), sintagma_nominal(SN, Q).

% >> Sintagmas nominales
sintagma_nominal([D,N], Q):- determinante(D,G,Q), nombre(N,G,Q). %REGLA:

% Sintagmas particulares
sintagma_nominal(["yo"], s). % HECHO: sintagma_nominal(palabra, numero) = indica una palabra unica que funciona como sintagma nominal
sintagma_nominal(["me"], s).

% >>> Oraciones
% [Validar que una oracion-lista dada sea puede construir con el diccionario conocido]
oracion_valida(Oracion):- palabras_validas(Oracion), descomponer(Oracion,A,[B,C]), oracion(A,B,C).

% [Verificar que las palabras usadas en una oracion sean parte del diccionario conocido]
palabras_validas([X|R]):- determinante(X,_,_), palabras_validas(R).
palabras_validas([X|R]):- nombre(X,_,_), palabras_validas(R).
palabras_validas([X|R]):- verbo(X,_), palabras_validas(R).
palabras_validas([X|R]):- auxiliar(X), palabras_validas(R).
palabras_validas([X|R]):- sintagma_nominal([X],_), palabras_validas(R).
palabras_validas([]).

% [Descomponer una oracion bien formada en su estructura Sintagmas Nominal+Sintagma Verbal]
 % X = (lista) sintagma nominal
 % [Y,Z] = [Y=(lista)verbo + Z=(lista)sintagma nominal]
 % <=> X1,X2: sintagmas nominales | Y1,Y2: verbos | Z1:Z2: sintagmas nominales
descomponer(Oracion,X,[Y,Z]):- descomponer(Oracion,[],[],[],X,Y,Z).

descomponer([N|L],X1,[],[],X2,Y2,Z2):- (determinante(N,_,_) ; nombre(N,_,_)), descomponer(L,[N|X1],[],[],X2,Y2,Z2).
descomponer([N|L],_,[],[],X2,Y2,Z2):- sintagma_nominal([N],_), descomponer(L,[N],[],[],X2,Y2,Z2).
descomponer([N|L],X1,[],[],X2,Y2,Z2):- verbo(N,_), descomponer(L,X1,[N],[],X2,Y2,Z2).
descomponer([N|L],X1,Y1,[],X2,Y2,Z2):- len(H,X1), H > 0, len(K,Y1),
                                        K > 0, verbo(N,_), descomponer(L,X1,[N|Y1],[],X2,Y2,Z2).
descomponer([N|L],X1,Y1,[],X2,Y2,Z2):- len(H,X1), H > 0, len(K,Y1), K > 0,
                                        (determinante(N,_,_) ; nombre(N,_,_)), descomponer(L,X1,Y1,[N],X2,Y2,Z2).
descomponer([N|L],X1,Y1,Z1,X2,Y2,Z2):- len(H,X1), H > 0, len(K,Y1), K > 0, len(T,Z1), T > 0,
                                        (determinante(N,_,_) ; nombre(N,_,_)), descomponer(L,X1,Y1,[N|Z1],X2,Y2,Z2).
% CASO BASE: la oracion-lista fue totalmente procesada y sintagmas encontrados coinciden con los propuestos
descomponer([],X1,Y1,Z1,X2,Y2,Z2):- inversa(X1,X2), inversa(Y1,Y2), inversa(Z1,Z2).

% [Verificar que una oracion dada en sus tres componenetes sea valida]
oracion(X,Y,Z):- sintagma_nominal(X,Q), sintagma_verbal([Y,Z],Q).
