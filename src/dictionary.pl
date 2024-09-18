% -------------------------------[ Determinates ]-------------------------------
determinante(el, m). % HECHO: determinante(palabra, genero) = es un determinante conocido clasificado por genero
determinante(la, f).

% -------------------------------[ Nombres o sujetos ]-------------------------------
nombre(hombre, m). % HECHO: nombre(palabra, genero) = es un nombre conocido clasificado por genero
nombre(manzana, f).
nombre(casa, f).

% -------------------------------[ Verbos y derivados]-------------------------------
lista([1,2,3,4]).
% lista([han,tenido]).

member(X,[X|_]).
member(X,[_|R]):- member(X,R).

len(0,[]).
len(N,[_|R]):- len(L,R), N is L+1.
% >> Verbos
verboso(V):- verbo(V).
verboso(VS):- len(L,VS) ,!, L > 0 ,!, member(A,VS), verbo(A).

verbo(V):- conjugado(_,V). % REGLA: verbo(conjugado) = analiza si un verbo es un conjugado de otro
verbo(haber).
verbo(comer).   % HECHO: verbo(palabra) = es una palabra conocida
verbo(estar).
verbo(tener).

% >> Conjugaciones de verbos
conjugado(comer, come). % HECHO: conjugado(verbo, palabra) = es una conjugacion conocida de un verbo
conjugado(estar, esta).
conjugado(tener, tiene).
conjugado(tener, tenido).
conjugado(haber, han).

% >> Auxiliares de verbos
auxiliar(en). %HECHO: auxiliar(palabra) = es un auxiliar conocido

% -------------------------------[ Composicion de oraciones ]-------------------------------
% Sintagmas
sintagma_nominal(D,N):- determinante(D,G), nombre(N,G).   %REGLA:
sintagma_nominal([yo]). %HECHO: sintagma_nominal(palabra) = indica una palabra unica que funciona como sintagma nominal

sintagma_verbal(V,D,N):- verbo(V), sintagma_nominal(D,N). %REGLA:

% Oraciones
oracion(A,B,C,D,E):- sintagma_nominal(A,B), sintagma_verbal(C,D,E), B\==E, write([A,B,C,D,E]). %REGLA: