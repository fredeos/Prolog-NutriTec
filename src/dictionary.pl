% -------------------------------[ Reglas/metodos auxiliares ]-------------------------------
member(X,[X|_]).
member(X,[_|R]):- member(X,R).

len(0,[]).
len(N,[_|R]):- len(L,R), N is L+1.

% -------------------------------[ Composicion de oraciones ]-------------------------------
% >> Sintagmas verbales
sintagma_verbal([V]):- verbo(V).
sintagma_verbal([V,SN]):- verbo(V), sintagma_nominal(SN).

% >> Sintagmas nominales
sintagma_nominal([D,N]):- determinante(D,G), nombre(N,G).   %REGLA:
% sintagma_nominal([que, R]):- respuesta(R).

% Sintagmas particulares
sintagma_nominal([yo]). %HECHO: sintagma_nominal(palabra) = indica una palabra unica que funciona como sintagma nominal
sintagma_nominal([me]).

% Oraciones
% oracion([SN, SV]):- .

% -------------------------------[ Determinates ]-------------------------------
determinante(el, m). % HECHO: determinante(palabra, genero) = es un determinante conocido clasificado por genero
determinante(la, f).
determinante(lo, n).
determinante(los, m).
determinante(las, f).

% -------------------------------[ Nombres o sujetos ]-------------------------------
nombre(hombre, m). % HECHO: nombre(palabra, genero) = es un nombre conocido clasificado por genero
nombre(manzana, f).
nombre(casa, f).
nombre(doctor, m).

% -------------------------------[ Respuestas sencillas ]-------------------------------
respuesta(si).
respuesta(no).

% -------------------------------[ Verbos y derivados]-------------------------------
% >> Verbos
verbo(LV):- combinacion(LV). % REGLA: verbo(lista) verifica que una lista de verbos que
                                                  % sean alguna combinacion conocida
verbo(V):- conjugado(_,V,_). % REGLA: verbo(conjugado) = analiza si un verbo es un conjugado de otro

verbo(haber). % HECHO: verbo(palabra) = es una palabra conocida en forma definitiva
verbo(comer).
verbo(estar).
verbo(tener).
verbo(necesitar).
verbo(diagnosticar).

% >> Conjugaciones de verbos
conjugado(comer, come, na). % HECHO: conjugado(verbo, palabra, terminacion) = es una conjugacion conocida de un verbo
conjugado(comer, como, na).

conjugado(estar, esta, na).

conjugado(tener, tiene, na).
conjugado(tener, tienen, na).
conjugado(tener, tengo, na).

conjugado(haber, han, na).
conjugado(haber, hay, na).

conjugado(necesitar, necesito, na).

conjugado(diagnosticar, diagnosticado, ado).
conjugado(diagnosticar, diagnostico, na).
conjugado(diagnosticar, diagnosticaron, na).


% >> Combinaciones verbales
combinacion([han, X]):- conjugado(_,X,ado).

% >> Auxiliares de verbos
auxiliar(en). %HECHO: auxiliar(palabra) = es un auxiliar conocido