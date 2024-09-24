:- module(diccionario,[determinante/2, nombre/2, verbo/1]).
:- use_module(misc).

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
verbo([V]):- conjugado(_,V,_).                                                                                            
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
auxiliar(de).