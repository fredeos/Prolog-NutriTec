:- module(diccionario,[determinante/3, nombre/3, verbo/2]).
:- use_module(misc).

% -------------------------------[ Determinates ]-------------------------------
determinante(el, m, s). % HECHO: determinante(palabra, genero, numero) = es un determinante conocido clasificado por genero y número
determinante(la, f, s).
determinante(lo, n, s).
determinante(los, m, p).
determinante(las, f, p).

% -------------------------------[ Nombres o sujetos ]-------------------------------
nombre(hombre, m, s). % HECHO: nombre(palabra, genero, numero) = es un nombre conocido clasificado por genero y número
nombre(manzana, f, s).
nombre(casa, f, s).
nombre(doctores, m, p).
nombre(manzanas, f, p).
nombre(casas, f, p).
nombre(doctores, m, p).

% -------------------------------[ Respuestas sencillas ]-------------------------------
respuesta(si).
respuesta(no).

% -------------------------------[ Verbos y derivados]-------------------------------
% >> Verbos
verbo(LV, Num):- combinacion(LV, Num). % REGLA: verbo(lista, numero) verifica que una lista de verbos sea alguna combinacion conocida en singular o plural
verbo([V], Num):- conjugado(_,V,Num,_).
verbo(V, Num):- conjugado(_,V,Num,_). % REGLA: verbo(conjugado, numero) = analiza si un verbo es un conjugado de otro en singular o plural

verbo(haber, s). % HECHO: verbo(palabra, numero) = es una palabra conocida en forma definitiva y número
verbo(comer, s).
verbo(estar, s).
verbo(tener, s).
verbo(necesitar, s).
verbo(diagnosticar, s).
verbo(haber, p).
verbo(tener, p).

% >> Conjugaciones de verbos
conjugado(comer, come, s, na). % HECHO: conjugado(verbo, palabra, numero, terminacion) = es una conjugacion conocida de un verbo en singular o plural
conjugado(comer, como, s, na).

conjugado(estar, esta, s, na).

conjugado(tener, tiene, s, na).
conjugado(tener, tienen, p, na).
conjugado(tener, tengo, s, na).

conjugado(haber, han, p, na).
conjugado(haber, hay, s, na).

conjugado(necesitar, necesito, s, na).

conjugado(diagnosticar, diagnosticado, s, ado).
conjugado(diagnosticar, diagnostico, s, na).
conjugado(diagnosticar, diagnosticaron, p, na).

% >> Combinaciones verbales
combinacion([han, X], p):- conjugado(_,X,p,ado).

% >> Auxiliares de verbos
auxiliar(en). % HECHO: auxiliar(palabra) = es un auxiliar conocido
auxiliar(de).
