:- consult('misc.pl').
% -------------------------------[ Determinates ]-------------------------------
% HECHO: determinante(palabra, genero, numero) = es un determinante conocido clasificado por genero y numero
determinante(el, m, s).
determinante(la, f, s).
determinante(lo, n, s).
determinante(los, m, p).
determinante(las, f, p).
determinante(un, m, s).
determinante(una, f, s).
determinante(unos, m, p).
determinante(unas, f, p).


% -------------------------------[ Nombres o sujetos ]-------------------------------
nombre(hombre, m, s). % HECHO: nombre(palabra, genero, numero) = es un nombre conocido clasificado por genero y numero
nombre(manzana, f, s).
nombre(casa, f, s).
nombre(doctor, m, s).
nombre(manzanas, f, p).
nombre(casas, f, p).
nombre(doctores, m, p).
nombre(mujer, f, s).
nombre(paciente, m, s).
nombre(dieta, f, s).
nombre(calorias, f, p).
nombre(persona, f, s).
nombre(ejercicio, m, s).
nombre(nutricion, f, s).
nombre(arroz, m, s).
nombre(pan, m, s).
nombre(huevo, m, s).
nombre(carne, f, s).
nombre(peso, m, s).
nombre(ensalada, f, s).
nombre(fruta, f, s).
nombre(verdura, f, s).
nombre(avena, f, s).
nombre(gimnasio, m, s).



% -------------------------------[ Respuestas sencillas ]-------------------------------
respuesta(si).
respuesta(no).
respuesta(claro).
respuesta(entendido).


% -------------------------------[ Verbos y derivados]-------------------------------
% >> Verbos
verbo(LV, Q):- combinacion(LV, Q). % REGLA: verbo(lista, numero) verifica que una lista de verbos sea alguna combinacion conocida en singular o plural
verbo([V], Q):- conjugado(_,V,Q,_).
verbo(V, Q):- conjugado(_,V,Q,_). % REGLA: verbo(conjugado, numero) = analiza si un verbo es un conjugado de otro en singular o plural

verbo(haber, s). % HECHO: verbo(palabra, numero) = es una palabra conocida en forma definitiva y pluralidad
verbo(haber, p).
verbo(comer, s).
verbo(perder,s).
verbo(estar, s).
verbo(tener, s).
verbo(tener, p).
verbo(querer, s).
verbo(necesitar, s).
verbo(diagnosticar, s).
verbo(recomendar, s).
verbo(sugerir, s).
verbo(considerar, s).
verbo(consumir, s).
verbo(evitar, s).
verbo(preferir, s).
verbo(controlar, s).
verbo(mejorar, s).
verbo(reducir, s).
verbo(aumentar, s).
verbo(controlar, s).


% >> Conjugaciones de verbos
conjugado(comer, come, s, na). % HECHO: conjugado(verbo, palabra, numero, terminacion) = es una conjugacion conocida de un verbo en singular o plural
conjugado(comer, como, s, na).
conjugado(comer, comen, p, na).
conjugado(comer, comemos, p, na).

conjugado(estar, esta, s, na).
conjugado(estar, estan, p, na).
conjugado(estar, estoy, s, na).
conjugado(estar, estamos, p, na).

conjugado(querer, quiero, s, na).
conjugado(querer, quieren, p, na).

conjugado(tener, tiene, s, na).
conjugado(tener, tienen, p, na).
conjugado(tener, tengo, s, na).
conjugado(tener, tenemos, p, na).

conjugado(haber, han, p, na).
conjugado(haber, hay, s, na).
conjugado(haber, he, s, na).
conjugado(haber, ha, s, na).

conjugado(perder, perder, s, er).

conjugado(necesitar, necesito, s, na).
conjugado(necesitar, necesita, s, na).
conjugado(necesitar, necesitan, p, na).

conjugado(diagnosticar, diagnosticado, s, ado).
conjugado(diagnosticar, diagnostico, s, na).
conjugado(diagnosticar, diagnosticaron, p, aron).

conjugado(recomendar, recomiendo, s, na).
conjugado(recomendar, recomienda, s, na).
conjugado(recomendar, recomiendan, p, na).

conjugado(sugerir, sugiero, s, na).
conjugado(sugerir, sugiere, s, na).
conjugado(sugerir, sugieren, p, na).
conjugado(sugerir, sugerimos, p, na).

conjugado(considerar, considero, s, na).
conjugado(considerar, considera, s, na).
conjugado(considerar, consideran, p, na).

conjugado(consumir, consume, s, na).
conjugado(consumir, consumen, p, na).
conjugado(consumir, consumo, s, na).

conjugado(evitar, evita, s, na).
conjugado(evitar, evitan, p, na).
conjugado(evitar, evito, s, na).

conjugado(preferir, prefiere, s, na).
conjugado(preferir, prefieren, p, na).
conjugado(preferir, prefiero, s, na).

conjugado(controlar, controla, s, na).
conjugado(controlar, controlan, p, na).
conjugado(controlar, controlo, s, na).

conjugado(mejorar, mejora, s, na).
conjugado(mejorar, mejorar, s, ar).
conjugado(mejorar, mejoran, p, na).

conjugado(reducir, reduce, s, na).
conjugado(reducir, reducen, p, na).
conjugado(reducir, reduzco, s, na).

conjugado(aumentar, aumenta, s, na).
conjugado(aumentar, aumentan, p, na).
conjugado(aumentar, aumento, s, na).




% >> Combinaciones verbales
combinacion([han, X], Q):- conjugado(_,X,Q,ado).
combinacion([ha, X], Q):- conjugado(_,X,Q,ado).
combinacion([ha, X], Q):- conjugado(_,X,Q,ido).
combinacion([esta, X], Q):- conjugado(_,X,Q,ado).
combinacion([esta, X], Q):- conjugado(_,X,Q,iendo).
combinacion([debe, X], Q):- conjugado(_, X, Q, ar).
combinacion([puede, X], Q):- conjugado(_, X, Q, ido).
combinacion([quiere, X], Q):- conjugado(_, X, Q, ar).
combinacion([quiere, X], Q):- conjugado(_, X, Q, er).
combinacion([quiero, X], Q):- conjugado(X, X, Q, ar).
combinacion([quiero, X], Q):- conjugado(X, X, Q, er).


% >> Auxiliares de verbos
auxiliar(en). % HECHO: auxiliar(palabra) = es un auxiliar conocido
auxiliar(de).
auxiliar(con).
auxiliar(para).
auxiliar(por).
auxiliar(sobre).
auxiliar(entre).
auxiliar(durante).
auxiliar(antes).
auxiliar(despues).
auxiliar(sin).
auxiliar(desde).