% Hechos: En este es el vocabulario que conoce el programa

% /////////// Palabras unicas
determinante(el).
determinante(la).

nombre(hombre).
nombre(mujer).
nombre(persona).

verbo(come).
verbo(necesita).
verbo(huye).
verbo(tiene).


% ////////// Casos particulares
sintagma_nominal(yo).


% Reglas: Construccion de sintagmas y oraciones
sintagma_nominal(D,N):- determinante(D), nombre(N).
