% Hechos: En este es el vocabulario que conoce el programa
/*
Los 'determinantes' estan definidos como (palabra,genero) esto permite que los valores de determinante y
un nombre tengan el mismo genero para que tengan sentido
*/
determinante(el, m).
determinante(la, f).

/*
Los 'nombres' estan definindos como (nombre,genero) para poder conectarla aun determinante del
mismo genero
*/
nombre(hombre, m).
nombre(manzana, f).
nombre(casa, f).

verbo(comer).
verbo(estar).
verbo(V):- conjugado(_,V).

conjugado(comer, come).

auxiliar(en).

% Reglas: Construccion de sintagmas y oraciones para el programa
sintagma_nominal(D,N):- determinante(D,G), nombre(N,G).
sintagma_verbal(V,D,N):- verbo(V), sintagma_nominal(D,N).
oracion(A,B,C,D,E):- sintagma_nominal(A,B), sintagma_verbal(C,D,E), B\==E.