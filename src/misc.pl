:- module(misc, [miembro/2, len/2, inversa/2]).

% -------------------------------[ Reglas/metodos auxiliares ]-------------------------------
miembro(X,[X|_]).
miembro(X,[_|R]):- miembro(X,R).

len(0,[]).
len(N,[_|R]):- len(L,R), N is L+1.

inversa(L1,L2):- inversa(L1,[],L2).
inversa([],L,L).
inversa([X|R], C, L):- inversa(R,[X|C],L).