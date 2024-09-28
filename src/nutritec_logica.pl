% -------------------------------[ NutriTec Lógica con BNF ]-------------------------------

:- consult('nutritec_base.pl').
:- consult('gramatica.pl').

% Variables dinámicas para almacenar la información del usuario y los matches
:- dynamic(info_usuario/2).
:- dynamic(match/1).

% Definición de la gramática BNF actualizada
oracion --> saludo | despedida | pregunta | objetivo | actividad | padecimiento | preferencia | dieta | calorias.

% Saludos y despedidas
saludo --> [hola] | [buenos, dias] | [buenas, tardes] | [buenas, noches].
despedida --> [gracias] | [adios] | [hasta, luego] | [chao].

% Preguntas comunes
pregunta --> [que] | [cual] | [como] | [por, que] | [cuando] | [donde] | [quien] | [cuanto] | [cuanta].

% Objetivo: Se añaden más verbos y conjugaciones
objetivo --> [quiero], verbo_objetivo, sustantivo_objetivo.
verbo_objetivo --> [perder] | [ganar] | [mantener] | [mejorar] | [reducir] | [aumentar] | [controlar] | [bajar] | [subir] | [eliminar].
sustantivo_objetivo --> [peso] | [salud] | [masa, muscular] | [grasa] | [colesterol] | [azucar].

% Actividad: Añadiendo más opciones
actividad --> nivel_actividad.
nivel_actividad --> [inicial] | [intermedio] | [avanzado] | [sedentario] | [activo] | [muy, activo].

% Padecimiento: Expansión de las formas de identificar enfermedades o condiciones
padecimiento --> [tengo], enfermedad
             | [me, diagnosticaron], enfermedad
             | [padezco], enfermedad
             | [sufro, de], enfermedad
             | [mi, medico, dice, que, tengo], enfermedad
             | [me, han, detectado], enfermedad
             | [fui, diagnosticado], enfermedad
             | [he, sido, diagnosticado, con], enfermedad
             | [estoy, en, tratamiento, por], enfermedad.

% Enfermedades comunes
enfermedad --> [hipertension] 
            | [diabetes] 
            | [colesterol, alto] 
            | [trigliceridos, altos] 
            | [obesidad] 
            | [anemia] 
            | [intolerancia, a, la, lactosa] 
            | [enfermedad, celiaca] 
            | [epilepsia] 
            | [sindrome, metabolico] 
            | [hipotiroidismo] 
            | [hipertiroidismo] 
            | [enfermedad, cardiovascular] 
            | [gota] 
            | [dislipidemia] 
            | [enfermedad, cronica].

% Preferencias alimentarias (expandido)
preferencia --> [no, me, gusta], alimento 
             | [prefiero, no, comer], alimento 
             | [soy, alergico, a], alimento 
             | [no, puedo, comer], alimento 
             | [evito], alimento.

% Alimentos comunes
alimento --> [carne] 
           | [pescado] 
           | [lacteos] 
           | [gluten] 
           | [frutos, secos] 
           | [mariscos] 
           | [huevos] 
           | [soja] 
           | [verduras] 
           | [frutas] 
           | [legumbres].

% Dietas
dieta --> [quiero, una, dieta], tipo_dieta.
tipo_dieta --> [keto] 
             | [proteica] 
             | [vegetariana] 
             | [vegana] 
             | [alcalina] 
             | [baja, en, grasas] 
             | [mediterranea] 
             | [paleo].

% Calorías
calorias --> [N], {atom(N), atom_number(N, _)} 
           | [N, calorias], {atom(N), atom_number(N, _)} 
           | [quiero, consumir], [N], [calorias], {atom(N), atom_number(N, _)}.

% Procesar la entrada del usuario
procesar_entrada(Entrada) :-
    string_lower(Entrada,EntradaLower), segmentos(EntradaLower,[Palabras|_]),
    ( (phrase(oracion, Palabras); oracion_valida(Palabras)) ->
        identificar_intencion(Palabras, Intencion),
        manejar_intencion(Intencion, Palabras)
    ;
        write("NutriTec: No entendí bien tu respuesta. ¿Podrías reformularla?"), nl
    ).

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

% Identificar la intención del usuario
identificar_intencion(Palabras, Intencion) :-
    (phrase(saludo, Palabras) -> Intencion = saludo
    ; phrase(despedida, Palabras) -> Intencion = despedida
    ; phrase(pregunta, Palabras) -> Intencion = pregunta
    ; phrase(objetivo, Palabras) -> Intencion = objetivo
    ; phrase(actividad, Palabras) -> Intencion = actividad
    ; phrase(padecimiento, Palabras) -> Intencion = padecimiento
    ; phrase(preferencia, Palabras) -> Intencion = preferencias
    ; phrase(dieta, Palabras) -> Intencion = dieta
    ; phrase(calorias, Palabras) -> Intencion = calorias
    ; Intencion = no_entendido
    ).

% Manejar la intención del usuario
manejar_intencion(saludo, _) :-
    write("NutriTec: Hola, encantado de verte mejorar tu estilo de vida. Cuéntame, ¿en qué te puedo ayudar?"), nl.

manejar_intencion(despedida, _) :-
    write("NutriTec: Gracias por usar NutriTec. ¡Hasta luego!"), nl,
    throw(fin_de_conversacion).

manejar_intencion(objetivo, _) :-
    write("NutriTec: Excelente iniciativa. ¿Tienes alguna enfermedad o condición de salud por la que has iniciado este proceso?"), nl.

manejar_intencion(padecimiento, Palabras) :-
    findall(Padecimiento, (es_padecimiento(Padecimiento), member(Padecimiento, Palabras)), ListaPadecimientos),
    (ListaPadecimientos \= [] ->
        ( \+ match(padecimiento(ListaPadecimientos)) -> 
            assertz(match(padecimiento(ListaPadecimientos)))
        ; true ),
        write("NutriTec: Entiendo. ¿Tienes pensado una cantidad específica de calorías diarias por consumir?"), nl
    ;
        write("NutriTec: No he identificado un padecimiento específico. ¿Podrías ser más claro sobre tu condición de salud?"), nl
    ).

manejar_intencion(calorias, Palabras) :-
    findall(Num, (member(Palabra, Palabras), atom(Palabra), atom_number(Palabra, Num)), Numeros),
    (Numeros \= [] ->
        [Cantidad|_] = Numeros,
        ( \+ match(calorias(Cantidad)) -> 
            assertz(match(calorias(Cantidad)))
        ; true ),
        write("NutriTec: Gracias por la información. ¿Cuál es tu nivel de actividad física (inicial, intermedio o avanzado)?"), nl
    ;
        write("NutriTec: No he captado una cantidad específica de calorías. ¿Podrías especificar cuántas calorías diarias te gustaría consumir?"), nl
    ).

manejar_intencion(actividad, Palabras) :-
    (member(Nivel, Palabras), member(Nivel, [inicial, intermedio, avanzado, sedentario, activo, muy_activo])) ->
        ( \+ match(actividad(Nivel)) -> 
            assertz(match(actividad(Nivel)))
        ; true ),
        write("NutriTec: Gracias por la información sobre tu nivel de actividad. "), nl,
        evaluar_recomendacion
    ;
        write("NutriTec: Por favor, especifica tu nivel de actividad física como inicial, intermedio o avanzado."), nl.

manejar_intencion(preferencias, Palabras) :-
    findall(Alimento, (alimento(Alimento), member(Alimento, Palabras)), AlimentosNoDeseados),
    ( \+ match(preferencias(AlimentosNoDeseados)) -> 
        assertz(match(preferencias(AlimentosNoDeseados)))
    ; true ),
    evaluar_recomendacion.

manejar_intencion(dieta, Palabras) :-
    (member(Dieta, Palabras),
     tipo_dieta(Dieta) ->
        ( \+ match(dieta(Dieta)) -> 
            assertz(match(dieta(Dieta)))
        ; true ),
        evaluar_recomendacion
    ;
        write("NutriTec: No he identificado un tipo de dieta específico. ¿Podrías mencionar qué tipo de dieta te interesa?"), nl
    ).

manejar_intencion(no_entendido, _) :-
    write("NutriTec: No entendí bien tu respuesta. ¿Podrías reformularla?"), nl.

% Funciones auxiliares
es_padecimiento(hipertension).
es_padecimiento(diabetes).
es_padecimiento(colesterol_alto).
es_padecimiento(trigliceridos_altos).
es_padecimiento(obesidad).
es_padecimiento(anemia).
es_padecimiento(intolerancia_a_la_lactosa).
es_padecimiento(enfermedad_celiaca).
es_padecimiento(epilepsia).
es_padecimiento(sindrome_metabolico).
es_padecimiento(hipotiroidismo).
es_padecimiento(hipertiroidismo).
es_padecimiento(enfermedad_cardiovascular).
es_padecimiento(gota).
es_padecimiento(dislipidemia).
es_padecimiento(enfermedad_cronica).

% Evaluar si hay suficiente información para una recomendación
evaluar_recomendacion :-
    findall(X, match(X), Matches),
    length(Matches, N),
    (N >= 3 -> 
        recomendar_dieta(Matches)
    ;
        write("NutriTec: Aún necesito más información para darte una recomendación completa. "), nl,
        write("¿Hay algo más que quieras compartir sobre tus objetivos o preferencias?"), nl
    ).

% Recomendar una dieta basada en los datos del usuario
recomendar_dieta(Matches) :-
    member(padecimiento(Padecimientos), Matches),
    member(calorias(CantidadCalorias), Matches),
    member(actividad(NivelActividad), Matches),
    (member(preferencias(AlimentosNoDeseados), Matches) -> true ; AlimentosNoDeseados = []),
    
    findall(Dieta, dieta_recomendada(Dieta, Padecimientos, CantidadCalorias, NivelActividad, AlimentosNoDeseados), DietasRecomendadas),
    
    (DietasRecomendadas \= [] ->
        random_member(DietaSeleccionada, DietasRecomendadas),
        write("NutriTec: Basado en tu información ("), write(Padecimientos), write(", "),
        write(CantidadCalorias), write(" calorías, nivel de actividad "), write(NivelActividad),
        write("), te recomiendo la siguiente dieta: "), nl,
        write(DietaSeleccionada), nl,
        mostrar_comidas(DietaSeleccionada), nl
    ;
        write("NutriTec: No he encontrado una dieta que coincida exactamente con tu perfil. "), nl,
        write("Te sugiero consultar con un nutricionista para un plan más personalizado."), nl
    ).

% Buscar una dieta que coincida con los datos obtenidos
dieta_recomendada(Dieta, Padecimientos, CantidadCalorias, NivelActividad, AlimentosNoDeseados) :-
    dieta(Dieta, _, CaloriasRecomendadas, PadecimientosRecomendados, PadecimientosNoRecomendados, NivelesActividadRecomendados, _, AlimentosPermitidos),
    
    % Comparación corregida de padecimientos
    member(Padecimiento, Padecimientos),
    member(Padecimiento, PadecimientosRecomendados),
    
    \+ (member(Padecimiento, Padecimientos), member(Padecimiento, PadecimientosNoRecomendados)),
    
    CantidadCalorias =< CaloriasRecomendadas,
    CantidadCalorias >= CaloriasRecomendadas - 300,  % Permitimos un margen de 300 calorías menos
    
    member(NivelActividad, NivelesActividadRecomendados),
    
    \+ (member(AlimentoNoDeseado, AlimentosNoDeseados), member(AlimentoNoDeseado, AlimentosPermitidos)).

% Mostrar las comidas de una dieta
mostrar_comidas(Dieta) :-
    dieta(Dieta, _, _, _, _, _, _, Comidas),
    write("Las comidas de esta dieta son:"), nl,
    forall(member(Comida, Comidas), (write("- "), write(Comida), nl)).

% Leer entrada del usuario
leer_entrada :- 
    write("Usuario: "),
    read_line_to_string(user_input, Entrada), 
    (Entrada \= end_of_file -> 
        procesar_entrada(Entrada), 
        leer_entrada
    ;
        write("NutriTec: Gracias por usar NutriTec. ¡Hasta luego!"), nl
    ).

% Iniciar la conversación
iniciar_conversacion :-
    catch(
        leer_entrada,
        fin_de_conversacion,
        write("Conversación terminada. Puedes iniciar una nueva conversación escribiendo 'iniciar_conversacion.'")
    ).
