% -------------------------------[ NutriTec Lógica ]-------------------------------
:- consult('nutritec_base.pl').

% Variables dinámicas para almacenar la información del usuario y los matches
:- dynamic(info_usuario/2).
:- dynamic(match/1).

% Palabras clave para identificar intenciones
palabra_clave(saludo, [hola, buenos, dias, tardes, noches]).
palabra_clave(despedida, [gracias, adios, hasta, luego]).
palabra_clave(afirmacion, [si, claro, por, supuesto, afirmativo]).
palabra_clave(negacion, [no, negativo, para, nada]).
palabra_clave(objetivo, [peso, normal, saludable, estilo, vida, mejorar, salud, perder, adelgazar]).
palabra_clave(actividad, [ejercicio, deporte, gimnasio, correr, nadar, ciclismo]).
palabra_clave(pregunta, [que, cual, como, por, que, cuando, donde, quien]).

% Procesar la entrada del usuario
procesar_entrada(Entrada) :-
    downcase_atom(Entrada, EntradaLower),
    atomic_list_concat(Palabras, ' ', EntradaLower),
    identificar_intencion(Palabras, Intencion),
    manejar_intencion(Intencion, Palabras).

% Identificar la intención del usuario
identificar_intencion(Palabras, Intencion) :-
    (contiene_palabras_clave(Palabras, saludo) -> Intencion = saludo
    ; contiene_palabras_clave(Palabras, despedida) -> Intencion = despedida
    ; contiene_palabras_clave(Palabras, pregunta) -> Intencion = pregunta
    ; contiene_palabras_clave(Palabras, objetivo) -> Intencion = objetivo
    ; contiene_palabras_clave(Palabras, actividad) -> Intencion = actividad
    ; (member(diagnosticado, Palabras); member(hipertension, Palabras)) -> Intencion = padecimiento
    ; (member(calorias, Palabras); detectar_numero_calorias(Palabras)) -> Intencion = calorias
    ; member(dieta, Palabras) -> Intencion = dieta
    ; (member(no, Palabras), member(gustan, Palabras)) -> Intencion = preferencias
    ; Intencion = no_entendido
    ).

contiene_palabras_clave(Palabras, Tipo) :-
    palabra_clave(Tipo, PalabrasClave),
    intersection(Palabras, PalabrasClave, Comunes),
    Comunes \= [].

% Detectar calorías como número y palabra clave
detectar_numero_calorias(Palabras) :-
    member(Palabra, Palabras),
    atom_number(Palabra, _).

% Manejar la intención de calorías
manejar_intencion(calorias, Palabras) :-
    (detectar_numero_calorias(Palabras) ->
        findall(Num, (member(Palabra, Palabras), atom_number(Palabra, Num)), [Cantidad|_]),
        assertz(match(calorias(Cantidad))),
        write("NutriTec: Gracias por la información. ¿Cuál es tu nivel de actividad física (inicial, intermedio o avanzado)?"), nl
    ;
        write("NutriTec: No he captado una cantidad específica de calorías. ¿Podrías especificar cuántas calorías diarias te gustaría consumir?"), nl
    ).

% Manejar la intención del usuario
manejar_intencion(saludo, _) :-
    write("NutriTec: Hola, encantado de verte mejorar tu estilo de vida. Cuéntame, ¿en qué te puedo ayudar?"), nl.

manejar_intencion(despedida, _) :-
    write("NutriTec: Gracias por usar NutriTec. ¡Hasta luego!"), nl,
    throw(fin_de_conversacion).

manejar_intencion(objetivo, _) :-
    write("NutriTec: Excelente iniciativa. ¿Tienes alguna enfermedad por la que has iniciado este proceso?"), nl.

% Manejar la intención de padecimiento
manejar_intencion(padecimiento, Palabras) :-
    findall(Padecimiento, (padecimiento(Padecimiento, _, _), member(Padecimiento, Palabras)), ListaPadecimientos),
    (ListaPadecimientos \= [] ->
        assertz(match(padecimiento(ListaPadecimientos))),
        write("NutriTec: Entiendo. ¿Tienes pensado una cantidad específica de calorías diarias por consumir?"), nl
    ;
        write("NutriTec: No he identificado un padecimiento específico. ¿Podrías ser más claro sobre tu condición de salud?"), nl
    ).

% Manejar la intención de actividad
manejar_intencion(actividad, Palabras) :-
    identificar_nivel_actividad(Palabras, Nivel),
    (Nivel \= desconocido -> 
        assertz(match(actividad(Nivel))),
        write("NutriTec: Gracias por la información sobre tu nivel de actividad. "), nl,
        evaluar_recomendacion
    ;
        write("NutriTec: No he entendido bien tu nivel de actividad física. "), nl,
        write("Por favor, especifica si es inicial (0-2 veces por semana), "), nl,
        write("intermedio (3-4 veces por semana) o avanzado (5 o más veces por semana)."), nl
    ).

% Identificar nivel de actividad
identificar_nivel_actividad(Palabras, Nivel) :-
    (member(inicial, Palabras) ; sub_atom_icl(Palabras, '0-2') ; sub_atom_icl(Palabras, 'una vez') ; sub_atom_icl(Palabras, 'dos veces')) -> Nivel = inicial ;
    (member(intermedio, Palabras) ; sub_atom_icl(Palabras, '3-4') ; sub_atom_icl(Palabras, 'tres veces') ; sub_atom_icl(Palabras, 'cuatro veces')) -> Nivel = intermedio ;
    (member(avanzado, Palabras) ; sub_atom_icl(Palabras, '5') ; sub_atom_icl(Palabras, 'cinco') ; sub_atom_icl(Palabras, 'mas')) -> Nivel = avanzado ;
    Nivel = desconocido.

sub_atom_icl(Palabras, SubAtom) :-
    atomic_list_concat(Palabras, ' ', Atom),
    sub_atom(Atom, _, _, _, SubAtom).

% Manejar la intención de dieta
manejar_intencion(dieta, Palabras) :-
    (member(Dieta, Palabras),
     tipo_dieta(Dieta) ->
        assertz(match(dieta(Dieta))),
        evaluar_recomendacion
    ;
        write("NutriTec: No he identificado un tipo de dieta específico. ¿Podrías mencionar qué tipo de dieta te interesa?"), nl
    ).

% Manejar la intención de preferencias alimentarias
manejar_intencion(preferencias, Palabras) :-
    findall(Alimento, (alimento(Alimento), member(Alimento, Palabras)), AlimentosNoDeseados),
    assertz(match(preferencias(AlimentosNoDeseados))),
    evaluar_recomendacion.

manejar_intencion(no_entendido, _) :-
    write("NutriTec: No entendí bien tu respuesta. ¿Podrías reformularla?"), nl.

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
    
    intersection(Padecimientos, PadecimientosRecomendados, PadecimientosComunes),
    PadecimientosComunes \= [],
    
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
