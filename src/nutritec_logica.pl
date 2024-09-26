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
    ; member(diagnosticado, Palabras) -> Intencion = padecimiento
    ; member(calorias, Palabras) -> Intencion = calorias
    ; member(dieta, Palabras) -> Intencion = dieta
    ; (member(no, Palabras), member(gustan, Palabras)) -> Intencion = preferencias
    ; Intencion = no_entendido
    ).

contiene_palabras_clave(Palabras, Tipo) :-
    palabra_clave(Tipo, PalabrasClave),
    intersection(Palabras, PalabrasClave, Comunes),
    Comunes \= [].

% Manejar la intención del usuario
manejar_intencion(saludo, _) :-
    write("NutriTec: Hola, encantado de verte mejorar tu estilo de vida. Cuéntame, ¿en qué te puedo ayudar?"), nl.

manejar_intencion(despedida, _) :-
    write("NutriTec: Gracias por usar NutriTec. ¡Hasta luego!"), nl,
    halt.

manejar_intencion(objetivo, _) :-
    write("NutriTec: Excelente iniciativa. ¿Tienes alguna enfermedad por la que has iniciado este proceso?"), nl.

manejar_intencion(padecimiento, Palabras) :-
    findall(Padecimiento, (member(Padecimiento, Palabras), padecimiento(Padecimiento, _, _)), ListaPadecimientos),
    (ListaPadecimientos \= [] ->
        assertz(match(padecimiento(ListaPadecimientos))),
        write("NutriTec: Entiendo. ¿Tienes pensado una cantidad específica de calorías diarias por consumir?"), nl
    ;
        write("NutriTec: No he identificado un padecimiento específico. ¿Podrías ser más claro sobre tu condición de salud?"), nl
    ).

manejar_intencion(calorias, Palabras) :-
    (member(Calorias, Palabras),
     atom_number(Calorias, Cantidad) ->
        assertz(match(calorias(Cantidad))),
        write("NutriTec: Gracias por la información. ¿Eres activo físicamente?"), nl
    ;
        write("NutriTec: No he captado una cantidad específica de calorías. ¿Podrías especificar cuántas calorías diarias te gustaría consumir?"), nl
    ).

manejar_intencion(actividad, Palabras) :-
    (identificar_nivel_actividad(Palabras, Nivel) ->
        assertz(match(actividad(Nivel))),
        write("NutriTec: Entendido. ¿Tienes un tipo de dieta que te gustaría realizar?"), nl
    ;
        write("NutriTec: No he entendido bien tu nivel de actividad física. ¿Podrías especificar cuántas veces a la semana haces ejercicio?"), nl
    ).

manejar_intencion(dieta, Palabras) :-
    (member(Dieta, Palabras),
     tipo_dieta(Dieta) ->
        assertz(match(dieta(Dieta))),
        write("NutriTec: Perfecto. ¿Qué alimentos preferirías no consumir?"), nl
    ;
        write("NutriTec: No he identificado un tipo de dieta específico. ¿Podrías mencionar qué tipo de dieta te interesa?"), nl
    ).

manejar_intencion(preferencias, Palabras) :-
    findall(Alimento, (alimento(Alimento), member(Alimento, Palabras)), AlimentosNoDeseados),
    assertz(match(preferencias(AlimentosNoDeseados))),
    evaluar_recomendacion.

manejar_intencion(no_entendido, _) :-
    write("NutriTec: No entendí bien tu respuesta. ¿Podrías reformularla?"), nl.

% Identificar nivel de actividad
identificar_nivel_actividad(Palabras, Nivel) :-
    (sub_atom_icl(Palabras, '0-2 veces') ; sub_atom_icl(Palabras, 'una vez') ; sub_atom_icl(Palabras, 'dos veces')) -> Nivel = inicial ;
    (sub_atom_icl(Palabras, '3-4 veces') ; sub_atom_icl(Palabras, 'tres veces') ; sub_atom_icl(Palabras, 'cuatro veces')) -> Nivel = intermedio ;
    (sub_atom_icl(Palabras, '5 o más veces') ; sub_atom_icl(Palabras, 'cinco veces')) -> Nivel = avanzado ;
    Nivel = desconocido.

sub_atom_icl(Palabras, SubAtom) :-
    atomic_list_concat(Palabras, ' ', Atom),
    sub_atom(Atom, _, _, _, SubAtom).

% Evaluar si hay suficiente información para una recomendación
evaluar_recomendacion :-
    findall(X, match(X), Matches),
    length(Matches, N),
    (N >= 3 ->
        recomendar_dieta(Matches)
    ;
        write("NutriTec: Aún necesito más información para darte una recomendación completa. ¿Hay algo más que quieras compartir sobre tus objetivos o preferencias?"), nl
    ).

% Iniciar el programa
iniciar_conversacion :-
    write("Usuario: "),
    leer_entrada.

leer_entrada :-
    read_line_to_string(user_input, Entrada),
    procesar_entrada(Entrada),
    leer_entrada.
