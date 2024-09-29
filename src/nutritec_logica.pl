% -------------------------------[ NutriTec Lógica con BNF ]-------------------------------
% Este archivo contiene la lógica del sistema NutriTec basada en una gramática BNF.

:- consult('nutritec_base.pl').   % Se consulta el archivo de base de datos con la información de NutriTec.
:- consult('gramatica.pl').       % Se consulta el archivo que contiene las reglas gramaticales para procesar las entradas del usuario.

% Variables dinámicas para almacenar la información del usuario y los matches.
:- dynamic(info_usuario/2).       % info_usuario/2 guarda información de los usuarios.
:- dynamic(match/1).              % match/1 guarda las coincidencias de la consulta con las reglas.

% Definición de la gramática BNF actualizada.
oracion --> saludo | despedida | pregunta | objetivo | actividad | padecimiento | preferencia | dieta | calorias.
% La regla "oracion" define los posibles tipos de oraciones que el sistema reconocerá (saludo, despedida, etc.).

% Definición de saludos y despedidas.
saludo --> [hola] | [buenos, dias] | [buenas, tardes] | [buenas, noches].  % Posibles saludos.
despedida --> [gracias] | [adios] | [hasta, luego] | [chao].               % Posibles despedidas.

% Preguntas comunes.
pregunta --> [que] | [cual] | [como] | [por, que] | [cuando] | [donde] | [quien] | [cuanto] | [cuanta].
% Estas son las preguntas comunes que puede reconocer el sistema.

% Respuesta negativa.
respuesta_no --> [no].  % Define que el sistema puede identificar cuando la respuesta es "no".

% Objetivos: Verbos y sustantivos que el usuario podría usar para expresar sus metas.
objetivo --> [quiero], verbo_objetivo, sustantivo_objetivo.
verbo_objetivo --> [perder] | [ganar] | [mantener] | [mejorar] | [reducir] | [aumentar] | [controlar] | [bajar] | [subir] | [eliminar].
sustantivo_objetivo --> [peso] | [mi, salud] | [masa, muscular] | [grasa] | [el, colesterol] | [el, azucar].
% Estos son verbos y sustantivos que el usuario podría utilizar para establecer metas relacionadas con la salud.

% Niveles de actividad: El usuario puede describir su nivel de actividad física.
actividad --> nivel_actividad.
nivel_actividad --> [inicial] | [intermedio] | [avanzado] | [sedentario] | [activo] | [muy, activo].
% Los niveles de actividad reconocidos por el sistema incluyen inicial, intermedio, avanzado, etc.

% Padecimientos: Formas en las que el usuario puede expresar que tiene una enfermedad o condición.
padecimiento --> [tengo], enfermedad
             | [me, diagnosticaron], enfermedad
             | [padezco], enfermedad
             | [sufro, de], enfermedad
             | [mi, medico, dice, que, tengo], enfermedad
             | [me, han, detectado], enfermedad
             | [fui, diagnosticado], enfermedad
             | [he, sido, diagnosticado, con], enfermedad
             | [estoy, en, tratamiento, por], enfermedad.
% Estas son las formas en que un usuario puede expresar un padecimiento.

% Enfermedades comunes reconocidas por el sistema.
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
% Estas son las enfermedades que el sistema puede reconocer.

% Preferencias alimentarias: Expresiones para evitar ciertos alimentos.
preferencia --> [no, me, gusta], alimento 
             | [prefiero, no, comer], alimento 
             | [soy, alergico, a], alimento 
             | [no, puedo, comer], alimento 
             | [evito], alimento.
% Esta regla define cómo el usuario puede expresar preferencias o restricciones alimentarias.

% Alimentos comunes reconocidos por el sistema.
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
% Alimentos comunes que el sistema puede reconocer en las preferencias del usuario.

% Tipos de dietas reconocidas por el sistema.
dieta --> [quiero, una, dieta], tipo_dieta.
tipo_dieta --> [keto] 
             | [proteica] 
             | [vegetariana] 
             | [vegana] 
             | [alcalina] 
             | [baja, en, grasas] 
             | [mediterranea] 
             | [paleo].
% Estos son los tipos de dietas que el sistema puede sugerir en función de la información del usuario.

% Calorías: Cómo el usuario puede especificar la cantidad de calorías deseada.
calorias --> [N], {atom(N), atom_number(N, _)} 
           | [N, calorias], {atom(N), atom_number(N, _)} 
           | [quiero, consumir], [N], [calorias], {atom(N), atom_number(N, _)}.
% El sistema puede procesar la información de calorías cuando el usuario especifica una cantidad.

% Procesar la entrada del usuario.
procesar_entrada(Entrada) :-
    string_lower(Entrada,EntradaLower), segmentos(EntradaLower,[Palabras|_]), % Convierte la entrada a minúsculas y la divide en palabras.
    (phrase(respuesta_no, Palabras) ->  % Verifica si la entrada es "no".
        write("NutriTec: Vamos a la siguiente pregunta."), nl, 
        evaluar_recomendacion  % Llama a la evaluación para ver si ya hay suficientes respuestas.
    ; phrase(oracion, Palabras) ->  % Si no es "no", procesa la oración normalmente.
        identificar_intencion(Palabras, Intencion),
        manejar_intencion(Intencion, Palabras)
    ;
        write("NutriTec: No entendí bien tu respuesta. ¿Podrías reformularla?"), nl
    ).

% Funciones para dividir las oraciones en segmentos (no se utilizan directamente en la lógica de recomendaciones).
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
% Este predicado identifica la intención del usuario basándose en las palabras de la oración.

% Manejar la intención del usuario.
manejar_intencion(saludo, _) :-
    write("NutriTec: Hola, encantado de verte mejorar tu estilo de vida. Cuéntame, ¿en qué te puedo ayudar?"), nl.
% Responde con un saludo si la intención es un saludo.

manejar_intencion(despedida, _) :-
    write("NutriTec: Gracias por usar NutriTec. ¡Hasta luego!"), nl,
    throw(fin_de_conversacion).
% Responde con una despedida y finaliza la conversación si la intención es despedida.

manejar_intencion(objetivo, _) :-
    write("NutriTec: Excelente iniciativa. ¿Tienes alguna enfermedad o condición de salud por la que has iniciado este proceso?"), nl.
% Pregunta al usuario si tiene alguna enfermedad después de haber indicado un objetivo.

% Manejar la intención de un padecimiento.
manejar_intencion(padecimiento, Palabras) :-
    (phrase(respuesta_no, Palabras) ->  % Si la respuesta es "no".
        write("NutriTec: Vamos a la siguiente pregunta."), nl, 
        evaluar_recomendacion  % Pasa a la siguiente pregunta.
    ; findall(Padecimiento, (es_padecimiento(Padecimiento), member(Padecimiento, Palabras)), ListaPadecimientos),
      (ListaPadecimientos \= [] ->  % Si se identifica un padecimiento.
          ( \+ match(padecimiento(ListaPadecimientos)) -> 
              assertz(match(padecimiento(ListaPadecimientos)))
          ; true ),
          write("NutriTec: Entiendo. ¿Tienes pensado una cantidad específica de calorías diarias por consumir?"), nl
      ;
          write("NutriTec: No he identificado un padecimiento específico. ¿Podrías ser más claro sobre tu condición de salud?"), nl
      )
    ).
% Si el usuario menciona un padecimiento, lo agrega a las coincidencias (matches). Si no se identifica un padecimiento, pide más información.

% Manejar la intención de las calorías.
manejar_intencion(calorias, Palabras) :-
    (phrase(respuesta_no, Palabras) ->  % Si la respuesta es "no".
        write("NutriTec: Vamos a la siguiente pregunta."), nl, 
        evaluar_recomendacion  % Pasa a la siguiente pregunta.
    ; findall(Num, (member(Palabra, Palabras), atom(Palabra), atom_number(Palabra, Num)), Numeros),
      (Numeros \= [] ->  % Si se menciona una cantidad de calorías.
          [Cantidad|_] = Numeros,
          ( \+ match(calorias(Cantidad)) -> 
              assertz(match(calorias(Cantidad)))
          ; true ),
          write("NutriTec: Gracias por la información. ¿Cuál es tu nivel de actividad física (inicial, intermedio o avanzado)?"), nl
      ;
          write("NutriTec: No he captado una cantidad específica de calorías. ¿Podrías especificar cuántas calorías diarias te gustaría consumir?"), nl
      )
    ).
% Pregunta por calorías si aún no se ha mencionado una cantidad específica.

% Manejar la intención para la actividad física.
manejar_intencion(actividad, Palabras) :-
    (phrase(respuesta_no, Palabras) ->  % Si la respuesta es "no".
        write("NutriTec: Vamos a la siguiente pregunta."), nl, 
        evaluar_recomendacion  % Pasa a la siguiente pregunta.
    ; member(Nivel, Palabras),
      member(Nivel, [inicial, intermedio, avanzado, sedentario, activo, muy_activo]) ->
        ( \+ match(actividad(Nivel)) -> 
            assertz(match(actividad(Nivel)))
        ; true ),
        write("NutriTec: Gracias por la información sobre tu nivel de actividad. "), nl,
        evaluar_recomendacion
    ;
        write("NutriTec: Por favor, especifica tu nivel de actividad física como inicial, intermedio o avanzado."), nl
    ).
% Procesa el nivel de actividad física si el usuario lo menciona.

% Manejar la intención para las preferencias alimentarias.
manejar_intencion(preferencias, Palabras) :-
    (phrase(respuesta_no, Palabras) ->  % Si la respuesta es "no".
        write("NutriTec: Vamos a la siguiente pregunta."), nl, 
        evaluar_recomendacion  % Pasa a la siguiente pregunta.
    ; findall(Alimento, (alimento(Alimento), member(Alimento, Palabras)), AlimentosNoDeseados),
      ( \+ match(preferencias(AlimentosNoDeseados)) -> 
          assertz(match(preferencias(AlimentosNoDeseados)))
      ; true ),
      evaluar_recomendacion
    ).
% Procesa las preferencias alimentarias y pasa a la evaluación.

% Manejar la intención para la dieta.
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
% Si el usuario menciona un tipo de dieta, lo agrega a las coincidencias.

% Manejar el caso en que no se entiende la intención.
manejar_intencion(no_entendido, _) :-
    write("NutriTec: No entendí bien tu respuesta. ¿Podrías reformularla?"), nl.

% Funciones auxiliares que verifican los padecimientos.
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
% Estas son las condiciones de salud (padecimientos) que el sistema reconoce.

% Evaluar si hay suficiente información para una recomendación.
evaluar_recomendacion :-
    findall(X, match(X), Matches),  % Recopila todas las coincidencias.
    length(Matches, N),  % Cuenta cuántas coincidencias hay.
    (N >= 3 ->  % Si hay al menos 3 coincidencias, se recomienda una dieta.
        recomendar_dieta(Matches)
    ;
        write("NutriTec: Aún necesito más información para darte una recomendación completa. "), nl,
        write("¿Hay algo más que quieras compartir sobre tus objetivos o preferencias?"), nl
    ).
% Evalúa si ya se tiene suficiente información para hacer una recomendación de dieta.

% Recomendar una dieta basada en las coincidencias obtenidas.
recomendar_dieta(Matches) :-
    % Revisar si hay respuestas para cada campo (padecimiento, calorías, nivel de actividad).
    (member(padecimiento(Padecimientos), Matches) -> true ; Padecimientos = []),
    (member(calorias(CantidadCalorias), Matches) -> true ; CantidadCalorias = 0),
    (member(actividad(NivelActividad), Matches) -> true ; NivelActividad = none),
    (member(preferencias(AlimentosNoDeseados), Matches) -> true ; AlimentosNoDeseados = []),
    
    % Encontrar las dietas recomendadas.
    findall(Dieta, dieta_recomendada(Dieta, Padecimientos, CantidadCalorias, NivelActividad, AlimentosNoDeseados), DietasRecomendadas),
    
    % Si hay dietas recomendadas, seleccionar una aleatoriamente.
    (DietasRecomendadas \= [] ->
        random_member(DietaSeleccionada, DietasRecomendadas),
        write("NutriTec: Basado en tu información ("), write(Padecimientos), write(", "),
        (CantidadCalorias > 0 -> write(CantidadCalorias), write(" calorías, ") ; true),
        (NivelActividad \= none -> write("nivel de actividad "), write(NivelActividad), write(", ") ; true),
        write("), te recomiendo la siguiente dieta: "), nl,
        write(DietaSeleccionada), nl,
        mostrar_comidas(DietaSeleccionada), nl
    ;
        write("NutriTec: No he encontrado una dieta que coincida exactamente con tu perfil. "), nl,
        write("Te sugiero consultar con un nutricionista para un plan más personalizado."), nl
    ).
% Recomienda una dieta basada en las coincidencias del usuario o sugiere consultar con un nutricionista si no hay coincidencias suficientes.

% Definición de cómo se recomienda una dieta basada en las coincidencias.
dieta_recomendada(Dieta, Padecimientos, CantidadCalorias, NivelActividad, AlimentosNoDeseados) :-
    dieta(Dieta, _, CaloriasRecomendadas, PadecimientosRecomendados, PadecimientosNoRecomendados, NivelesActividadRecomendados, Notiene, AlimentosPermitidos),
    
    % Verificar que el padecimiento esté entre los recomendados.
    (Padecimientos = [] -> true ; member(Padecimiento, Padecimientos), member(Padecimiento, PadecimientosRecomendados)),
    
    \+ (member(Padecimiento, Padecimientos), member(Padecimiento, PadecimientosNoRecomendados)),
    
    % Verificar que las calorías estén dentro del rango permitido.
    (CantidadCalorias = 0 -> true ; (CantidadCalorias =< CaloriasRecomendadas, CantidadCalorias >= CaloriasRecomendadas - 300)),  % Se permite un margen de 300 calorías menos.
    
    % Verificar que el nivel de actividad coincida con las recomendaciones.
    (NivelActividad = none -> true ; member(NivelActividad, NivelesActividadRecomendados)),
    
    % Verificar que los alimentos no deseados no estén en la dieta recomendada.
    (AlimentosNoDeseados = [] -> true ; forall(member(AlimentoNoDeseado, AlimentosNoDeseados), member(AlimentoNoDeseado, Notiene))).

% Mostrar las comidas de la dieta seleccionada.
mostrar_comidas(Dieta) :-
    dieta(Dieta, _, _, _, _, _, _, Comidas),  % Obtener las comidas de la dieta.
    write("Las comidas de esta dieta son:"), nl,
    forall(member(Comida, Comidas), (write("- "), write(Comida), nl)).
% Esta regla muestra las comidas incluidas en la dieta recomendada.

% Leer la entrada del usuario y procesarla.
leer_entrada :- 
    write("Usuario: "),
    read_line_to_string(user_input, Entrada), 
    (Entrada \= end_of_file -> 
        procesar_entrada(Entrada), 
        leer_entrada
    ;
        write("NutriTec: Gracias por usar NutriTec. ¡Hasta luego!"), nl
    ).
% Este predicado espera la entrada del usuario y la procesa.

% Iniciar la conversación con el usuario.
iniciar_conversacion :-
    catch(
        leer_entrada,
        fin_de_conversacion,
        write("Conversación terminada. Puedes iniciar una nueva conversación escribiendo 'iniciar_conversacion.'")
    ).
% Inicia la conversación con el usuario y la controla hasta que el usuario decida finalizar.
