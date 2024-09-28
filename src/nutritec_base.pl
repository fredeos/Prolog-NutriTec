% -------------------------------[ Base de Datos NutriTec Actualizada ]-------------------------------

% Tipos de dietas disponibles
tipo_dieta(keto).
tipo_dieta(proteica).
tipo_dieta(vegetariana).
tipo_dieta(vegana).
tipo_dieta(alcalina).
tipo_dieta(baja_en_grasas).
tipo_dieta(mediterranea).
tipo_dieta(paleo).

% Padecimientos y sus recomendaciones dietéticas
padecimiento(dislipidemia, "Problemas del control del colesterol", baja_en_grasas).
padecimiento(hipercolesterolemia, "Aumento de los niveles normales de colesterol en la sangre", vegana).
padecimiento(diabetes, "Problemas con la regulación de la glucosa", baja_en_grasas).
padecimiento(sobrepeso, "Peso corporal por encima del promedio saludable", baja_en_grasas).
padecimiento(desnutricion, "Deficiencia grave de nutrientes", proteica).
padecimiento(hipertension, "Presión arterial alta", baja_en_grasas).
padecimiento(enfermedad_celiaca, "Intolerancia al gluten", paleo).
padecimiento(osteoporosis, "Debilidad en los huesos", alcalina).
padecimiento(enfermedad_cardiovascular, "Problemas del corazón y sistema circulatorio", mediterranea).
padecimiento(obesidad, "Exceso de grasa corporal", baja_en_grasas).
padecimiento(anemia, "Bajos niveles de hierro en la sangre", proteica).
padecimiento(intolerancia_lactosa, "Dificultad para digerir lactosa", vegana).
padecimiento(sindrome_metabolico, "Conjunto de condiciones que aumentan el riesgo de enfermedad cardiaca", mediterranea).
padecimiento(gota, "Acumulación de ácido úrico en las articulaciones", baja_en_grasas).
padecimiento(hipotiroidismo, "Glándula tiroides poco activa", baja_en_grasas).
padecimiento(hipertiroidismo, "Glándula tiroides demasiado activa", baja_en_grasas).
padecimiento(colesterol_alto, "Niveles elevados de colesterol en sangre", baja_en_grasas).
padecimiento(trigliceridos_altos, "Niveles elevados de triglicéridos en sangre", baja_en_grasas).
padecimiento(epilepsia, "Trastorno neurológico caracterizado por convulsiones", keto).
padecimiento(enfermedad_renal, "Problemas con la función renal", baja_en_grasas).
padecimiento(fibrosis_quistica, "Enfermedad genética que afecta los pulmones y el sistema digestivo", proteica).
padecimiento(cirrosis, "Enfermedad hepática crónica", mediterranea).

% Niveles de actividad física
nivel_actividad(inicial, "0-2 veces por semana").
nivel_actividad(intermedio, "3-4 veces por semana").
nivel_actividad(avanzado, "5 o más veces por semana").

% Alimentos comunes (para reconocimiento en preferencias)
alimento(carne).
alimento(pescado).
alimento(lacteos).
alimento(gluten).
alimento(frutos_secos).
alimento(mariscos).
alimento(huevos).
alimento(soja).
alimento(verduras).
alimento(frutas).
alimento(legumbres).
alimento(azucar).
alimento(sal).

% Dietas recomendadas
% dieta(Nombre, TipoDieta, Calorias, PadecimientosRecomendados, PadecimientosNoRecomendados, ActividadesRecomendadas, ActividadesNoRecomendadas, Comidas).

dieta(baja_en_grasas_1, baja_en_grasas, 1800, [sobrepeso, dislipidemia, hipertension, gota, colesterol_alto, trigliceridos_altos, enfermedad_renal, hipotiroidismo], [desnutricion], [inicial, intermedio], [avanzado],
      ["Desayuno: 1/2 taza de avena con leche descremada y frutas",
       "Almuerzo: Ensalada de pollo a la plancha con verduras variadas",
       "Cena: Pescado al horno con vegetales al vapor y una pequeña porción de arroz integral"]).

dieta(proteica_1, proteica, 2200, [desnutricion, anemia, fibrosis_quistica, sobrepeso], [hipercolesterolemia], [intermedio, avanzado], [inicial],
      ["Desayuno: Batido de proteínas con plátano y espinacas",
       "Almuerzo: Pechuga de pollo a la parrilla con quinoa y brócoli",
       "Cena: Salmón a la plancha con ensalada mixta y aguacate"]).

dieta(vegetariana_1, vegetariana, 2000, [hipercolesterolemia, hipertension, dislipidemia, sindrome_metabolico], [desnutricion], [inicial, intermedio], [avanzado],
      ["Desayuno: Yogur de soja con granola y frutas del bosque",
       "Almuerzo: Ensalada de garbanzos, tomate, pepino y aceitunas",
       "Cena: Curry de lentejas con arroz integral y vegetales salteados"]).

dieta(keto_1, keto, 1900, [diabetes, sobrepeso, epilepsia], [hipercolesterolemia], [intermedio, avanzado], [inicial],
      ["Desayuno: Huevos revueltos con aguacate y espinacas",
       "Almuerzo: Ensalada de atún con mayonesa, apio y nueces",
       "Cena: Filete de ternera con mantequilla de hierbas y espárragos a la parrilla"]).

dieta(mediterranea_1, mediterranea, 2100, [hipertension, dislipidemia, enfermedad_cardiovascular, sindrome_metabolico, cirrosis], [], [inicial, intermedio, avanzado], [],
      ["Desayuno: Tostada de pan integral con tomate, aceite de oliva y jamón serrano",
       "Almuerzo: Ensalada griega con feta, aceitunas y un poco de pasta integral",
       "Cena: Pescado a la plancha con verduras asadas y una copa de vino tinto"]).

dieta(vegana_1, vegana, 1800, [hipercolesterolemia, intolerancia_lactosa], [desnutricion], [inicial, intermedio], [avanzado],
      ["Desayuno: Batido verde con espinacas, plátano, chía y leche de almendras",
       "Almuerzo: Bowl de quinoa con garbanzos, aguacate y vegetales asados",
       "Cena: Curry de lentejas y verduras con arroz integral"]).

dieta(paleo_1, paleo, 2000, [enfermedad_celiaca, diabetes], [osteoporosis], [intermedio, avanzado], [inicial],
      ["Desayuno: Tortilla de huevos con espinacas y champiñones",
       "Almuerzo: Pollo a la parrilla con batata asada y ensalada verde",
       "Cena: Salmón al horno con brócoli y calabacín a la parrilla"]).

dieta(alcalina_1, alcalina, 1700, [osteoporosis], [], [inicial, intermedio], [avanzado],
      ["Desayuno: Batido de espinacas, pera y semillas de cáñamo",
       "Almuerzo: Ensalada de quinoa con aguacate, pepino y brotes de alfalfa",
       "Cena: Sopa de verduras con tofu y una porción de mijo"]).

dieta(baja_en_grasas_2, baja_en_grasas, 2000, [sobrepeso, diabetes, hipertension, trigliceridos_altos], [desnutricion], [intermedio, avanzado], [inicial],
      ["Desayuno: Tostadas integrales con queso cottage y rodajas de tomate",
       "Almuerzo: Wrap de pavo con vegetales y hummus",
       "Cena: Filete de pescado a la plancha con puré de coliflor y ensalada verde"]).

dieta(proteica_2, proteica, 2400, [desnutricion, sobrepeso, anemia], [hipercolesterolemia], [avanzado], [inicial, intermedio],
      ["Desayuno: Omelette de claras con espinacas y champiñones",
       "Almuerzo: Pechuga de pavo a la plancha con arroz integral y judías verdes",
       "Cena: Filete de ternera magra con batata asada y brócoli al vapor"]).

dieta(vegetariana_2, vegetariana, 1800, [hipertension, dislipidemia], [desnutricion], [inicial, intermedio], [avanzado],
      ["Desayuno: Tostada de pan integral con aguacate y tomate",
       "Almuerzo: Ensalada de lentejas con pimiento, cebolla y zanahorias",
       "Cena: Hamburguesa de garbanzos con pan integral y ensalada mixta"]).

dieta(keto_2, keto, 2100, [epilepsia, diabetes], [hipercolesterolemia], [intermedio, avanzado], [inicial],
      ["Desayuno: Café con mantequilla y aceite de coco (Bulletproof coffee)",
       "Almuerzo: Ensalada César con pollo y aderezo alto en grasas",
       "Cena: Salmón al horno con espárragos envueltos en bacon"]).

dieta(mediterranea_2, mediterranea, 1900, [enfermedad_cardiovascular, diabetes], [], [inicial, intermedio], [avanzado],
      ["Desayuno: Yogur griego con nueces y miel",
       "Almuerzo: Ensalada de atún con tomate, cebolla y aceite de oliva",
       "Cena: Pollo al limón con orégano, acompañado de verduras asadas"]).

dieta(vegana_2, vegana, 2000, [hipercolesterolemia, sobrepeso, intolerancia_lactosa], [desnutricion], [intermedio, avanzado], [inicial],
      ["Desayuno: Tostadas de pan integral con hummus y rodajas de aguacate",
       "Almuerzo: Ensalada de lentejas con tomate, pepino y vinagreta de mostaza",
       "Cena: Curry de garbanzos y espinacas con arroz basmati integral"]).

dieta(paleo_2, paleo, 2200, [enfermedad_celiaca, diabetes], [osteoporosis], [avanzado], [inicial, intermedio],
      ["Desayuno: Batido de coco con frutos rojos y proteína en polvo",
       "Almuerzo: Ensalada de pollo con aguacate, nueces y aderezo de limón",
       "Cena: Estofado de ternera con verduras de raíz"]).
