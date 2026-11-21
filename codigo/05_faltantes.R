#===============================================#
# MAESTRÍA EN CIENCIA DE DATOS - UNIV. AUSTRAL  #
#                                               #
# Materia: ANÁLISIS INTELIGENTE DE DATOS (2026) #
#                                               #
# Trabajo Final:                                #
#   "¿Qué hace que una oportunidad se gane?"    #
#===============================================#

#===============================================#
# ETAPA III. TRATAMIENTO DE LOS FALTANTES       #
#===============================================#
# Sigue las reglas prácticas vistas en clase:   #
#   1. Separar los faltantes estructurales (el  #
#      dato no corresponde) de los imputables.  #
#   2. Formular una hipótesis sobre el          #
#      mecanismo (MCAR / MAR / MNAR).           #
#   3. Comparar métodos de imputación contra    #
#      un baseline simple con una métrica común #
#      e informar supuestos y limitaciones.     #
#                                               #
# Se ejecuta a continuación de                  #
# 04_consolidacion.R (usa el objeto `crm`).     #
#===============================================#

#-----------------------------------------------#
# 1. PREÁMBULO + FUNCIONES                      #
#-----------------------------------------------#

source("codigo/00_preambulo_funciones.R", encoding = "UTF-8")
if (!exists("crm")) source("codigo/04_consolidacion.R", encoding = "UTF-8")


#-----------------------------------------------#
# 2. CARGA DE DATOS                             #
#-----------------------------------------------#

# Variables tal como llegaron, antes de recodificar los faltantes. El monto
# igual a 0 se toma como faltante (dato no cargado, ver 01_limpieza_leads.R)
datos_faltantes <- read_csv("datos_crudos/leads.csv", show_col_types = FALSE) |>
  select(!starts_with("_")) |>
  mutate(expected_revenue = na_if(expected_revenue, 0)) |>
  left_join(
    select(crm, gen_id, known_since, odoo_pls_default, odoo_pls_extended,
           automl_12_fields, automl_20_fields),
    by = "gen_id"
  )


#-----------------------------------------------#
# 3. DIAGNÓSTICO: CUÁNTOS FALTAN Y DÓNDE        #
#-----------------------------------------------#

# 3.1. Resúmenes numéricos con naniar:
if (mostrar_controles) {
  cat("Proporción de casos completos:", prop_complete_case(datos_faltantes), "\n")
  cat("Proporción de celdas faltantes:", prop_miss(datos_faltantes), "\n")
  cat("Proporción de variables con algún faltante:",
      prop_miss_var(datos_faltantes), "\n")
}

resumen_faltantes <- miss_var_summary(datos_faltantes) |>
  filter(n_miss > 0) |>
  mutate(prop_na = as.numeric(pct_miss) / 100)

if (mostrar_controles) print(resumen_faltantes, n = Inf)


#-----------------------------------------------#
# 4. FALTANTES ESTRUCTURALES                    #
#    Regla práctica 1: antes de cualquier otra  #
#    cosa, distinguir los datos que no          #
#    corresponden de los que no se cargaron     #
#-----------------------------------------------#

# 4.1. Verificación de cada regla con los datos (✔ = la regla se cumple en
#      todos los registros):

informar("Faltantes estructurales:\n")

paises_con_provincia <- datos_faltantes |>
  filter(!is.na(state_code)) |>
  distinct(country_code) |>
  pull()

reglas <- tribble(
  ~variable, ~regla, ~inconsistencias,

  "user_login", "el lead todavía no fue asignado (date_open vacía)",
  verificar_estructural(datos_faltantes, "user_login", is.na(date_open),
                        "el lead no fue asignado (date_open vacía)"),

  "date_open", "el lead todavía no fue asignado (user_login vacío)",
  verificar_estructural(datos_faltantes, "date_open", is.na(user_login),
                        "el lead no fue asignado (user_login vacío)"),

  "date_closed", "la oportunidad sigue abierta",
  verificar_estructural(datos_faltantes, "date_closed", outcome == "open",
                        "la oportunidad sigue abierta"),

  "date_qualified", "el lead no pasó de la etapa New",
  verificar_estructural(datos_faltantes, "date_qualified", stage == "New",
                        "el lead no pasó de la etapa New"),

  "date_proposition", "el lead no llegó a la etapa Proposition",
  verificar_estructural(datos_faltantes, "date_proposition",
                        stage %in% c("New", "Qualified"),
                        "el lead no llegó a la etapa Proposition"),

  "date_conversion", "el registro no se convirtió de lead a oportunidad",
  verificar_estructural(datos_faltantes, "date_conversion",
                        !(type_initial == "lead" & type == "opportunity"),
                        "el registro no se convirtió de lead a oportunidad"),

  "recurring_plan", "la venta no tiene ingreso recurrente",
  verificar_estructural(datos_faltantes, "recurring_plan",
                        recurring_revenue == 0,
                        "la venta no tiene ingreso recurrente"),

  "odoo_pls_default", "el lead es del conjunto de entrenamiento",
  verificar_estructural(datos_faltantes, "odoo_pls_default", split == "train",
                        "el lead es del conjunto de entrenamiento"),

  "odoo_pls_extended", "el lead es del conjunto de entrenamiento",
  verificar_estructural(datos_faltantes, "odoo_pls_extended",
                        split == "train",
                        "el lead es del conjunto de entrenamiento"),

  "automl_12_fields", "el lead no es del conjunto de prueba",
  verificar_estructural(datos_faltantes, "automl_12_fields", split != "test",
                        "el lead no es del conjunto de prueba"),

  "automl_20_fields", "el lead no es del conjunto de prueba",
  verificar_estructural(datos_faltantes, "automl_20_fields", split != "test",
                        "el lead no es del conjunto de prueba")
)

# 4.2. Variables con faltantes de los dos tipos: se verifica la parte
#      estructural y se cuenta la parte que corresponde a datos no cargados

# lost_reason: no corresponde en las ganadas y abiertas
motivo_en_no_perdidas <- sum(
  !is.na(datos_faltantes$lost_reason) & datos_faltantes$outcome != "lost"
)
motivo_no_cargado <- sum(
  is.na(datos_faltantes$lost_reason) & datos_faltantes$outcome == "lost"
)
informar(if (motivo_en_no_perdidas == 0) "  ✔ " else "  ✘ ",
         "lost_reason falta en todas las oportunidades ganadas y abiertas; ",
         "además falta en ", motivo_no_cargado, " perdidas (dato no cargado)\n")

# state_code: en algunos países nunca se carga la provincia
provincia_fuera <- sum(
  !is.na(datos_faltantes$state_code) &
    !datos_faltantes$country_code %in% paises_con_provincia
)
provincia_no_cargada <- sum(
  is.na(datos_faltantes$state_code) &
    datos_faltantes$country_code %in% paises_con_provincia
)
informar(if (provincia_fuera == 0) "  ✔ " else "  ✘ ",
         "state_code solo se carga en ", paste(paises_con_provincia,
                                               collapse = ", "),
         "; en esos países falta en ", provincia_no_cargada,
         " leads (dato no cargado)\n")

# known_since: no corresponde si el lead no tiene cliente asociado
antiguedad_sin_cliente <- sum(
  !is.na(datos_faltantes$known_since) & is.na(datos_faltantes$gen_partner_id)
)
antiguedad_no_cargada <- sum(
  is.na(datos_faltantes$known_since) & !is.na(datos_faltantes$gen_partner_id)
)
informar(if (antiguedad_sin_cliente == 0) "  ✔ " else "  ✘ ",
         "known_since falta en todos los leads sin cliente; además falta en ",
         antiguedad_no_cargada, " leads con cliente (dato no cargado)\n")


# 4.3. Diccionario de faltantes: tipo y tratamiento de cada variable
#      (se muestra en la pestaña "Datos" de la app)

tratamientos <- tribble(
  ~variable, ~tipo, ~tratamiento,
  "gen_partner_id", "Estructural",
  "El lead es de un contacto nuevo: define el tipo de cliente",
  "user_login", "Estructural", "Categoría \"Sin asignar\"",
  "date_open", "Estructural", "Se mantiene vacía",
  "date_closed", "Estructural",
  "Se mantiene vacía; las abiertas se excluyen de las tasas de éxito",
  "date_qualified", "Estructural",
  "Se mantiene vacía; define las etapas alcanzadas del embudo",
  "date_proposition", "Estructural",
  "Se mantiene vacía; define las etapas alcanzadas del embudo",
  "date_conversion", "Estructural", "Se mantiene vacía",
  "recurring_plan", "Estructural", "Se mantiene vacía",
  "odoo_pls_default", "Estructural", "Se evalúa solo en el conjunto de prueba",
  "odoo_pls_extended", "Estructural", "Se evalúa solo en el conjunto de prueba",
  "automl_12_fields", "Estructural", "Se evalúa solo en el conjunto de prueba",
  "automl_20_fields", "Estructural", "Se evalúa solo en el conjunto de prueba",
  "lost_reason", "Mixto",
  "\"No corresponde\" en ganadas y abiertas; \"Sin motivo registrado\" en perdidas",
  "state_code", "Mixto", "No se usa en el análisis",
  "expected_revenue", "No estructural",
  "Se conserva vacío; para los totales se imputa con la media del grupo",
  "function", "No estructural",
  "Nivel de cargo \"Sin dato\" (el faltante se asocia al resultado)",
  "source", "No estructural",
  "Categoría \"Sin dato\" (el faltante se asocia al resultado)",
  "country_code", "No estructural",
  "Se recupera desde la ciudad cuando está cargada; si no, \"Sin dato\"",
  "email_state", "No estructural", "Calidad de contacto \"Incompleto\"",
  "phone_state", "No estructural", "Calidad de contacto \"Incompleto\"",
  "tags", "No estructural", "Etiqueta \"Sin etiqueta\"",
  "city", "No estructural", "No se usa en el análisis",
  "campaign", "No estructural", "No se usa en el análisis",
  "date_deadline", "No estructural", "No se usa en el análisis",
  "known_since", "Mixto", "No se usa en el análisis"
)

diccionario_faltantes <- resumen_faltantes |>
  select(variable, n_miss, prop_na) |>
  left_join(tratamientos, by = "variable") |>
  left_join(select(reglas, variable, regla, inconsistencias), by = "variable") |>
  mutate(
    regla = case_when(
      variable == "gen_partner_id" ~ "el lead no tiene un cliente asociado",
      variable == "lost_reason" ~
        "la oportunidad no se perdió (o se perdió sin cargar el motivo)",
      variable == "state_code" ~
        "el país no usa provincias (o no se cargó)",
      variable == "known_since" ~
        "el lead no tiene un cliente asociado (o no se cargó)",
      TRUE ~ regla
    ),
    verificada = case_when(
      variable == "lost_reason" ~ motivo_en_no_perdidas == 0,
      variable == "state_code" ~ provincia_fuera == 0,
      variable == "known_since" ~ antiguedad_sin_cliente == 0,
      !is.na(inconsistencias) ~ inconsistencias == 0,
      TRUE ~ NA
    )
  ) |>
  select(variable, n_miss, prop_na, tipo, regla, verificada, tratamiento)

# Todas las variables con faltantes tienen que tener un tratamiento asignado
sin_tratamiento <- setdiff(resumen_faltantes$variable, tratamientos$variable)
informar(if (length(sin_tratamiento) == 0) "  ✔ " else "  ✘ ",
         "Variables con faltantes sin tratamiento asignado: ",
         length(sin_tratamiento), "\n")


#-----------------------------------------------#
# 5. MECANISMO DE LOS FALTANTES NO              #
#    ESTRUCTURALES                              #
#    Si fueran MCAR, la tasa de éxito sería la  #
#    misma con o sin el dato                    #
#-----------------------------------------------#

# 5.1. Tasa de éxito según falte o no cada dato (oportunidades cerradas, sin
#      duplicados), con intervalo de confianza del 95 % y test de proporciones

variables_mecanismo <- c(
  "Monto esperado" = "expected_revenue",
  "Cargo del contacto" = "function",
  "Origen" = "source",
  "País" = "country_code",
  "Email" = "email_state",
  "Teléfono" = "phone_state",
  "Etiquetas" = "tags",
  "Vendedor (estructural)" = "user_login"
)

cerradas_faltantes <- datos_faltantes |>
  filter(outcome != "open", !lost_reason %in% "Duplicate") |>
  mutate(ganada = outcome == "won")

tasa_segun_faltante <- imap_dfr(variables_mecanismo, \(variable, etiqueta) {
  falta <- is.na(cerradas_faltantes[[variable]])
  ganada <- cerradas_faltantes$ganada

  cerradas_faltantes |>
    mutate(dato = if_else(falta, "Falta el dato", "Dato cargado")) |>
    group_by(dato) |>
    summarise(n = n(), ganadas = sum(ganada), .groups = "drop") |>
    mutate(
      variable = etiqueta,
      tasa = ganadas / n,
      error = 1.96 * sqrt(tasa * (1 - tasa) / n),
      li = pmax(tasa - error, 0),
      ls = pmin(tasa + error, 1),
      p_valor = prop.test(table(falta, ganada))$p.value
    )
})

if (mostrar_controles) print(tasa_segun_faltante)

# 5.2. ¿De qué depende que falte el monto? Modelo logístico con variables
#      observadas (no se incluye el resultado, que es lo que se quiere explicar)

crm <- crm |>
  mutate(grupo_monto = paste(team, "·", type))

modelo_falta_monto <- glm(
  is.na(expected_revenue) ~ grupo_monto + source + vendedor + prioridad,
  data = crm,
  family = binomial()
)

mecanismo_monto <- anova(modelo_falta_monto, test = "Chisq") |>
  as_tibble(rownames = "variable") |>
  filter(variable != "NULL") |>
  transmute(
    variable = recode(
      variable,
      grupo_monto = "Equipo y tipo de registro",
      source = "Origen",
      vendedor = "Vendedor",
      prioridad = "Prioridad"
    ),
    gl = Df,
    deviance = Deviance,
    p_valor = `Pr(>Chi)`
  )

if (mostrar_controles) print(mecanismo_monto)

# Conclusión: la ausencia se asocia con variables observadas (no es MCAR).
# Es compatible con MAR, aunque no se puede descartar MNAR: el vendedor podría
# no cargar el monto justamente en las oportunidades de menor valor.


#-----------------------------------------------#
# 6. COMPARACIÓN DE MÉTODOS DE IMPUTACIÓN       #
#    PARA EL MONTO                              #
#    Regla práctica 2: comparar contra un       #
#    baseline simple con una métrica común      #
#-----------------------------------------------#

# 6.1. Datos para la comparación: los niveles poco frecuentes entre los
#      montos observados se agrupan en "Otros" para que el modelo sea estable
montos <- crm |>
  transmute(
    monto = expected_revenue,
    grupo_monto, source, medium, vendedor, prioridad, nivel_cargo,
    tipo_cliente,
    pais = country_code,
    recurrente = recurring_revenue > 0,
    p_falta = predict(modelo_falta_monto, type = "response")
  ) |>
  mutate(across(
    c(source, medium, vendedor, pais, tipo_cliente, nivel_cargo),
    \(x) fct_lump_min(factor(x), 60, w = as.numeric(!is.na(monto)),
                      other_level = "Otros")
  ))

montos_observados <- filter(montos, !is.na(monto))

# 6.2. Se ocultan montos conocidos y se imputan con cada método. Para imitar
#      el mecanismo real (MAR), los montos a ocultar se eligen con probabilidad
#      proporcional a la probabilidad estimada de faltar (punto 5.2).
#      Métricas:
#        - MAPE: error porcentual medio en cada monto individual
#        - Error en el total: diferencia relativa entre la suma imputada y la
#          suma real de los montos ocultos (es lo que importa para la app,
#          que usa el monto en totales y forecasts). Se resume con su
#          promedio (sesgo) y con su promedio en valor absoluto (error típico
#          en una aplicación concreta)
#        - Dispersión conservada: desvío del log del monto imputado sobre el
#          desvío del log del monto real (1 = conserva la variabilidad)

n_repeticiones <- 100
prop_ocultos <- 0.2

set.seed(2026)
resultados_imputacion <- map_dfr(seq_len(n_repeticiones), \(repeticion) {
  ocultos <- sample(
    nrow(montos_observados),
    round(prop_ocultos * nrow(montos_observados)),
    prob = montos_observados$p_falta
  )
  train <- montos_observados[-ocultos, ]
  test <- montos_observados[ocultos, ]

  imap_dfr(metodos_imputacion, \(metodo, nombre) {
    imputado <- metodo(train, test)
    tibble(
      metodo = nombre,
      mape = mean(abs((test$monto - imputado) / test$monto)),
      error_total = sum(imputado) / sum(test$monto) - 1,
      dispersion = sd(log(imputado)) / sd(log(test$monto))
    )
  })
})

comparacion_imputacion <- resultados_imputacion |>
  group_by(metodo) |>
  summarise(
    mape = mean(mape),
    error_total_abs = mean(abs(error_total)),
    error_total = mean(error_total),
    dispersion = mean(dispersion)
  ) |>
  arrange(error_total_abs)

if (mostrar_controles) print(comparacion_imputacion)

# Conclusión:
#   - Ningún método predice bien cada monto individual (MAPE de 100 % o más):
#     el monto casi no se relaciona con las demás variables observadas.
#   - Las medianas subestiman los totales en más de un 50 %, porque la
#     distribución del monto es muy asimétrica (media >> mediana).
#   - Las medias estiman bien el total (error típico del 7 % al 8 %). La media
#     por grupo (equipo y tipo de registro) lo hace prácticamente sin sesgo,
#     respeta la variable de la que más depende el faltante (que también
#     cambia el monto medio) y es simple de explicar. La regresión no la
#     mejora y el valor al azar agrega ruido.
#   - Ninguna imputación conserva la variabilidad salvo el valor al azar, por
#     eso los montos imputados no se usan para describir la distribución.


metodo_elegido <- "Media por grupo"


#-----------------------------------------------#
# 7. IMPUTACIÓN APLICADA                        #
#    Regla práctica 3: el monto original queda  #
#    sin imputar y se marca qué valores se      #
#    imputaron                                  #
#-----------------------------------------------#

# expected_revenue      monto cargado (vacío si no se cargó). Se usa para
#                       describir la distribución del monto.
# expected_revenue_imp  monto cargado o, si falta, media del grupo. Se usa
#                       solo para totales (ingresos ganados y forecast).
# monto_imputado        TRUE si el monto fue imputado.

crm <- crm |>
  group_by(grupo_monto) |>
  mutate(
    monto_imputado = is.na(expected_revenue),
    expected_revenue_imp = coalesce(
      expected_revenue,
      mean(expected_revenue, na.rm = TRUE)
    )
  ) |>
  ungroup()


# 7.2. País recuperado desde la ciudad. Cada ciudad corresponde a un único
#      país, según los leads que tienen ambos datos y según la geocodificación
#      de datos_crudos/ciudades_coordenadas.csv (ver geocodificar_ciudades.R).
#      Es una imputación determinística: no se estima, se deduce.

ciudades_coordenadas <- read_csv(
  "datos_crudos/ciudades_coordenadas.csv",
  show_col_types = FALSE
)

# Validación: el país de la geocodificación coincide con el cargado en el lead
pais_inconsistente <- crm |>
  filter(country_code != "Sin dato", !is.na(city)) |>
  left_join(ciudades_coordenadas, by = "city", suffix = c("", "_ciudad")) |>
  summarise(n = sum(country_code != country_code_ciudad, na.rm = TRUE)) |>
  pull(n)

registrar_chequeo(
  "Leads cuyo país no coincide con el de su ciudad",
  pais_inconsistente,
  "Ninguna: cada ciudad corresponde a un único país"
)

# pais_recuperado  TRUE si el país se dedujo de la ciudad
crm <- crm |>
  left_join(
    select(ciudades_coordenadas, city, pais_ciudad = country_code),
    by = "city"
  ) |>
  mutate(
    pais_recuperado = country_code == "Sin dato" & !is.na(pais_ciudad),
    country_code = if_else(pais_recuperado, pais_ciudad, country_code)
  ) |>
  select(!pais_ciudad)

registrar_chequeo(
  "Leads sin país que tienen ciudad cargada",
  sum(crm$pais_recuperado),
  "Se recupera el país desde la ciudad"
)


#-----------------------------------------------#
# 8. CONTROL POST-IMPUTACIÓN                    #
#-----------------------------------------------#

informar("\nImputación del monto:\n")
informar(if (!anyNA(crm$expected_revenue_imp)) "  ✔ " else "  ✘ ",
         sum(is.na(crm$expected_revenue_imp)),
         " montos sin valor después de imputar\n")
informar("  ", sum(crm$monto_imputado), " montos imputados (",
         round(100 * mean(crm$monto_imputado), 1), " %)\n")

informar("\nPaís recuperado desde la ciudad:\n")
informar(if (pais_inconsistente == 0) "  ✔ " else "  ✘ ", pais_inconsistente,
         " leads con un país distinto al de su ciudad\n")
informar("  ", sum(crm$pais_recuperado), " países recuperados; quedan ",
         sum(crm$country_code == "Sin dato"), " leads sin país\n")

if (mostrar_controles) {
  crm |>
    group_by(grupo_monto) |>
    summarise(
      imputados = sum(monto_imputado),
      media_observada = mean(expected_revenue, na.rm = TRUE),
      media_imputada = mean(expected_revenue_imp[monto_imputado]),
      .groups = "drop"
    ) |>
    print()
}
