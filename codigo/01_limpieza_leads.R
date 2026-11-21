#===============================================#
# MAESTRÍA EN CIENCIA DE DATOS - UNIV. AUSTRAL  #
#                                               #
# Materia: ANÁLISIS INTELIGENTE DE DATOS (2026) #
#                                               #
# Trabajo Final:                                #
#   "¿Qué hace que una oportunidad se gane?"    #
#===============================================#

#===============================================#
# ETAPA I. LIMPIEZA DE LOS DATOS RECIBIDOS      #
#===============================================#
# ARCHIVO "leads.csv"                           #
#    Contenido: 3.386 leads y oportunidades     #
#    (modelo crm.lead de Odoo)                  #
#===============================================#

#-----------------------------------------------#
# 1. PREÁMBULO + FUNCIONES                      #
#-----------------------------------------------#

source("codigo/00_preambulo_funciones.R", encoding = "UTF-8")


#-----------------------------------------------#
# 2. CARGA DE DATOS                             #
#-----------------------------------------------#

leads_raw <- read_csv("datos_crudos/leads.csv", show_col_types = FALSE)


#-----------------------------------------------#
# 3. INSPECCIÓN DE DATOS IMPORTADOS             #
#-----------------------------------------------#

# 3.1. Con las funciones de R:
if (mostrar_controles) {
  print(dim(leads_raw))
  glimpse(select(leads_raw, !starts_with("_")))
  print(summary(select(leads_raw, !starts_with("_"))))
  print(count(leads_raw, outcome, stage))
}

# 3.2. Los valores faltantes se diagnostican y tratan en 05_faltantes.R

# 3.3. Con funciones propias - Validaciones de integridad y tipo de dato:
comparar_tipos(leads_raw, "leads")    # --> con discrepancias (ver punto 4)
verificar_rangos(leads_raw)

# 3.4. Validaciones que además se registran en la tabla de chequeos de la app
#      (verificar_clave y verificar_secuencia_fechas se ejecutan acá):
cargos <- na.omit(leads_raw$`function`)

registrar_chequeo(
  "Variables internas de la simulación (prefijo _) en leads",
  sum(str_starts(names(leads_raw), "_")),
  "Se eliminan: no existen en Odoo y filtrarían el resultado"
)
registrar_chequeo(
  "gen_id duplicados en leads",
  verificar_clave(leads_raw, "gen_id"),
  "Ninguna: la clave es única"
)
registrar_chequeo(
  "Cargos distintos tal como fueron cargados",
  n_distinct(cargos),
  "Se normaliza el texto y se corrigen abreviaturas y errores de tipeo"
)
registrar_chequeo(
  "Cargos distintos ignorando mayúsculas",
  n_distinct(str_to_lower(str_squish(cargos))),
  "Se agrupan en niveles de cargo"
)
registrar_chequeo(
  "Leads con monto esperado igual a 0",
  sum(leads_raw$expected_revenue == 0),
  "Se recodifican como faltantes (monto no cargado)"
)
registrar_chequeo(
  "Leads cerrados como duplicados",
  sum(leads_raw$lost_reason == "Duplicate", na.rm = TRUE),
  "Se excluyen del cálculo de tasas de éxito"
)
registrar_chequeo(
  "Leads con fecha de cierre anterior a la de creación",
  verificar_secuencia_fechas(leads_raw, "create_date", "date_closed"),
  "Ninguna"
)


#-----------------------------------------------#
# 4. TRANSFORMACIÓN DE DATOS                    #
#-----------------------------------------------#

leads <- leads_raw |>

  # 4.1. Eliminar las variables internas de la simulación (prefijo "_"):
  select(!starts_with("_")) |>

  # 4.2. Renombrar "function" (palabra reservada en R):
  rename(cargo_original = `function`) |>

  mutate(
    # 4.3. Resultado de la oportunidad:
    ganada = outcome == "won",
    es_duplicado = lost_reason %in% "Duplicate",

    # 4.4. Cargo normalizado y agrupado (funciones de 00_preambulo):
    cargo = normalizar_cargo(cargo_original),
    nivel_cargo = clasificar_cargo(cargo),

    # 4.5. Un monto igual a 0 significa que el vendedor no lo cargó:
    expected_revenue = na_if(expected_revenue, 0),

    # 4.6. Categorías explícitas para los faltantes (ver 05_faltantes.R):
    #      no se imputan porque el hecho de que falten se asocia al resultado
    source = replace_na(source, "Sin dato"),
    medium = replace_na(medium, "Sin dato"),
    country_code = replace_na(country_code, "Sin dato"),
    vendedor = replace_na(user_login, "Sin asignar"),

    # El motivo solo corresponde a las oportunidades perdidas: en las ganadas
    # y abiertas el faltante es estructural; en las perdidas, un dato no cargado
    motivo_perdida = case_when(
      !is.na(lost_reason) ~ lost_reason,
      outcome == "lost" ~ "Sin motivo registrado",
      TRUE ~ "No corresponde"
    ),

    # 4.7. Calidad de los datos de contacto:
    calidad_contacto = case_when(
      email_state %in% "incorrect" | phone_state %in% "incorrect" ~
        "Email o teléfono incorrecto",
      email_state %in% "correct" & phone_state %in% "correct" ~
        "Ambos correctos",
      TRUE ~ "Incompleto"
    ),

    # 4.8. Fechas derivadas:
    fecha_creacion = as_date(create_date),
    mes_creacion = floor_date(fecha_creacion, "month"),
    ciclo_dias = as.numeric(
      difftime(date_closed, create_date, units = "days")
    ),

    # 4.9. Etapas alcanzadas en el embudo:
    llego_calificada = !is.na(date_qualified) |
      stage %in% c("Qualified", "Proposition", "Won"),
    llego_propuesta = !is.na(date_proposition) |
      stage %in% c("Proposition", "Won")
  )


#-----------------------------------------------#
# 5. CONTROL POST-TRANSFORMACIÓN                #
#-----------------------------------------------#

comparar_tipos(leads, "leads")
verificar_orden(leads, "leads")
verificar_rangos(leads)
if (mostrar_controles) {
  print(count(leads, nivel_cargo, sort = TRUE))
  print(count(leads, calidad_contacto))
}


#-----------------------------------------------#
# 6. GUARDADO INDIVIDUAL                        #
#-----------------------------------------------#

write_tsv(leads, "datos_salida/leads_limpios.txt", na = "")

