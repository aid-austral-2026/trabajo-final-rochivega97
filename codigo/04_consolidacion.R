#===============================================#
# MAESTRÍA EN CIENCIA DE DATOS - UNIV. AUSTRAL  #
#                                               #
# Materia: ANÁLISIS INTELIGENTE DE DATOS (2026) #
#                                               #
# Trabajo Final:                                #
#   "¿Qué hace que una oportunidad se gane?"    #
#===============================================#

#===============================================#
# ETAPA II. CONSOLIDACIÓN DE LAS TABLAS         #
#===============================================#
# Une las tablas limpias de datos_salida/ en un #
# único data frame (`crm`) con una fila por     #
# lead. Los faltantes se tratan en              #
# 05_faltantes.R.                               #
#===============================================#

#-----------------------------------------------#
# 1. PREÁMBULO + FUNCIONES                      #
#-----------------------------------------------#

source("codigo/00_preambulo_funciones.R", encoding = "UTF-8")


#-----------------------------------------------#
# 2. CARGA DE DATOS                             #
#-----------------------------------------------#

# Tablas limpias, leídas con los tipos del diseño de registro
leads <- leer_limpio("leads")
partners <- leer_limpio("partners")
scores <- leer_limpio("scores")


#-----------------------------------------------#
# 3. VALIDACIONES PREVIAS A LA UNIÓN            #
#-----------------------------------------------#

# 3.1. Estructura de cada tabla contra el diseño de registro:
for (tabla in c("leads", "partners", "scores")) {
  informar("\n===== ", tabla, " =====\n")
  comparar_tipos(get(tabla), tabla)
  verificar_orden(get(tabla), tabla)
}

# 3.2. Claves únicas en las tablas que se agregan al lead:
verificar_clave(partners, "gen_partner_id")
verificar_clave(scores, "gen_id")

# 3.3. Consistencia entre tablas:
registrar_chequeo(
  "Leads con un partner inexistente en partners",
  verificar_referencias(leads, "gen_partner_id", partners,
                        nombre_maestro = "partners"),
  "Ninguna: todas las referencias son válidas"
)

# El país "Sin dato" del lead no cuenta como diferencia (era un faltante)
leads_con_partner <- leads |>
  filter(country_code != "Sin dato") |>
  inner_join(partners, by = "gen_partner_id", suffix = c("", "_partner"))

registrar_chequeo(
  "Países distintos entre el lead y su partner",
  sum(leads_con_partner$country_code != leads_con_partner$country_code_partner,
      na.rm = TRUE),
  "Solo se incorporan tipo de cuenta y antigüedad del partner"
)


#-----------------------------------------------#
# 4. UNIÓN Y TRANSFORMACIÓN                     #
#-----------------------------------------------#

# 4.1. Unión con leads como tabla base (left joins: no se pierde ningún lead)
crm <- leads |>
  left_join(select(partners, !country_code), by = "gen_partner_id") |>
  left_join(scores, by = "gen_id") |>
  mutate(
    # 4.2. Variables categóricas con un orden definido:
    resultado = factor(
      outcome,
      levels = c("won", "lost", "open"),
      labels = c("Ganada", "Perdida", "Abierta")
    ),
    nivel_cargo = factor(
      nivel_cargo,
      levels = c(
        "Dirección", "Gerencia", "Diseño / Arquitectura", "Operativo",
        "Sin dato"
      )
    ),
    prioridad = factor(priority, levels = 0:3),

    # 4.3. Relación previa con el cliente (requiere la tabla partners):
    tipo_cliente = case_when(
      is.na(gen_partner_id) ~ "Contacto nuevo",
      account_type == "reseller" ~ "Cliente revendedor",
      TRUE ~ "Cliente final"
    )
  ) |>
  # 4.4. Ordenar: variables originales, del partner, scores y derivadas
  relocate(account_type:automl_20_fields, .after = lost_reason) |>
  relocate(resultado, .before = ganada) |>
  relocate(tipo_cliente, prioridad, .after = calidad_contacto)


#-----------------------------------------------#
# 5. CONTROL POST-UNIÓN                         #
#-----------------------------------------------#

informar("\n===== crm =====\n")
verificar_clave(crm, "gen_id")
informar(if (nrow(crm) == nrow(leads)) "  ✔ " else "  ✘ ", nrow(crm),
         " filas en crm (", nrow(leads), " leads)\n")
verificar_rangos(crm)
if (mostrar_controles) {
  print(count(crm, resultado))
  print(count(crm, tipo_cliente))
}
