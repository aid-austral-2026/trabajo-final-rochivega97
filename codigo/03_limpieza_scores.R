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
# ARCHIVOS "odoo_pls_scores_at_cutoff.csv" y    #
#          "benchmark_test_scores.csv"          #
#    Contenido: probabilidades de cierre de     #
#    los modelos de scoring                     #
#                                               #
# Requiere haber corrido 01_limpieza_leads.R    #
# (usa datos_salida/leads_limpios.txt)          #
#===============================================#

#-----------------------------------------------#
# 1. PREÁMBULO + FUNCIONES                      #
#-----------------------------------------------#

source("codigo/00_preambulo_funciones.R", encoding = "UTF-8")


#-----------------------------------------------#
# 2. CARGA DE DATOS                             #
#-----------------------------------------------#

odoo_raw <- read_csv(
  "datos_crudos/odoo_pls_scores_at_cutoff.csv",
  show_col_types = FALSE
)
benchmark_raw <- read_csv(
  "datos_crudos/benchmark_test_scores.csv",
  show_col_types = FALSE
)
leads_limpios <- leer_limpio("leads")  # Para validar las referencias


#-----------------------------------------------#
# 3. INSPECCIÓN DE DATOS IMPORTADOS             #
#-----------------------------------------------#

# 3.1. Con las funciones de R:
if (mostrar_controles) {
  print(dim(odoo_raw))
  print(summary(odoo_raw))
  print(dim(benchmark_raw))
  print(summary(benchmark_raw))
}

# 3.2. Con funciones propias - Validaciones de integridad y tipo de dato:
verificar_clave(odoo_raw, "gen_id")
verificar_clave(benchmark_raw, "gen_id")
verificar_rangos(odoo_raw)       # Odoo informa los scores en %
verificar_rangos(benchmark_raw)  # El benchmark, como proporción

registrar_chequeo(
  "Scores de Odoo sin lead asociado",
  verificar_referencias(odoo_raw, "gen_id", leads_limpios,
                        nombre_maestro = "leads"),
  "Ninguna: todas las referencias son válidas"
)
registrar_chequeo(
  "Scores del benchmark sin lead asociado",
  verificar_referencias(benchmark_raw, "gen_id", leads_limpios,
                        nombre_maestro = "leads"),
  "Ninguna: todas las referencias son válidas"
)

# 3.3. Ambas tablas traen el score de Odoo: verifico que coincidan
scores_ambas_tablas <- inner_join(benchmark_raw, odoo_raw, by = "gen_id")

registrar_chequeo(
  "Diferencia máxima entre los scores de Odoo de ambas tablas",
  max(abs(
    scores_ambas_tablas$odoo_pls_default -
      scores_ambas_tablas$odoo_pls_default_pct / 100
  )),
  "Se conserva una sola versión, expresada como proporción"
)

# 3.4. `won` del benchmark frente al resultado registrado en el lead
won_ambas_tablas <- inner_join(benchmark_raw, leads_limpios, by = "gen_id")

registrar_chequeo(
  "Casos en que `won` (benchmark) difiere del resultado del lead",
  sum(won_ambas_tablas$won != won_ambas_tablas$ganada),
  "Se descarta `won` por redundante"
)


#-----------------------------------------------#
# 4. TRANSFORMACIÓN DE DATOS                    #
#-----------------------------------------------#

# 4.1. Expresar los scores de Odoo como proporción
# 4.2. Incorporar los scores de los modelos AutoML (solo conjunto de prueba),
#      descartando las columnas redundantes del benchmark (won y los scores
#      de Odoo repetidos) y la probabilidad "real" de la simulación
scores <- odoo_raw |>
  transmute(
    gen_id,
    odoo_pls_default = odoo_pls_default_pct / 100,
    odoo_pls_extended = odoo_pls_extended_pct / 100
  ) |>
  left_join(
    select(benchmark_raw, gen_id, automl_12_fields, automl_20_fields),
    by = "gen_id"
  )


#-----------------------------------------------#
# 5. CONTROL POST-TRANSFORMACIÓN                #
#-----------------------------------------------#

comparar_tipos(scores, "scores")
verificar_orden(scores, "scores")
verificar_clave(scores, "gen_id")
verificar_rangos(scores)


#-----------------------------------------------#
# 6. GUARDADO INDIVIDUAL                        #
#-----------------------------------------------#

write_tsv(scores, "datos_salida/scores_limpios.txt", na = "")
