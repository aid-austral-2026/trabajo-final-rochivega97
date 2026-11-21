#===============================================#
# MAESTRÍA EN CIENCIA DE DATOS - UNIV. AUSTRAL  #
#                                               #
# Materia: ANÁLISIS INTELIGENTE DE DATOS (2026) #
#                                               #
# Trabajo Final:                                #
#   "¿Qué hace que una oportunidad se gane?"    #
#===============================================#

#===============================================#
# ETAPA IV. ARCHIVO FINAL Y TABLAS PARA LA APP  #
#===============================================#
# Se ejecuta a continuación de 05_faltantes.R.  #
# Objetos que deja disponibles para la app:     #
#   - crm          tabla unificada y limpia     #
#   - cerradas     oportunidades ganadas o      #
#                  perdidas, sin duplicados     #
#   - crm_tags     una fila por lead y etiqueta #
#   - scores_test  scores de los cuatro modelos #
#                  en el conjunto de prueba     #
#===============================================#

#-----------------------------------------------#
# 1. PREÁMBULO + FUNCIONES                      #
#-----------------------------------------------#

source("codigo/00_preambulo_funciones.R", encoding = "UTF-8")
if (!exists("monto_imputado", where = as.environment(crm))) {
  source("codigo/05_faltantes.R", encoding = "UTF-8")
}


#-----------------------------------------------#
# 2. CONTROL FINAL                              #
#-----------------------------------------------#

informar("\n===== crm (final) =====\n")
verificar_clave(crm, "gen_id")
verificar_rangos(crm)


#-----------------------------------------------#
# 3. TABLAS DERIVADAS PARA LA APP               #
#-----------------------------------------------#

# 3.1. Oportunidades con desenlace conocido y que no son errores de carga
cerradas <- crm |>
  filter(resultado != "Abierta", !es_duplicado)

# 3.2. Una fila por lead y etiqueta (`tags` admite varias, separadas por ;).
#      Los leads sin etiqueta forman su propia categoría.
crm_tags <- crm |>
  mutate(tags = replace_na(tags, "Sin etiqueta")) |>
  separate_longer_delim(tags, delim = ";") |>
  mutate(tags = str_squish(tags))

# 3.3. Scores del conjunto de prueba en formato largo
nombres_modelos <- c(
  odoo_pls_default = "Odoo PLS (default)",
  odoo_pls_extended = "Odoo PLS (extended)",
  automl_12_fields = "AutoML (12 campos)",
  automl_20_fields = "AutoML (20 campos)"
)

scores_test <- crm |>
  filter(split == "test") |>
  select(gen_id, ganada, all_of(names(nombres_modelos))) |>
  pivot_longer(
    -c(gen_id, ganada),
    names_to = "modelo",
    values_to = "prob"
  ) |>
  mutate(modelo = factor(nombres_modelos[modelo], levels = nombres_modelos))


#-----------------------------------------------#
# 4. GUARDADO DEL ARCHIVO FINAL                 #
#-----------------------------------------------#

write_tsv(crm, "resultados/crm_unificado.txt", na = "")
