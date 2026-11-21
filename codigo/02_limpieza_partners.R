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
# ARCHIVO "partners.csv"                        #
#    Contenido: 446 clientes (res.partner)      #
#===============================================#

#-----------------------------------------------#
# 1. PREÁMBULO + FUNCIONES                      #
#-----------------------------------------------#

source("codigo/00_preambulo_funciones.R", encoding = "UTF-8")


#-----------------------------------------------#
# 2. CARGA DE DATOS                             #
#-----------------------------------------------#

partners_raw <- read_csv("datos_crudos/partners.csv", show_col_types = FALSE)


#-----------------------------------------------#
# 3. INSPECCIÓN DE DATOS IMPORTADOS             #
#-----------------------------------------------#

# 3.1. Con las funciones de R:
if (mostrar_controles) {
  print(dim(partners_raw))
  glimpse(partners_raw)
  print(summary(partners_raw))
  print(count(partners_raw, account_type))
}

# 3.2. Con funciones propias - Validaciones de integridad y tipo de dato:
comparar_tipos(partners_raw, "partners")  # --> con discrepancias (ver punto 4)

registrar_chequeo(
  "gen_partner_id duplicados en partners",
  verificar_clave(partners_raw, "gen_partner_id"),
  "Ninguna: la clave es única"
)


#-----------------------------------------------#
# 4. TRANSFORMACIÓN DE DATOS                    #
#-----------------------------------------------#

# 4.1. Conservar solo las variables útiles. La ubicación ya está en el lead,
#      pero se guarda el país para controlar la consistencia en 04; las
#      variables con prefijo "_" son internas de la simulación.
partners <- partners_raw |>
  select(gen_partner_id, country_code, account_type, known_since) |>

  # 4.2. Redondear known_since al segundo: las fracciones de segundo no
  #      aportan información y se pierden al guardar el archivo de texto
  mutate(known_since = round_date(known_since, "second"))


#-----------------------------------------------#
# 5. CONTROL POST-TRANSFORMACIÓN                #
#-----------------------------------------------#

comparar_tipos(partners, "partners")
verificar_orden(partners, "partners")


#-----------------------------------------------#
# 6. GUARDADO INDIVIDUAL                        #
#-----------------------------------------------#

write_tsv(partners, "datos_salida/partners_limpios.txt", na = "")
