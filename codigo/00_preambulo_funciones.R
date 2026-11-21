#===============================================#
# MAESTRÍA EN CIENCIA DE DATOS - UNIV. AUSTRAL  #
#                                               #
# Materia: ANÁLISIS INTELIGENTE DE DATOS (2026) #
#                                               #
# Trabajo Final:                                #
#   "¿Qué hace que una oportunidad se gane?"    #
#   Pipeline comercial de un CRM Odoo           #
#===============================================#
#
# Preámbulo común a los scripts de limpieza (01 a 04): paquetes, información
# auxiliar para los chequeos y funciones propias de validación y
# transformación. Cada script de limpieza lo carga con source().


#===============================================#
# PAQUETES DE USO COMÚN                         #
#===============================================#

library(tidyverse)  
library(naniar)   


#===============================================#
# OPCIONES                                      #
#===============================================#

# Si es TRUE (valor por defecto), las validaciones e inspecciones se imprimen
# en la consola. La app fija options(mostrar_controles = FALSE) antes de correr
# la limpieza para no llenar la consola cada vez que se inicia.
mostrar_controles <- getOption("mostrar_controles", default = TRUE)


#===============================================#
# INFORMACIÓN AUXILIAR PARA LOS CHEQUEOS        #
#===============================================#

# Diseño de registro: nombre, orden y tipo esperado de las variables de cada
# tabla limpia que se guarda en datos_salida/
disenio_registro <- read_csv("disenio_registro.csv", show_col_types = FALSE) |>
  arrange(tabla, nro_variable)

# Rangos válidos de las variables numéricas acotadas
rangos <- list(
  priority = c(0, 3),
  expected_revenue = c(0, Inf),
  recurring_revenue = c(0, Inf),
  odoo_pls_default_pct = c(0, 100),
  odoo_pls_extended_pct = c(0, 100),
  odoo_pls_default = c(0, 1),
  odoo_pls_extended = c(0, 1),
  automl_12_fields = c(0, 1),
  automl_20_fields = c(0, 1)
)

# Registro de los chequeos realizados, que se muestra en la pestaña "Datos"
# de la app. Se completa con registrar_chequeo() a lo largo de la limpieza.
if (!exists("chequeos")) {
  chequeos <- tibble(chequeo = character(), resultado = numeric(),
                     accion = character())
}


#===============================================#
# FUNCIONES GENERALES                           #
#===============================================#

#-----------------------------------------------#
# 0. Funciones de apoyo                         #
#-----------------------------------------------#

# Imprime un mensaje solo si mostrar_controles es TRUE
informar <- function(...) {
  if (mostrar_controles) cat(..., sep = "")
}

# Tipo de una variable con el vocabulario del diseño de registro
tipo_variable <- function(x) {
  if (inherits(x, "POSIXct")) return("datetime")
  if (inherits(x, "Date")) return("date")
  if (is.numeric(x)) return("numeric")
  if (is.logical(x)) return("logical")
  if (is.character(x)) return("character")
  class(x)[1]
}

# Agrega (o actualiza, si ya existe) una fila en la tabla de chequeos
registrar_chequeo <- function(chequeo, resultado, accion) {
  if (chequeo %in% chequeos$chequeo) {
    fila <- chequeos$chequeo == chequeo
    chequeos$resultado[fila] <<- resultado
    chequeos$accion[fila] <<- accion
  } else {
    chequeos <<- add_row(chequeos, chequeo = chequeo, resultado = resultado,
                         accion = accion)
  }
}


#-----------------------------------------------#
# 1. Función "comparar_tipos" - Validación:     #
#    Verificar nombres y tipos del dataset      #
#    contra el diseño de registro de la tabla   #
#-----------------------------------------------#

#' @param df    Data frame a validar.
#' @param tabla Nombre de la tabla en disenio_registro (leads, partners, scores).
#' @return Cantidad de discrepancias (invisible).

comparar_tipos <- function(df, tabla) {
  disenio <- filter(disenio_registro, tabla == !!tabla)
  esperados <- set_names(disenio$tipo_dato, disenio$nombre_variable)

  presentes <- intersect(names(esperados), names(df))
  faltantes <- setdiff(names(esperados), names(df))
  sobrantes <- setdiff(names(df), names(esperados))

  actual <- map_chr(df[presentes], tipo_variable)
  tipo_ok <- actual == esperados[presentes]

  informar("Tipos de las variables (tabla ", tabla, "):\n")
  for (v in presentes[tipo_ok]) informar("  ✔ ", v, "\n")
  for (v in presentes[!tipo_ok]) {
    informar("  ✘ ", v, " - tipo ", actual[v], " (se esperaba ",
             esperados[v], ")\n")
  }
  for (v in faltantes) informar("  ✘ ", v, " - falta la columna\n")
  if (length(sobrantes) > 0) {
    informar("  ✘ ", length(sobrantes), " columnas que no van en la tabla ",
             "limpia: ", paste(sobrantes, collapse = ", "), "\n")
  }

  n_problemas <- sum(!tipo_ok) + length(faltantes) + length(sobrantes)
  if (n_problemas == 0) informar("\nSin discrepancias\n")
  invisible(n_problemas)
}


#-----------------------------------------------#
# 2. Función "verificar_orden" - Validación:    #
#    Verificar que las columnas respeten el     #
#    orden del diseño de registro               #
#-----------------------------------------------#

verificar_orden <- function(df, tabla) {
  esperadas <- disenio_registro |>
    filter(tabla == !!tabla) |>
    pull(nombre_variable)
  presentes <- intersect(esperadas, names(df))

  # Solo se comparan las columnas esperadas que están en df, para que las que
  # sobran o faltan no desplacen al resto
  orden_df <- names(df)[names(df) %in% esperadas]
  orden_ok <- orden_df == presentes

  informar("Orden de las columnas (tabla ", tabla, "):\n")
  for (i in seq_along(orden_df)) {
    if (orden_ok[i]) {
      informar("  ✔ ", i, ": ", orden_df[i], "\n")
    } else {
      informar("  ✘ ", i, ": está ", orden_df[i], ", debería estar ",
               presentes[i], "\n")
    }
  }

  if (all(orden_ok)) informar("\nSin discrepancias\n")
  invisible(sum(!orden_ok))
}


#-----------------------------------------------#
# 3. Función "verificar_clave" - Validación:    #
#    Verificar que la clave sea única y no      #
#    tenga faltantes                            #
#-----------------------------------------------#

verificar_clave <- function(df, clave) {
  x <- df[[clave]]
  duplicados <- sum(duplicated(x, incomparables = NA))
  faltantes <- sum(is.na(x))

  informar("Clave ", clave, ":\n")
  informar(if (duplicados == 0) "  ✔ " else "  ✘ ", duplicados,
           " valores duplicados\n")
  informar(if (faltantes == 0) "  ✔ " else "  ✘ ", faltantes,
           " valores faltantes\n")

  invisible(duplicados)
}


#-----------------------------------------------#
# 4. Función "verificar_referencias" -          #
#    Validación: verificar que los valores de   #
#    una clave existan en la tabla maestra      #
#    (los faltantes no se consideran error)     #
#-----------------------------------------------#

verificar_referencias <- function(df, clave, maestro, clave_maestro = clave,
                                  nombre_maestro = "la tabla maestra") {
  x <- df[[clave]]
  huerfanos <- unique(x[!is.na(x) & !x %in% maestro[[clave_maestro]]])

  informar("Referencias de ", clave, " en ", nombre_maestro, ":\n")
  if (length(huerfanos) == 0) {
    informar("  ✔ todos los valores existen\n")
  } else {
    informar("  ✘ ", length(huerfanos), " valores no existen: ",
             paste(head(huerfanos, 10), collapse = ", "),
             if (length(huerfanos) > 10) ", ...", "\n")
  }

  invisible(sum(!is.na(x) & x %in% huerfanos))
}


#-----------------------------------------------#
# 5. Función "verificar_rangos" - Validación:   #
#    Verificar que las variables numéricas      #
#    estén dentro de su rango válido            #
#-----------------------------------------------#

verificar_rangos <- function(df) {
  variables <- intersect(names(rangos), names(df))
  n_fuera <- 0

  informar("Rangos de valores:\n")
  for (v in variables) {
    x <- df[[v]]
    lim <- rangos[[v]]
    fuera <- sum(x < lim[1] | x > lim[2], na.rm = TRUE)
    n_fuera <- n_fuera + fuera

    informar(if (fuera == 0) "  ✔ " else "  ✘ ", v, ": min ",
             min(x, na.rm = TRUE), ", max ", max(x, na.rm = TRUE),
             " (esperado entre ", lim[1], " y ", lim[2], ")",
             if (fuera > 0) paste0(" - ", fuera, " valores fuera de rango"),
             "\n")
  }

  if (n_fuera == 0) informar("\nSin discrepancias\n")
  invisible(n_fuera)
}


#-----------------------------------------------#
# 6. Función "verificar_secuencia_fechas" -     #
#    Validación: verificar que una fecha no     #
#    sea anterior a otra (p. ej., el cierre     #
#    anterior a la creación)                    #
#-----------------------------------------------#

verificar_secuencia_fechas <- function(df, desde, hasta) {
  invertidas <- sum(df[[hasta]] < df[[desde]], na.rm = TRUE)

  informar(if (invertidas == 0) "  ✔ " else "  ✘ ", invertidas,
           " registros con ", hasta, " anterior a ", desde, "\n")

  invisible(invertidas)
}


#-----------------------------------------------#
# 7. Función "leer_limpio" - Carga:             #
#    Lee una tabla limpia de datos_salida/ con  #
#    los tipos del diseño de registro           #
#-----------------------------------------------#

leer_limpio <- function(tabla) {
  disenio <- filter(disenio_registro, tabla == !!tabla)
  lectores <- list(
    character = col_character(),
    numeric = col_double(),
    logical = col_logical(),
    date = col_date(),
    datetime = col_datetime()
  )
  tipos <- set_names(lectores[disenio$tipo_dato], disenio$nombre_variable)

  read_tsv(
    file.path("datos_salida", paste0(tabla, "_limpios.txt")),
    col_types = do.call(cols, tipos),
    na = ""
  )
}


#-----------------------------------------------#
# 8. Funciones "normalizar_cargo" y             #
#    "clasificar_cargo" - Transformación:       #
#    unifican la escritura del cargo y lo       #
#    agrupan en niveles jerárquicos             #
#    Usadas en: 01_limpieza_leads.R             #
#-----------------------------------------------#

# Abreviaturas y errores de tipeo frecuentes en el cargo
correcciones_cargo <- c(
  "\\bmgr\\b" = "manager",
  "\\bdir\\.? of\\b" = "director of",
  "\\bdir\\.?$" = "director",
  "\\bofice\\b" = "office"
)

# Normaliza el cargo: espacios, mayúsculas, abreviaturas y siglas
normalizar_cargo <- function(cargo) {
  cargo |>
    str_squish() |>
    str_to_lower() |>
    str_replace_all(correcciones_cargo) |>
    str_to_title() |>
    str_replace_all("\\b(Hr|Ceo|Cfo|Coo|Vp)\\b", str_to_upper) |>
    str_replace_all("\\bOf\\b", "of")
}

# Agrupa los cargos normalizados en niveles jerárquicos
clasificar_cargo <- function(cargo) {
  cargo <- str_to_lower(cargo)
  directivos <- "ceo|cfo|coo|founder|owner|managing director|vp|director|head of"

  case_when(
    is.na(cargo) ~ "Sin dato",
    str_detect(cargo, directivos) ~ "Dirección",
    str_detect(cargo, "manager") ~ "Gerencia",
    str_detect(cargo, "designer|architect") ~ "Diseño / Arquitectura",
    TRUE ~ "Operativo"
  )
}


#-----------------------------------------------#
# 9. Función "verificar_estructural" -          #
#    Validación: verificar que una variable     #
#    falte exactamente cuando se cumple la      #
#    condición que hace que no corresponda      #
#    (faltante estructural)                     #
#    Usada en: 05_faltantes.R                   #
#-----------------------------------------------#

#' @param df        Data frame a validar.
#' @param variable  Nombre de la variable con faltantes.
#' @param condicion Expresión que indica cuándo el dato no corresponde.
#' @param regla     Texto con la regla, para la consola y la app.
#' @return Cantidad de registros que no cumplen la regla (invisible).

verificar_estructural <- function(df, variable, condicion, regla) {
  no_corresponde <- eval(substitute(condicion), df, parent.frame())
  falta <- is.na(df[[variable]])
  inconsistentes <- sum(falta != no_corresponde)

  informar(if (inconsistentes == 0) "  ✔ " else "  ✘ ", variable,
           " falta si y solo si ", regla,
           if (inconsistentes > 0) paste0(" - ", inconsistentes,
                                          " registros no cumplen la regla"),
           "\n")

  invisible(inconsistentes)
}


#-----------------------------------------------#
# 10. Funciones de imputación del monto -       #
#     Transformación: cada una recibe los       #
#     registros con monto observado (train) y   #
#     devuelve el monto imputado para test      #
#     Usadas en: 05_faltantes.R                 #
#-----------------------------------------------#

# Asigna a los niveles que no aparecen en train el nivel más frecuente, para
# que el modelo lineal pueda predecir sobre categorías poco frecuentes
alinear_niveles <- function(test, modelo, train) {
  for (v in names(modelo$xlevels)) {
    nuevo <- !test[[v]] %in% modelo$xlevels[[v]]
    test[[v]][nuevo] <- names(which.max(table(train[[v]])))
  }
  test
}

# Medida resumen (media o mediana) dentro de cada grupo de `grupo`
imputar_resumen_grupo <- function(train, test, grupo, resumen) {
  valores <- train |>
    group_by(across(all_of(grupo))) |>
    summarise(valor = resumen(monto), .groups = "drop")
  left_join(test, valores, by = grupo)$valor
}

metodos_imputacion <- list(
  "Mediana global" = \(train, test) rep(median(train$monto), nrow(test)),
  "Mediana por grupo" = \(train, test) {
    imputar_resumen_grupo(train, test, "grupo_monto", median)
  },
  "Media global" = \(train, test) rep(mean(train$monto), nrow(test)),
  "Media por grupo" = \(train, test) {
    imputar_resumen_grupo(train, test, "grupo_monto", mean)
  },
  "Valor al azar del grupo" = \(train, test) {
    map_dbl(test$grupo_monto, \(g) {
      observados <- train$monto[train$grupo_monto == g]
      observados[sample.int(length(observados), 1)]
    })
  },
  "Regresión lineal (log)" = \(train, test) {
    modelo <- lm(
      log(monto) ~ grupo_monto + source + medium + vendedor + prioridad +
        pais + tipo_cliente + nivel_cargo + recurrente,
      data = train
    )
    # Corrección por retransformación: E[monto] = exp(mu + sigma^2 / 2)
    exp(predict(modelo, alinear_niveles(test, modelo, train)) +
          sigma(modelo)^2 / 2)
  }
)
