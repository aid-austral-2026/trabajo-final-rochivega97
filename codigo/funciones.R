##############################################################################
# funciones.R
#
# Funciones auxiliares de formato, cálculo y visualización usadas por la app.
##############################################################################


# Estética general ------------------------------------------------------------

theme_set(
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.title.position = "plot",
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
)

colores_resultado <- c(
  "Ganada" = "#2a9d8f",
  "Perdida" = "#e76f51",
  "Abierta" = "#8d99ae"
)

color_principal <- "#264653"

colores_faltante <- c(
  "Estructural" = "#8d99ae",
  "Mixto" = "#e9c46a",
  "No estructural" = "#e76f51"
)


# Formatos numéricos en español (punto para miles, coma para decimales) -------

fmt_num <- label_comma(big.mark = ".", decimal.mark = ",")
fmt_usd <- label_dollar(big.mark = ".", decimal.mark = ",")

fmt_pct <- function(x, accuracy = 1) {
  percent(x, accuracy = accuracy, big.mark = ".", decimal.mark = ",")
}

fmt_dec <- function(x, digitos = 3) {
  format(round(x, digitos), nsmall = digitos, decimal.mark = ",")
}


# Variables disponibles para analizar la tasa de éxito ------------------------

# Nombre visible en la app = nombre de la columna en `crm`
variables_exito <- c(
  "Origen del lead" = "source",
  "Medio" = "medium",
  "Tipo de cliente" = "tipo_cliente",
  "Prioridad (estrellas)" = "prioridad",
  "Vendedor" = "vendedor",
  "País" = "country_code",
  "Nivel del cargo del contacto" = "nivel_cargo",
  "Calidad de los datos de contacto" = "calidad_contacto",
  "Equipo" = "team",
  "Etiqueta" = "tags"
)


# Indicadores disponibles para los mapas ---------------------------------------

# Nombre visible en la app = columna de mapa_paises y mapa_estados
indicadores_mapa <- c(
  "Tasa de éxito" = "tasa",
  "Cantidad de leads" = "leads",
  "Ingresos ganados (USD)" = "ingresos"
)

# Formato de cada indicador para las etiquetas y la leyenda
formatear_indicador <- function(x, indicador) {
  switch(
    indicador,
    tasa = fmt_pct(x),
    leads = fmt_num(x),
    ingresos = fmt_usd(x)
  )
}

# Paleta de cada indicador: divergente para la tasa (rojo bajo, verde alto),
# secuencial para las cantidades
paleta_indicador <- function(indicador, valores) {
  colores <- if (indicador == "tasa") {
    c("#e76f51", "#f4f1de", "#2a9d8f")
  } else {
    c("#f1faee", "#264653")
  }
  colorNumeric(colores, domain = valores, na.color = "#d9d9d9")
}

# Texto en HTML que se muestra al pasar el mouse sobre cada región
etiquetas_mapa <- function(datos) {
  paste0(
    "<b>", datos$nombre, "</b>",
    "<br>Leads: ", fmt_num(datos$leads),
    "<br>Cerradas: ", fmt_num(datos$cerradas),
    "<br>Tasa de éxito: ",
    if_else(is.na(datos$tasa), "pocos casos", fmt_pct(datos$tasa)),
    "<br>Ingresos ganados: ", fmt_usd(datos$ingresos)
  ) |>
    lapply(HTML)
}


# Tasa de éxito por grupo -----------------------------------------------------

# Calcula la tasa de éxito de cada categoría de `variable` junto con un
# intervalo de confianza del 95 % (aproximación normal). Se descartan las
# categorías con menos de `min_casos` oportunidades.
tasa_por_grupo <- function(datos, variable, min_casos = 20) {
  datos |>
    mutate(grupo = as.character(.data[[variable]])) |>
    group_by(grupo) |>
    summarise(n = n(), ganadas = sum(ganada), .groups = "drop") |>
    filter(n >= min_casos) |>
    mutate(
      tasa = ganadas / n,
      error = 1.96 * sqrt(tasa * (1 - tasa) / n),
      li = pmax(tasa - error, 0),
      ls = pmin(tasa + error, 1)
    )
}

# Gráfico de la tasa de éxito por grupo: puntos con IC o barras
graficar_tasa <- function(tabla, tasa_global, titulo, tipo = "puntos") {
  base <- ggplot(tabla, aes(tasa, fct_reorder(grupo, tasa))) +
    geom_vline(
      xintercept = tasa_global,
      linetype = "dashed",
      color = "grey50"
    ) +
    labs(
      title = titulo,
      subtitle = "La línea punteada marca la tasa de éxito global",
      x = "Tasa de éxito",
      y = NULL
    )

  if (tipo == "barras") {
    base +
      geom_col(fill = colores_resultado[["Ganada"]], width = 0.7) +
      geom_text(aes(label = fmt_pct(tasa)), hjust = -0.2, size = 4) +
      scale_x_continuous(
        labels = fmt_pct,
        expand = expansion(mult = c(0, 0.15))
      )
  } else {
    base +
      scale_x_continuous(labels = fmt_pct) +
      geom_errorbar(
        aes(xmin = li, xmax = ls),
        width = 0.2,
        orientation = "y",
        color = "grey60"
      ) +
      geom_point(aes(size = n), color = colores_resultado[["Ganada"]]) +
      scale_size_area(max_size = 8, name = "Oportunidades")
  }
}


# Embudo de ventas ------------------------------------------------------------

calcular_embudo <- function(datos) {
  datos |>
    summarise(
      New = n(),
      Qualified = sum(llego_calificada),
      Proposition = sum(llego_propuesta),
      Won = sum(ganada)
    ) |>
    pivot_longer(everything(), names_to = "etapa", values_to = "n") |>
    mutate(
      etapa = fct_inorder(etapa),
      prop = n / first(n),
      conversion = n / lag(n)
    )
}


# Métricas de los modelos de scoring ------------------------------------------

# AUC calculado a partir de los rangos (estadístico de Mann-Whitney)
calcular_auc <- function(prob, y) {
  rangos <- rank(prob)
  n_pos <- sum(y)
  n_neg <- sum(!y)
  (sum(rangos[y]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

# AUC, Brier y precisión entre el `top` % de leads con mayor score
calcular_metricas <- function(datos, top = 0.2) {
  datos |>
    group_by(modelo) |>
    summarise(
      auc = calcular_auc(prob, ganada),
      brier = mean((prob - ganada)^2),
      prob_media = mean(prob),
      tasa_real = mean(ganada),
      precision_top = mean(ganada[prob >= quantile(prob, 1 - top)])
    )
}

# Agrupa los leads en `n_grupos` cuantiles de score y compara la probabilidad
# media predicha con la proporción observada de ganadas
calcular_calibracion <- function(datos, n_grupos = 10) {
  datos |>
    group_by(modelo) |>
    mutate(grupo = ntile(prob, n_grupos)) |>
    group_by(modelo, grupo) |>
    summarise(
      predicha = mean(prob),
      observada = mean(ganada),
      n = n(),
      .groups = "drop"
    )
}
