##############################################################################
# server.R
#
# Lógica del servidor: filtra los datos según los controles elegidos por el
# usuario y genera las tablas y gráficos de cada pestaña.
##############################################################################

# Comentarios sobre cada variable de la pestaña "Factores de éxito".
# Describen lo observado con todos los datos (sin filtrar por fecha).
comentarios_variables <- c(
  source = paste(
    "Los referidos, las ferias y las campañas de reactivación (Lead Recall)",
    "convierten muy por encima del promedio. Los leads de redes sociales,",
    "portales de avisos y buscadores convierten bastante peor. Los leads sin",
    "origen registrado convierten mejor que el promedio, posiblemente porque",
    "son contactos cargados a mano por los vendedores."
  ),
  medium = paste(
    "El contacto telefónico y el directo convierten muy por encima del",
    "promedio. Los leads que ingresan por el sitio web y por publicidad paga",
    "(Google Adwords, Facebook, LinkedIn, banners) convierten muy poco."
  ),
  tipo_cliente = paste(
    "Vender a quien ya es cliente es mucho más fácil: las oportunidades de",
    "clientes finales existentes se ganan unas 2,5 veces más que las de",
    "contactos nuevos."
  ),
  prioridad = paste(
    "La tasa de éxito crece con la cantidad de estrellas: la prioridad que",
    "asignan los vendedores refleja bien su intuición sobre cada oportunidad."
  ),
  vendedor = paste(
    "Los leads sin vendedor asignado casi nunca se ganan. Entre los",
    "vendedores, la mayoría tiene tasas similares, salvo rep.nguyen, que",
    "queda por debajo. No se controla por la calidad de los leads que recibe",
    "cada uno."
  ),
  country_code = paste(
    "Estados Unidos, el mercado principal, convierte por encima del promedio;",
    "la mayoría de los mercados europeos y latinoamericanos queda por debajo.",
    "En los países con pocos casos los intervalos son amplios."
  ),
  nivel_cargo = paste(
    "Cuanto más alto es el cargo del contacto, más chances de cerrar la venta.",
    "Los leads sin cargo registrado tienen la tasa más baja."
  ),
  calidad_contacto = paste(
    "Un email o teléfono mal cargado reduce mucho la probabilidad de cerrar",
    "la venta."
  ),
  team = paste(
    "El equipo de Ventas trabaja oportunidades ya calificadas, mientras que",
    "Pre-Sales recibe leads sin calificar, lo que explica su menor tasa."
  ),
  tags = paste(
    "Las etiquetas muestran diferencias chicas y sus intervalos se superponen:",
    "el tipo de interés declarado no parece asociado al resultado. Un mismo",
    "lead puede tener varias etiquetas."
  )
)

server <- function(input, output, session) {

  # Inicio --------------------------------------------------------------------

  output$tabla_fuentes <- renderTable({
    tibble(
      Archivo = c(
        "leads.csv", "partners.csv", "odoo_pls_scores_at_cutoff.csv",
        "benchmark_test_scores.csv"
      ),
      `Unidad de análisis` = c("Lead", "Cliente", "Lead", "Lead"),
      Filas = fmt_num(c(
        nrow(leads_raw), nrow(partners_raw), nrow(odoo_raw),
        nrow(benchmark_raw)
      )),
      Contenido = c(
        "Origen, vendedor, país, etapa, fechas, montos y resultado",
        "País, ciudad y tipo de cuenta (cliente final o revendedor)",
        "Probabilidad asignada por el PLS de Odoo en una fecha de corte (%)",
        "Probabilidades de los modelos AutoML y Odoo en el conjunto de prueba"
      )
    )
  })


  # Datos ---------------------------------------------------------------------

  output$tabla_chequeos <- renderDT({
    chequeos |>
      mutate(
        resultado = if_else(
          resultado %% 1 == 0,
          fmt_num(resultado),
          format(resultado, digits = 2)
        )
      ) |>
      datatable(
        colnames = c("Verificación", "Resultado", "Acción tomada"),
        rownames = FALSE,
        options = list(dom = "t", pageLength = nrow(chequeos), ordering = FALSE)
      )
  })

  output$grafico_faltantes <- renderPlot({
    req(input$tipos_faltante)

    diccionario_faltantes |>
      filter(tipo %in% input$tipos_faltante) |>
      mutate(tipo = factor(tipo, levels = names(colores_faltante))) |>
      ggplot(aes(prop_na, fct_reorder(variable, prop_na), fill = tipo)) +
      geom_col() +
      geom_text(aes(label = fmt_pct(prop_na)), hjust = -0.2, size = 3.5) +
      scale_fill_manual(values = colores_faltante, drop = FALSE) +
      scale_x_continuous(
        labels = fmt_pct,
        expand = expansion(mult = c(0, 0.15))
      ) +
      labs(
        subtitle = "Datos crudos de leads.csv (montos en 0 como faltantes)",
        x = "Porcentaje de faltantes",
        y = NULL,
        fill = NULL
      )
  })

  output$tabla_diccionario_faltantes <- renderDT({
    diccionario_faltantes |>
      arrange(factor(tipo, levels = names(colores_faltante)), desc(prop_na)) |>
      mutate(
        prop_na = fmt_pct(prop_na),
        regla = if_else(is.na(regla), "—", regla),
        verificada = case_when(verificada ~ "✔", !verificada ~ "✘", TRUE ~ "")
      ) |>
      select(variable, prop_na, tipo, regla, verificada, tratamiento) |>
      datatable(
        colnames = c(
          "Variable", "Faltantes", "Tipo", "Falta cuando…", "Verificada",
          "Tratamiento"
        ),
        rownames = FALSE,
        options = list(dom = "t", pageLength = 50, ordering = FALSE)
      )
  })

  output$grafico_mecanismo <- renderPlot({
    tasa_segun_faltante |>
      mutate(variable = fct_reorder(variable, -tasa * (dato == "Falta el dato"))) |>
      ggplot(aes(tasa, variable, color = dato)) +
      geom_vline(
        xintercept = mean(cerradas$ganada),
        linetype = "dashed",
        color = "grey50"
      ) +
      geom_errorbar(
        aes(xmin = li, xmax = ls),
        width = 0.3,
        orientation = "y",
        position = position_dodge(width = 0.6)
      ) +
      geom_point(aes(size = n), position = position_dodge(width = 0.6)) +
      scale_color_manual(
        values = c("Dato cargado" = color_principal, "Falta el dato" = "#e9c46a")
      ) +
      scale_size_area(max_size = 6, guide = "none") +
      scale_x_continuous(labels = fmt_pct) +
      labs(
        subtitle = "Cerradas sin duplicados · IC del 95 % · línea: tasa global",
        x = "Tasa de éxito",
        y = NULL,
        color = NULL
      )
  })

  output$tabla_mecanismo_monto <- renderTable({
    mecanismo_monto |>
      transmute(
        Variable = variable,
        `Grados de libertad` = gl,
        Deviance = fmt_dec(deviance, 1),
        `p-valor` = format.pval(p_valor, digits = 2, eps = 1e-4)
      )
  })

  output$tabla_imputacion <- renderTable({
    comparacion_imputacion |>
      transmute(
        `Método` = if_else(
          metodo == metodo_elegido,
          paste(metodo, "(elegido)"),
          metodo
        ),
        MAPE = fmt_pct(mape),
        `Error típico del total` = fmt_pct(error_total_abs, 0.1),
        `Sesgo del total` = fmt_pct(error_total, 0.1),
        `Dispersión conservada` = fmt_pct(dispersion)
      )
  })

  output$texto_imputacion <- renderUI({
    tagList(
      p(
        "Se ocultaron al azar el 20 % de los montos conocidos, eligiéndolos",
        "con la probabilidad estimada de que falten (para imitar el mecanismo",
        "real), y se imputaron con cada método.", n_repeticiones,
        "repeticiones. Los grupos son las combinaciones de equipo y tipo de",
        "registro."
      ),
      tags$ul(
        tags$li(
          strong("Ningún método predice bien cada monto"), " (MAPE de 100 %",
          "o más): el monto casi no se relaciona con las demás variables."
        ),
        tags$li(
          strong("Las medianas subestiman el total"), " en más de un 50 %,",
          "porque la distribución del monto es muy asimétrica."
        ),
        tags$li(
          strong("Las medias estiman bien el total."), " La media por grupo",
          "lo hace prácticamente sin sesgo, con un error similar al de la",
          "media global, y respeta la variable de la que más depende el",
          "faltante (que también cambia el monto medio). La regresión no la",
          "mejora y el valor al azar agrega ruido. Se usa solo para los",
          "totales (ingresos ganados y forecast)."
        ),
        tags$li(
          "Como las imputaciones no conservan la variabilidad, los gráficos",
          "y medidas de la distribución del monto usan solo los montos",
          "cargados."
        )
      )
    )
  })

  output$tabla_crm <- renderDT({
    req(input$columnas_tabla)

    datatable(
      crm |> select(all_of(input$columnas_tabla)),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })


  # Panorama ------------------------------------------------------------------

  # Leads filtrados por fecha y equipo
  crm_panorama <- reactive({
    req(input$fechas_panorama, input$equipo_panorama)

    crm |>
      filter(
        between(
          fecha_creacion,
          input$fechas_panorama[1],
          input$fechas_panorama[2]
        ),
        team %in% input$equipo_panorama
      )
  })

  cerradas_panorama <- reactive({
    crm_panorama() |>
      filter(resultado != "Abierta", !es_duplicado)
  })

  output$vb_leads <- renderText(fmt_num(nrow(crm_panorama())))

  output$vb_tasa <- renderText({
    fmt_pct(mean(cerradas_panorama()$ganada), 0.1)
  })

  # Los totales usan el monto imputado (media del grupo) cuando falta
  output$vb_ingresos <- renderText({
    cerradas_panorama() |>
      filter(ganada) |>
      pull(expected_revenue_imp) |>
      sum() |>
      fmt_usd()
  })

  output$vb_ingresos_nota <- renderText({
    ganadas <- filter(cerradas_panorama(), ganada)
    paste0(
      "incluye ", sum(ganadas$monto_imputado), " montos imputados (",
      fmt_pct(
        sum(ganadas$expected_revenue_imp[ganadas$monto_imputado]) /
          sum(ganadas$expected_revenue_imp)
      ),
      " del total)"
    )
  })

  output$vb_ciclo <- renderText({
    dias <- cerradas_panorama() |>
      filter(ganada) |>
      pull(ciclo_dias) |>
      median()

    paste(round(dias), "días")
  })

  output$grafico_evolucion <- renderPlotly({
    por_mes <- crm_panorama() |>
      filter(!es_duplicado) |>
      count(mes_creacion, resultado) |>
      group_by(mes_creacion) |>
      mutate(prop = n / sum(n)) |>
      ungroup()

    eje_y <- if (input$escala_evolucion == "prop") {
      scale_y_continuous(labels = fmt_pct)
    } else {
      scale_y_continuous(labels = fmt_num)
    }

    grafico <- ggplot(
      por_mes,
      aes(mes_creacion, .data[[input$escala_evolucion]], fill = resultado)
    ) +
      geom_col() +
      scale_fill_manual(values = colores_resultado) +
      scale_x_date(date_labels = "%m/%Y") +
      eje_y +
      labs(x = NULL, y = NULL, fill = NULL)

    ggplotly(grafico) |>
      layout(legend = list(orientation = "h", y = 1.1))
  })

  embudo <- reactive(calcular_embudo(cerradas_panorama()))

  output$grafico_embudo <- renderPlot({
    ggplot(embudo(), aes(prop, fct_rev(etapa))) +
      geom_col(fill = color_principal, width = 0.7) +
      geom_text(
        aes(label = paste0(fmt_num(n), " (", fmt_pct(prop), ")")),
        hjust = -0.1,
        size = 4.5
      ) +
      scale_x_continuous(
        labels = fmt_pct,
        limits = c(0, 1.3),
        breaks = seq(0, 1, 0.25)
      ) +
      labs(x = "% de las oportunidades cerradas", y = NULL)
  })

  output$texto_embudo <- renderText({
    conversion <- embudo()$conversion

    paste0(
      "Conversión entre etapas: New → Qualified ", fmt_pct(conversion[2]),
      "; Qualified → Proposition ", fmt_pct(conversion[3]),
      "; Proposition → Won ", fmt_pct(conversion[4]), "."
    )
  })

  output$grafico_motivos <- renderPlot({
    cerradas_panorama() |>
      filter(resultado == "Perdida") |>
      count(motivo_perdida) |>
      mutate(prop = n / sum(n)) |>
      ggplot(aes(prop, fct_reorder(motivo_perdida, prop))) +
      geom_col(fill = colores_resultado[["Perdida"]]) +
      geom_text(aes(label = fmt_pct(prop)), hjust = -0.2, size = 4) +
      scale_x_continuous(
        labels = fmt_pct,
        expand = expansion(mult = c(0, 0.15))
      ) +
      labs(x = "% de las oportunidades perdidas", y = NULL)
  })


  # Factores de éxito ---------------------------------------------------------

  # Oportunidades cerradas en el rango de fechas elegido. Si la variable es la
  # etiqueta, se usa la tabla con una fila por lead y etiqueta.
  datos_factores <- reactive({
    req(input$fechas_factores)

    base <- if (input$variable_exito == "tags") crm_tags else crm

    base |>
      filter(
        resultado != "Abierta",
        !es_duplicado,
        between(
          fecha_creacion,
          input$fechas_factores[1],
          input$fechas_factores[2]
        )
      )
  })

  output$grafico_exito <- renderPlot({
    tabla <- tasa_por_grupo(
      datos_factores(),
      input$variable_exito,
      input$min_casos
    )

    validate(need(
      nrow(tabla) > 0,
      "Ninguna categoría alcanza el mínimo de oportunidades elegido."
    ))

    nombre <- names(variables_exito)[variables_exito == input$variable_exito]
    tasa_global <- mean(
      cerradas$ganada[between(
        cerradas$fecha_creacion,
        input$fechas_factores[1],
        input$fechas_factores[2]
      )]
    )

    graficar_tasa(
      tabla,
      tasa_global,
      paste("Tasa de éxito según", str_to_lower(nombre)),
      input$tipo_grafico
    )
  })

  output$comentario_variable <- renderUI({
    p(
      class = "text-muted",
      comentarios_variables[[input$variable_exito]]
    )
  })

  output$grafico_numerica <- renderPlot({
    variable <- input$variable_numerica

    grafico <- datos_factores() |>
      distinct(gen_id, .keep_all = TRUE) |>
      filter(!is.na(.data[[variable]]), .data[[variable]] > 0) |>
      ggplot(aes(.data[[variable]], fill = resultado)) +
      geom_density(alpha = 0.6, color = NA) +
      scale_fill_manual(values = colores_resultado) +
      labs(x = NULL, y = "Densidad", fill = NULL)

    if (input$escala_log) {
      grafico + scale_x_log10(labels = fmt_num)
    } else {
      grafico + scale_x_continuous(labels = fmt_num)
    }
  })

  output$tabla_numerica <- renderTable({
    variable <- input$variable_numerica

    datos_factores() |>
      distinct(gen_id, .keep_all = TRUE) |>
      group_by(Resultado = resultado) |>
      summarise(
        n = n(),
        Mediana = median(.data[[variable]], na.rm = TRUE),
        RIC = IQR(.data[[variable]], na.rm = TRUE),
        `Sin dato` = fmt_pct(mean(is.na(.data[[variable]])))
      ) |>
      mutate(across(c(Mediana, RIC), \(x) fmt_num(round(x))))
  })


  # Geografía -----------------------------------------------------------------

  mapa_elegido <- reactive({
    switch(
      input$nivel_mapa,
      paises = mapa_paises,
      estados = mapa_estados,
      ciudades = puntos_ciudades
    )
  })

  puntos_elegidos <- reactive({
    switch(
      input$nivel_mapa,
      paises = puntos_paises,
      estados = puntos_estados,
      ciudades = puntos_ciudades
    )
  })

  output$mapa_leaflet <- renderLeaflet({
    puntos <- puntos_elegidos()
    indicador <- input$indicador_mapa
    titulo <- names(indicadores_mapa)[indicadores_mapa == indicador]
    radio <- ~ 3 + 25 * sqrt(leads / max(leads))

    # Mapas de fondo de Esri, que no requieren clave de API (los de CARTO sí)
    mapa <- leaflet() |>
      addProviderTiles("Esri.WorldGrayCanvas", group = "Mapa claro") |>
      addProviderTiles("Esri.WorldImagery", group = "Satelital")

    if (input$nivel_mapa == "ciudades") {
      # Marcadores: un círculo por ciudad, coloreado según el indicador, y una
      # capa con un marcador por lead que se agrupan según el zoom
      paleta <- paleta_indicador(indicador, puntos[[indicador]])
      capas <- c("Ciudades", "Leads (agrupados)")

      mapa <- mapa |>
        addCircleMarkers(
          data = puntos,
          lng = ~lng,
          lat = ~lat,
          radius = radio,
          fillColor = ~ paleta(puntos[[indicador]]),
          fillOpacity = 0.8,
          color = "white",
          weight = 1,
          label = etiquetas_mapa(puntos),
          group = "Ciudades"
        ) |>
        addMarkers(
          data = leads_ciudad,
          lng = ~lng,
          lat = ~lat,
          label = ~ paste0(gen_id, " · ", city, " · ", resultado),
          clusterOptions = markerClusterOptions(),
          group = "Leads (agrupados)"
        ) |>
        hideGroup("Leads (agrupados)")
      valores <- puntos[[indicador]]
    } else {
      # Mapa coroplético: polígonos coloreados según el indicador y círculos
      # proporcionales a la cantidad de leads
      regiones <- mapa_elegido()
      paleta <- paleta_indicador(indicador, regiones[[indicador]])
      capas <- c("Regiones", "Cantidad de leads")

      mapa <- mapa |>
        addPolygons(
          data = regiones,
          fillColor = ~ paleta(regiones[[indicador]]),
          fillOpacity = 0.75,
          color = "white",
          weight = 1,
          label = etiquetas_mapa(regiones),
          highlightOptions = highlightOptions(weight = 3, color = "#264653"),
          group = "Regiones"
        ) |>
        addCircleMarkers(
          data = puntos,
          lng = ~lng,
          lat = ~lat,
          radius = radio,
          color = color_principal,
          weight = 1,
          fillOpacity = 0.35,
          label = etiquetas_mapa(puntos),
          group = "Cantidad de leads"
        )
      valores <- regiones[[indicador]]
    }

    mapa |>
      addLegend(
        position = "bottomright",
        pal = paleta,
        values = valores,
        title = titulo,
        labFormat = labelFormat(
          transform = if (indicador == "tasa") \(x) 100 * x else identity,
          suffix = if (indicador == "tasa") " %" else "",
          big.mark = "."
        ),
        na.label = "Pocos casos"
      ) |>
      addLayersControl(
        baseGroups = c("Mapa claro", "Satelital"),
        overlayGroups = capas,
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  output$nota_mapa <- renderUI({
    nota <- if (input$nivel_mapa == "paises") {
      paste(
        "Singapur no tiene polígono en el mapa por su tamaño: aparece solo",
        "como círculo. A", sum(crm$pais_recuperado), "leads sin país se les",
        "recuperó el país a partir de la ciudad; no se ubican los",
        sum(crm$country_code == "Sin dato"), "que no tienen ninguno de los dos."
      )
    } else if (input$nivel_mapa == "ciudades") {
      paste(
        "Coordenadas obtenidas con la API de geocodificación de Open-Meteo",
        "(datos de GeoNames, CC BY 4.0). Activá la capa \"Leads (agrupados)\"",
        "para ver un marcador por lead, agrupados según el zoom. No se ubican",
        "los", sum(is.na(crm$city)), "leads sin ciudad."
      )
    } else {
      paste(
        "No se ubican los", sum(crm$country_code == "US" & is.na(crm$state_code)),
        "leads de Estados Unidos sin estado ni los", leads_estados_fuera,
        "de Alaska y Hawái, que no están en el mapa."
      )
    }
    p(class = "text-muted small", nota)
  })

  output$mapa_estatico <- renderPlot({
    world |>
      filter(continent != "Antarctica") |>
      left_join(st_drop_geometry(select(mapa_paises, iso_a2, tasa)),
                by = "iso_a2") |>
      st_transform(crs = "+proj=eqearth") |>
      ggplot() +
      geom_sf(aes(fill = tasa), color = "white", linewidth = 0.1) +
      scale_fill_gradient2(
        low = "#e76f51",
        mid = "#f4f1de",
        high = "#2a9d8f",
        midpoint = mean(cerradas$ganada),
        labels = fmt_pct,
        na.value = "#e5e5e5",
        name = "Tasa de éxito"
      ) +
      theme_void(base_size = 13) +
      theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm"))
  })

  output$tabla_mapa <- renderDT({
    mapa_elegido() |>
      st_drop_geometry() |>
      arrange(desc(leads)) |>
      transmute(
        Región = nombre,
        Leads = leads,
        Cerradas = cerradas,
        `Tasa de éxito` = tasa,
        `Ingresos ganados` = ingresos
      ) |>
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, dom = "tp")
      ) |>
      formatPercentage("Tasa de éxito", digits = 1, dec.mark = ",") |>
      formatCurrency("Ingresos ganados", digits = 0, mark = ".")
  })


  # Lead scoring --------------------------------------------------------------

  scores_elegidos <- reactive({
    req(input$modelos)

    scores_test |>
      filter(modelo %in% input$modelos)
  })

  output$tabla_metricas <- renderTable({
    scores_elegidos() |>
      calcular_metricas(top = input$top_pct / 100) |>
      transmute(
        Modelo = modelo,
        AUC = fmt_dec(auc),
        Brier = fmt_dec(brier),
        `Prob. media predicha` = fmt_pct(prob_media, 0.1),
        `Tasa real de éxito` = fmt_pct(tasa_real, 0.1),
        "Precisión top {input$top_pct} %" := fmt_pct(precision_top, 0.1)
      )
  })

  output$grafico_calibracion <- renderPlot({
    scores_elegidos() |>
      calcular_calibracion(input$n_grupos) |>
      ggplot(aes(predicha, observada)) +
      geom_abline(linetype = "dashed", color = "grey50") +
      geom_line(color = color_principal) +
      geom_point(color = color_principal, size = 2) +
      facet_wrap(~modelo) +
      coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
      scale_x_continuous(labels = fmt_pct) +
      scale_y_continuous(labels = fmt_pct) +
      labs(
        x = "Probabilidad media predicha",
        y = "Proporción observada de ganadas"
      )
  })

  output$grafico_scores <- renderPlot({
    scores_elegidos() |>
      mutate(resultado = if_else(ganada, "Ganada", "Perdida")) |>
      ggplot(aes(prob, fill = resultado)) +
      geom_density(alpha = 0.6, color = NA) +
      facet_wrap(~modelo) +
      scale_fill_manual(values = colores_resultado) +
      scale_x_continuous(labels = fmt_pct) +
      labs(x = "Probabilidad predicha", y = "Densidad", fill = NULL)
  })

  # Análisis de sensibilidad: forecast solo con los montos cargados y con los
  # montos faltantes imputados por la media del grupo
  output$tabla_pipeline <- renderTable({
    abiertas <- filter(crm, resultado == "Abierta")

    resumir <- function(monto, escenario) {
      tibble(
        Escenario = escenario,
        `Oportunidades con monto` = sum(!is.na(monto)),
        `Monto total` = sum(monto, na.rm = TRUE),
        `Forecast Odoo (default)` = sum(
          monto * abiertas$odoo_pls_default,
          na.rm = TRUE
        ),
        `Forecast Odoo (extended)` = sum(
          monto * abiertas$odoo_pls_extended,
          na.rm = TRUE
        )
      )
    }

    bind_rows(
      resumir(abiertas$expected_revenue, "Solo montos cargados"),
      resumir(abiertas$expected_revenue_imp, "Con montos imputados")
    ) |>
      mutate(across(c(starts_with("Monto"), starts_with("Forecast")), fmt_usd))
  })
}
