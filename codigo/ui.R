##############################################################################
# ui.R
#
# Interfaz de usuario de la app. Se organiza en seis pestañas que siguen las
# etapas del Análisis Inteligente de Datos:
#   Inicio y Datos            -> selección y limpieza
#   Panorama, Factores y      -> exploración (con controles interactivos)
#   Lead scoring
#   Conclusiones              -> comunicación de los hallazgos
##############################################################################

# Rango de fechas disponible, usado como valor inicial de los filtros
fecha_min <- min(crm$fecha_creacion)
fecha_max <- max(crm$fecha_creacion)


# Pestaña 1: Inicio -----------------------------------------------------------

tab_inicio <- nav_panel(
  "Inicio",
  layout_columns(
    col_widths = c(7, 5),
    card(
      card_header("El problema"),
      p(
        "Las empresas que venden a otras empresas registran cada contacto",
        "comercial en un CRM. En", strong("Odoo"), ", cada registro comienza",
        "como un", em("lead"), "o como una", em("oportunidad"), "y avanza",
        "por un embudo de etapas (New → Qualified → Proposition → Won) hasta",
        "ganarse o perderse."
      ),
      p(
        "Odoo incluye un módulo de", strong("Predictive Lead Scoring (PLS)"),
        "que asigna a cada oportunidad una probabilidad de cierre. Ese número",
        "se usa para priorizar el trabajo de los vendedores y para proyectar",
        "ingresos, así que conviene saber qué tan confiable es."
      ),
      p("Esta aplicación busca responder dos preguntas:"),
      tags$ol(
        tags$li(
          strong("¿Qué características de una oportunidad se asocian a que se gane?"),
          "Se analizan el origen, el país, la prioridad, el vendedor, el tipo",
          "de cliente, el cargo del contacto y la calidad de los datos."
        ),
        tags$li(
          strong("¿Qué tan bien predice el PLS de Odoo el resultado real?"),
          "Se lo compara con dos modelos de AutoML entrenados con los mismos datos."
        )
      )
    ),
    card(
      card_header("Cómo usar la app"),
      tags$ul(
        tags$li(strong("Datos:"), " fuente, limpieza y tabla unificada."),
        tags$li(
          strong("Panorama:"),
          " indicadores generales, evolución y embudo, filtrables por fecha y equipo."
        ),
        tags$li(
          strong("Factores de éxito:"),
          " elegí la variable a analizar y el tipo de gráfico."
        ),
        tags$li(
          strong("Lead scoring:"),
          " compará los modelos y ajustá los parámetros de evaluación."
        ),
        tags$li(strong("Conclusiones:"), " principales hallazgos.")
      )
    )
  ),
  card(
    card_header("Fuente de los datos"),
    p(
      "Los datos provienen de una exportación de un CRM Odoo de una empresa",
      "que comercializa mobiliario y equipamiento de oficina. Para no exponer",
      "información comercial real, los registros fueron",
      strong("generados sintéticamente"), ". Mantienen la estructura del",
      "modelo", code("crm.lead"), "de Odoo y patrones de comportamiento",
      "similares a los de un pipeline real. Todos los archivos están en la",
      "carpeta", code("datos_crudos/"), "del repositorio."
    ),
    tableOutput("tabla_fuentes")
  )
)


# Pestaña 2: Datos y limpieza -------------------------------------------------

tab_datos <- nav_panel(
  "Datos",
  navset_card_underline(
    nav_panel(
      "Limpieza",
      p(
        "Las cuatro tablas se relacionan mediante", code("gen_id"),
        "(identificador del lead) y", code("gen_partner_id"),
        "(identificador del cliente). Antes de unirlas en un único data frame",
        "con una fila por lead se realizaron las siguientes verificaciones:"
      ),
      DTOutput("tabla_chequeos", fill = FALSE),
      p(
        class = "mt-3",
        "Las claves son únicas y consistentes, así que la unión se hizo con",
        em("left joins"), "tomando a", code("leads.csv"), "como tabla base.",
        "Las 37 variables con prefijo", code("_"), "son internas del",
        "proceso que generó los datos (por ejemplo, la probabilidad \"real\"",
        "de cada lead): no existirían en una exportación real y filtrarían el",
        "resultado que se quiere explicar, por lo que se eliminaron."
      )
    ),
    nav_panel(
      "Valores faltantes",
      p(
        "El tratamiento sigue tres reglas: primero se separan los faltantes",
        strong("estructurales"), "(el dato no corresponde, por ejemplo la",
        "fecha de cierre de una oportunidad abierta) de los", strong("no"),
        strong("estructurales"), "(el dato existía pero no se cargó); luego",
        "se estudia si estos últimos faltan al azar; por último, se comparan",
        "métodos de imputación contra un método simple y se informan los",
        "supuestos."
      ),
      card(
        full_screen = TRUE,
        card_header("Faltantes por variable"),
        checkboxGroupInput(
          "tipos_faltante",
          NULL,
          choices = c("Estructural", "Mixto", "No estructural"),
          selected = c("Estructural", "Mixto", "No estructural"),
          inline = TRUE
        ),
        plotOutput("grafico_faltantes", height = "520px", fill = FALSE)
      ),
      card(
        full_screen = TRUE,
        card_header("Diccionario de faltantes"),
        p(
          class = "text-muted",
          "Cada regla estructural se verificó con los datos: la variable",
          "falta en todos los registros que cumplen la condición y solo en",
          "ellos. En las mixtas, la regla vale para una parte de los",
          "faltantes y el resto son datos no cargados."
        ),
        DTOutput("tabla_diccionario_faltantes", fill = FALSE)
      ),
      card(
        full_screen = TRUE,
        card_header("¿Los datos faltan al azar?"),
        plotOutput("grafico_mecanismo", height = "420px"),
        p(
          "Si los datos faltaran en forma completamente aleatoria (MCAR), la",
          "tasa de éxito sería la misma con o sin el dato. No es así: los",
          "leads sin monto, sin cargo, sin teléfono o sin país se ganan mucho",
          "menos, y los que no tienen origen registrado, algo más. Que falte",
          "el dato ", em("ya es información"), " sobre el resultado. Por eso,",
          "en las variables categóricas no se imputa la moda (borraría esa",
          "señal): el faltante se conserva como una categoría explícita",
          "(\"Sin dato\")."
        )
      ),
      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("¿De qué depende que falte el monto?"),
          p(
            "Modelo logístico para la probabilidad de que falte el monto,",
            "con variables observadas (sin incluir el resultado):"
          ),
          tableOutput("tabla_mecanismo_monto"),
          p(
            "La ausencia del monto depende sobre todo del equipo y del tipo",
            "de registro: falta en el 37 % de los registros tipo lead de",
            "Pre-Sales y solo en el 5 % de las oportunidades de Sales. Es",
            "compatible con un",
            "mecanismo", strong("MAR"), ", aunque no se puede descartar que",
            "sea MNAR (que el vendedor no cargue el monto justamente en las",
            "oportunidades de menor valor)."
          )
        ),
        card(
          card_header("Imputación del monto: comparación de métodos"),
          tableOutput("tabla_imputacion"),
          uiOutput("texto_imputacion")
        )
      )
    ),
    nav_panel(
      "Tabla unificada",
      p(
        "Data frame final con", strong(fmt_num(nrow(crm))), "filas y",
        strong(ncol(crm)), "columnas. Elegí las columnas a mostrar; la tabla",
        "se puede filtrar y ordenar."
      ),
      selectizeInput(
        "columnas_tabla",
        "Columnas",
        choices = names(crm),
        selected = c(
          "gen_id", "split", "vendedor", "source", "country_code",
          "nivel_cargo", "tipo_cliente", "prioridad", "expected_revenue",
          "resultado", "odoo_pls_default"
        ),
        multiple = TRUE,
        width = "100%"
      ),
      DTOutput("tabla_crm", fill = FALSE)
    )
  )
)


# Pestaña 3: Panorama ---------------------------------------------------------

tab_panorama <- nav_panel(
  "Panorama",
  layout_sidebar(
    sidebar = sidebar(
      title = "Filtros",
      dateRangeInput(
        "fechas_panorama",
        "Fecha de creación",
        start = fecha_min,
        end = fecha_max,
        min = fecha_min,
        max = fecha_max,
        format = "dd/mm/yyyy",
        language = "es",
        separator = "a"
      ),
      checkboxGroupInput(
        "equipo_panorama",
        "Equipo",
        choices = sort(unique(crm$team)),
        selected = unique(crm$team)
      ),
      radioButtons(
        "escala_evolucion",
        "Evolución mensual",
        choices = c("Cantidad de leads" = "n", "Proporción" = "prop")
      )
    ),
    layout_columns(
      fill = FALSE,
      value_box("Leads", textOutput("vb_leads"), theme = "primary"),
      value_box(
        "Tasa de éxito",
        textOutput("vb_tasa"),
        p("ganadas / cerradas"),
        theme = "success"
      ),
      value_box(
        "Ingresos ganados",
        textOutput("vb_ingresos"),
        textOutput("vb_ingresos_nota"),
        theme = "info"
      ),
      value_box(
        "Ciclo mediano",
        textOutput("vb_ciclo"),
        p("de una venta ganada"),
        theme = "secondary"
      )
    ),
    card(
      card_header("Leads creados por mes según su resultado"),
      plotlyOutput("grafico_evolucion", height = "350px")
    ),
    layout_columns(
      card(
        card_header("Embudo de ventas"),
        plotOutput("grafico_embudo", height = "320px"),
        textOutput("texto_embudo")
      ),
      card(
        card_header("Motivos de pérdida"),
        plotOutput("grafico_motivos", height = "320px")
      )
    )
  )
)


# Pestaña 4: Factores de éxito ------------------------------------------------

tab_factores <- nav_panel(
  "Factores de éxito",
  layout_sidebar(
    sidebar = sidebar(
      title = "Opciones",
      selectInput(
        "variable_exito",
        "Variable a analizar",
        choices = variables_exito
      ),
      radioButtons(
        "tipo_grafico",
        "Tipo de gráfico",
        choices = c(
          "Puntos con intervalo de confianza" = "puntos",
          "Barras" = "barras"
        )
      ),
      sliderInput(
        "min_casos",
        "Mínimo de oportunidades por categoría",
        min = 5,
        max = 100,
        value = 20,
        step = 5
      ),
      dateRangeInput(
        "fechas_factores",
        "Fecha de creación",
        start = fecha_min,
        end = fecha_max,
        min = fecha_min,
        max = fecha_max,
        format = "dd/mm/yyyy",
        language = "es",
        separator = "a"
      ),
      helpText(
        "Las tasas se calculan sobre las oportunidades cerradas, sin",
        "duplicados. Las barras de error son intervalos de confianza del 95 %."
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Tasa de éxito por categoría"),
      plotOutput("grafico_exito", height = "450px"),
      uiOutput("comentario_variable")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Monto y duración según el resultado"),
        radioButtons(
          "variable_numerica",
          NULL,
          choices = c(
            "Monto esperado (USD)" = "expected_revenue",
            "Duración del ciclo (días)" = "ciclo_dias"
          ),
          inline = TRUE
        ),
        checkboxInput("escala_log", "Escala logarítmica", value = TRUE),
        plotOutput("grafico_numerica", height = "300px")
      ),
      card(
        card_header("Medidas descriptivas"),
        tableOutput("tabla_numerica"),
        p(
          "Las oportunidades perdidas se cierran mucho más rápido: muchas se",
          "descartan tras el primer contacto. Además, una proporción mayor de",
          "ellas no tiene monto cargado. Solo se usan los montos cargados:",
          "los imputados no conservan la variabilidad del monto."
        )
      )
    )
  )
)


# Pestaña 5: Geografía --------------------------------------------------------

tab_geografia <- nav_panel(
  "Geografía",
  layout_sidebar(
    sidebar = sidebar(
      title = "Opciones",
      radioButtons(
        "nivel_mapa",
        "Nivel",
        choices = c(
          "Países" = "paises",
          "Estados de EE.UU." = "estados",
          "Ciudades" = "ciudades"
        )
      ),
      radioButtons(
        "indicador_mapa",
        "Color de cada región",
        choices = indicadores_mapa
      ),
      p(
        class = "text-muted small",
        "Los círculos son proporcionales a la cantidad de leads. Desde el",
        "control de capas del mapa se pueden mostrar u ocultar. La tasa de éxito solo",
        "se informa en las regiones con al menos", min_cerradas_mapa,
        "oportunidades cerradas."
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Mapa interactivo"),
      leafletOutput("mapa_leaflet", height = "520px"),
      uiOutput("nota_mapa")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header("Tasa de éxito por país (proyección Equal Earth)"),
        plotOutput("mapa_estatico", height = "560px", fill = FALSE),
        p(
          class = "text-muted",
          "Mapa estático hecho con", code("sf"), "y", code("ggplot2"), ".",
          "Se usa la proyección Equal Earth, que conserva las áreas: la de",
          "Mercator (la del mapa interactivo) agranda las regiones lejanas al",
          "Ecuador."
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Indicadores por región"),
        DTOutput("tabla_mapa", fill = FALSE)
      )
    ),
    card(
      card_header("Lectura"),
      p(
        "Estados Unidos concentra más de la mitad de los leads y tiene una",
        "de las tasas de éxito más altas (30 %), solo superada por los",
        "Emiratos Árabes Unidos, que tienen muy pocos casos. Fuera de",
        "Estados Unidos, Canadá e Irlanda, la conversión es más baja: en el",
        "Reino Unido, México, Francia, Australia, los Países Bajos y Brasil",
        "se gana menos de una de cada cinco oportunidades. Dentro de Estados",
        "Unidos, los leads se concentran en los estados más poblados",
        "(California, Texas, Nueva York y Florida). Entre ciudades las",
        "diferencias son mayores: en Orlando y Pittsburgh se gana más de la",
        "mitad de las oportunidades, y en Atlanta, Londres y Singapur menos",
        "de una de cada siete. Pero cada ciudad tiene entre 20 y 50",
        "oportunidades cerradas, así que estas tasas son poco precisas."
      )
    )
  )
)


# Pestaña 6: Lead scoring -----------------------------------------------------

tab_scoring <- nav_panel(
  "Lead scoring",
  layout_sidebar(
    sidebar = sidebar(
      title = "Opciones",
      checkboxGroupInput(
        "modelos",
        "Modelos",
        choices = levels(scores_test$modelo),
        selected = levels(scores_test$modelo)
      ),
      sliderInput(
        "n_grupos",
        "Grupos para la calibración",
        min = 4,
        max = 20,
        value = 10
      ),
      sliderInput(
        "top_pct",
        "Porcentaje de leads priorizados",
        min = 5,
        max = 50,
        value = 20,
        step = 5,
        post = " %"
      ),
      helpText(
        "La evaluación usa los", fmt_num(n_distinct(scores_test$gen_id)),
        "leads del conjunto de prueba (creados entre abril y octubre de 2026),",
        "que no se usaron para entrenar los modelos."
      )
    ),
    card(
      card_header("Desempeño de los modelos"),
      tableOutput("tabla_metricas"),
      tags$ul(
        tags$li(
          strong("AUC:"), " probabilidad de que el modelo le asigne un score",
          "mayor a una oportunidad ganada que a una perdida (0,5 = azar; 1 =",
          "perfecto)."
        ),
        tags$li(
          strong("Brier:"), " error cuadrático medio entre la probabilidad",
          "predicha y el resultado (más bajo es mejor)."
        ),
        tags$li(
          strong("Precisión:"), " proporción de ganadas entre los leads con",
          "mayor score, según el porcentaje elegido en el panel."
        )
      )
    ),
    layout_columns(
      card(
        full_screen = TRUE,
        card_header("Calibración"),
        plotOutput("grafico_calibracion", height = "420px"),
        p(
          "En un modelo bien calibrado los puntos caen sobre la diagonal. Los",
          "modelos AutoML quedan cerca de ella, mientras que los de Odoo",
          strong("sobreestiman"), "la probabilidad en los grupos de score alto."
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Distribución de los scores"),
        plotOutput("grafico_scores", height = "420px"),
        p(
          "Cuanto menos se superponen las curvas de ganadas y perdidas, mejor",
          "discrimina el modelo."
        )
      )
    ),
    card(
      card_header("Forecast del pipeline abierto"),
      p(
        "Ingreso esperado de las", sum(crm$resultado == "Abierta"),
        "oportunidades abiertas (monto × probabilidad de Odoo). Como Odoo",
        "sobreestima las probabilidades altas, estos valores deben leerse",
        "como un límite superior. La segunda fila completa los montos que",
        "faltan con la media de su grupo (ver Datos → Valores faltantes)."
      ),
      tableOutput("tabla_pipeline")
    )
  )
)


# Pestaña 7: Conclusiones -----------------------------------------------------

tab_conclusiones <- nav_panel(
  "Conclusiones",
  card(
    card_header("Principales hallazgos"),
    tags$ul(
      tags$li(
        strong("Datos:"), " la unión de las cuatro tablas fue directa porque",
        "las claves son únicas y consistentes. El trabajo de limpieza consistió",
        "en eliminar las variables internas de la simulación, normalizar el",
        "cargo del contacto (texto libre con más de 70 variantes), tratar los",
        "montos en cero como faltantes y excluir los duplicados. La mayoría",
        "de los faltantes son estructurales; los demás no faltan al azar (los",
        "leads sin monto, cargo o teléfono se ganan mucho menos), por lo que",
        "se conservan como una categoría propia. El monto solo se imputa,",
        "con la media de su grupo, para calcular totales."
      ),
      tags$li(
        strong("Embudo:"), " la mayor pérdida ocurre en la calificación: solo",
        "la mitad de los leads llega a calificarse. Cerca de la mitad de las",
        "pérdidas corresponde a clientes que dejan de responder o no tiene",
        "motivo registrado."
      ),
      tags$li(
        strong("Factores de éxito:"), " las oportunidades con más chances son",
        "las de clientes existentes, las que llegan por referidos, ferias o",
        "campañas de reactivación, las que tienen datos de contacto correctos,",
        "las de contactos con cargos directivos y las de mayor prioridad. Los",
        "leads sin vendedor asignado casi nunca se convierten, y los que llegan",
        "por redes sociales convierten muy poco."
      ),
      tags$li(
        strong("Lead scoring:"), " el PLS de Odoo ordena razonablemente bien",
        "los leads, pero queda por detrás de los modelos AutoML y sus",
        "probabilidades altas están sobreestimadas. Configurarlo con más",
        "variables (extended) no lo mejoró. Sirve para priorizar el trabajo",
        "de los vendedores, pero no conviene usar sus porcentajes directamente",
        "para proyectar ingresos sin antes recalibrarlos."
      )
    )
  )
)


# Página completa -------------------------------------------------------------

ui <- page_navbar(
  title = "Pipeline comercial · Odoo CRM",
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  fillable = FALSE,
  tab_inicio,
  tab_datos,
  tab_panorama,
  tab_factores,
  tab_geografia,
  tab_scoring,
  tab_conclusiones
)
