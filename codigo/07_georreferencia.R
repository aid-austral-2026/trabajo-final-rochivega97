#===============================================#
# MAESTRÍA EN CIENCIA DE DATOS - UNIV. AUSTRAL  #
#                                               #
# Materia: ANÁLISIS INTELIGENTE DE DATOS (2026) #
#                                               #
# Trabajo Final:                                #
#   "¿Qué hace que una oportunidad se gane?"    #
#===============================================#

#===============================================#
# ETAPA V. GEORREFERENCIA                       #
#===============================================#
# Une los indicadores comerciales por país y    #
# por estado de EE.UU. con los mapas del        #
# paquete spData, para graficarlos con sf y     #
# leaflet. Se ejecuta a continuación de         #
# 06_resultado.R (usa los objetos `crm` y       #
# `cerradas`).                                  #
#                                               #
# Objetos que deja disponibles para la app:     #
#   - mapa_paises   países con sus indicadores  #
#   - mapa_estados  estados de EE.UU. con sus   #
#                   indicadores                 #
#   - puntos_paises centroide de cada país      #
#   - puntos_estados centroide de cada estado   #
#   - puntos_ciudades coordenadas de cada       #
#                   ciudad con sus indicadores  #
#   - leads_ciudad  un punto por lead ubicado   #
#                   en su ciudad                #
#===============================================#

#-----------------------------------------------#
# 1. PREÁMBULO + FUNCIONES                      #
#-----------------------------------------------#

source("codigo/00_preambulo_funciones.R", encoding = "UTF-8")
suppressPackageStartupMessages({
  library(sf)      # datos geoespaciales
  library(spData)  # mapas de países (world) y de estados de EE.UU. (us_states)
})

if (!exists("cerradas")) source("codigo/06_resultado.R", encoding = "UTF-8")

# Mínimo de oportunidades cerradas para informar una tasa de éxito (el mismo
# criterio que en la pestaña "Factores de éxito")
min_cerradas_mapa <- 20


#-----------------------------------------------#
# 2. CARGA DE LOS MAPAS                         #
#-----------------------------------------------#

# 2.1. Países (CRS WGS84, el que usa leaflet):
mapa_mundo <- world |>
  select(iso_a2, name_long)

# 2.2. Estados de EE.UU. continental. Viene en NAD83: se transforma a WGS84
estados_eeuu <- us_states |>
  select(NAME) |>
  st_transform(4326)


#-----------------------------------------------#
# 3. INDICADORES POR REGIÓN                     #
#-----------------------------------------------#

# Leads, oportunidades cerradas, tasa de éxito (solo si hay suficientes
# cerradas) e ingresos ganados (con el monto imputado, ver 05_faltantes.R)
resumir_region <- function(datos_crm, datos_cerradas, region) {
  leads <- datos_crm |>
    count(region = .data[[region]], name = "leads")

  resultados <- datos_cerradas |>
    group_by(region = .data[[region]]) |>
    summarise(
      cerradas = n(),
      ganadas = sum(ganada),
      ingresos = sum(expected_revenue_imp[ganada]),
      .groups = "drop"
    )

  leads |>
    left_join(resultados, by = "region") |>
    mutate(
      across(c(cerradas, ganadas, ingresos), \(x) replace_na(x, 0)),
      tasa = if_else(cerradas >= min_cerradas_mapa, ganadas / cerradas, NA)
    )
}

# 3.1. Por país (sin los leads de país desconocido):
indicadores_paises <- resumir_region(
  filter(crm, country_code != "Sin dato"),
  filter(cerradas, country_code != "Sin dato"),
  "country_code"
)

# 3.2. Por estado de EE.UU. El mapa identifica a cada estado por su nombre y
#      los leads por su código postal de dos letras: se traduce el código con
#      los vectores state.abb y state.name de R base (más Washington D.C.)
nombres_estados <- c(set_names(state.name, state.abb),
                     DC = "District of Columbia")

indicadores_estados <- resumir_region(
  filter(crm, country_code == "US", !is.na(state_code)),
  filter(cerradas, country_code == "US", !is.na(state_code)),
  "state_code"
) |>
  mutate(nombre = nombres_estados[region])


# 3.3. Por ciudad (coordenadas de datos_crudos/ciudades_coordenadas.csv,
#      generado por geocodificar_ciudades.R):
indicadores_ciudades <- resumir_region(
  filter(crm, !is.na(city)),
  filter(cerradas, !is.na(city)),
  "city"
)


#-----------------------------------------------#
# 4. VALIDACIONES DE LA UNIÓN CON LOS MAPAS     #
#-----------------------------------------------#

# 4.1. Países que no tienen polígono en el mapa:
verificar_referencias(indicadores_paises, "region", mapa_mundo, "iso_a2",
                      nombre_maestro = "el mapa de países")
# --> Singapur (SG) no está en `world` por su tamaño: se muestra solo como
#     punto, con las coordenadas de su centro

# 4.2. Estados sin nombre o sin polígono (Alaska y Hawái no están en el mapa):
informar(if (!anyNA(indicadores_estados$nombre)) "  ✔ " else "  ✘ ",
         "Códigos de estado sin traducción: ",
         sum(is.na(indicadores_estados$nombre)), "\n")
leads_estados_fuera <- verificar_referencias(
  indicadores_estados, "nombre", estados_eeuu, "NAME",
  nombre_maestro = "el mapa de estados"
)
informar("  Leads en estados que no están en el mapa: ", leads_estados_fuera,
         "\n")
informar("  Leads de EE.UU. sin estado (no se ubican en el mapa): ",
         sum(crm$country_code == "US" & is.na(crm$state_code)), "\n")

# 4.3. Ciudades sin coordenadas:
verificar_referencias(indicadores_ciudades, "region", ciudades_coordenadas,
                      "city", nombre_maestro = "la tabla de coordenadas")

# 4.4. Consistencia entre la ciudad y el estado cargado (EE.UU. y Canadá):
#      el estado de la geocodificación es el más frecuente de cada ciudad
codigos_estados <- c(
  set_names(state.abb, state.name), "District of Columbia" = "DC",
  "Alberta" = "AB", "British Columbia" = "BC", "Ontario" = "ON",
  "Quebec" = "QC", "Manitoba" = "MB", "Nova Scotia" = "NS"
)

estado_inconsistente <- crm |>
  filter(country_code %in% c("US", "CA"), !is.na(city), !is.na(state_code)) |>
  left_join(select(ciudades_coordenadas, city, estado), by = "city") |>
  filter(state_code != codigos_estados[estado])

informar(if (nrow(estado_inconsistente) == 0) "  ✔ " else "  ✘ ",
         nrow(estado_inconsistente), " leads con un estado que no corresponde ",
         "a su ciudad (p. ej., ",
         paste(head(unique(paste(estado_inconsistente$city,
                                 estado_inconsistente$state_code)), 3),
               collapse = ", "),
         ")\n")

registrar_chequeo(
  "Leads con un estado que no corresponde a su ciudad",
  nrow(estado_inconsistente),
  paste("No se corrige: no se sabe cuál de los dos datos es el erróneo;",
        "el mapa por estado usa el estado cargado")
)


#-----------------------------------------------#
# 5. UNIÓN DE LOS INDICADORES CON LOS MAPAS     #
#-----------------------------------------------#

# Nombres de los países en español, para las etiquetas del mapa
nombres_paises <- c(
  US = "Estados Unidos", GB = "Reino Unido", CA = "Canadá",
  DE = "Alemania", MX = "México", FR = "Francia", AU = "Australia",
  NL = "Países Bajos", IE = "Irlanda", BR = "Brasil", SG = "Singapur",
  AE = "Emiratos Árabes Unidos", IT = "Italia", ES = "España", IN = "India"
)

mapa_paises <- mapa_mundo |>
  inner_join(indicadores_paises, by = c("iso_a2" = "region")) |>
  mutate(nombre = nombres_paises[iso_a2])

mapa_estados <- estados_eeuu |>
  inner_join(indicadores_estados, by = c("NAME" = "nombre")) |>
  rename(nombre = NAME)

# Centroide de cada región, para los círculos proporcionales a la cantidad de
# leads. Se usa el polígono más grande de cada región para que, por ejemplo,
# el de Francia no se desplace hacia la Guayana Francesa.
centroide_principal <- function(mapa) {
  poligonos <- mapa |>
    mutate(fila = row_number()) |>
    st_cast("POLYGON", warn = FALSE)
  poligonos$area <- as.numeric(st_area(poligonos))

  centroides <- poligonos |>
    group_by(fila) |>
    slice_max(area, n = 1, with_ties = FALSE) |>
    ungroup() |>
    st_set_agr("constant") |>  # los atributos valen para todo el polígono
    st_centroid()

  coordenadas <- st_coordinates(centroides)
  centroides |>
    st_drop_geometry() |>
    mutate(lng = coordenadas[, "X"], lat = coordenadas[, "Y"]) |>
    select(!c(fila, area))
}

puntos_paises <- bind_rows(
  centroide_principal(mapa_paises) |> rename(region = iso_a2),
  # Singapur, que no tiene polígono en el mapa
  indicadores_paises |>
    filter(!region %in% mapa_paises$iso_a2) |>
    mutate(nombre = nombres_paises[region], lng = 103.82, lat = 1.35)
)

puntos_estados <- centroide_principal(mapa_estados)

puntos_ciudades <- indicadores_ciudades |>
  inner_join(ciudades_coordenadas, by = c("region" = "city")) |>
  mutate(nombre = paste0(region, " (", country_code, ")"))

# Un punto por lead, para el mapa de marcadores agrupados
leads_ciudad <- crm |>
  filter(!is.na(city)) |>
  select(gen_id, city, resultado) |>
  inner_join(select(ciudades_coordenadas, city, lat, lng), by = "city")


#-----------------------------------------------#
# 6. CONTROL                                    #
#-----------------------------------------------#

informar("\nGeorreferencia:\n")
informar("  ", sum(indicadores_paises$leads), " leads con país; ",
         sum(mapa_paises$leads), " ubicados en polígonos y ",
         sum(indicadores_paises$leads) - sum(mapa_paises$leads),
         " como punto\n")
informar("  ", sum(indicadores_estados$leads), " leads de EE.UU. con estado; ",
         sum(mapa_estados$leads), " ubicados en el mapa\n")
informar("  ", nrow(leads_ciudad), " leads ubicados en ", nrow(puntos_ciudades),
         " ciudades; ", sum(is.na(crm$city)), " leads sin ciudad\n")
if (mostrar_controles) {
  print(st_drop_geometry(mapa_paises) |> arrange(desc(leads)))
}
