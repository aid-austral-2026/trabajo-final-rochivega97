#===============================================#
# MAESTRÍA EN CIENCIA DE DATOS - UNIV. AUSTRAL  #
#                                               #
# Materia: ANÁLISIS INTELIGENTE DE DATOS (2026) #
#                                               #
# Trabajo Final:                                #
#   "¿Qué hace que una oportunidad se gane?"    #
#===============================================#

#===============================================#
# GEOCODIFICACIÓN DE LAS CIUDADES               #
#===============================================#
# Script auxiliar: se corre UNA sola vez (no lo #
# ejecuta la app) y guarda las coordenadas en   #
# datos_crudos/ciudades_coordenadas.csv.        #
#                                               #
# Fuente: API de geocodificación de Open-Meteo  #
# (https://open-meteo.com/en/docs/geocoding-api)#
# basada en datos de GeoNames                   #
# (https://www.geonames.org), licencia          #
# CC BY 4.0. No requiere clave; se hace una     #
# consulta por segundo.                         #
#===============================================#

#-----------------------------------------------#
# 1. PAQUETES                                   #
#-----------------------------------------------#

library(tidyverse)
library(httr2)  # consultas a la API de geocodificación


#-----------------------------------------------#
# 2. CIUDADES A GEOCODIFICAR                    #
#-----------------------------------------------#

leads_raw <- read_csv("datos_crudos/leads.csv", show_col_types = FALSE)

# País de cada ciudad, según los leads que lo tienen cargado (cada ciudad
# aparece con un único país)
pais_ciudad <- leads_raw |>
  filter(!is.na(city), !is.na(country_code)) |>
  distinct(city, country_code)

# Estado más frecuente de cada ciudad (para distinguir, por ejemplo, entre
# ciudades homónimas de distintos estados de EE.UU.)
estado_ciudad <- leads_raw |>
  filter(!is.na(city), !is.na(state_code)) |>
  count(city, country_code, state_code) |>
  slice_max(n, n = 1, by = c(city, country_code), with_ties = FALSE) |>
  select(city, country_code, state_code)

nombres_provincias_ca <- c(
  AB = "Alberta", BC = "British Columbia", MB = "Manitoba",
  NB = "New Brunswick", NL = "Newfoundland and Labrador", NS = "Nova Scotia",
  ON = "Ontario", PE = "Prince Edward Island", QC = "Quebec",
  SK = "Saskatchewan"
)

ciudades <- leads_raw |>
  filter(!is.na(city)) |>
  distinct(city) |>
  left_join(pais_ciudad, by = "city") |>
  left_join(estado_ciudad, by = c("city", "country_code")) |>
  mutate(
    estado = case_when(
      country_code == "US" ~ state.name[match(state_code, state.abb)],
      country_code == "CA" ~ nombres_provincias_ca[state_code],
      TRUE ~ NA
    )
  ) |>
  arrange(country_code, city)


#-----------------------------------------------#
# 3. CONSULTA A LA API                          #
#-----------------------------------------------#

# Devuelve hasta 10 lugares con ese nombre (ordenados por relevancia). Si se
# conoce el estado, se elige el lugar de ese estado; si no, el primero.
# Ciudades que la API reconoce con otro nombre
nombres_alternativos <- c("Ciudad de México" = "Mexico City")

geocodificar <- function(ciudad, pais, estado) {
  nombre <- if (ciudad %in% names(nombres_alternativos)) {
    nombres_alternativos[[ciudad]]
  } else {
    ciudad
  }

  pedido <- request("https://geocoding-api.open-meteo.com/v1/search") |>
    req_url_query(
      name = nombre,
      countryCode = if (is.na(pais)) NULL else pais,
      count = 10,
      language = "en",
      format = "json"
    ) |>
    req_user_agent("aid-trabajo-final-crm/1.0 (script academico en R)") |>
    req_throttle(rate = 1)  # una consulta por segundo

  lugares <- resp_body_json(req_perform(pedido))$results

  if (length(lugares) == 0) {
    return(tibble(lat = NA_real_, lng = NA_real_, pais_api = NA_character_,
                  estado_api = NA_character_))
  }

  estados <- map_chr(lugares, \(x) x$admin1 %||% NA_character_)
  elegido <- if (!is.na(estado) && estado %in% estados) {
    lugares[[match(estado, estados)]]
  } else {
    lugares[[1]]
  }

  tibble(
    lat = elegido$latitude,
    lng = elegido$longitude,
    pais_api = elegido$country_code,
    estado_api = elegido$admin1 %||% NA_character_
  )
}

coordenadas <- ciudades |>
  mutate(resultado = pmap(list(city, country_code, estado), geocodificar)) |>
  unnest(resultado)


#-----------------------------------------------#
# 4. CONTROL Y GUARDADO                         #
#-----------------------------------------------#

# Todas las ciudades tienen que tener coordenadas, el país devuelto tiene que
# coincidir con el cargado en los leads y, si se conoce, también el estado
cat("Ciudades sin coordenadas:", sum(is.na(coordenadas$lat)), "\n")
cat("Ciudades con país distinto al de los leads:",
    sum(coordenadas$pais_api != coordenadas$country_code, na.rm = TRUE), "\n")
cat("Ciudades con estado distinto al más frecuente en los leads:",
    sum(coordenadas$estado_api != coordenadas$estado, na.rm = TRUE), "\n")
print(filter(coordenadas, is.na(lat) | is.na(country_code)))

coordenadas |>
  select(city, country_code = pais_api, estado = estado_api, lat, lng) |>
  write_csv("datos_crudos/ciudades_coordenadas.csv")
