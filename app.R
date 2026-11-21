##############################################################################
# app.R
#
# Trabajo final · Análisis Inteligente de Datos
# Pipeline comercial de un CRM Odoo y evaluación de su Predictive Lead Scoring
#
# Punto de entrada de la aplicación Shiny. El código se organiza en la carpeta
# codigo/ y debe ejecutarse con la raíz del repositorio como directorio de
# trabajo:
#   codigo/00_preambulo_funciones.R  paquetes y funciones de validación
#   codigo/01_limpieza_leads.R       limpieza de leads.csv
#   codigo/02_limpieza_partners.R    limpieza de partners.csv
#   codigo/03_limpieza_scores.R      limpieza de los scores de los modelos
#   codigo/04_consolidacion.R        unión de las tablas limpias
#   codigo/05_faltantes.R            diagnóstico y tratamiento de faltantes
#   codigo/06_resultado.R            archivo final y tablas para la app
#   codigo/07_georreferencia.R       indicadores por país y estado para mapas
#   codigo/funciones.R               formato, cálculo de métricas y gráficos
#   codigo/ui.R                      interfaz de usuario
#   codigo/server.R                  lógica del servidor
##############################################################################

# Paquetes --------------------------------------------------------------------

library(shiny)      # aplicación web
library(bslib)      # diseño de la interfaz (Bootstrap 5)
library(tidyverse)  # lectura, manipulación y visualización de datos
library(scales)     # formato de números, porcentajes y moneda
library(plotly)     # gráficos interactivos
library(DT)         # tablas interactivas
library(naniar)     # resúmenes de valores faltantes
library(sf)         # datos geoespaciales
library(leaflet)    # mapas interactivos
library(spData)     # mapas de países y de estados de EE.UU.


# Limpieza --------------------------------------------------------------------

# Los scripts imprimen sus validaciones en la consola; al iniciar la app se
# ejecutan en silencio. Para ver los controles, correr cada script por separado.
options(mostrar_controles = FALSE)

scripts_limpieza <- c(
  "codigo/01_limpieza_leads.R",
  "codigo/02_limpieza_partners.R",
  "codigo/03_limpieza_scores.R",
  "codigo/04_consolidacion.R",
  "codigo/05_faltantes.R",
  "codigo/06_resultado.R",
  "codigo/07_georreferencia.R"
)
for (script in scripts_limpieza) source(script, encoding = "UTF-8")


# Código de la app ------------------------------------------------------------

source("codigo/funciones.R", encoding = "UTF-8")
source("codigo/ui.R", encoding = "UTF-8")
source("codigo/server.R", encoding = "UTF-8")

shinyApp(ui, server)
