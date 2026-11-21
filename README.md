# Trabajo Final · Análisis Inteligente de Datos

**¿Qué hace que una oportunidad se gane?** Aplicación Shiny que analiza el pipeline comercial de un CRM Odoo y evalúa qué tan bien predice el resultado su módulo de *Predictive Lead Scoring*, en comparación con dos modelos AutoML.

## Cómo ejecutar la app

``` r
shiny::runGitHub("trabajo-final-rochivega97", "aid-austral-2025")
```

O, desde una copia local del repositorio, con la raíz como directorio de trabajo:

``` r
shiny::runApp()
```

Paquetes necesarios: `shiny`, `bslib`, `tidyverse`, `scales`, `plotly`, `DT`, `naniar`, `sf`, `leaflet` y `spData` (más `httr2` para volver a geocodificar las ciudades).

``` r
install.packages(c(
  "shiny", "bslib", "tidyverse", "scales", "plotly", "DT", "naniar",
  "sf", "leaflet", "spData"
))
```

## Estructura del repositorio

| Ruta | Contenido |
|---|---|
| `app.R` | Punto de entrada de la app: carga los paquetes, corre la limpieza y el código de la app |
| `codigo/00_preambulo_funciones.R` | Paquetes y funciones propias de validación (tipos, orden, claves, referencias, rangos y fechas) y de transformación |
| `codigo/01_limpieza_leads.R` | Limpieza de `leads.csv` |
| `codigo/02_limpieza_partners.R` | Limpieza de `partners.csv` |
| `codigo/03_limpieza_scores.R` | Limpieza de los scores de Odoo y del benchmark |
| `codigo/04_consolidacion.R` | Unión de las tablas limpias en un único data frame con una fila por lead |
| `codigo/05_faltantes.R` | Diagnóstico y tratamiento de los valores faltantes |
| `codigo/06_resultado.R` | Archivo final y tablas derivadas que usa la app |
| `codigo/07_georreferencia.R` | Indicadores por país, por estado de EE.UU. y por ciudad, unidos a los mapas de `spData` y a las coordenadas de las ciudades |
| `codigo/geocodificar_ciudades.R` | Script auxiliar (se corre una sola vez, no lo ejecuta la app) que obtiene las coordenadas de las ciudades |
| `codigo/funciones.R` | Funciones auxiliares de formato, cálculo de métricas y gráficos |
| `codigo/ui.R` | Interfaz de usuario |
| `codigo/server.R` | Lógica del servidor |
| `datos_crudos/` | Datos originales |
| `datos_salida/` | Tablas limpias generadas por los scripts `01` a `03` |
| `resultados/crm_unificado.txt` | Tabla unificada final generada por `06_resultado.R` |
| `disenio_registro.csv` | Diseño de registro: nombre, orden y tipo esperado de las variables de cada tabla limpia |

## Limpieza

Cada script de limpieza sigue los mismos pasos: carga, inspección, validación con funciones propias (que informan en la consola cada control con ✔ o ✘), transformación, control posterior a la transformación y guardado de la tabla limpia en `datos_salida/`. Luego, `04_consolidacion.R` lee las tablas limpias con los tipos del diseño de registro, verifica la consistencia entre ellas y las une.

Los scripts deben correrse en orden (`01` a `07`), con la raíz del repositorio como directorio de trabajo. Al iniciar, la app los ejecuta sin imprimir los controles; para verlos, correr cada script por separado.

### Valores faltantes

`05_faltantes.R` sigue las reglas prácticas vistas en clase:

1. **Separar los faltantes estructurales de los imputables.** Cada regla estructural (por ejemplo, `date_closed` falta si y solo si la oportunidad sigue abierta) se verifica con los datos.
2. **Formular una hipótesis sobre el mecanismo.** Los faltantes no estructurales no son MCAR: los leads sin monto, cargo, teléfono o país se ganan mucho menos. Por eso, en las variables categóricas el faltante se conserva como una categoría explícita (`"Sin dato"`) en lugar de imputar la moda. La ausencia del monto depende sobre todo del equipo y del tipo de registro, lo que es compatible con un mecanismo MAR.
3. **Comparar métodos contra un baseline simple.** Para el monto se compararon seis métodos ocultando montos conocidos con la probabilidad estimada de que falten. Ninguno predice bien cada monto individual y las medianas subestiman los totales en más de un 50 %. Se eligió la media por grupo (equipo y tipo de registro), que estima el total prácticamente sin sesgo. El monto original (`expected_revenue`) se conserva sin imputar; `expected_revenue_imp` y `monto_imputado` se usan solo para calcular totales.

## Datos

Los datos corresponden a una exportación del CRM **Odoo** (modelo `crm.lead`) de una empresa que comercializa mobiliario de oficina. Para no exponer información comercial real, los registros fueron **generados sintéticamente**, manteniendo la estructura de tablas y campos de Odoo.

| Archivo | Contenido |
|---|---|
| `datos_crudos/leads.csv` | 3.386 leads y oportunidades: origen, vendedor, país, etapa, fechas, montos y resultado |
| `datos_crudos/partners.csv` | 446 clientes: ubicación y tipo de cuenta |
| `datos_crudos/odoo_pls_scores_at_cutoff.csv` | Probabilidad de cierre asignada por el PLS de Odoo (en %) |
| `datos_crudos/ciudades_coordenadas.csv` | Coordenadas de las 77 ciudades, de la [API de geocodificación de Open-Meteo](https://open-meteo.com/en/docs/geocoding-api) (datos de [GeoNames](https://www.geonames.org), CC BY 4.0) |
| `datos_crudos/benchmark_test_scores.csv` | Probabilidades de los modelos AutoML y Odoo en el conjunto de prueba |

Las variables de `leads.csv` y `partners.csv` cuyo nombre empieza con `_` son internas del proceso de generación de los datos y se descartan durante la limpieza.

## Georreferencia

`07_georreferencia.R` resume, por país y por estado de EE.UU., la cantidad de leads, la tasa de éxito (solo con al menos 20 oportunidades cerradas) y los ingresos ganados. Luego los une con los mapas del paquete `spData`: `world` para los países, por código ISO, y `us_states` para los estados, cuyo código de dos letras se traduce a nombre con `state.abb` y `state.name`. La unión se valida igual que el resto de las tablas:

- Singapur no tiene polígono en `world` por su tamaño, así que se muestra como punto.
- Alaska y Hawái no están en `us_states` (son 2 leads).

Las ciudades no traen coordenadas. `geocodificar_ciudades.R` las obtiene de la API de geocodificación de Open-Meteo (una consulta por segundo) y las guarda en `datos_crudos/ciudades_coordenadas.csv`, así la app no depende de internet al arrancar. Para distinguir ciudades homónimas se usa el estado más frecuente de cada ciudad en los leads. Para validar el resultado se comprobó que el país y el estado devueltos coinciden con los cargados en los leads. La geocodificación se aprovecha además en dos pasos:

- **Recuperar el país de 70 leads.** Esos leads tenían la ciudad cargada pero no el país. Como cada ciudad corresponde a un único país, el país se deduce de la ciudad (`pais_recuperado`). Esto se hace en `05_faltantes.R`.
- **Detectar 14 leads cuyo estado no corresponde a su ciudad.** Por ejemplo, Vancouver figura en Ontario. Se informan pero no se corrigen, porque no se sabe cuál de los dos datos es el erróneo.

La pestaña "Geografía" de la app incluye:

- Un mapa interactivo de `leaflet` por país, por estado o por ciudad, con:
  - polígonos coloreados según el indicador elegido;
  - círculos proporcionales a la cantidad de leads;
  - en el nivel de ciudades, una capa con un marcador por lead, agrupados según el zoom (`markerClusterOptions`);
  - control de capas para mostrar u ocultar los círculos y cambiar el mapa de fondo.
- Un mapa estático de `sf` y `ggplot2` en proyección Equal Earth, que conserva las áreas.
- Los mapas de base son de `spData`, que los toma de Natural Earth y de la Oficina del Censo de EE.UU.

