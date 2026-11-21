##############################################################################
# limpieza.R
#
# Etapas de SELECCIÓN y LIMPIEZA.
# Lee las cuatro tablas exportadas del CRM Odoo, verifica la consistencia de
# sus claves y las une en un único data frame (`crm`) con una fila por lead.
#
# Objetos que deja disponibles para la app:
#   - crm            data frame unificado y limpio
#   - cerradas       oportunidades cerradas (ganadas o perdidas), sin duplicados
#   - crm_tags       una fila por lead y etiqueta
#   - scores_test    scores de los cuatro modelos en el conjunto de prueba
#   - chequeos       tabla con las verificaciones realizadas durante la limpieza
#   - faltantes      porcentaje de valores faltantes por variable (datos crudos)
##############################################################################


# Lectura ---------------------------------------------------------------------

leads_raw <- read_csv("data/leads.csv", show_col_types = FALSE)
partners_raw <- read_csv("data/partners.csv", show_col_types = FALSE)
odoo_raw <- read_csv(
  "data/odoo_pls_scores_at_cutoff.csv",
  show_col_types = FALSE
)
benchmark_raw <- read_csv(
  "data/benchmark_test_scores.csv",
  show_col_types = FALSE
)


# Verificaciones previas a la unión -------------------------------------------

# Leads que tienen asociado un cliente, junto con los datos de ese cliente
leads_con_partner <- leads_raw |>
  inner_join(partners_raw, by = "gen_partner_id", suffix = c("", "_partner"))

# Scores de Odoo presentes en ambas tablas (benchmark en proporción, Odoo en %)
scores_ambas_tablas <- benchmark_raw |>
  inner_join(odoo_raw, by = "gen_id")

# Variable de resultado del benchmark junto al resultado registrado en leads
won_ambas_tablas <- benchmark_raw |>
  inner_join(leads_raw, by = "gen_id")

# Cargos distintos antes y después de ignorar mayúsculas y espacios
cargos <- na.omit(leads_raw$`function`)

chequeos <- tibble(
  chequeo = c(
    "Variables internas de la simulación (prefijo _) en leads",
    "gen_id duplicados en leads",
    "gen_partner_id duplicados en partners",
    "Leads con un partner inexistente en partners",
    "Scores de Odoo sin lead asociado",
    "Scores del benchmark sin lead asociado",
    "Países distintos entre el lead y su partner",
    "Diferencia máxima entre los scores de Odoo de ambas tablas",
    "Casos en que `won` (benchmark) difiere del resultado del lead",
    "Cargos distintos tal como fueron cargados",
    "Cargos distintos ignorando mayúsculas",
    "Leads con monto esperado igual a 0",
    "Leads cerrados como duplicados",
    "Leads con fecha de cierre anterior a la de creación"
  ),
  resultado = c(
    sum(str_starts(names(leads_raw), "_")),
    sum(duplicated(leads_raw$gen_id)),
    sum(duplicated(partners_raw$gen_partner_id)),
    sum(
      !is.na(leads_raw$gen_partner_id) &
        !leads_raw$gen_partner_id %in% partners_raw$gen_partner_id
    ),
    sum(!odoo_raw$gen_id %in% leads_raw$gen_id),
    sum(!benchmark_raw$gen_id %in% leads_raw$gen_id),
    sum(
      leads_con_partner$country_code != leads_con_partner$country_code_partner,
      na.rm = TRUE
    ),
    max(abs(
      scores_ambas_tablas$odoo_pls_default -
        scores_ambas_tablas$odoo_pls_default_pct / 100
    )),
    sum(won_ambas_tablas$won != (won_ambas_tablas$outcome == "won")),
    n_distinct(cargos),
    n_distinct(str_to_lower(str_squish(cargos))),
    sum(leads_raw$expected_revenue == 0),
    sum(leads_raw$lost_reason == "Duplicate", na.rm = TRUE),
    sum(leads_raw$date_closed < leads_raw$create_date, na.rm = TRUE)
  ),
  accion = c(
    "Se eliminan: no existen en Odoo y filtrarían el resultado",
    "Ninguna: la clave es única",
    "Ninguna: la clave es única",
    "Ninguna: todas las referencias son válidas",
    "Ninguna: todas las referencias son válidas",
    "Ninguna: todas las referencias son válidas",
    "Solo se incorporan tipo de cuenta y antigüedad del partner",
    "Se conserva una sola versión, expresada como proporción",
    "Se descarta `won` por redundante",
    "Se normaliza el texto y se corrigen abreviaturas y errores de tipeo",
    "Se agrupan en niveles de cargo",
    "Se recodifican como faltantes (monto no cargado)",
    "Se excluyen del cálculo de tasas de éxito",
    "Ninguna"
  )
)

# Porcentaje de faltantes por variable en los datos crudos
faltantes <- leads_raw |>
  select(!starts_with("_")) |>
  summarise(across(everything(), \(x) mean(is.na(x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "prop_na") |>
  filter(prop_na > 0) |>
  arrange(desc(prop_na))


# Funciones auxiliares de limpieza --------------------------------------------

# Corrige abreviaturas y errores de tipeo frecuentes en el cargo
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


# Unión de las tablas ---------------------------------------------------------

# Del partner solo interesan las variables que no están ya en el lead
partners <- partners_raw |>
  select(gen_partner_id, account_type, known_since)

# Scores de los cuatro modelos, todos expresados como proporción
scores <- odoo_raw |>
  transmute(
    gen_id,
    odoo_pls_default = odoo_pls_default_pct / 100,
    odoo_pls_extended = odoo_pls_extended_pct / 100
  ) |>
  left_join(
    benchmark_raw |> select(gen_id, automl_12_fields, automl_20_fields),
    by = "gen_id"
  )

crm <- leads_raw |>
  select(!starts_with("_")) |>
  rename(cargo_original = `function`) |>
  left_join(partners, by = "gen_partner_id") |>
  left_join(scores, by = "gen_id") |>
  mutate(
    # Resultado de la oportunidad
    resultado = factor(
      outcome,
      levels = c("won", "lost", "open"),
      labels = c("Ganada", "Perdida", "Abierta")
    ),
    ganada = outcome == "won",
    es_duplicado = lost_reason %in% "Duplicate",

    # Cargo normalizado y agrupado
    cargo = normalizar_cargo(cargo_original),
    nivel_cargo = factor(
      clasificar_cargo(cargo),
      levels = c(
        "Dirección", "Gerencia", "Diseño / Arquitectura", "Operativo",
        "Sin dato"
      )
    ),

    # Un monto igual a 0 significa que el vendedor no lo cargó
    expected_revenue = na_if(expected_revenue, 0),

    # Categorías explícitas para los faltantes
    source = replace_na(source, "Sin dato"),
    medium = replace_na(medium, "Sin dato"),
    country_code = replace_na(country_code, "Sin dato"),
    vendedor = replace_na(user_login, "Sin asignar"),
    motivo_perdida = replace_na(lost_reason, "Sin motivo registrado"),

    # Calidad de los datos de contacto
    calidad_contacto = case_when(
      email_state %in% "incorrect" | phone_state %in% "incorrect" ~
        "Email o teléfono incorrecto",
      email_state %in% "correct" & phone_state %in% "correct" ~
        "Ambos correctos",
      TRUE ~ "Incompleto"
    ),

    # Relación previa con el cliente
    tipo_cliente = case_when(
      is.na(gen_partner_id) ~ "Contacto nuevo",
      account_type == "reseller" ~ "Cliente revendedor",
      TRUE ~ "Cliente final"
    ),

    # Prioridad (estrellas en Odoo)
    prioridad = factor(priority, levels = 0:3),

    # Fechas derivadas
    fecha_creacion = as_date(create_date),
    mes_creacion = floor_date(fecha_creacion, "month"),
    ciclo_dias = as.numeric(
      difftime(date_closed, create_date, units = "days")
    ),

    # Etapas alcanzadas en el embudo
    llego_calificada = !is.na(date_qualified) |
      stage %in% c("Qualified", "Proposition", "Won"),
    llego_propuesta = !is.na(date_proposition) |
      stage %in% c("Proposition", "Won")
  )

# Oportunidades con desenlace conocido y que no son errores de carga
cerradas <- crm |>
  filter(resultado != "Abierta", !es_duplicado)

# Una fila por lead y etiqueta (el campo `tags` admite varias, separadas por ;)
crm_tags <- crm |>
  filter(!is.na(tags)) |>
  separate_longer_delim(tags, delim = ";") |>
  mutate(tags = str_squish(tags))


# Scores del conjunto de prueba en formato largo ------------------------------

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
