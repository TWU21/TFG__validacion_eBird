# ==============================================================================
# PREPROCESAMIENTO DE DATOS EBIRD - TFG
# ==============================================================================

# Cargar librerías necesarias
library(data.table)
library(dplyr)
library(tibble)
library(janitor)
library(lubridate)
library(stringr)
library(tidyr)

# ==============================================================================
# 1. CARGA DE DATOS
# ==============================================================================

# Cargar datos eBird
data <- fread(
  "ebd_ES-VC_202101_202503_relMay-2025.txt",# Reemplaza por tu archivo txt
  na.strings = c("", "NA")
) %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(
    observation_date = ymd(observation_date),
    time_observations_started = parse_time(time_observations_started, format = "%H:%M:%S")
  )

# Cargar datos meteorológicos
weather <- read.csv("weather_completo_2021_2025.csv", stringsAsFactors = FALSE)
weather$datetime <- as.POSIXct(weather$datetime, format = "%Y-%m-%d %H:%M:%S")

# ==============================================================================
# 2. PROCESAMIENTO TEMPORAL
# ==============================================================================

# Crear datetime estandarizado y redondeado a hora
data <- data %>%
  mutate(
    datetime_raw = paste(observation_date, time_observations_started),
    datetime = as.POSIXct(datetime_raw, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    datetime = floor_date(datetime, "hour")
  ) %>%
  filter(!is.na(datetime))

# ==============================================================================
# 3. FILTROS DE CALIDAD
# ==============================================================================

# Aplicar filtros de calidad eBird
data <- data %>%
  filter(
    approved == 1,
    reviewed == 0 | reviewed == 1,
    !is.na(scientific_name),
    !str_detect(scientific_name, "sp\\.|hybrid|\\?")
  )

# ==============================================================================
# 4. SELECCIÓN DE VARIABLES POR COMPLETITUD
# ==============================================================================

# Calcular completitud de variables
completitud <- data %>%
  summarise_all(~ sum(!is.na(.))/n() * 100) %>%
  tidyr::gather(variable, completitud_pct) %>%
  arrange(completitud_pct)

# Seleccionar variables con completitud >= 25%
variables_buenas <- completitud %>%
  filter(completitud_pct >= 25) %>%
  pull(variable)

data <- data %>%
  select(all_of(variables_buenas))

# ==============================================================================
# 5. ELIMINACIÓN DE VARIABLES REDUNDANTES
# ==============================================================================

# Variables redundantes o irrelevantes
vars_redundantes <- c(
  "country", "country_code", "state", "state_code", "date",
  "bcr_code", "usfws_code", "atlas_block", "reason", "v53",
  "project_names", "project_identifiers", "observer_orcid_id",
  "approved", "reviewed", "protocol_name", "group_identifier",
  "taxon_concept_id", "county_code", "observation_count_num"
)

data <- data %>% 
  select(-one_of(vars_redundantes))

# ==============================================================================
# 6. IMPUTACIÓN DE VALORES FALTANTES
# ==============================================================================

# Imputar number_observers
data <- data %>%
  mutate(number_observers = ifelse(is.na(number_observers), 1, number_observers))

# Imputar duration_minutes y effort_distance_km por mediana según protocolo
data <- data %>%
  group_by(protocol_code) %>%
  mutate(
    duration_minutes = ifelse(is.na(duration_minutes), 
                              median(duration_minutes, na.rm = TRUE), 
                              duration_minutes),
    effort_distance_km = ifelse(is.na(effort_distance_km), 
                                median(effort_distance_km, na.rm = TRUE), 
                                effort_distance_km)
  ) %>%
  ungroup()

# ==============================================================================
# 7. DIAGNÓSTICO DE CALIDAD
# ==============================================================================

# Verificar completitud por tipo de observación
quality_check <- data %>% 
  group_by(observation_type) %>%
  summarise(
    total = n(),
    na_duration = sum(is.na(duration_minutes)),
    na_distance = sum(is.na(effort_distance_km)),
    .groups = 'drop'
  )

print("Resumen de calidad por tipo de observación:")
print(quality_check)

# ==============================================================================
# 8. COMBINACIÓN CON DATOS METEOROLÓGICOS
# ==============================================================================

# Unir datos de observaciones con datos meteorológicos
combined <- inner_join(
  data,
  weather,
  by = c("latitude" = "lat", "longitude" = "lon", "datetime"),
  suffix = c("_obs", "_weather")
)

# ==============================================================================
# 9. RESUMEN FINAL
# ==============================================================================

cat("Datos procesados exitosamente:\n")
cat("- Observaciones originales:", nrow(data), "\n")
cat("- Observaciones con datos meteorológicos:", nrow(combined), "\n")
cat("- Variables finales:", ncol(combined), "\n")
cat("- Rango temporal:", min(combined$datetime), "a", max(combined$datetime), "\n")
