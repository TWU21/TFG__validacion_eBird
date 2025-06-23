# ============================================================================
# MÓDULO DE ANÁLISIS METEOROLÓGICO COMPLETO
# ============================================================================

# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(sf)
library(lubridate)
library(corrplot) # Para visualizar matrices de correlación
library(mgcv)     # Para Modelos Aditivos Generalizados (GAMs)
library(broom)    # Para limpiar y resumir resultados de modelos
library(gridExtra) # Para organizar gráficos
library(scales)   # Para formateo de escalas en gráficos
library(pwr)      # Para análisis de poder estadístico
library(RcppRoll) # Para cálculos de rolling window (sumas/medias móviles)

# ============================================================================
# 1. VERIFICACIÓN Y PREPARACIÓN INICIAL DE DATOS
# ============================================================================

message("=== INICIANDO ANÁLISIS METEOROLÓGICO COMPLETO ===")

# Variables meteorológicas esperadas
vars_meteo_esperadas <- c("humidity", "precipitation", "temperature", "windspeed")

# Verificar disponibilidad de variables y reportar
vars_meteo_disponibles_en_df <- vars_meteo_esperadas[vars_meteo_esperadas %in% names(combined_final)]
vars_meteo_no_disponibles_en_df <- setdiff(vars_meteo_esperadas, names(combined_final))

message("\nVariables meteorológicas disponibles en combined_final:")
for(var in vars_meteo_disponibles_en_df) {
  message(paste("✓", var, "- Disponible"))
}
for(var in vars_meteo_no_disponibles_en_df) {
  warning(paste("✗", var, "- NO disponible. Algunas secciones del análisis podrían verse afectadas."))
}

# Calcular completitud de datos meteorológicos
completitud_meteo <- combined_final %>%
  st_drop_geometry() %>%
  summarise(
    n_total = n(),
    across(all_of(vars_meteo_disponibles_en_df),
           list(
             disponibles = ~sum(!is.na(.)),
             faltantes = ~sum(is.na(.)),
             prop_completos = ~round(100 * sum(!is.na(.)) / n(), 1)
           )
    )
  )

message("\n=== COMPLETITUD DE DATOS METEOROLÓGICOS ===")
print(completitud_meteo)

# Filtrar por años relevantes y por registros con datos meteorológicos completos
# Se incluyen solo los años a partir de 2015 para un análisis más consistente.
# También se extraen variables temporales y espaciales.
datos_temporales <- combined_final %>%
  st_drop_geometry() %>%
  mutate(
    fecha = as.Date(observation_date),
    año = year(fecha),
    mes = month(fecha),
    dia_año = yday(fecha),
    dia_semana = lubridate::wday(fecha, label = TRUE, abbr = FALSE), # Nombre completo del día
    fin_semana = dia_semana %in% c("Sábado", "Domingo"),
    estacion = case_when(
      mes %in% c(12, 1, 2) ~ "Invierno",
      mes %in% c(3, 4, 5) ~ "Primavera",
      mes %in% c(6, 7, 8) ~ "Verano",
      mes %in% c(9, 10, 11) ~ "Otoño",
      TRUE ~ NA_character_ # Manejar posibles NAs en mes
    ),
    protegido = !is.na(nombre_espacio_protegido)
  ) %>%
  filter(año >= 2015)

# Filtro final de NA para variables meteorológicas clave para la agregación
# Esto asegura que los promedios y sumas no estén sesgados por la ausencia de datos.
datos_temporales <- datos_temporales %>%
  filter(across(all_of(vars_meteo_disponibles_en_df), ~!is.na(.)))

message(paste("\nRegistros con datos meteorológicos completos (desde 2015):", nrow(datos_temporales)))

# Agregación a nivel diario: Calcular actividad ornitológica y promedios meteo
registros_diarios <- datos_temporales %>%
  group_by(fecha, protegido) %>%
  summarise(
    n_registros = n(),
    n_especies = n_distinct(scientific_name),
    # Cálculo de diversidad Shannon, manejando casos con pocas especies
    diversidad_shannon = ifelse(n_distinct(scientific_name) > 1,
                                -sum((prop.table(table(scientific_name))) * log(prop.table(table(scientific_name)))),
                                0),
    temperatura = mean(temperature, na.rm = TRUE),
    precipitacion = sum(precipitation, na.rm = TRUE),
    humedad = mean(humidity, na.rm = TRUE),
    velocidad_viento = mean(windspeed, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    año = year(fecha),
    mes = month(fecha),
    dia_año = yday(fecha),
    dia_semana = lubridate::wday(fecha, label = TRUE, abbr = FALSE),
    fin_semana = dia_semana %in% c("Sábado", "Domingo"),
    estacion = case_when(
      mes %in% c(12, 1, 2) ~ "Invierno",
      mes %in% c(3, 4, 5) ~ "Primavera",
      mes %in% c(6, 7, 8) ~ "Verano",
      mes %in% c(9, 10, 11) ~ "Otoño"
    ),
    # Variables binarias de condiciones meteorológicas extremas o notables
    dia_lluvioso = precipitacion > 0.5, # Umbral para día lluvioso
    dia_frio = temperatura < 10,       # Umbral para día frío
    dia_caluroso = temperatura > 25,   # Umbral para día caluroso
    viento_fuerte = velocidad_viento > 5, # Umbral para viento fuerte
    humedad_alta = humedad > 80        # Umbral para humedad alta
  )

message(paste("Datos diarios agregados para el análisis:", nrow(registros_diarios), "días de observación."))

---
  
  ### 2. Análisis de Normalidad y Correlación
  
  Evaluamos la distribución de las variables clave para decidir si usar la correlación de Pearson o Spearman. Luego, calculamos las correlaciones entre las variables meteorológicas y las métricas de actividad ornitológica, aplicando **correcciones por comparaciones múltiples** para una mayor robustez estadística.

```R
# ============================================================================
# 2. ANÁLISIS DE NORMALIDAD Y CORRELACIÓN
# ============================================================================

message("\n=== PRUEBAS DE NORMALIDAD DE LAS VARIABLES CLAVE ===")

variables_analisis_normalidad <- c("n_registros", "n_especies", "diversidad_shannon",
                                   "temperatura", "precipitacion", "humedad", "velocidad_viento")

# Función para probar normalidad (considera el tamaño de la muestra)
probar_normalidad <- function(data, variable) {
  x <- data[[variable]]
  x <- x[!is.na(x)]
  
  shapiro_p <- NA
  if (length(x) > 3 & length(x) < 5000) { # Shapiro-Wilk es sensible a N grandes
    shapiro_test <- tryCatch(shapiro.test(x), error = function(e) list(p.value = NA))
    shapiro_p <- shapiro_test$p.value
  }
  
  ks_test <- tryCatch(ks.test(x, "pnorm", mean(x), sd(x)), error = function(e) list(p.value = NA)) # Kolmogorov-Smirnov
  ks_p <- ks_test$p.value
  
  # Considerar "normal" si P > 0.05 para ambos tests, o si Shapiro no es aplicable
  es_normal <- (is.na(shapiro_p) || shapiro_p > 0.05) && (is.na(ks_p) || ks_p > 0.05)
  
  return(data.frame(
    Variable = variable,
    n = length(x),
    Shapiro_p = round(shapiro_p, 4),
    KS_p = round(ks_p, 4),
    Es_Normal = es_normal,
    Correlacion_Recomendada = ifelse(es_normal, "Pearson", "Spearman")
  ))
}

resultados_normalidad <- map_dfr(variables_analisis_normalidad, ~probar_normalidad(registros_diarios, .x))
print(resultados_normalidad)

# Determinar el tipo de correlación a usar (si la mayoría son normales, usar Pearson)
if (sum(resultados_normalidad$Es_Normal) >= (nrow(resultados_normalidad) / 2)) {
  tipo_correlacion <- "pearson"
} else {
  tipo_correlacion <- "spearman" # Spearman es más robusto para datos no normales
}

message(paste("\nTipo de correlación recomendado según normalidad:", tipo_correlacion))

# Seleccionar variables para la matriz de correlaciones
vars_para_correlacion <- registros_diarios %>%
  select(n_registros, n_especies, diversidad_shannon,
         temperatura, precipitacion, humedad, velocidad_viento)

# Calcular la matriz de correlaciones
matriz_correlaciones <- cor(vars_para_correlacion,
                            use = "pairwise.complete.obs", # Usar todas las observaciones posibles por par
                            method = tipo_correlacion)

message(paste("\n=== MATRIZ DE CORRELACIONES (Método:", toupper(tipo_correlacion), ") ==="))
print(round(matriz_correlaciones, 3))

# Correlaciones específicas con actividad ornitológica (primeras 3 filas, últimas 4 columnas)
correlaciones_actividad <- matriz_correlaciones[1:3, 4:7, drop = FALSE]
message("\n=== CORRELACIONES ENTRE ACTIVIDAD ORNITOLÓGICA Y VARIABLES METEOROLÓGICAS ===")
print(round(correlaciones_actividad, 3))

# Función para calcular correlaciones con corrección por comparaciones múltiples
analizar_correlaciones_corregidas <- function(data, variables_respuesta, variables_predictoras, metodo_correlacion) {
  combinaciones <- expand_grid(
    respuesta = variables_respuesta,
    predictora = variables_predictoras
  )
  
  calcular_correlacion_segura <- function(x, y, metodo) {
    datos_completos <- complete.cases(x, y)
    x_clean <- x[datos_completos]
    y_clean <- y[datos_completos]
    
    if(length(x_clean) < 3) {
      return(list(correlacion = NA, p_valor = NA, n_obs = length(x_clean)))
    }
    
    test_result <- tryCatch(
      cor.test(x_clean, y_clean, method = metodo),
      error = function(e) list(estimate = NA, p.value = NA)
    )
    return(list(correlacion = as.numeric(test_result$estimate),
                p_valor = as.numeric(test_result$p.value),
                n_obs = length(x_clean)))
  }
  
  resultados <- combinaciones %>%
    rowwise() %>%
    mutate(
      resultado_cor = list(calcular_correlacion_segura(data[[respuesta]], data[[predictora]], metodo_correlacion)),
      correlacion = resultado_cor$correlacion,
      p_valor_crudo = resultado_cor$p_valor,
      n_observaciones = resultado_cor$n_obs
    ) %>%
    select(-resultado_cor) %>%
    ungroup()
  
  # Aplicar correcciones múltiples (solo a los p-valores no NA)
  p_values_for_adjustment <- na.omit(resultados$p_valor_crudo)
  adjusted_p_values_bonferroni <- p.adjust(p_values_for_adjustment, method = "bonferroni")
  adjusted_p_values_fdr <- p.adjust(p_values_for_adjustment, method = "BH")
  
  # Mapear los p-valores ajustados de vuelta a la tabla original
  resultados$p_bonferroni <- NA_real_
  resultados$p_fdr <- NA_real_
  resultados$p_bonferroni[!is.na(resultados$p_valor_crudo)] <- adjusted_p_values_bonferroni
  resultados$p_fdr[!is.na(resultados$p_valor_crudo)] <- adjusted_p_values_fdr
  
  resultados <- resultados %>%
    mutate(
      sig_crudo = p_valor_crudo < 0.05,
      sig_bonferroni = p_bonferroni < 0.05,
      sig_fdr = p_fdr < 0.05,
      tamaño_efecto = case_when(
        abs(correlacion) >= 0.5 ~ "Grande",
        abs(correlacion) >= 0.3 ~ "Medio",
        abs(correlacion) >= 0.1 ~ "Pequeño",
        TRUE ~ "Trivial"
      )
    ) %>%
    arrange(desc(abs(correlacion)))
  
  return(resultados)
}

variables_respuesta <- c("n_registros", "n_especies", "diversidad_shannon")
variables_meteo <- c("temperatura", "precipitacion", "humedad", "velocidad_viento")

correlaciones_corregidas <- analizar_correlaciones_corregidas(
  registros_diarios, variables_respuesta, variables_meteo, tipo_correlacion
)

message("\n=== CORRELACIONES CON CORRECCIÓN POR COMPARACIONES MÚLTIPLES (FDR) ===")
print(correlaciones_corregidas %>%
        select(respuesta, predictora, correlacion, p_valor_crudo, p_bonferroni, p_fdr,
               sig_crudo, sig_bonferroni, sig_fdr, tamaño_efecto))
# ============================================================================
# 3. MODELADO DE EFECTOS METEOROLÓGICOS (GAMs)
# ============================================================================

message("\n=== MODELOS DE EFECTOS METEOROLÓGICOS (GAMs) ===\n")

# Modelo para Temperatura
message("--- Modelo: n_registros ~ Temperatura ---")
modelo_temperatura <- gam(n_registros ~ s(temperatura) + s(dia_año) +
                            protegido + fin_semana + estacion,
                          data = registros_diarios,
                          family = tw())
print(summary(modelo_temperatura)$p.table)
message(paste("AIC (Temperatura):", round(AIC(modelo_temperatura), 2)))

# Modelo para Precipitación
message("\n--- Modelo: n_registros ~ Precipitación ---")
modelo_precipitacion <- gam(n_registros ~ s(precipitacion) + dia_lluvioso + s(dia_año) +
                              protegido + fin_semana + estacion,
                            data = registros_diarios, family = tw())
print(summary(modelo_precipitacion)$p.table)
message(paste("AIC (Precipitación):", round(AIC(modelo_precipitacion), 2)))

# Modelo para Velocidad del Viento
message("\n--- Modelo: n_registros ~ Velocidad del Viento ---")
modelo_viento <- gam(n_registros ~ s(velocidad_viento) + viento_fuerte + s(dia_año) +
                       protegido + fin_semana + estacion,
                     data = registros_diarios, family = tw())
print(summary(modelo_viento)$p.table)
message(paste("AIC (Viento):", round(AIC(modelo_viento), 2)))

# Modelo para Humedad
message("\n--- Modelo: n_registros ~ Humedad ---")
modelo_humedad <- gam(n_registros ~ s(humedad) + humedad_alta + s(dia_año) +
                        protegido + fin_semana + estacion,
                      data = registros_diarios, family = tw())
print(summary(modelo_humedad)$p.table)
message(paste("AIC (Humedad):", round(AIC(modelo_humedad), 2)))

# Modelo Completo (incluye efectos suaves y categóricos, así como interacciones)
message("\n--- Modelo Completo (todas las variables meteorológicas) ---")
modelo_completo <- gam(n_registros ~ s(temperatura) + s(precipitacion) + s(velocidad_viento) + s(humedad) +
                         s(dia_año) + # Efecto no lineal del día del año
                         ti(temperatura, dia_año) + # Interacción tensor de temperatura y día del año
                         protegido + fin_semana + estacion + año, # Efectos categóricos y anuales
                       data = registros_diarios, family = tw())
print(summary(modelo_completo))
message(paste("AIC (Completo):", round(AIC(modelo_completo), 2)))
# ============================================================================
# 4. ANÁLISIS DE SESGOS Y GAPS DE DETECCIÓN
# ============================================================================

message("\n=== SESGOS POR CONDICIONES CLIMÁTICAS ===\n")

sesgos_meteo <- registros_diarios %>%
  mutate(
    condicion_clima = case_when(
      dia_lluvioso ~ "Lluvioso",
      dia_frio ~ "Frío",
      dia_caluroso ~ "Caluroso",
      viento_fuerte ~ "Viento Fuerte",
      humedad_alta ~ "Humedad Alta",
      TRUE ~ "Normal"
    )
  ) %>%
  group_by(condicion_clima, protegido) %>%
  summarise(
    n_dias = n(),
    registros_total = sum(n_registros),
    registros_promedio = round(mean(n_registros), 1),
    especies_promedio = round(mean(n_especies), 1),
    diversidad_promedio = round(mean(diversidad_shannon), 2),
    .groups = 'drop'
  ) %>%
  arrange(condicion_clima, protegido)

print(sesgos_meteo)

message("\n=== EVALUACIÓN DE SESGOS METEOROLÓGICOS GENERALES ===\n")

evaluacion_sesgos <- registros_diarios %>%
  mutate(
    condiciones_suboptimas = dia_lluvioso | dia_frio | dia_caluroso | viento_fuerte | humedad_alta
  ) %>%
  group_by(protegido) %>%
  summarise(
    dias_total = n(),
    dias_suboptimos = sum(condiciones_suboptimas),
    prop_dias_suboptimas = round(dias_suboptimos / dias_total, 3),
    
    registros_normales = mean(n_registros[!condiciones_suboptimas], na.rm = TRUE),
    registros_suboptimos = mean(n_registros[condiciones_suboptimas], na.rm = TRUE),
    reduccion_actividad = ifelse(registros_normales > 0, round(1 - (registros_suboptimos / registros_normales), 3), NA),
    
    especies_normales = mean(n_especies[!condiciones_suboptimas], na.rm = TRUE),
    especies_suboptimos = mean(n_especies[condiciones_suboptimas], na.rm = TRUE),
    reduccion_especies = ifelse(especies_normales > 0, round(1 - (especies_suboptimos / especies_normales), 3), NA),
    .groups = 'drop'
  )

print(evaluacion_sesgos)

# Resumen de impactos significativos (basado en correlaciones con FDR)
efectos_significativos <- correlaciones_corregidas %>%
  filter(sig_fdr == TRUE) %>%
  mutate(
    magnitud = case_when(
      abs(correlacion) >= 0.3 ~ "Fuerte",
      abs(correlacion) >= 0.1 ~ "Moderada",
      TRUE ~ "Débil"
    ),
    direccion = ifelse(correlacion > 0, "Positiva", "Negativa")
  ) %>%
  arrange(desc(abs(correlacion)))

message("\n=== EFECTOS METEOROLÓGICOS SIGNIFICATIVOS (tras corrección FDR) ===")
print(efectos_significativos)

# Impacto general en la representatividad del sistema
impacto_general <- evaluacion_sesgos %>%
  summarise(
    prop_dias_suboptimos_promedio = round(mean(prop_dias_suboptimas, na.rm = TRUE), 3),
    reduccion_actividad_promedio = round(mean(reduccion_actividad, na.rm = TRUE), 3),
    reduccion_especies_promedio = round(mean(reduccion_especies, na.rm = TRUE), 3),
    sesgo_deteccion_estimado = round(mean(reduccion_actividad, na.rm = TRUE), 3)
  )

message("\n=== IMPACTO GENERAL EN LA REPRESENTATIVIDAD DEL SISTEMA ===\n")
message(paste0("Proporción promedio de días con condiciones subóptimas: ", impacto_general$prop_dias_suboptimos_promedio * 100, "%"))
message(paste0("Reducción promedio de actividad de observación en días subóptimas: ", impacto_general$reduccion_actividad_promedio * 100, "%"))
message(paste0("Reducción promedio en el número de especies detectadas: ", impacto_general$reduccion_especies_promedio * 100, "%"))
message(paste0("Sesgo de detección estimado (basado en reducción de actividad): ", impacto_general$sesgo_deteccion_estimado * 100, "%"))
# ============================================================================
# 5. ANÁLISIS DETALLADO DE PRECIPITACIÓN
# ============================================================================

message("\n=== ANÁLISIS PROFUNDO: EFECTOS DE LA PRECIPITACIÓN ===\n")

# Crear variables de precipitación más detalladas
datos_precipitacion_detallado <- registros_diarios %>%
  arrange(fecha) %>%
  mutate(
    # Precipitación en días anteriores
    precip_dia_anterior = lag(precipitacion, 1),
    precip_2dias_anterior = lag(precipitacion, 2),
    precip_3dias_anterior = lag(precipitacion, 3),
    
    # Precipitación acumulada (usar rolling sum para mayor robustez)
    precip_acum_3dias = RcppRoll::roll_sum(precipitacion, n = 3, align = "right", fill = NA),
    precip_acum_7dias = RcppRoll::roll_sum(precipitacion, n = 7, align = "right", fill = NA),
    
    # Categorías de intensidad de precipitación
    intensidad_lluvia = case_when(
      is.na(precipitacion) ~ "Desconocido",
      precipitacion == 0 ~ "Sin lluvia",
      precipitacion <= 2 ~ "Ligera",
      precipitacion <= 10 ~ "Moderada",
      precipitacion <= 30 ~ "Fuerte",
      TRUE ~ "Muy fuerte"
    ),
    # Asegurar el orden de los factores para visualización
    intensidad_lluvia = factor(intensidad_lluvia, levels = c("Sin lluvia", "Ligera", "Moderada", "Fuerte", "Muy fuerte", "Desconocido")),
    
    # Días post-lluvia (se basa en dia_lluvioso del día anterior)
    dia_post_lluvia = lag(dia_lluvioso, 1),
    segundo_dia_post_lluvia = lag(dia_lluvioso, 2),
    
    # Contraste con días anteriores
    cambio_humedad = humedad - lag(humedad, 1),
    cambio_temperatura = temperatura - lag(temperatura, 1)
  ) %>%
  # Quitar los primeros días que no tienen datos previos para los lags/acumulados
  filter(!is.na(precip_dia_anterior))

message(paste("Registros con datos de precipitación detallada:", nrow(datos_precipitacion_detallado), "días."))

# Hipótesis 1: Efecto post-lluvia en detectabilidad
if(!all(is.na(datos_precipitacion_detallado$dia_post_lluvia)) &&
   sum(!is.na(datos_precipitacion_detallado$dia_post_lluvia) & datos_precipitacion_detallado$dia_post_lluvia) > 1 &&
   sum(!is.na(datos_precipitacion_detallado$dia_post_lluvia) & !datos_precipitacion_detallado$dia_post_lluvia) > 1) {
  
  efectos_post_lluvia <- datos_precipitacion_detallado %>%
    filter(!is.na(dia_post_lluvia)) %>%
    group_by(dia_post_lluvia) %>%
    summarise(
      n_dias = n(),
      registros_promedio = mean(n_registros, na.rm = TRUE),
      especies_promedio = mean(n_especies, na.rm = TRUE),
      diversidad_promedio = mean(diversidad_shannon, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      tipo = "Día post-lluvia",
      condicion = ifelse(dia_post_lluvia, "Sí", "No")
    )
  
  message("\nHIPÓTESIS 1: Efectos post-lluvia en la actividad de observación\n")
  print(efectos_post_lluvia)
  
  # Test estadístico para efecto post-lluvia
  datos_test_post_lluvia <- datos_precipitacion_detallado %>%
    filter(!is.na(dia_post_lluvia))
  if(nrow(datos_test_post_lluvia) > 10 && n_distinct(datos_test_post_lluvia$dia_post_lluvia) == 2) {
    test_post_lluvia_n_registros <- t.test(n_registros ~ dia_post_lluvia, data = datos_test_post_lluvia)
    message(paste("T-test n_registros ~ dia_post_lluvia - p-valor:", round(test_post_lluvia_n_registros$p.value, 4)))
    test_post_lluvia_n_especies <- t.test(n_especies ~ dia_post_lluvia, data = datos_test_post_lluvia)
    message(paste("T-test n_especies ~ dia_post_lluvia - p-valor:", round(test_post_lluvia_n_especies$p.value, 4)))
  } else {
    message("Datos insuficientes para test post-lluvia (ambas categorías o total de datos bajo).")
  }
} else {
  message("HIPÓTESIS 1: No se puede evaluar efecto post-lluvia (datos insuficientes o solo una categoría de día post-lluvia).")
  efectos_post_lluvia <- data.frame()
}

# Hipótesis 2: Efectos por intensidad de precipitación
efectos_intensidad <- datos_precipitacion_detallado %>%
  group_by(intensidad_lluvia) %>%
  summarise(
    n_dias = n(),
    registros_promedio = round(mean(n_registros, na.rm = TRUE), 1),
    especies_promedio = round(mean(n_especies, na.rm = TRUE), 1),
    diversidad_promedio = round(mean(diversidad_shannon, na.rm = TRUE), 2),
    humedad_promedio = round(mean(humedad, na.rm = TRUE), 1),
    .groups = 'drop'
  ) %>%
  arrange(factor(intensidad_lluvia, levels = c("Sin lluvia", "Ligera", "Moderada", "Fuerte", "Muy fuerte")))

message("\nHIPÓTESIS 2: Efectos por intensidad de lluvia en la actividad de observación\n")
print(efectos_intensidad)

# Hipótesis 3: Efectos de la precipitación acumulada
variables_precip_acumulada <- c("precipitacion", "precip_dia_anterior", "precip_2dias_anterior", "precip_3dias_anterior",
                                "precip_acum_3dias", "precip_acum_7dias")
variables_precip_acumulada_disponibles <- intersect(variables_precip_acumulada, names(datos_precipitacion_detallado))

if(length(variables_precip_acumulada_disponibles) >= 1) {
  datos_para_cor_precip_acum <- datos_precipitacion_detallado %>%
    select(n_registros, n_especies, diversidad_shannon, all_of(variables_precip_acumulada_disponibles)) %>%
    filter(complete.cases(.))
  
  if(nrow(datos_para_cor_precip_acum) > 10) {
    correlaciones_precipitacion_detallada <- cor(datos_para_cor_precip_acum,
                                                 use = "pairwise.complete.obs",
                                                 method = tipo_correlacion)
    
    message("\nHIPÓTESIS 3: Correlaciones con precipitación en diferentes escalas temporales (actual y acumulada)\n")
    cols_to_print <- intersect(names(correlaciones_precipitacion_detallada), variables_precip_acumulada_disponibles)
    print(round(correlaciones_precipitacion_detallada[c("n_registros", "n_especies", "diversidad_shannon"), cols_to_print, drop = FALSE], 3))
  } else {
    message("\nHIPÓTESIS 3: Datos insuficientes para correlaciones detalladas (acumuladas).")
  }
} else {
  message("\nHIPÓTESIS 3: Variables de precipitación acumulada no disponibles.")
}

# Hipótesis 4: Interacción precipitación-temperatura
if ("precipitacion" %in% names(datos_precipitacion_detallado) &&
    "temperatura" %in% names(datos_precipitacion_detallado)) {
  tryCatch({
    modelo_interaccion_precip <- gam(
      n_registros ~ s(precipitacion) + s(temperatura) + ti(precipitacion, temperatura) + # Interacción tensor de precipitación y temperatura
        s(humedad) + s(velocidad_viento) + s(dia_año) + protegido + estacion,
      data = datos_precipitacion_detallado,
      family = tw()
    )
    
    message("\nHIPÓTESIS 4: Modelo con interacción suave precipitación-temperatura\n")
    print(summary(modelo_interaccion_precip)$p.table)
  }, error = function(e) {
    warning(paste("No se pudo ajustar el modelo de interacción precipitación-temperatura:", e$message))
    modelo_interaccion_precip <- NULL
  })
} else {
  message("\nHIPÓTESIS 4: Variables 'precipitacion' o 'temperatura' no disponibles para el modelo de interacción.")
  modelo_interaccion_precip <- NULL
}
# ============================================================================
# 6. ANÁLISIS POR ESPECIES Y COMPORTAMIENTO DEL OBSERVADOR
# ============================================================================

# Análisis de comportamiento de observadores en días lluviosos
message("\n=== COMPORTAMIENTO DE OBSERVADORES EN DÍAS LLUVIOSOS ===\n")
comportamiento_observadores <- datos_temporales %>%
  mutate(
    dia_lluvioso = precipitation > 0.5 # Usando el mismo umbral que en el análisis principal
  ) %>%
  group_by(fecha, dia_lluvioso, protegido) %>%
  summarise(
    n_observadores = n_distinct(observer_id),
    n_registros = n(),
    .groups = 'drop'
  ) %>%
  group_by(dia_lluvioso, protegido) %>%
  summarise(
    observadores_promedio = mean(n_observadores, na.rm = TRUE),
    registros_promedio = mean(n_registros, na.rm = TRUE),
    registros_por_observador = mean(n_registros/n_observadores, na.rm = TRUE),
    .groups = 'drop'
  )
print(comportamiento_observadores)

# Visualización de la productividad en días lluviosos
p_productividad_lluvia <- ggplot(comportamiento_observadores,
                                 aes(x = dia_lluvioso, y = registros_por_observador, fill = protegido)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Productividad de Observadores en Días Lluviosos",
       x = "Día Lluvioso (>0.5mm precipitación)",
       y = "Registros por Observador",
       fill = "Ubicación") +
  scale_fill_manual(values = c("FALSE" = "#320000", "TRUE" = "#97862e"),
                    labels = c("No Protegida", "Área Protegida")) +
  scale_x_discrete(labels = c("FALSE" = "No Lluvioso", "TRUE" = "Lluvioso")) +
  theme_minimal() +
  theme(legend.position = "top")

print(p_productividad_lluvia)

# Asegúrate de que la carpeta 'figuras' exista en tu directorio de trabajo
if (!dir.exists("figuras")) {
  dir.create("figuras")
}
ggsave(
  filename = "figuras/productividad_lluvia.png", # Nombre del archivo
  plot = p_productividad_lluvia, # El objeto ggplot que quieres guardar
  width = 8,  # Ancho de la imagen en pulgadas
  height = 6, # Alto de la imagen en pulgadas
  units = "in", # Unidades para width y height (pulgadas)
  dpi = 300 # Resolución de la imagen (puntos por pulgada)
)
message("Gráfico 'productividad_lluvia.png' guardado en la carpeta 'figuras'.")


# Análisis detallado por experiencia y cada variable meteorológica
message("\n=== SELECCIÓN DE HÁBITAT POR EXPERIENCIA Y CONDICIONES METEOROLÓGICAS ===\n")
analisis_experiencia_meteo <- datos_temporales %>%
  group_by(observer_id) %>%
  mutate(experiencia = n_distinct(fecha)) %>% # Días únicos de observación
  ungroup() %>%
  mutate(
    grupo_experiencia = cut(experiencia, breaks = c(0, 5, 20, Inf),
                            labels = c("Novato", "Intermedio", "Experto"),
                            right = FALSE), # `right=FALSE` para incluir el límite inferior en el intervalo
    
    lluvia = if_else(precipitation > 0.5, "Lluvioso", "Seco"),
    temperatura_cat = if_else(
      temperature > quantile(temperature, 0.9, na.rm=T) |
        temperature < quantile(temperature, 0.1, na.rm=T),
      "Extrema", "Normal"
    ),
    humedad_cat = if_else(humidity > quantile(humidity, 0.8, na.rm=T),
                          "Alta", "Normal"),
    viento_cat = if_else(windspeed > quantile(windspeed, 0.8, na.rm=T),
                         "Fuerte", "Normal")
  ) %>%
  
  pivot_longer(
    cols = c(lluvia, temperatura_cat, humedad_cat, viento_cat),
    names_to = "variable_meteo",
    values_to = "condicion"
  ) %>%
  
  group_by(variable_meteo, condicion, grupo_experiencia) %>%
  summarise(
    n_observaciones = n(),
    proporcion_protegido = mean(protegido, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  
  mutate(
    variable_meteo = case_when(
      variable_meteo == "lluvia" ~ "Precipitación",
      variable_meteo == "temperatura_cat" ~ "Temperatura",
      variable_meteo == "humedad_cat" ~ "Humedad",
      variable_meteo == "viento_cat" ~ "Velocidad del Viento"
    ),
    condicion = factor(condicion, levels = c("Normal", "Seco", "Lluvioso", "Extrema", "Alta", "Fuerte"))
  )

# Visualización: Proporción en áreas protegidas por experiencia y condición meteo
p_experiencia_meteo <- analisis_experiencia_meteo %>%
  ggplot(aes(x = grupo_experiencia, y = proporcion_protegido, fill = condicion)) +
  geom_col(position = "dodge", alpha = 0.8) +
  facet_wrap(~variable_meteo, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Seco" = "#8d2535", "Lluvioso" = "#8d4925",
      "Normal" = "#320000", "Extrema" = "#ffb48a",
      "Alta" = "#c57d56", "Fuerte" = "#97862e"
    ),
    name = "Condición"
  ) +
  labs(
    title = "Uso de Áreas Protegidas por Experiencia y Condiciones Meteorológicas",
    subtitle = "Proporción de observaciones en áreas protegidas",
    x = "Experiencia del Observador",
    y = "Proporción en Áreas Protegidas"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::percent_format())

print(p_experiencia_meteo)

# Tabla de resultados para el informe
message("\nTabla: Proporciones de observación en áreas protegidas por experiencia y condición meteorológica\n")
tabla_experiencia_meteo <- analisis_experiencia_meteo %>%
  select(-n_observaciones) %>%
  pivot_wider(
    names_from = condicion,
    values_from = proporcion_protegido
  ) %>%
  arrange(variable_meteo, grupo_experiencia)
print(tabla_experiencia_meteo)

# Análisis de diferencias específicas
message("\nDiferencias en la proporción de uso de áreas protegidas (condición adversa - normal/seca)\n")
diferencias_meteo <- analisis_experiencia_meteo %>%
  group_by(variable_meteo, grupo_experiencia) %>%
  summarise(
    prop_adversa = proporcion_protegido[condicion %in% c("Lluvioso", "Extrema", "Alta", "Fuerte")],
    prop_normal_o_seca = proporcion_protegido[condicion %in% c("Normal", "Seco")],
    diferencia = if(length(prop_adversa) > 0 && length(prop_normal_o_seca) > 0) {
      prop_adversa[1] - prop_normal_o_seca[1]
    } else {
      NA_real_
    },
    .groups = 'drop'
  ) %>%
  mutate(diferencia_pct = round(diferencia * 100, 1)) %>%
  filter(!is.na(diferencia)) # Filtrar casos donde no se pudo calcular la diferencia
print(diferencias_meteo)

# Análisis de la respuesta diferencial por especies frecuentes a la lluvia
if(exists("combined_final") && "scientific_name" %in% names(combined_final)) {
  message("\n=== RESPUESTA DIFERENCIAL DE ESPECIES FRECUENTES A LA LLUVIA ===\n")
  especies_frecuentes <- combined_final %>%
    st_drop_geometry() %>%
    count(scientific_name, common_name, sort = TRUE) %>%
    slice_head(n = 10) %>% # Top 10 especies más frecuentes
    pull(scientific_name)
  
  if(length(especies_frecuentes) > 0) {
    respuesta_especies_lluvia <- combined_final %>%
      st_drop_geometry() %>%
      filter(scientific_name %in% especies_frecuentes,
             !is.na(precipitation)) %>%
      mutate(
        fecha = as.Date(observation_date),
        dia_lluvioso = precipitation > 0.5
      ) %>%
      group_by(scientific_name, common_name, dia_lluvioso) %>%
      summarise(
        n_observaciones = n(),
        .groups = 'drop'
      ) %>%
      pivot_wider(names_from = dia_lluvioso, values_from = n_observaciones,
                  names_prefix = "lluvia_", values_fill = 0) %>%
      mutate(
        total = lluvia_FALSE + lluvia_TRUE,
        prop_dias_lluvia = ifelse(total > 0, lluvia_TRUE / total, 0),
        preferencia_lluvia = case_when(
          prop_dias_lluvia > 0.3 ~ "Alta",
          prop_dias_lluvia > 0.15 ~ "Media",
          TRUE ~ "Baja"
        )
      ) %>%
      arrange(desc(prop_dias_lluvia))
    print(respuesta_especies_lluvia)
  } else {
    message("No hay suficientes especies frecuentes para el análisis individual.")
  }
} else {
  message("\nAnálisis por especies no disponible: el dataframe 'combined_final' no contiene 'scientific_name'.")
}
# ============================================================================
# 7. VISUALIZACIÓN: HEATMAP DE CORRELACIONES
# ============================================================================

message("\n=== GENERANDO HEATMAP DE CORRELACIONES ===\n")

# Nombres de las variables para el heatmap
nombres_variables_heatmap <- c("Nº Registros", "Nº Especies", "Diversidad Shannon",
                               "Temperatura", "Precipitación", "Humedad", "Velocidad Viento")

# Asignar nombres a la matriz de correlaciones para mejor legibilidad
if(all(dim(matriz_correlaciones) == c(length(nombres_variables_heatmap), length(nombres_variables_heatmap)))) {
  colnames(matriz_correlaciones) <- nombres_variables_heatmap
  rownames(matriz_correlaciones) <- nombres_variables_heatmap
  
  # Definir la paleta de colores personalizada (verde-blanco-rojo)
  colores_personalizados <- colorRampPalette(c("#a6a447", "white", "#a84b4c"))(200)
  
  # Crear el heatmap de correlaciones
  corrplot(matriz_correlaciones,
           method = "color",
           type = "upper", # Mostrar solo la mitad superior para evitar redundancia
           diag = FALSE,   # No mostrar correlaciones de la variable consigo misma
           addCoef.col = "black", # Añadir valores de correlación en negro
           number.cex = 0.7, # Tamaño de los números
           tl.col = "black", # Color de las etiquetas de texto
           tl.srt = 45,    # Rotar etiquetas para evitar solapamiento
           col = colores_personalizados, # Aplicar la paleta personalizada
           mar = c(0,0,1,0), # Ajustar márgenes
           title = paste0("Heatmap de Correlaciones (Método: ", toupper(tipo_correlacion), ")"))
  
  message("Heatmap generado. Puedes verlo en la ventana de Plots de RStudio.")
} else {
  warning("Las dimensiones de 'matriz_correlaciones' no coinciden con 'nombres_variables_heatmap'. No se pudo generar el heatmap.")
}
# ============================================================================
# 8. RESUMEN FINAL Y GUARDADO DE RESULTADOS
# ============================================================================


resultados_analisis_meteo_completo <- list(
  completitud_meteo = completitud_meteo,
  registros_diarios = registros_diarios,
  resultados_normalidad = resultados_normalidad,
  tipo_correlacion_usado = tipo_correlacion,
  matriz_correlaciones = matriz_correlaciones,
  correlaciones_corregidas = correlaciones_corregidas,
  modelos_gam = list(
    temperatura = modelo_temperatura,
    precipitacion = modelo_precipitacion,
    viento = modelo_viento,
    humedad = modelo_humedad,
    completo = modelo_completo
  ),
  sesgos_meteo = sesgos_meteo,
  evaluacion_sesgos = evaluacion_sesgos,
  efectos_significativos = efectos_significativos,
  impacto_general_representatividad = impacto_general,
  analisis_precipitacion_detallado = list(
    efectos_post_lluvia = if(exists("efectos_post_lluvia")) efectos_post_lluvia else NULL,
    efectos_intensidad = if(exists("efectos_intensidad")) efectos_intensidad else NULL,
    correlaciones_acumuladas = if(exists("correlaciones_precipitacion_detallada")) correlaciones_precipitacion_detallada else NULL,
    modelo_interaccion_precip = if(exists("modelo_interaccion_precip")) modelo_interaccion_precip else NULL
  ),
  comportamiento_observadores_lluvia = if(exists("comportamiento_observadores")) comportamiento_observadores else NULL,
  analisis_experiencia_meteo = if(exists("analisis_experiencia_meteo")) analisis_experiencia_meteo else NULL,
  respuesta_especies_frecuentes = if(exists("respuesta_especies_lluvia")) respuesta_especies_lluvia else NULL
)

message("\n=== ANÁLISIS METEOROLÓGICO COMPLETADO ===\n")

