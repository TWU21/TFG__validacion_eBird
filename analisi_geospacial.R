# ============================================================================
# SCRIPT COMPLETO DE ANÁLISIS GEOSSPACIAL DE REGISTROS EBIRD
# ============================================================================

# Cargar librerías necesarias
library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(leaflet)
library(RColorBrewer)
library(leaflet.extras)
library(lubridate)
library(ggplot2) # Comúnmente usada con dplyr, incluida para consistencia
library(htmlwidgets) # Para guardar mapas interactivos
library(scales) # Para formatear números grandes en las salidas

# ============================================================================
# PASO 1: CONFIGURACIÓN INICIAL Y CARGA DE DATOS
#
# IMPORTANTE: Asegúrate de que el dataframe 'combined' (tus registros eBird)
# y los directorios de shapefiles ('ZEPA', 'HUMEDALES') sean accesibles.
# 'combined' debe tener al menos las columnas 'latitude', 'longitude',
# 'scientific_name' y 'common_name'.
# ============================================================================

# Ejemplo para cargar 'combined' si no está ya en tu entorno:
# combined <- read.csv("ruta/a/tu/archivo_ebird.csv")

# Cargar capas de espacios protegidos
# Ajusta las rutas si tus shapefiles no están en subdirectorios de tu directorio de trabajo
zeps <- st_read("ZEPA/eepp_zepas0.shp", quiet = TRUE)
humedales <- st_read("HUMEDALES/eepp_zonas_humedas0.shp", quiet = TRUE)

# Función auxiliar para asegurar que la columna 'nombre' existe en los shapefiles.
# Esto ayuda a estandarizar los nombres para las operaciones de unión.
ensure_nombre_col <- function(sf_obj, layer_name) {
  if (!"nombre" %in% names(sf_obj)) {
    warning(paste0("Columna 'nombre' no encontrada en ", layer_name, ". Intentando usar la primera columna de texto."))
    text_cols <- sapply(sf_obj, is.character)
    if (any(text_cols)) {
      names(sf_obj)[which(text_cols)[1]] <- "nombre"
    } else {
      stop(paste0("No se encontró una columna de texto adecuada en ", layer_name, " para usar como 'nombre'."))
    }
  }
  return(sf_obj)
}

zeps <- ensure_nombre_col(zeps, "ZEPAs")
humedales <- ensure_nombre_col(humedales, "Humedales")

# Definir CRS proyectado para cálculos precisos de distancia y buffer (ETRS89 UTM 30N para Valencia)
crs_proj <- 25830

# Crear ID único para registros eBird si no existe
if (!"id" %in% names(combined)) {
  combined$id <- 1:nrow(combined)
}

# Convertir el dataframe 'combined' a un objeto sf (eliminando registros sin coordenadas válidas)
combined_sf <- combined %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# ============================================================================
# PASO 2: DEFINIR TABLAS DE EQUIVALENCIAS PARA UNIFICACIÓN DE NOMBRES DE ESPACIOS PROTEGIDOS
#
# Estas tablas estandarizan nombres que pueden aparecer de forma diferente en las distintas capas.
# Revísalas y modifícalas si tus datos específicos requieren otros mapeos.
# ============================================================================

# Tabla para la unificación inicial de nombres basada en pares de humedales y ZEPAs
equivalencias <- data.frame(
  nombre_humedal = c(
    "Parque Natural de las Lagunas de La Mata-Torrevieja", "Parque Natural de las Salinas de Santa Pola",
    "Parque Natural de la Marjal de Pego-Oliva", "Desembocadura y frente litoral del Riu Racons",
    "Desembocadura del Millars", "El Barchell", "Lavajos de Sinarcas", "Marjal i Estany d'Almenara",
    "Marjal de la Safor", "Desembocadura del Riu Xeraco", "Ullal de l'Estany del Duc",
    "Laguna de San Benito", "Embalse de Tibi", "Els Bassars-Clot de Galvany",
    "Parque Natural del Prat de Cabanes", "Dehesa de Soneja", "Balsa de Chóvar",
    "Marjal dels Moros", "Parque Natural de l'Albufera de València", "Embalse de Embarcaderos",
    "Parque Natural del Fondó d'Elx", "Els Carrissars d'Elx", "El Hondo de Amorós",
    "Fonts de l'Algar"
  ),
  nombre_zepa = c(
    "Lagunas de la Mata y Torrevieja", "Salines de Santa Pola (ZEPA)", "Marjal de Pego-Oliva (ZEPA)",
    "Marjal de Pego-Oliva (ZEPA)", "Desembocadura del riu Millars", "Alto Turia y Sierra del Negrete",
    "Alto Turia y Sierra del Negrete", "Marjal i Estanys d'Almenara", "Montdúver - Marjal de la Safor",
    "Montdúver - Marjal de la Safor", "Montdúver - Marjal de la Safor", "Meca - Mugrón - San Benito",
    "Riu Montnegre", "Clot de Galvany", "Prat de Cabanes i Torreblanca (ZEPA)",
    "Serra d'Espadà (ZEPA)", "Serra d'Espadà (ZEPA)", "Marjal dels Moros (ZEPA)",
    "l'Albufera (ZEPA)", "Sierra de Martés - Muela de Cortes", "el Fondó d'Elx-Crevillent (ZEPA)",
    "el Fondó d'Elx-Crevillent (ZEPA)", "el Fondó d'Elx-Crevillent (ZEPA)", "Montañas de la Marina"
  ),
  nombre_unificado = c(
    "Parque Natural de las Lagunas de La Mata-Torrevieja", "Parque Natural de las Salinas de Santa Pola",
    "Parque Natural de la Marjal de Pego-Oliva", "Parque Natural de la Marjal de Pego-Oliva",
    "Desembocadura del Millars", "El Barchell", "Lavajos de Sinarcas", "Marjal i Estany d'Almenara",
    "Marjal de la Safor", "Marjal de la Safor", "Marjal de la Safor", "Laguna de San Benito",
    "Embalse de Tibi", "Clot de Galvany", "Parque Natural del Prat de Cabanes",
    "Dehesa de Soneja", "Balsa de Chóvar", "Marjal dels Moros",
    "Parque Natural de l'Albufera de València", "Parque Natural del Fondó d'Elx",
    "Parque Natural del Fondó d'Elx", "Parque Natural del Fondó d'Elx",
    "Parque Natural del Fondó d'Elx", "Fonts de l'Algar"
  ),
  stringsAsFactors = FALSE
)

# Tabla para la unificación final de nombres preferidos (si quedan inconsistencias)
mapa_unificacion_final <- data.frame(
  nombre_actual = c(
    "l'Albufera (ZEPA)", "Lagunas de la Mata y Torrevieja", "Salines de Santa Pola (ZEPA)",
    "Prat de Cabanes i Torreblanca (ZEPA)", "Montdúver - Marjal de la Safor", "Meca - Mugrón - San Benito",
    "Serra d'Espadà (ZEPA)", "el Fondó d'Elx-Crevillent (ZEPA)", "Marjal de Pego-Oliva (ZEPA)",
    "Els Bassars-Clot de Galvany", "Desembocadura y frente litoral del Riu Racons",
    "Marjal de la Safor", "Riu Montnegre", "Desembocadura del Riu de l'Algar",
    "Embalse d'Elx", "El Hondo de Amorós", "Els Carrissars d'Elx", "Dehesa de Soneja",
    "Balsa de Chóvar", "Embalse de Embarcaderos"
  ),
  nombre_preferido = c(
    "Parque Natural de l'Albufera de València", "Parque Natural de las Lagunas de La Mata-Torrevieja",
    "Parque Natural de las Salinas de Santa Pola", "Parque Natural del Prat de Cabanes",
    "Marjal de la Safor", "Laguna de San Benito", "Serra d'Espadà", "Parque Natural del Fondó d'Elx",
    "Parque Natural de la Marjal de Pego-Oliva", "Clot de Galvany",
    "Parque Natural de la Marjal de Pego-Oliva", "Marjal de la Safor", "Montnegre - riu Sec",
    "Fonts de l'Algar", "Parque Natural del Fondó d'Elx", "Parque Natural del Fondó d'Elx",
    "Parque Natural del Fondó d'Elx", "Serra d'Espadà", "Serra d'Espadà",
    "Sierra de Martés - Muela de Cortes"
  ),
  stringsAsFactors = FALSE
)

# ============================================================================
# PASO 3: ANÁLISIS DE SENSIBILIDAD DEL BUFFER Y ASIGNACIÓN
#
# Esta sección aplica diferentes distancias de buffer a las áreas protegidas
# y calcula cuántos registros caen dentro de ellas.
# ============================================================================

# Definir distancias de buffer a analizar (en metros)
buffer_distances <- c(0, 1000, 2500, 5000, 10000) # 0m, 1km, 2.5km, 5km, 10km

# Función para aplicar buffer y calcular intersecciones para una distancia dada
analizar_buffer_completo <- function(dist_metros) {
  # Aplicar buffer si la distancia es > 0, de lo contrario usar la geometría original
  zeps_buffered <- zeps %>%
    st_transform(crs = crs_proj) %>%
    {if (dist_metros > 0) st_buffer(., dist = dist_metros) else .}
  
  humedales_buffered <- humedales %>%
    st_transform(crs = crs_proj) %>%
    {if (dist_metros > 0) st_buffer(., dist = dist_metros) else .}
  
  # Proyectar los registros de eBird al mismo CRS
  combined_sf_proj <- st_transform(combined_sf, crs = crs_proj)
  
  # Unión espacial con las áreas de humedales bufferizadas
  intersect_humedales <- st_join(combined_sf_proj, humedales_buffered["nombre"], left = TRUE) %>%
    as_tibble() %>%
    # Manejar múltiples solapamientos concatenando nombres (y luego seleccionando el primero)
    group_by(id) %>%
    summarise(nombre_humedal_raw = paste(unique(nombre), collapse = "; "), .groups = 'drop') %>%
    mutate(nombre_humedal_raw = na_if(nombre_humedal_raw, "NA")) # Convertir la cadena "NA" a NA real
  
  # Unión espacial con las áreas ZEPA bufferizadas
  intersect_zeps <- st_join(combined_sf_proj, zeps_buffered["nombre"], left = TRUE) %>%
    as_tibble() %>%
    # Manejar múltiples solapamientos
    group_by(id) %>%
    summarise(nombre_zepa_raw = paste(unique(nombre), collapse = "; "), .groups = 'drop') %>%
    mutate(nombre_zepa_raw = na_if(nombre_zepa_raw, "NA"))
  
  # Combinar los resultados de la intersección con los datos originales de eBird
  combined_with_intersections <- combined_sf %>%
    left_join(intersect_humedales, by = "id") %>%
    left_join(intersect_zeps, by = "id") %>%
    # Seleccionar el primer nombre si hay múltiples solapamientos (o mantener NA si no hay)
    mutate(
      nombre_humedal_selected = sapply(strsplit(as.character(nombre_humedal_raw), ";\\s*"), function(x) {
        if(length(x) > 0 && !is.na(x[1]) && x[1] != "NA") x[1] else NA_character_
      }),
      nombre_zepa_selected = sapply(strsplit(as.character(nombre_zepa_raw), ";\\s*"), function(x) {
        if(length(x) > 0 && !is.na(x[1]) && x[1] != "NA") x[1] else NA_character_
      })
    )
  
  # Aplicar la unificación inicial de nombres de la tabla 'equivalencias'
  if(nrow(equivalencias) > 0) {
    combined_with_intersections <- combined_with_intersections %>%
      left_join(equivalencias, by = c("nombre_humedal_selected" = "nombre_humedal", "nombre_zepa_selected" = "nombre_zepa"))
  } else {
    combined_with_intersections$nombre_unificado <- NA_character_ # Añadir columna aunque esté vacía
  }
  
  # Coalescer nombres: nombre_unificado preferido > nombre_humedal_selected > nombre_zepa_selected
  combined_with_intersections <- combined_with_intersections %>%
    mutate(
      nombre_espacio_protegido_temp = coalesce(
        nombre_unificado,
        nombre_humedal_selected,
        nombre_zepa_selected
      )
    )
  
  # Aplicar la unificación final de nombres de la tabla 'mapa_unificacion_final'
  if(nrow(mapa_unificacion_final) > 0) {
    combined_with_intersections <- combined_with_intersections %>%
      left_join(mapa_unificacion_final, by = c("nombre_espacio_protegido_temp" = "nombre_actual"))
  } else {
    combined_with_intersections$nombre_preferido <- NA_character_ # Añadir columna aunque esté vacía
  }
  
  # Determinación final del nombre del espacio protegido
  combined_with_intersections <- combined_with_intersections %>%
    mutate(
      nombre_espacio_protegido = coalesce(nombre_preferido, nombre_espacio_protegido_temp),
      buffer_km = dist_metros / 1000,
      asignado = !is.na(nombre_espacio_protegido)
    )
  
  return(combined_with_intersections)
}

# Ejecutar el análisis para cada distancia de buffer definida
resultados_buffer_list <- map(buffer_distances, analizar_buffer_completo)
names(resultados_buffer_list) <- paste0("buffer_", buffer_distances/1000, "km")

# Calcular estadísticas resumen para cada resultado de buffer
estadisticas_buffer <- map_dfr(resultados_buffer_list, function(x) {
  x %>%
    st_drop_geometry() %>%
    summarise(
      total_registros = n(),
      registros_asignados = sum(asignado),
      registros_no_asignados = sum(!asignado),
      porcentaje_asignados = round(100 * sum(asignado) / n(), 1),
      porcentaje_no_asignados = round(100 * sum(!asignado) / n(), 1),
      espacios_identificados = n_distinct(nombre_espacio_protegido, na.rm = TRUE)
    )
}, .id = "buffer") %>%
  mutate(buffer_km = as.numeric(gsub("buffer_|km", "", buffer)))

# Mostrar los resultados del análisis de sensibilidad del buffer
print("--- RESULTADOS DEL ANÁLISIS DE SENSIBILIDAD DEL BUFFER ---")
print(estadisticas_buffer)

# ============================================================================
# PASO 4: ASIGNACIÓN FINAL CON BUFFER ELEGIDO
#
# Selecciona el conjunto de datos procesado con la distancia de buffer preferida.
# Por defecto es 'buffer_5km', pero puedes cambiarlo basándote en 'estadisticas_buffer'.
# ============================================================================

buffer_optimo <- "buffer_5km" # Define aquí tu buffer deseado, por ejemplo, "buffer_0km", "buffer_10km"
combined_final <- resultados_buffer_list[[buffer_optimo]]

# Calcular y mostrar las estadísticas de la asignación final
total_registros_final <- nrow(combined_final)
registros_asignados_final <- sum(!is.na(combined_final$nombre_espacio_protegido))
registros_no_asignados_final <- sum(is.na(combined_final$nombre_espacio_protegido))
espacios_identificados_final <- combined_final %>%
  st_drop_geometry() %>%
  filter(!is.na(nombre_espacio_protegido)) %>%
  distinct(nombre_espacio_protegido) %>%
  nrow()

print("--- RESUMEN DE LA ASIGNACIÓN FINAL ---")
cat(paste0("Buffer Seleccionado: ", gsub("buffer_|km", "", buffer_optimo), " km\n"))
cat(paste0("Total de Registros: ", format(total_registros_final, big.mark = ","), "\n"))
cat(paste0("Registros Asignados a Espacios Protegidos: ", format(registros_asignados_final, big.mark = ","), " (",
           round(100 * registros_asignados_final / total_registros_final, 1), "%)\n"))
cat(paste0("Registros No Asignados: ", format(registros_no_asignados_final, big.mark = ","), " (",
           round(100 * registros_no_asignados_final / total_registros_final, 1), "%)\n"))
cat(paste0("Espacios Protegidos Únicos Identificados: ", espacios_identificados_final, "\n"))

# ============================================================================
# PASO 5: ANÁLISIS DETALLADO DE REGISTROS NO ASIGNADOS
#
# Esta sección caracteriza los registros que no cayeron dentro de ningún
# espacio protegido (ni siquiera con el buffer elegido).
# ============================================================================

registros_no_asignados <- combined_final %>%
  filter(is.na(nombre_espacio_protegido))

registros_asignados <- combined_final %>%
  filter(!is.na(nombre_espacio_protegido))

# Top 20 especies en registros no asignados
especies_no_asignadas <- registros_no_asignados %>%
  st_drop_geometry() %>%
  count(scientific_name, common_name, sort = TRUE) %>%
  mutate(
    porcentaje = round(100 * n / sum(n), 2),
    acumulado = round(cumsum(100 * n / sum(n)), 1)
  ) %>%
  head(20)

print("--- TOP 20 ESPECIES EN REGISTROS NO ASIGNADOS ---")
print(especies_no_asignadas)

# Análisis de diversidad comparativa (Asignados vs. No Asignados)
diversidad_detallada <- data.frame(
  Ubicacion = c("Espacios Protegidos", "Fuera de Espacios Protegidos"),
  Registros = c(nrow(registros_asignados), nrow(registros_no_asignados)),
  Especies = c(
    n_distinct(registros_asignados$scientific_name),
    n_distinct(registros_no_asignados$scientific_name)
  )
) %>%
  mutate(
    Porcentaje_Registros = round(100 * Registros / sum(Registros), 1),
    Porcentaje_Especies = round(100 * Especies / sum(Especies), 1),
    Registros_por_Especie = round(Registros / Especies, 1),
    Eficiencia_Deteccion = round(Especies / (Registros/1000), 2) # Especies por 1000 registros
  )

print("--- COMPARATIVA DE DIVERSIDAD (PROTEGIDOS VS. NO ASIGNADOS) ---")
print(diversidad_detallada)

# Especies encontradas exclusivamente en Espacios Protegidos
especies_solo_protegidos_df <- registros_asignados %>%
  st_drop_geometry() %>%
  distinct(scientific_name, common_name) %>%
  anti_join(
    registros_no_asignados %>%
      st_drop_geometry() %>%
      distinct(scientific_name),
    by = "scientific_name"
  )

# Especies encontradas exclusivamente fuera de Espacios Protegidos
especies_solo_no_protegidos_df <- registros_no_asignados %>%
  st_drop_geometry() %>%
  distinct(scientific_name, common_name) %>%
  anti_join(
    registros_asignados %>%
      st_drop_geometry() %>%
      distinct(scientific_name),
    by = "scientific_name"
  )

print("--- ANÁLISIS DE ESPECIES EXCLUSIVAS ---")
cat(paste0("Especies encontradas SOLO en Espacios Protegidos: ", nrow(especies_solo_protegidos_df), "\n"))
cat(paste0("Especies encontradas SOLO Fuera de Espacios Protegidos: ", nrow(especies_solo_no_protegidos_df), "\n"))

if (nrow(especies_solo_no_protegidos_df) > 0) {
  print("Especies Detectadas SOLO Fuera de Espacios Protegidos:")
  print(especies_solo_no_protegidos_df)
}

# Análisis de preferencia de especies por hábitats no protegidos
analisis_preferencia <- combined_final %>%
  st_drop_geometry() %>%
  mutate(
    ubicacion = ifelse(is.na(nombre_espacio_protegido), "Fuera", "Dentro")
  ) %>%
  group_by(scientific_name, common_name) %>%
  summarise(
    total = n(),
    dentro_espacios = sum(ubicacion == "Dentro"),
    fuera_espacios = sum(ubicacion == "Fuera"),
    .groups = 'drop'
  ) %>%
  filter(total >= 50) %>% # Filtrar por especies con suficientes registros
  mutate(
    proporcion_fuera = fuera_espacios / total,
    indice_preferencia = (fuera_espacios / nrow(registros_no_asignados)) /
      (dentro_espacios / nrow(registros_asignados))
  ) %>%
  arrange(desc(indice_preferencia))

especies_habitats_no_protegidos <- analisis_preferencia %>%
  filter(proporcion_fuera > 0.3, indice_preferencia > 1.5) %>%
  head(15)

print("--- ESPECIES CON PREFERENCIA POR HÁBITATS NO PROTEGIDOS ---")
print(especies_habitats_no_protegidos %>%
        dplyr::select(common_name, total, proporcion_fuera, indice_preferencia))

# Evaluación de Gaps de Conservación (especies frecuentes mayormente fuera de PAs)
gaps_conservacion <- analisis_preferencia %>%
  filter(
    total >= 100, # Especies frecuentes
    proporcion_fuera > 0.4, # >40% de los registros fuera de PAs
    fuera_espacios >= 50 # Al menos 50 registros fuera de PAs
  ) %>%
  arrange(desc(fuera_espacios)) %>%
  head(20)

print("--- POTENCIALES GAPS DE CONSERVACIÓN (ESPECIES FRECUENTES FUERA DE ESPACIOS PROTEGIDOS) ---")
print(gaps_conservacion %>%
        dplyr::select(common_name, total, dentro_espacios, fuera_espacios, proporcion_fuera))

# Análisis de Distancia a Espacios Protegidos
# Cálculo de distancias para TODOS los registros no asignados (sin muestreo)
estadisticas_distancia <- NULL
tabla_rangos <- NULL

if (exists("humedales") && exists("zeps") && exists("crs_proj")) {
  tryCatch({
    # Crear unión de espacios protegidos con un buffer amplio para el cálculo de distancias
    # Esto asegura que incluso si un registro está lejos, se calcula la distancia al área protegida más cercana.
    buffer_para_distancia <- 5000 # Un buffer amplio de referencia
    humedales_buffer_union <- humedales %>% st_transform(crs = crs_proj) %>% st_buffer(dist = buffer_para_distancia)
    zeps_buffer_union <- zeps %>% st_transform(crs = crs_proj) %>% st_buffer(dist = buffer_para_distancia)
    espacios_union <- st_union(c(st_geometry(humedales_buffer_union), st_geometry(zeps_buffer_union))) %>% st_sf()
    
    # Proyectar los registros no asignados y la unión de espacios protegidos al mismo CRS
    registros_no_asignados_proj <- st_transform(registros_no_asignados, crs_proj)
    espacios_union_proj <- st_transform(espacios_union, crs_proj)
    
    # Calcular la distancia de cada punto no asignado al área protegida más cercana
    distancias <- st_distance(registros_no_asignados_proj, espacios_union_proj)
    distancias_km <- as.numeric(distancias) / 1000 # Convertir a kilómetros
    
    estadisticas_distancia <- data.frame(
      Estadistica = c("Media", "Mediana", "Q1", "Q3", "Mínima", "Máxima"),
      Distancia_km = c(
        round(mean(distancias_km), 1),
        round(median(distancias_km), 1),
        round(quantile(distancias_km, 0.25), 1),
        round(quantile(distancias_km, 0.75), 1),
        round(min(distancias_km), 1),
        round(max(distancias_km), 1)
      )
    )
    
    # Distribuir las distancias en rangos predefinidos
    rangos_distancia <- cut(distancias_km,
                            breaks = c(0, 2, 5, 10, 20, 50, Inf),
                            labels = c("0-2km", "2-5km", "5-10km", "10-20km", "20-50km", ">50km"))
    
    tabla_rangos <- table(rangos_distancia) %>%
      as.data.frame() %>%
      mutate(Porcentaje = round(100 * Freq / sum(Freq), 1))
    
    print("--- ESTADÍSTICAS DE DISTANCIA DE REGISTROS NO ASIGNADOS A ESPACIOS PROTEGIDOS ---")
    print(estadisticas_distancia)
    print("--- DISTRIBUCIÓN POR RANGOS DE DISTANCIA (REGISTROS NO ASIGNADOS) ---")
    print(tabla_rangos)
    
  }, error = function(e) {
    warning(paste("Error al calcular distancias:", e$message))
  })
} else {
  warning("No se encontraron las capas de espacios protegidos (humedales, zeps) o el CRS proyectado (crs_proj) para el análisis de distancias.")
}

# ============================================================================
# PASO 6: VERIFICACIÓN DE COBERTURA TAXONÓMICA
# ============================================================================

# Especies totales únicas en todo el dataset
especies_totales_unicas_df <- combined_final %>%
  st_drop_geometry() %>%
  distinct(scientific_name, common_name)
especies_totales_unicas <- nrow(especies_totales_unicas_df)

# Especies que aparecen en espacios protegidos (solo el scientific_name para el conteo)
especies_en_protegidos_scientific <- combined_final %>%
  filter(!is.na(nombre_espacio_protegido)) %>%
  st_drop_geometry() %>%
  distinct(scientific_name) %>%
  pull(scientific_name)

# Especies que aparecen fuera de espacios protegidos (solo el scientific_name para el conteo)
especies_fuera_protegidos_scientific <- combined_final %>%
  filter(is.na(nombre_espacio_protegido)) %>%
  st_drop_geometry() %>%
  distinct(scientific_name) %>%
  pull(scientific_name)

# Especies que aparecen en AMBOS lugares (solapamiento de nombres científicos)
especies_solapamiento_scientific <- intersect(especies_en_protegidos_scientific, especies_fuera_protegidos_scientific)

# Especies exclusivas de cada ámbito (solo el scientific_name para el conteo)
especies_solo_protegidos_scientific <- setdiff(especies_en_protegidos_scientific, especies_fuera_protegidos_scientific)
especies_solo_fuera_scientific <- setdiff(especies_fuera_protegidos_scientific, especies_en_protegidos_scientific)

# Verificación de cálculos (la suma de exclusivos y solapamiento debe ser igual al total)
verificacion_total_tax <- length(especies_solo_protegidos_scientific) +
  length(especies_solo_fuera_scientific) +
  length(especies_solapamiento_scientific)

# Cálculo de la cobertura taxonómica real (porcentaje de especies en protegidos sobre el total)
cobertura_taxonomica_real <- round(100 * length(especies_en_protegidos_scientific) / especies_totales_unicas, 1)

# Tabla resumen detallada de la cobertura taxonómica
resumen_taxonomico <- data.frame(
  Categoría = c(
    "Total especies únicas",
    "En espacios protegidos",
    "Fuera de espacios protegidos",
    "Solo en espacios protegidos",
    "Solo fuera de espacios protegidos",
    "En ambos lugares (solapamiento)"
  ),
  Número = c(
    especies_totales_unicas,
    length(especies_en_protegidos_scientific),
    length(especies_fuera_protegidos_scientific),
    length(especies_solo_protegidos_scientific),
    length(especies_solo_fuera_scientific),
    length(especies_solapamiento_scientific)
  ),
  Porcentaje_del_Total = c(
    100,
    round(100 * length(especies_en_protegidos_scientific) / especies_totales_unicas, 1),
    round(100 * length(especies_fuera_protegidos_scientific) / especies_totales_unicas, 1),
    round(100 * length(especies_solo_protegidos_scientific) / especies_totales_unicas, 1),
    round(100 * length(especies_solo_fuera_scientific) / especies_totales_unicas, 1),
    round(100 * length(especies_solapamiento_scientific) / especies_totales_unicas, 1)
  )
)

print("--- RESUMEN TAXONÓMICO DETALLADO ---")
print(resumen_taxonomico)

# Estadísticas adicionales sobre la distribución de especies
print("--- ESTADÍSTICAS ADICIONALES DE COBERTURA TAXONÓMICA ---")
cat(paste0("Proporción de especies que usa SOLO espacios protegidos: ",
           round(100 * length(especies_solo_protegidos_scientific) / especies_totales_unicas, 1), "%\n"))
cat(paste0("Proporción de especies que usa SOLO áreas no protegidas: ",
           round(100 * length(especies_solo_fuera_scientific) / especies_totales_unicas, 1), "%\n"))
cat(paste0("Proporción de especies generalistas (ambos hábitats): ",
           round(100 * length(especies_solapamiento_scientific) / especies_totales_unicas, 1), "%\n"))

# Lista de especies exclusivas (las 10 primeras, si las hay)
print("--- ESPECIES EXCLUSIVAS DE ESPACIOS PROTEGIDOS (primeras 10) ---")
if(length(especies_solo_protegidos_scientific) > 0) {
  especies_solo_protegidos_nombres <- especies_totales_unicas_df %>%
    filter(scientific_name %in% especies_solo_protegidos_scientific) %>%
    head(10) # Mostrar solo las 10 primeras
  print(especies_solo_protegidos_nombres)
} else {
  cat("No hay especies exclusivas de espacios protegidos.\n")
}

print("--- ESPECIES EXCLUSIVAS FUERA DE ESPACIOS PROTEGIDOS ---")
if(length(especies_solo_fuera_scientific) > 0) {
  especies_solo_fuera_nombres <- especies_totales_unicas_df %>%
    filter(scientific_name %in% especies_solo_fuera_scientific)
  print(especies_solo_fuera_nombres) # Mostrar todas las exclusivas de fuera
} else {
  cat("No hay especies exclusivas fuera de espacios protegidos.\n")
}

print("--- RESULTADO PRINCIPAL DE COBERTURA TAXONÓMICA ---")
cat(paste0("LA COBERTURA TAXONÓMICA REAL DE LOS ESPACIOS PROTEGIDOS ES: ", cobertura_taxonomica_real, "%\n"))
cat(paste0("Esto significa que de las ", especies_totales_unicas, " especies detectadas en la Comunidad Valenciana,\n"))
cat(paste0(length(especies_en_protegidos_scientific), " especies (", cobertura_taxonomica_real, "%) se encuentran en espacios protegidos.\n"))

# ============================================================================
# PASO 7: GENERACIÓN DE MAPAS INTERACTIVOS
#
# Esta sección crea mapas interactivos Leaflet para visualizar los resultados.
# ============================================================================

# Convertir a WGS84 para Leaflet las capas de espacios protegidos (sin buffer)
humedales_wgs84 <- st_transform(humedales, crs = 4326)
zeps_wgs84 <- st_transform(zeps, crs = 4326)

# Crear capas bufferizadas para visualización (usando el buffer_optimo definido previamente)
buffer_seleccionado_km <- as.numeric(gsub("buffer_|km", "", buffer_optimo))
buffer_seleccionado_metros <- buffer_seleccionado_km * 1000

humedales_buffer_viz <- humedales %>%
  st_transform(crs = crs_proj) %>%
  st_buffer(dist = buffer_seleccionado_metros) %>%
  st_transform(crs = 4326)

zeps_buffer_viz <- zeps %>%
  st_transform(crs = crs_proj) %>%
  st_buffer(dist = buffer_seleccionado_metros) %>%
  st_transform(crs = 4326)

# Extraer coordenadas para el heatmap de registros no asignados
coords_heatmap <- st_coordinates(registros_no_asignados)
heatmap_data <- data.frame(
  lng = coords_heatmap[, "X"],
  lat = coords_heatmap[, "Y"]
)

# Mapa con buffer del tamaño elegido + heatmap de registros no asignados
mapa_calor_no_asignados <- leaflet() %>%
  setView(lng = -0.4, lat = 39.5, zoom = 8) %>% # Centrar en Valencia
  # Capas base
  addProviderTiles("CartoDB.Positron", group = "Mapa Base") %>%
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satélite") %>%
  # Espacios protegidos con buffer del tamaño elegido
  addPolygons(
    data = humedales_buffer_viz,
    color = "#8d4925", fillColor = "#8d4925", fillOpacity = 0.3,
    weight = 2, opacity = 0.8,
    group = paste0("Zonas Húmedas (", buffer_seleccionado_km, "km buffer)"),
    popup = ~paste0("<b>", nombre, "</b><br>Tipo: Zona Húmeda + Buffer ", buffer_seleccionado_km, "km")
  ) %>%
  addPolygons(
    data = zeps_buffer_viz,
    color = "#ffb48a", fillColor = "#ffb48a", fillOpacity = 0.3,
    weight = 2, opacity = 0.8,
    group = paste0("Zonas ZEPA (", buffer_seleccionado_km, "km buffer)"),
    popup = ~paste0("<b>", nombre, "</b><br>Tipo: ZEPA + Buffer ", buffer_seleccionado_km, "km")
  ) %>%
  # HEATMAP de registros NO asignados
  addHeatmap(
    data = heatmap_data, lng = ~lng, lat = ~lat,
    blur = 20, max = 0.6, radius = 15,
    group = "Densidad Registros NO Asignados"
  ) %>%
  # Control de capas
  addLayersControl(
    baseGroups = c("Mapa Base", "OpenStreetMap", "Satélite"),
    overlayGroups = c(
      paste0("Zonas Húmedas (", buffer_seleccionado_km, "km buffer)"),
      paste0("Zonas ZEPA (", buffer_seleccionado_km, "km buffer)"),
      "Densidad Registros NO Asignados"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Leyenda
  addLegend(
    position = "bottomright",
    colors = c("#8d4925", "#ffb48a", "red"), # 'red' es conceptual para heatmap, Leaflet.heat no usa colores fijos
    labels = c(paste0("Zonas Húmedas + Buffer ", buffer_seleccionado_km, "km"),
               paste0("Zonas ZEPA + Buffer ", buffer_seleccionado_km, "km"),
               "Densidad Registros NO Asignados"),
    title = "Análisis de Cobertura", opacity = 0.8
  ) %>%
  addScaleBar(position = "bottomleft") # Barra de escala

print(mapa_calor_no_asignados)

# Mapa con límites exactos de espacios protegidos + heatmap de no asignados
mapa_limites_heatmap <- leaflet() %>%
  setView(lng = -0.4, lat = 39.5, zoom = 8) %>% # Centrar en Valencia
  # Capas base
  addProviderTiles("CartoDB.Positron", group = "Mapa Base") %>%
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satélite") %>%
  # Límites exactos de espacios protegidos
  addPolygons(
    data = humedales_wgs84,
    color = "#8d4925", fillColor = "#8d4925", fillOpacity = 0.4,
    weight = 2, opacity = 0.9,
    group = "Zonas Húmedas"
  ) %>%
  addPolygons(
    data = zeps_wgs84,
    color = "#ffb48a", fillColor = "#ffb48a", fillOpacity = 0.4,
    weight = 2, opacity = 0.9,
    group = "Zonas ZEPA"
  ) %>%
  # Heatmap encima
  addHeatmap(
    data = heatmap_data, lng = ~lng, lat = ~lat,
    blur = 20, max = 0.6, radius = 15,
    group = "Densidad Registros NO Asignados"
  ) %>%
  # Control de capas
  addLayersControl(
    baseGroups = c("Mapa Base", "OpenStreetMap", "Satélite"),
    overlayGroups = c("Zonas Húmedas", "Zonas ZEPA", "Densidad Registros NO Asignados"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addScaleBar(position = "bottomleft") # Barra de escala

print(mapa_limites_heatmap)

# ============================================================================
# PASO 8: RESUMEN FINAL POR ESPACIO PROTEGIDO
#
# Esta sección genera un resumen de las observaciones y especies por cada
# espacio protegido identificado en la asignación final.
# ============================================================================

# Número de observaciones por espacio protegido
obs_por_espacio <- combined_final %>%
  st_drop_geometry() %>%
  count(nombre_espacio_protegido, name = "n_observaciones") %>%
  arrange(desc(n_observaciones))

# Número de especies distintas por espacio protegido
especies_por_espacio <- combined_final %>%
  st_drop_geometry() %>%
  group_by(nombre_espacio_protegido) %>%
  summarise(n_especies = n_distinct(scientific_name), .groups = "drop")

# Top 3 especies más comunes por espacio protegido
top_especies_por_espacio <- combined_final %>%
  st_drop_geometry() %>%
  group_by(nombre_espacio_protegido, common_name) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(nombre_espacio_protegido) %>%
  slice_max(order_by = n, n = 3) %>%
  # Concatenar las top especies si hay múltiples para un mismo espacio
  summarise(top_especies = paste(common_name, collapse = "; "), .groups = 'drop')

# Unir toda la información en un resumen final
resumen_espacios <- obs_por_espacio %>%
  left_join(especies_por_espacio, by = "nombre_espacio_protegido") %>%
  left_join(top_especies_por_espacio, by = "nombre_espacio_protegido")

print("--- RESUMEN DETALLADO POR ESPACIO PROTEGIDO ---")
print(resumen_espacios)

# ============================================================================
# ANÁLISIS COMPLETADO
# ============================================================================