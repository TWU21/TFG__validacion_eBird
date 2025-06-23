library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(scales)
library(viridis) # Para paletas de color, aunque no se usa en este script, es buena práctica tenerla si se usó antes

# Asegúrate de que 'combined' esté cargado en tu entorno.
# Por ejemplo: combined <- read.csv("ruta/a/tu/archivo_combined.csv")

# --- 1. Preparación de Datos ---
nombres_meses_esp <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
                       "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
dias_esp <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")

colores_provincias <- c(
  "Valencia" = "#a84b4c",
  "Alicante" = "#320000",
  "Castellón" = "#a6a447"
)

combined_temporal <- combined %>%
  mutate(
    año = year(datetime),
    mes = month(datetime),
    dia_semana = as.integer(format(datetime, "%u")), # ISO 8601: Lunes=1, Domingo=7
    hora = hour(datetime),
    mes_nombre = factor(nombres_meses_esp[mes], levels = nombres_meses_esp),
    dia_nombre = factor(dias_esp[dia_semana], levels = dias_esp)
  ) %>%
  filter(año >= 2021 & año <= 2024) # Años completos solamente

# --- 2. Análisis de Estacionalidad Mensual ---
estadisticas_mensuales <- combined_temporal %>%
  count(año, mes, mes_nombre) %>%
  group_by(mes, mes_nombre) %>%
  summarise(
    promedio = mean(n),
    mediana = median(n),
    desviacion = sd(n),
    min_registros = min(n),
    max_registros = max(n),
    coef_variacion = sd(n) / mean(n),
    .groups = "drop"
  ) %>%
  arrange(mes)

plot_mensual_general <- estadisticas_mensuales %>%
  ggplot(aes(x = mes_nombre, y = promedio, group = 1)) +
  geom_line(color = "#8d4925", linewidth = 1.2) +
  geom_point(color = "#320000", size = 3) +
  geom_text(aes(label = comma(round(promedio, 0))),
            vjust = -0.8, size = 3, color = "#320000") +
  labs(
    title = "Promedio mensual de avistamientos",
    x = "Mes",
    y = "Promedio de avistamientos",
    caption = "Fuente: eBird 2021–2024"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "italic"),
    plot.caption = element_text(face = "italic", hjust = 0)
  ) +
  scale_y_continuous(labels = comma)

estadisticas_provinciales <- combined_temporal %>%
  filter(county %in% names(colores_provincias)) %>% # Asegurar que solo se incluyan provincias con color definido
  count(county, año, mes, mes_nombre) %>%
  group_by(county, mes, mes_nombre) %>%
  summarise(
    promedio = mean(n),
    desviacion = sd(n),
    .groups = "drop"
  )

plot_mensual_provincias <- estadisticas_provinciales %>%
  ggplot(aes(x = mes_nombre, y = promedio, color = county, group = county)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = colores_provincias) +
  labs(
    title = "Promedio mensual por provincia",
    x = "Mes",
    y = "Promedio de avistamientos",
    color = "Provincia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    axis.title = element_text(face = "italic"),
    legend.title = element_text(face = "italic")
  ) +
  scale_y_continuous(labels = comma)

# --- 3. Análisis de Patrones Semanales (Día de la Semana) ---
estadisticas_semanales <- combined_temporal %>%
  count(año, dia_semana, dia_nombre) %>%
  group_by(dia_semana, dia_nombre) %>%
  summarise(
    promedio = mean(n),
    mediana = median(n),
    desviacion = sd(n),
    .groups = "drop"
  ) %>%
  arrange(dia_semana)

plot_semanal <- estadisticas_semanales %>%
  ggplot(aes(x = dia_nombre, y = promedio, group = 1)) +
  geom_line(color = "#8d4925", linewidth = 1.2) +
  geom_point(color = "#320000", size = 3) +
  geom_text(aes(label = comma(round(promedio, 0))),
            vjust = -0.8, size = 3) +
  labs(
    title = "Promedio de avistamientos por día de la semana",
    x = "Día de la semana",
    y = "Promedio de avistamientos"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "italic")
  ) +
  scale_y_continuous(labels = comma)

plot_semanal_provincias <- combined_temporal %>%
  filter(county %in% names(colores_provincias)) %>%
  count(county, año, dia_semana, dia_nombre) %>%
  group_by(county, dia_semana, dia_nombre) %>%
  summarise(promedio = mean(n), .groups = "drop") %>%
  ggplot(aes(x = dia_nombre, y = promedio, color = county, group = county)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = colores_provincias) +
  labs(
    title = "Promedio semanal por provincia",
    x = "Día de la semana",
    y = "Promedio de avistamientos",
    color = "Provincia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    axis.title = element_text(face = "italic")
  ) +
  scale_y_continuous(labels = comma)

# --- 4. Análisis de Patrones Horarios (Hora del Día) ---
estadisticas_horarias <- combined_temporal %>%
  count(año, hora) %>%
  group_by(hora) %>%
  summarise(
    promedio = mean(n),
    mediana = median(n),
    desviacion = sd(n),
    .groups = "drop"
  ) %>%
  arrange(hora)

pico_matutino <- estadisticas_horarias$hora[which.max(estadisticas_horarias$promedio)]

plot_horario <- estadisticas_horarias %>%
  ggplot(aes(x = hora, y = promedio)) +
  geom_line(color = "#8d4925", linewidth = 1.2) +
  geom_point(color = "#320000", size = 2) +
  geom_vline(xintercept = pico_matutino, linetype = "dashed",
             color = "red", alpha = 0.7) +
  annotate("text", x = pico_matutino + 1, y = max(estadisticas_horarias$promedio) * 0.9,
           label = paste("Pico:", pico_matutino, ":00"), color = "red") +
  labs(
    title = "Promedio de avistamientos por hora del día",
    x = "Hora (0-24)",
    y = "Promedio de avistamientos"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "italic")
  ) +
  scale_x_continuous(breaks = seq(0, 24, 2)) +
  scale_y_continuous(labels = comma)

# --- 5. Análisis de Tendencias Interanuales ---
evolucion_anual <- combined_temporal %>%
  count(año) %>%
  arrange(año)

plot_tendencia_anual <- evolucion_anual %>%
  ggplot(aes(x = año, y = n)) +
  geom_line(color = "#8d4925", linewidth = 1.2) +
  geom_point(color = "#320000", size = 3) +
  geom_text(aes(label = comma(n)), vjust = -0.8, size = 3) +
  labs(
    title = "Evolución anual del número de registros",
    x = "Año",
    y = "Número total de registros"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "italic")
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = unique(evolucion_anual$año)) # Asegura que todos los años en los datos aparezcan como breaks

# --- 6. Combinación y Exportación de Gráficos ---

# Gráfico combinado de estacionalidad mensual
plot_estacionalidad_combinado <- plot_mensual_general + plot_mensual_provincias +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    title = "Análisis de estacionalidad de avistamientos de aves (2021–2024)",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )
print(plot_estacionalidad_combinado)
ggsave("figuras/patron_mensual.png", plot_estacionalidad_combinado, width = 14, height = 8, dpi = 300, bg = "white")

# Gráfico combinado de patrones temporales detallados (semanal y horario)
plot_temporal_detallado <- plot_semanal / plot_horario +
  plot_annotation(
    title = "Patrones temporales detallados de avistamientos",
    subtitle = "Comunidad Valenciana (2021-2024)",
    caption = "Fuente: eBird. Análisis basado en años completos para evitar sesgos estacionales.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(face = "italic", hjust = 0)
    )
  )
print(plot_temporal_detallado)
ggsave("figuras/patron_temporal_detallado.png", plot_temporal_detallado, width = 12, height = 10, dpi = 300, bg = "white")

# Gráfico de tendencia anual
print(plot_tendencia_anual)
ggsave("figuras/tendencia_anual.png", plot_tendencia_anual, width = 10, height = 6, dpi = 300, bg = "white")

# Gráfico semanal por provincias
print(plot_semanal_provincias)
ggsave("figuras/patron_semanal_provincias.png", plot_semanal_provincias, width = 12, height = 8, dpi = 300, bg = "white")

# --- 7. Resumen Estadístico para el Informe ---
# Cálculos de métricas para el resumen
completitud_datos <- combined_temporal %>%
  count(año, mes) %>%
  group_by(año) %>%
  summarise(
    meses_con_datos = n(),
    total_registros = sum(n),
    .groups = "drop"
  )

pico_primavera <- estadisticas_mensuales$mes_nombre[which.max(estadisticas_mensuales$promedio)]
minimo_verano <- estadisticas_mensuales$mes_nombre[which.min(estadisticas_mensuales$promedio)]

fin_semana <- estadisticas_semanales %>%
  filter(dia_semana %in% c(6, 7)) %>%
  summarise(promedio_fds = mean(promedio)) %>% pull(promedio_fds)

entre_semana <- estadisticas_semanales %>%
  filter(dia_semana %in% c(1, 2, 3, 4, 5)) %>%
  summarise(promedio_es = mean(promedio)) %>% pull(promedio_es)

efecto_fds <- (fin_semana - entre_semana) / entre_semana * 100

tasa_crecimiento <- if(nrow(evolucion_anual) > 1) {
  ((evolucion_anual$n[nrow(evolucion_anual)] / evolucion_anual$n[1])^(1/(nrow(evolucion_anual)-1)) - 1) * 100
} else {
  NA # No se puede calcular si solo hay un año
}

actividad_matutina <- estadisticas_horarias %>%
  filter(hora >= 6 & hora <= 12) %>%
  summarise(total = sum(promedio)) %>% pull(total)

actividad_total <- sum(estadisticas_horarias$promedio)
proporcion_matutina <- actividad_matutina / actividad_total * 100

cat("
============================================================
RESUMEN ESTADÍSTICO DEL ANÁLISIS TEMPORAL
============================================================

DATOS GENERALES:
- Período analizado: 2021-2024 (años completos)
- Total de registros: ", comma(nrow(combined_temporal)), "
- Promedio anual: ", comma(round(mean(evolucion_anual$n), 0)), "

PATRONES ESTACIONALES:
- Mes con mayor actividad: ", as.character(pico_primavera), "
- Mes con menor actividad: ", as.character(minimo_verano), "
- Ratio máximo/mínimo: ", round(max(estadisticas_mensuales$promedio)/min(estadisticas_mensuales$promedio), 1), "

PATRONES SEMANALES:
- Efecto fin de semana: +", round(efecto_fds, 1), "%
- Día con más registros: ", as.character(estadisticas_semanales$dia_nombre[which.max(estadisticas_semanales$promedio)]), "

PATRONES HORARIOS:
- Pico de actividad: ", pico_matutino, ":00 horas
- Actividad matutina (6-12h): ", round(proporcion_matutina, 1), "% del total

TENDENCIAS:
- Crecimiento anual promedio: ", if(is.na(tasa_crecimiento)) "No calculable con un solo año" else paste0("+", round(tasa_crecimiento, 1), "%"), "

Análisis temporal completado. Gráficos guardados en la carpeta 'figuras/'.
")