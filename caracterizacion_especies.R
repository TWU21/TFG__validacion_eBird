library(dplyr)
library(ggplot2)
library(scales) # Para formato de números en gráficos
library(forcats) # Para reordenar factores en ggplot
library(patchwork) # Para combinar gráficos

#Partimos de combined

# --- 1. Preparación de datos de especies ---
tabla_especies <- combined %>%
  count(scientific_name, sort = TRUE) %>%
  mutate(
    proporcion = n / sum(n),
    prop_acumulada = cumsum(proporcion),
    rank = row_number()
  )

# --- 2. Métricas de Diversidad Básicas ---
num_especies <- nrow(tabla_especies)
total_registros <- sum(tabla_especies$n)

concentracion_top20 <- tabla_especies %>%
  slice_head(n = 20) %>%
  summarise(prop_top20 = sum(proporcion)) %>%
  pull(prop_top20)

top5_pct_especies <- ceiling(num_especies * 0.05)
concentracion_top5pct <- tabla_especies %>%
  slice_head(n = top5_pct_especies) %>%
  summarise(prop_top5pct = sum(proporcion)) %>%
  pull(prop_top5pct)

shannon <- -sum(tabla_especies$proporcion * log(tabla_especies$proporcion))
shannon_max <- log(num_especies)
equitatividad <- shannon / shannon_max

especies_raras <- tabla_especies %>%
  filter(n <= 2) %>%
  nrow()

prop_especies_raras <- especies_raras / num_especies

cat("=== MÉTRICAS DE DIVERSIDAD GENERAL ===\n")
cat("Riqueza total:", num_especies, "especies\n")
cat("Total registros:", comma(total_registros), "\n")
cat("Concentración top 20 especies:", percent(concentracion_top20, 1), "\n")
cat("Concentración top 5% especies:", percent(concentracion_top5pct, 1), "\n")
cat("Índice de Shannon (H'):", round(shannon, 3), "\n")
cat("Equitatividad (J'):", round(equitatividad, 3), "\n")
cat("Especies raras (≤2 registros):", especies_raras, "(", percent(prop_especies_raras, 1), ")\n\n")

# --- 3. Visualización de Abundancia ---
plot_rank_abundance <- ggplot(tabla_especies, aes(x = rank, y = n)) +
  geom_point(alpha = 0.6, color = "#8d4925") +
  geom_line(alpha = 0.4, color = "#8d4925") +
  scale_y_log10(labels = comma) +
  scale_x_log10() +
  labs(
    title = "Curva Rank-Abundance",
    subtitle = "Distribución de frecuencias de especies (escala log-log)",
    x = "Rango (especies ordenadas por abundancia)",
    y = "Número de registros (escala log)",
    caption = "Fuente: eBird 2021-2025"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

freq_distribution <- tabla_especies %>%
  mutate(
    freq_class = case_when(
      n == 1 ~ "1",
      n == 2 ~ "2",
      n <= 5 ~ "3-5",
      n <= 10 ~ "6-10",
      n <= 50 ~ "11-50",
      n <= 100 ~ "51-100",
      n <= 500 ~ "101-500",
      n <= 1000 ~ "501-1000",
      TRUE ~ ">1000"
    )
  ) %>%
  count(freq_class, name = "num_especies") %>%
  mutate(
    freq_class = factor(freq_class, levels = c("1", "2", "3-5", "6-10", "11-50",
                                               "51-100", "101-500", "501-1000", ">1000"))
  )

plot_freq_dist <- ggplot(freq_distribution, aes(x = freq_class, y = num_especies)) +
  geom_col(fill = "#8d4925", alpha = 0.8) +
  geom_text(aes(label = num_especies), vjust = -0.3, size = 3) +
  labs(
    title = "Distribución de especies por frecuencia de registros",
    x = "Número de registros por especie",
    y = "Número de especies",
    caption = "Fuente: eBird 2021-2025"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(plot_rank_abundance)
print(plot_freq_dist)

# --- 4. Análisis por Provincia ---
provincias_interes <- c("Alicante", "Castellón", "Valencia")

metricas_provincia <- combined %>%
  filter(county %in% provincias_interes) %>%
  group_by(county) %>%
  summarise(
    num_especies = n_distinct(scientific_name),
    total_registros = n(),
    registros_por_especie_media = total_registros / num_especies,
    .groups = 'drop'
  ) %>%
  arrange(desc(total_registros))

shannon_provincia <- combined %>%
  filter(county %in% provincias_interes) %>%
  count(county, scientific_name) %>%
  group_by(county) %>%
  mutate(
    proporcion = n / sum(n),
    shannon_contrib = -proporcion * log(proporcion)
  ) %>%
  summarise(
    shannon = sum(shannon_contrib),
    riqueza = n_distinct(scientific_name),
    equitatividad = shannon / log(riqueza),
    .groups = 'drop'
  )

metricas_completas_prov <- metricas_provincia %>%
  left_join(shannon_provincia, by = "county")

cat("\n=== MÉTRICAS DE DIVERSIDAD POR PROVINCIA ===\n")
print(metricas_completas_prov)

plot_diversidad_prov <- metricas_completas_prov %>%
  select(county, riqueza = num_especies, shannon, equitatividad) %>%
  pivot_longer(cols = -county, names_to = "metrica", values_to = "valor") %>%
  mutate(
    metrica = factor(metrica,
                     levels = c("riqueza", "shannon", "equitatividad"),
                     labels = c("Riqueza\n(especies)", "Shannon\n(H')", "Equitatividad\n(J')"))
  ) %>%
  ggplot(aes(x = county, y = valor, fill = county)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~metrica, scales = "free_y") +
  scale_fill_manual(values = c("#320000", "#a6a447", "#a84b4c")) +
  labs(
    title = "Métricas de diversidad por provincia",
    x = "Provincia",
    y = "Valor",
    caption = "Fuente: eBird 2021-2025"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(plot_diversidad_prov)

# --- 5. Clasificación de Especies por Abundancia ---
especies_clasificadas <- tabla_especies %>%
  mutate(
    categoria = case_when(
      rank <= 10 ~ "Muy abundantes (Top 10)",
      rank <= 50 ~ "Abundantes (11-50)",
      n >= 100 & rank > 50 ~ "Frecuentes (≥100 registros)", # No en top 50, pero frecuentes
      n >= 10 & n < 100 ~ "Ocasionales (10-99 registros)",
      TRUE ~ "Raras (<10 registros)"
    ),
    categoria = factor(categoria, levels = c("Muy abundantes (Top 10)",
                                             "Abundantes (11-50)",
                                             "Frecuentes (≥100 registros)",
                                             "Ocasionales (10-99 registros)",
                                             "Raras (<10 registros)"))
  )

resumen_categorias <- especies_clasificadas %>%
  group_by(categoria) %>%
  summarise(
    num_especies = n(),
    total_registros = sum(n),
    prop_especies = num_especies / nrow(tabla_especies),
    prop_registros = total_registros / sum(tabla_especies$n),
    .groups = 'drop'
  )

cat("\n=== RESUMEN DE ESPECIES POR CATEGORÍA DE ABUNDANCIA ===\n")
print(resumen_categorias)

plot_categorias <- resumen_categorias %>%
  select(categoria, num_especies, total_registros) %>%
  pivot_longer(cols = -categoria, names_to = "tipo", values_to = "valor") %>%
  mutate(
    tipo = if_else(tipo == "num_especies", "Número de especies", "Total de registros")
  ) %>%
  ggplot(aes(x = tipo, y = valor, fill = categoria)) +
  geom_col(position = "stack") +
  scale_fill_viridis_d(name = "Categoría de abundancia") +
  labs(
    title = "Distribución de especies y registros por categoría de abundancia",
    x = NULL,
    y = "Cantidad",
    caption = "Fuente: eBird 2021-2025"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 10)
  ) +
  guides(fill = guide_legend(ncol = 2))

print(plot_categorias)

# --- 6. Especies Más Caracteristicas por Provincia ---
especies_por_provincia <- combined %>%
  filter(county %in% provincias_interes) %>%
  count(county, scientific_name) %>%
  group_by(scientific_name) %>%
  mutate(
    total_especie = sum(n),
    prop_en_provincia = n / total_especie
  ) %>%
  filter(total_especie >= 20) %>% # Solo especies con suficientes registros para ser relevantes
  ungroup()

especies_caracteristicas <- especies_por_provincia %>%
  group_by(county) %>%
  arrange(desc(prop_en_provincia)) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  select(county, scientific_name, prop_en_provincia, total_registros_provincia = n)

cat("\n=== ESPECIES MÁS CARACTERÍSTICAS POR PROVINCIA ===\n")
print(especies_caracteristicas)


# --- 7. Visualización de Top Especies (General y por Provincia) ---
top20_general <- combined %>%
  count(scientific_name, sort = TRUE) %>%
  head(20)

plot_general <- ggplot(top20_general, aes(x = n, y = fct_reorder(scientific_name, n))) +
  geom_col(fill = "#8d4925", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = comma(n)),
            hjust = -0.1, size = 3, color = "black") +
  labs(
    title = "Top 20 especies en toda la Comunidad Valenciana",
    subtitle = "Total de avistamientos registrados",
    x = "Número de avistamientos",
    y = NULL,
    caption = "Fuente de datos: eBird 2021-2025"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "italic")
  )

top_provincias_plot <- combined %>%
  filter(county %in% provincias_interes) %>%
  group_by(county) %>%
  count(scientific_name, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ungroup()

plot_provincias <- ggplot(top_provincias_plot, aes(x = n, y = fct_reorder(scientific_name, n))) +
  geom_col(aes(fill = county), alpha = 0.8, width = 0.7) +
  geom_text(aes(label = n),
            hjust = -0.1, size = 2.5, color = "black") +
  facet_wrap(~county, scales = "free_y", ncol = 1) +
  labs(
    title = "Top 10 especies por provincia",
    x = "Número de avistamientos",
    y = NULL
  ) +
  scale_fill_manual(values = c("#320000", "#a6a447", "#a84b4c")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(face = "italic", size = 8),
    panel.spacing = unit(1, "lines")
  )

plot_final_combinado <- plot_general + plot_provincias +
  plot_layout(widths = c(2, 1)) +
  plot_annotation(
    title = "Análisis de avistamientos de aves en la Comunidad Valenciana",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

print(plot_final_combinado)

ggsave("avistamientos_aves_cv.png", plot_final_combinado,
       width = 14, height = 10, dpi = 300)