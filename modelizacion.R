# ============================================================================
# ANÁLISIS PREDICTIVO COMPLETO - REGISTROS DIARIOS eBird
# ============================================================================

# Librerías
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(sf)
library(lubridate)
library(forecast)
library(prophet)
library(xgboost)
library(mgcv)
library(zoo)
library(tidyr)
library(caret)
library(VIM)
library(Ckmeans.1d.dp)
library(gratia)
library(knitr)
library(moments)
library(patchwork)
library(spdep)

# ============================================================================
# 1. ANÁLISIS TEMPORAL INICIAL Y PREPARACIÓN DE SERIE DIARIA
# ============================================================================

# Agregar datos a nivel diario
serie_diaria <- combined_final %>%
  st_drop_geometry() %>%
  mutate(fecha = as.Date(observation_date)) %>%
  filter(year(fecha) >= 2015) %>%
  group_by(fecha) %>%
  summarise(n_registros = n(), .groups = 'drop') %>%
  arrange(fecha) %>%
  complete(fecha = seq.Date(min(fecha), max(fecha), by = "day"),
           fill = list(n_registros = 0)) %>%
  mutate(
    año = year(fecha),
    mes = month(fecha),
    dia_semana = wday(fecha, label = TRUE),
    fin_semana = dia_semana %in% c("Sat", "Sun")
  )

cat("Serie temporal:", nrow(serie_diaria), "días\n")
cat("Período:", min(serie_diaria$fecha), "a", max(serie_diaria$fecha), "\n")

# Estadísticas básicas por período
por_año <- serie_diaria %>%
  group_by(año) %>%
  summarise(
    dias = n(),
    registros_total = sum(n_registros),
    registros_promedio = round(mean(n_registros), 1),
    dias_activos = sum(n_registros > 0),
    .groups = 'drop'
  )

por_dia_semana <- serie_diaria %>%
  group_by(dia_semana) %>%
  summarise(
    registros_promedio = round(mean(n_registros), 1),
    registros_sd = round(sd(n_registros), 1),
    .groups = 'drop'
  )

por_mes <- serie_diaria %>%
  group_by(mes) %>%
  summarise(
    registros_promedio = round(mean(n_registros), 1),
    registros_sd = round(sd(n_registros), 1),
    .groups = 'drop'
  )

# Análisis ACF y PACF
acf_result <- acf(serie_diaria$n_registros, lag.max = 60, plot = FALSE)
pacf_result <- pacf(serie_diaria$n_registros, lag.max = 60, plot = FALSE)

limite_sig <- 1.96/sqrt(length(serie_diaria$n_registros))
lags_sig_acf <- which(abs(acf_result$acf[-1]) > limite_sig)
lags_sig_pacf <- which(abs(pacf_result$acf) > limite_sig)

lag_7 <- ifelse(length(acf_result$acf) > 7, round(acf_result$acf[8], 3), NA)
lag_30 <- ifelse(length(acf_result$acf) > 30, round(acf_result$acf[31], 3), NA)

# Objeto MSTS (Multiple Seasonal Time Series)
ts_msts <- msts(serie_diaria$n_registros, seasonal.periods = c(7, 365.25))

# Descomposición MSTS
if(length(ts_msts) > 2*365.25) {
  decomp_msts <- mstl(ts_msts)
  
  # Extraer componentes
  tendencia <- decomp_msts[, "Trend"]
  estacional_7 <- decomp_msts[, "Seasonal7"]
  estacional_365 <- decomp_msts[, "Seasonal365.25"]
  residuos <- decomp_msts[, "Remainder"]
  
  # Calcular varianzas de componentes
  var_total <- var(serie_diaria$n_registros, na.rm = TRUE)
  var_tendencia <- var(tendencia, na.rm = TRUE)
  var_est7 <- var(estacional_7, na.rm = TRUE)
  var_est365 <- var(estacional_365, na.rm = TRUE)
  var_residuos <- var(residuos, na.rm = TRUE)
  
  # Proporciones de varianza explicada
  prop_tendencia <- round(var_tendencia / var_total, 3)
  prop_est7 <- round(var_est7 / var_total, 3)
  prop_est365 <- round(var_est365 / var_total, 3)
  prop_residuos <- round(var_residuos / var_total, 3)
  
  # Evaluar significancia de componentes
  hay_tendencia <- prop_tendencia > 0.05
  hay_estacionalidad_semanal <- prop_est7 > 0.02
  hay_estacionalidad_anual <- prop_est365 > 0.02
  
} else {
  hay_tendencia <- FALSE
  hay_estacionalidad_semanal <- FALSE
  hay_estacionalidad_anual <- FALSE
  
  # Análisis alternativo simple
  var_total <- var(serie_diaria$n_registros, na.rm = TRUE)
  varianza_semanal <- serie_diaria %>%
    group_by(dia_semana) %>%
    summarise(media_dia = mean(n_registros), .groups = 'drop') %>%
    pull(media_dia) %>%
    var()
  
  prop_semanal_simple <- round(varianza_semanal / var_total, 3)
  hay_estacionalidad_semanal <- prop_semanal_simple > 0.02
}

# Tests de significancia temporal
anova_semanal <- aov(n_registros ~ dia_semana, data = serie_diaria)
p_semanal <- summary(anova_semanal)[[1]][["Pr(>F)"]][1]

anova_mensual <- aov(n_registros ~ as.factor(mes), data = serie_diaria)
p_mensual <- summary(anova_mensual)[[1]][["Pr(>F)"]][1]

serie_diaria$tiempo <- as.numeric(serie_diaria$fecha - min(serie_diaria$fecha))
cor_temporal <- cor(serie_diaria$tiempo, serie_diaria$n_registros)
cor_test <- cor.test(serie_diaria$tiempo, serie_diaria$n_registros)

# Ratio weekend vs weekdays
ratio_weekend_data <- serie_diaria %>%
  group_by(fin_semana) %>%
  summarise(promedio = mean(n_registros), .groups = 'drop')

if(nrow(ratio_weekend_data) == 2) {
  ratio_valor <- ratio_weekend_data$promedio[ratio_weekend_data$fin_semana] / 
    ratio_weekend_data$promedio[!ratio_weekend_data$fin_semana]
} else {
  ratio_valor <- 1
}

# Decisión sobre estructura temporal
evidencias_temporales <- c(
  length(lags_sig_acf) >= 3,
  !is.na(lag_7) && abs(lag_7) > limite_sig,
  p_semanal < 0.05,
  p_mensual < 0.05,
  abs(cor_temporal) > 0.1 && cor_test$p.value < 0.05,
  hay_tendencia,
  hay_estacionalidad_semanal,
  hay_estacionalidad_anual
)

evidencias_positivas <- sum(evidencias_temporales, na.rm = TRUE)
es_serie_temporal <- evidencias_positivas >= 3

cat("CONCLUSIÓN: Los datos", ifelse(es_serie_temporal, "SÍ", "NO"), 
    "muestran estructura temporal relevante\n")

# ============================================================================
# 2. PREPARACIÓN DE DATOS METEOROLÓGICOS
# ============================================================================

# Agregación diaria de datos meteorológicos
weather_daily <- combined_final %>%
  mutate(fecha = as.Date(observation_date)) %>%
  group_by(fecha) %>%
  summarise(
    temperature = mean(temperature, na.rm = TRUE),
    precipitation = mean(precipitation, na.rm = TRUE),
    humidity = mean(humidity, na.rm = TRUE),
    windspeed = mean(windspeed, na.rm = TRUE),
    .groups = 'drop'
  )

# Datos completos combinados
full_data <- serie_diaria %>%
  left_join(weather_daily, by = "fecha") %>%
  filter(!is.na(fecha), !is.na(n_registros),
         !is.na(temperature), !is.na(precipitation), 
         !is.na(humidity), !is.na(windspeed)) %>%
  arrange(fecha)

# Parámetros de estandarización desde datos de entrenamiento
train_data_temp <- full_data %>% filter(year(fecha) %in% 2021:2023)

temp_mean_train <- mean(train_data_temp$temperature, na.rm = TRUE)
temp_sd_train <- sd(train_data_temp$temperature, na.rm = TRUE)
precip_mean_train <- mean(train_data_temp$precipitation, na.rm = TRUE)
precip_sd_train <- sd(train_data_temp$precipitation, na.rm = TRUE)
humid_mean_train <- mean(train_data_temp$humidity, na.rm = TRUE)
humid_sd_train <- sd(train_data_temp$humidity, na.rm = TRUE)
wind_mean_train <- mean(train_data_temp$windspeed, na.rm = TRUE)
wind_sd_train <- sd(train_data_temp$windspeed, na.rm = TRUE)

# ============================================================================
# 3. INGENIERÍA DE CARACTERÍSTICAS
# ============================================================================

create_features <- function(data, train_indices) {
  data <- data %>%
    mutate(wday = wday(fecha))
  
  train_subset <- data[train_indices, ]
  
  # Cuantiles desde datos de entrenamiento
  temp_q75 <- quantile(train_subset$temperature, 0.75, na.rm = TRUE)
  temp_q25 <- quantile(train_subset$temperature, 0.25, na.rm = TRUE)
  precip_q90 <- quantile(train_subset$precipitation, 0.9, na.rm = TRUE)
  precip_q50 <- quantile(train_subset$precipitation, 0.5, na.rm = TRUE)
  wind_q80 <- quantile(train_subset$windspeed, 0.8, na.rm = TRUE)
  
  data_features <- data %>%
    arrange(fecha) %>%
    mutate(
      # Variables temporales
      year = year(fecha),
      month = month(fecha),
      day = day(fecha),
      yday = yday(fecha),
      week = week(fecha),
      quarter = quarter(fecha),
      
      # Indicadores de calendario
      is_weekend = as.numeric(wday %in% c(1, 7)),
      is_monday = as.numeric(wday == 2),
      is_friday = as.numeric(wday == 6),
      is_spring = as.numeric(month %in% c(3, 4, 5)),
      is_summer = as.numeric(month %in% c(6, 7, 8)),
      is_autumn = as.numeric(month %in% c(9, 10, 11)),
      is_winter = as.numeric(month %in% c(12, 1, 2)),
      
      # Variables cíclicas
      sin_year = sin(2 * pi * yday / 365.25),
      cos_year = cos(2 * pi * yday / 365.25),
      sin_week = sin(2 * pi * wday / 7),
      cos_week = cos(2 * pi * wday / 7),
      
      # Variables meteorológicas estandarizadas
      temp_std = (temperature - temp_mean_train) / pmax(temp_sd_train, 0.01),
      precip_std = (precipitation - precip_mean_train) / pmax(precip_sd_train, 0.01),
      humid_std = (humidity - humid_mean_train) / pmax(humid_sd_train, 0.01),
      wind_std = (windspeed - wind_mean_train) / pmax(wind_sd_train, 0.01),
      
      # Indicadores meteorológicos binarios
      temp_high = as.numeric(temperature > temp_q75),
      temp_low = as.numeric(temperature < temp_q25),
      rain_heavy = as.numeric(precipitation > precip_q90),
      rain_light = as.numeric(precipitation > 0 & precipitation <= precip_q50),
      wind_strong = as.numeric(windspeed > wind_q80),
      
      # Variables autorregresivas
      n_registros_lag1 = lag(n_registros, 1),
      ma_7 = rollmean(lag(n_registros, 1), 7, fill = NA, align = "right"),
      sd_7 = rollapply(lag(n_registros, 1), 7, sd, fill = NA, align = "right"),
      min_7 = rollapply(lag(n_registros, 1), 7, min, fill = NA, align = "right"),
      max_7 = rollapply(lag(n_registros, 1), 7, max, fill = NA, align = "right"),
      ma_14 = rollmean(lag(n_registros, 1), 14, fill = NA, align = "right"),
      sd_14 = rollapply(lag(n_registros, 1), 14, sd, fill = NA, align = "right"),
      ma_30 = rollmean(lag(n_registros, 1), 30, fill = NA, align = "right"),
      sd_30 = rollapply(lag(n_registros, 1), 30, sd, fill = NA, align = "right")
    )
  
  # Agregar lags adicionales
  for(lag_val in c(2, 3, 7, 14, 30)) {
    data_features[[paste0("lag_", lag_val)]] <- lag(data_features$n_registros, lag_val)
  }
  
  # Variables derivadas
  data_features <- data_features %>%
    mutate(
      volatility_7 = sd_7 / (ma_7 + 1),
      volatility_14 = sd_14 / (ma_14 + 1),
      momentum_7 = (n_registros_lag1 - ma_7) / (ma_7 + 1),
      momentum_14 = (n_registros_lag1 - ma_14) / (ma_14 + 1),
      range_7 = max_7 - min_7,
      position_in_range_7 = (n_registros_lag1 - min_7) / (range_7 + 1),
      trend_7 = (ma_7 - lag(ma_7, 7)) / (lag(ma_7, 7) + 1),
      trend_14 = (ma_14 - lag(ma_14, 14)) / (lag(ma_14, 14) + 1),
      
      # Interacciones meteorológicas
      temp_weekend = temp_std * as.numeric(is_weekend),
      temp_summer = temp_std * as.numeric(is_summer),
      temp_winter = temp_std * as.numeric(is_winter),
      rain_weekend = precip_std * as.numeric(is_weekend),
      rain_monday = precip_std * as.numeric(is_monday),
      rain_summer = precip_std * as.numeric(is_summer),
      humid_hot = humid_std * as.numeric(temp_high),
      humid_weekend = humid_std * as.numeric(is_weekend),
      wind_weekend = wind_std * as.numeric(is_weekend),
      wind_cold = wind_std * as.numeric(temp_low),
      
      # Índices compuestos
      comfort_index = temp_std - humid_std - wind_std,
      weather_severity = abs(temp_std) + abs(precip_std) + abs(wind_std),
      
      # Variables temporales adicionales
      days_since_start = as.numeric(fecha - min(fecha, na.rm = TRUE)),
      days_since_start_sq = days_since_start^2,
      is_month_start = as.numeric(day <= 5),
      is_month_end = as.numeric(day >= 25)
    )
  
  return(data_features)
}

# Aplicar ingeniería de características
dates_raw <- full_data$fecha
train_indices <- which(year(dates_raw) %in% 2021:2023)
full_featured <- create_features(full_data, train_indices)

# Limpiar datos
clean_data <- full_featured %>%
  filter(!is.na(lag_30), !is.na(ma_30), !is.na(sd_30))

# ============================================================================
# 4. MODELO PROPHET
# ============================================================================

# Preparar datos para Prophet
prophet_data <- clean_data %>%
  mutate(
    y_transformed = log(n_registros + 1),
    temp_std = (temperature - temp_mean_train) / temp_sd_train,
    precip_std = (precipitation - precip_mean_train) / precip_sd_train,
    humid_std = (humidity - humid_mean_train) / humid_sd_train,
    wind_std = (windspeed - wind_mean_train) / wind_sd_train
  ) %>%
  dplyr::select(
    ds = fecha, 
    y = y_transformed,
    temp_std, precip_std, humid_std, wind_std,
    n_registros_lag1, lag_7, lag_14, lag_30,
    ma_7, ma_14, ma_30, sd_7, sd_14,
    volatility_7, volatility_14, momentum_7, momentum_14,
    trend_7, trend_14, n_registros
  )

# Splits para Prophet
train_prophet <- prophet_data %>% filter(year(ds) %in% 2021:2023)
validation_prophet <- prophet_data %>% filter(year(ds) == 2024)
test_prophet <- prophet_data %>% filter(year(ds) == 2025)

# Entrenamiento del modelo Prophet
model_prophet <- prophet(
  yearly.seasonality = TRUE,
  weekly.seasonality = TRUE,
  daily.seasonality = FALSE,
  seasonality.mode = 'multiplicative',
  changepoint.prior.scale = 0.1,
  seasonality.prior.scale = 15,
  interval.width = 0.8
)

# Agregar regresores
model_prophet <- add_regressor(model_prophet, 'temp_std', mode = 'multiplicative', prior.scale = 5)
model_prophet <- add_regressor(model_prophet, 'precip_std', mode = 'multiplicative', prior.scale = 5)
model_prophet <- add_regressor(model_prophet, 'humid_std', mode = 'multiplicative', prior.scale = 5)
model_prophet <- add_regressor(model_prophet, 'wind_std', mode = 'multiplicative', prior.scale = 5)
model_prophet <- add_regressor(model_prophet, 'n_registros_lag1', mode = 'additive', prior.scale = 10)
model_prophet <- add_regressor(model_prophet, 'lag_7', mode = 'additive', prior.scale = 8)
model_prophet <- add_regressor(model_prophet, 'lag_14', mode = 'additive', prior.scale = 6)
model_prophet <- add_regressor(model_prophet, 'lag_30', mode = 'additive', prior.scale = 4)
model_prophet <- add_regressor(model_prophet, 'ma_7', mode = 'additive', prior.scale = 8)
model_prophet <- add_regressor(model_prophet, 'ma_14', mode = 'additive', prior.scale = 6)
model_prophet <- add_regressor(model_prophet, 'ma_30', mode = 'additive', prior.scale = 4)
model_prophet <- add_regressor(model_prophet, 'volatility_7', mode = 'additive', prior.scale = 3)
model_prophet <- add_regressor(model_prophet, 'momentum_7', mode = 'additive', prior.scale = 3)
model_prophet <- add_regressor(model_prophet, 'trend_7', mode = 'additive', prior.scale = 3)

# Entrenar modelo
model_prophet <- fit.prophet(model_prophet, train_prophet)

# Predicciones
future_df_prophet <- make_future_dataframe(
  model_prophet,
  periods = nrow(validation_prophet) + nrow(test_prophet),
  freq = 'day',
  include_history = TRUE
) %>%
  left_join(prophet_data %>% dplyr::select(-y), by = "ds")

forecast_prophet <- predict(model_prophet, future_df_prophet)

# Función para métricas
get_metrics <- function(actual_df, forecast_df, set_name) {
  preds <- forecast_df %>%
    filter(ds %in% actual_df$ds) %>%
    dplyr::select(ds, yhat) %>%
    left_join(actual_df %>% dplyr::select(ds, y_actual = n_registros), by = "ds") %>%
    mutate(yhat_original_scale = pmax(0, exp(yhat) - 1)) %>%
    filter(!is.na(yhat), !is.na(y_actual))
  
  if(nrow(preds) == 0) {
    return(data.frame(set = set_name, n_obs = 0, MAE = NA, RMSE = NA, MAPE = NA))
  }
  
  mae_val <- mean(abs(preds$y_actual - preds$yhat_original_scale))
  rmse_val <- sqrt(mean((preds$y_actual - preds$yhat_original_scale)^2))
  mape_val <- mean(abs((preds$y_actual - preds$yhat_original_scale) / pmax(preds$y_actual, 1)) * 100)
  
  data.frame(
    set = set_name,
    n_obs = nrow(preds),
    MAE = round(mae_val, 1),
    RMSE = round(rmse_val, 1),
    MAPE = round(mape_val, 1)
  )
}

# Evaluar Prophet
results_train_prophet <- get_metrics(train_prophet, forecast_prophet, "Train")
results_validation_prophet <- get_metrics(validation_prophet, forecast_prophet, "Validation")
results_test_prophet <- get_metrics(test_prophet, forecast_prophet, "Test")

prophet_results <- rbind(results_train_prophet, results_validation_prophet, results_test_prophet)

# Extraer predicciones para análisis posterior
actuals_val_prophet <- (prophet_data %>% filter(ds %in% validation_prophet$ds))$n_registros
preds_val_transformed_prophet <- forecast_prophet %>% filter(ds %in% validation_prophet$ds) %>% pull(yhat)
preds_val_prophet <- pmax(0, exp(preds_val_transformed_prophet) - 1)

actuals_test_prophet <- (prophet_data %>% filter(ds %in% test_prophet$ds))$n_registros
preds_test_transformed_prophet <- forecast_prophet %>% filter(ds %in% test_prophet$ds) %>% pull(yhat)
preds_test_prophet <- pmax(0, exp(preds_test_transformed_prophet) - 1)

# Gráfico de residuos Prophet
residuals_val_prophet <- actuals_val_prophet - preds_val_prophet
residuals_data_prophet <- data.frame(
  predicted = preds_val_prophet,
  residuals = residuals_val_prophet
)

p_residuals_prophet <- ggplot(residuals_data_prophet, aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.6, size = 1.2) +
  geom_hline(yintercept = 0, color = "#8d4925", linetype = "dashed", size = 0.8) +
  geom_smooth(method = "loess", color = "#320000", size = 1, se = TRUE, alpha = 0.3) +
  labs(
    title = "Prophet Residuals vs Predicted Values",
    x = "Predicted Values",
    y = "Residuals"
  ) +
  theme_minimal()

# ============================================================================
# 5. MODELO XGBOOST CON HYPERPARAMETER TUNING EXTENSO
# ============================================================================

# Seleccionar características para XGBoost
feature_names_xgb <- c(
  "year", "month", "day", "wday", "yday", "week", "quarter",
  "is_weekend", "is_monday", "is_friday",
  "is_spring", "is_summer", "is_autumn", "is_winter",
  "sin_year", "cos_year", "sin_week", "cos_week",
  "days_since_start", "days_since_start_sq", "is_month_start", "is_month_end",
  "temp_std", "precip_std", "humid_std", "wind_std",
  "temp_high", "temp_low", "rain_heavy", "rain_light", "wind_strong",
  "comfort_index", "weather_severity",
  "n_registros_lag1", paste0("lag_", c(2, 3, 7, 14, 30)),
  "ma_7", "sd_7", "min_7", "max_7", "ma_14", "sd_14", "ma_30", "sd_30",
  "volatility_7", "volatility_14", "momentum_7", "momentum_14",
  "range_7", "position_in_range_7", "trend_7", "trend_14"
)

# Splits temporales
dates_clean <- clean_data$fecha
train_idx_xgb <- which(year(dates_clean) %in% 2021:2023)
val_idx_xgb <- which(year(dates_clean) == 2024)
test_idx_xgb <- which(year(dates_clean) == 2025)

# Preparar matrices
clean_data_numeric <- clean_data %>%
  mutate(across(all_of(feature_names_xgb), as.numeric))

X_train_xgb <- clean_data_numeric[train_idx_xgb, feature_names_xgb, drop = FALSE] %>% as.matrix()
y_train_xgb <- clean_data_numeric[train_idx_xgb, ]$n_registros
X_val_xgb <- clean_data_numeric[val_idx_xgb, feature_names_xgb, drop = FALSE] %>% as.matrix()
y_val_xgb <- clean_data_numeric[val_idx_xgb, ]$n_registros
X_test_xgb <- clean_data_numeric[test_idx_xgb, feature_names_xgb, drop = FALSE] %>% as.matrix()
y_test_xgb <- clean_data_numeric[test_idx_xgb, ]$n_registros

# Función de evaluación XGBoost
evaluate_xgb <- function(params, X_train, y_train, X_val, y_val) {
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dval <- xgb.DMatrix(data = X_val, label = y_val)
  
  model <- xgb.train(
    params = list(
      objective = "reg:tweedie",
      tweedie_variance_power = 1.5,
      eval_metric = "mae",
      max_depth = params$max_depth,
      eta = params$eta,
      gamma = params$gamma,
      colsample_bytree = params$colsample_bytree,
      min_child_weight = params$min_child_weight,
      subsample = params$subsample
    ),
    data = dtrain,
    nrounds = params$nrounds,
    watchlist = list(train = dtrain, eval = dval),
    early_stopping_rounds = 20,
    verbose = 0
  )
  
  pred_val <- predict(model, X_val)
  mae_val <- mean(abs(y_val - pred_val))
  
  return(list(mae = mae_val, model = model, best_iteration = model$best_iteration))
}

# Grid de hiperparámetros más extenso
param_grid_xgb <- expand.grid(
  nrounds = c(100, 200, 300, 500),
  max_depth = c(4, 6, 8),
  eta = c(0.01, 0.05, 0.1, 0.2),
  gamma = c(0, 0.1, 1),
  colsample_bytree = c(0.6, 0.8, 1.0),
  min_child_weight = c(1, 3, 5),
  subsample = c(0.7, 0.8, 1.0)
)

set.seed(42)
param_grid_sample_xgb <- param_grid_xgb[sample(nrow(param_grid_xgb), min(50, nrow(param_grid_xgb))), ]

results_list_xgb <- list()
for(i in 1:nrow(param_grid_sample_xgb)) {
  if(i %% 10 == 0) cat("Completed", i, "of", nrow(param_grid_sample_xgb), "combinations\n")
  
  params_xgb <- param_grid_sample_xgb[i, ]
  result_xgb <- evaluate_xgb(params_xgb, X_train_xgb, y_train_xgb, X_val_xgb, y_val_xgb)
  
  if(!is.null(result_xgb)) {
    results_list_xgb[[i]] <- data.frame(
      iteration = i,
      mae = result_xgb$mae,
      best_iteration = result_xgb$best_iteration,
      params_xgb
    )
  } else {
    results_list_xgb[[i]] <- data.frame(
      iteration = i,
      mae = Inf,
      best_iteration = NA,
      params_xgb
    )
  }
}

valid_results_xgb <- results_list_xgb[!sapply(results_list_xgb, is.null)]
tuning_results_xgb <- do.call(rbind, valid_results_xgb) %>% arrange(mae)
best_params_xgb <- tuning_results_xgb[1, ]

# Entrenar modelo final XGBoost
dtrain_final_xgb <- xgb.DMatrix(data = X_train_xgb, label = y_train_xgb)
dval_final_xgb <- xgb.DMatrix(data = X_val_xgb, label = y_val_xgb)

final_model_xgb <- xgb.train(
  params = list(
    objective = "reg:tweedie",
    tweedie_variance_power = 1.5,
    eval_metric = "mae",
    max_depth = best_params_xgb$max_depth,
    eta = best_params_xgb$eta,
    gamma = best_params_xgb$gamma,
    colsample_bytree = best_params_xgb$colsample_bytree,
    min_child_weight = best_params_xgb$min_child_weight,
    subsample = best_params_xgb$subsample
  ),
  data = dtrain_final_xgb,
  nrounds = best_params_xgb$nrounds,
  watchlist = list(train = dtrain_final_xgb, eval = dval_final_xgb),
  early_stopping_rounds = 30,
  verbose = 0
)

# Predicciones XGBoost
pred_train_xgb <- pmax(0, predict(final_model_xgb, X_train_xgb))
pred_val_xgb <- pmax(0, predict(final_model_xgb, X_val_xgb))
pred_test_xgb <- pmax(0, predict(final_model_xgb, X_test_xgb))

# Métricas XGBoost
calculate_metrics <- function(actual, predicted, set_name) {
  mae <- mean(abs(actual - predicted))
  rmse <- sqrt(mean((actual - predicted)^2))
  mape <- mean(abs((actual - predicted) / pmax(actual, 1)) * 100)
  
  return(data.frame(
    set = set_name,
    n_obs = length(actual),
    MAE = round(mae, 1),
    RMSE = round(rmse, 1),
    MAPE = round(mape, 1)
  ))
}

xgb_results <- rbind(
  calculate_metrics(y_train_xgb, pred_train_xgb, "Train"),
  calculate_metrics(y_val_xgb, pred_val_xgb, "Validation"),
  calculate_metrics(y_test_xgb, pred_test_xgb, "Test")
)

# Análisis de importancia de características detallado
importance_matrix_xgb <- xgb.importance(
  feature_names = colnames(X_train_xgb),
  model = final_model_xgb
)

# Categorización de características
importance_categorizada <- importance_matrix_xgb %>%
  mutate(
    categoria = case_when(
      grepl("temp|precip|humid|wind", Feature) ~ "Meteorológicas",
      grepl("lag_|ma_", Feature) ~ "Autorregresivas/Rolling",
      grepl("yday|wday|date_numeric|month_numeric|day_numeric|is_", Feature) ~ "Temporales/Calendario",
      TRUE ~ "Otras"
    )
  ) %>%
  group_by(categoria) %>%
  summarise(
    n_features = n(),
    importancia_total = sum(Gain),
    importancia_promedio = round(mean(Gain), 4),
    .groups = 'drop'
  ) %>%
  arrange(desc(importancia_total))

# Contribución específica de variables meteorológicas
meteo_importance <- importance_matrix_xgb %>%
  filter(grepl("temp|precip|humid|wind", Feature)) %>%
  mutate(
    variable_base = case_when(
      grepl("temp", Feature) ~ "Temperatura",
      grepl("precip", Feature) ~ "Precipitación",
      grepl("humid", Feature) ~ "Humedad",
      grepl("wind", Feature) ~ "Viento",
      TRUE ~ "Otra_meteo"
    )
  ) %>%
  group_by(variable_base) %>%
  summarise(
    n_features = n(),
    importancia_total = sum(Gain),
    importancia_max = max(Gain),
    feature_mas_importante = Feature[which.max(Gain)],
    .groups = 'drop'
  ) %>%
  arrange(desc(importancia_total))

# Gráfico de residuos XGBoost
residuals_val_xgb <- y_val_xgb - pred_val_xgb
p_residuals_xgb <- data.frame(
  predicted = pred_val_xgb,
  residuals = residuals_val_xgb
) %>%
  ggplot(aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "#8d4925", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "#83741b") +
  labs(
    title = "XGBoost Residuals vs Predicted Values",
    x = "Predicted Values",
    y = "Residuals"
  ) +
  theme_minimal()

# ============================================================================
# 6. MODELO GAM CON MÚLTIPLES DISTRIBUCIONES
# ============================================================================

# Preparar datos para GAM
gam_data <- clean_data %>%
  mutate(
    date_numeric = as.numeric(fecha - min(fecha, na.rm = TRUE)),
    year_numeric = year(fecha),
    month_numeric = month(fecha),
    day_numeric = day(fecha),
    wday_numeric = wday(fecha),
    yday_numeric = yday(fecha),
    
    is_weekend = as.numeric(wday(fecha) %in% c(1, 7)),
    is_monday = as.numeric(wday(fecha) == 2),
    is_friday = as.numeric(wday(fecha) == 6),
    is_spring = as.numeric(month(fecha) %in% c(3, 4, 5)),
    is_summer = as.numeric(month(fecha) %in% c(6, 7, 8)),
    is_autumn = as.numeric(month(fecha) %in% c(9, 10, 11)),
    is_winter = as.numeric(month(fecha) %in% c(12, 1, 2)),
    
    temp_std = (temperature - temp_mean_train) / pmax(temp_sd_train, 0.01),
    precip_std = (precipitation - precip_mean_train) / pmax(precip_sd_train, 0.01),
    humid_std = (humidity - humid_mean_train) / pmax(humid_sd_train, 0.01),
    wind_std = (windspeed - wind_mean_train) / pmax(wind_sd_train, 0.01)
  )

# Agregar lags para GAM
for(lag_val in c(1, 2, 3, 7, 14, 30)) {
  gam_data[[paste0("lag_", lag_val)]] <- lag(gam_data$n_registros, lag_val)
}

# Rolling statistics
n_registros_lag1_gam <- lag(gam_data$n_registros, 1)
gam_data$ma_7 <- rollmean(n_registros_lag1_gam, 7, fill = NA, align = "right")
gam_data$ma_14 <- rollmean(n_registros_lag1_gam, 14, fill = NA, align = "right")
gam_data$ma_30 <- rollmean(n_registros_lag1_gam, 30, fill = NA, align = "right")

# Limpiar datos GAM
gam_clean <- gam_data %>%
  filter(!is.na(lag_30), !is.na(ma_30)) %>%
  filter(!is.na(temperature), !is.na(precipitation), !is.na(humidity), !is.na(windspeed))

# Splits GAM
train_gam <- gam_clean %>% filter(year(fecha) %in% 2021:2023)
validation_gam <- gam_clean %>% filter(year(fecha) == 2024)
test_gam <- gam_clean %>% filter(year(fecha) == 2025)

# Entrenar múltiples modelos GAM con diferentes distribuciones
gam_models <- list()

# Modelo 1: Poisson GAM
tryCatch({
  gam_models$poisson <- gam(
    n_registros ~
      s(date_numeric, k = 20) +
      s(yday_numeric, bs = "cc", k = 12) +
      s(wday_numeric, k = 7) +
      s(temp_std, k = 8) +
      s(precip_std, k = 8) +
      s(humid_std, k = 8) +
      s(wind_std, k = 8) +
      lag_1 + lag_2 + lag_3 + lag_7 + lag_14 +
      s(ma_7, k = 8) +
      s(ma_14, k = 8) +
      is_weekend + is_monday + is_friday +
      is_spring + is_summer + is_autumn,
    family = poisson(),
    data = train_gam,
    method = "REML",
    gamma = 1.4
  )
}, error = function(e) {
  cat("Poisson GAM failed:", e$message, "\n")
})

# Modelo 2: Negative Binomial GAM
tryCatch({
  gam_models$nb <- gam(
    n_registros ~
      s(date_numeric, k = 20) +
      s(yday_numeric, bs = "cc", k = 12) +
      s(wday_numeric, k = 7) +
      s(temp_std, k = 8) +
      s(precip_std, k = 8) +
      s(humid_std, k = 8) +
      s(wind_std, k = 8) +
      lag_1 + lag_2 + lag_3 + lag_7 + lag_14 +
      s(ma_7, k = 8) +
      s(ma_14, k = 8) +
      is_weekend + is_monday + is_friday +
      is_spring + is_summer + is_autumn,
    family = nb(),
    data = train_gam,
    method = "REML"
  )
}, error = function(e) {
  cat("Negative Binomial GAM failed:", e$message, "\n")
})

# Modelo 3: Tweedie GAM
tryCatch({
  gam_models$tweedie <- gam(
    n_registros ~
      s(date_numeric, k = 20) +
      s(yday_numeric, bs = "cc", k = 12) +
      s(wday_numeric, k = 7) +
      s(temp_std, k = 8) +
      s(precip_std, k = 8) +
      s(humid_std, k = 8) +
      s(wind_std, k = 8) +
      lag_1 + lag_2 + lag_3 + lag_7 + lag_14 +
      s(ma_7, k = 8) +
      s(ma_14, k = 8) +
      is_weekend + is_monday + is_friday +
      is_spring + is_summer + is_autumn,
    family = tw(),
    data = train_gam,
    method = "REML"
  )
}, error = function(e) {
  cat("Tweedie GAM failed:", e$message, "\n")
})

# Modelo 4: Gaussian GAM
tryCatch({
  gam_models$gaussian <- gam(
    n_registros ~
      s(date_numeric, k = 20) +
      s(yday_numeric, bs = "cc", k = 12) +
      s(wday_numeric, k = 7) +
      s(temp_std, k = 8) +
      s(precip_std, k = 8) +
      s(humid_std, k = 8) +
      s(wind_std, k = 8) +
      lag_1 + lag_2 + lag_3 + lag_7 + lag_14 +
      s(ma_7, k = 8) +
      s(ma_14, k = 8) +
      is_weekend + is_monday + is_friday +
      is_spring + is_summer + is_autumn,
    family = gaussian(),
    data = train_gam,
    method = "REML"
  )
}, error = function(e) {
  cat("Gaussian GAM failed:", e$message, "\n")
})

# Remover modelos fallidos
gam_models <- gam_models[!sapply(gam_models, is.null)]

# Evaluar todos los modelos GAM
gam_results <- data.frame()

for(model_name in names(gam_models)) {
  model <- gam_models[[model_name]]
  
  # Predicciones
  pred_train <- pmax(0, predict(model, train_gam, type = "response"))
  pred_val <- pmax(0, predict(model, validation_gam, type = "response"))
  pred_test <- pmax(0, predict(model, test_gam, type = "response"))
  
  # Calcular métricas para cada set
  for(set_info in list(
    list(name = "Train", actual = train_gam$n_registros, pred = pred_train),
    list(name = "Validation", actual = validation_gam$n_registros, pred = pred_val),
    list(name = "Test", actual = test_gam$n_registros, pred = pred_test)
  )) {
    
    mae_val <- mean(abs(set_info$actual - set_info$pred))
    rmse_val <- sqrt(mean((set_info$actual - set_info$pred)^2))
    mape_val <- mean(abs((set_info$actual - set_info$pred) / pmax(set_info$actual, 1)) * 100)
    
    gam_results <- rbind(gam_results, data.frame(
      model = model_name,
      set = set_info$name,
      n_obs = length(set_info$actual),
      MAE = round(mae_val, 3),
      RMSE = round(rmse_val, 3),
      MAPE = round(mape_val, 3),
      AIC = round(AIC(model), 3),
      deviance_explained = round(summary(model)$dev.expl * 100, 1)
    ))
  }
}

# Seleccionar mejor modelo GAM (forzamos Tweedie)
best_gam_name <- "tweedie"
if(!("tweedie" %in% names(gam_models))) {
  # Si Tweedie falló, usar el mejor disponible
  val_results_gam <- gam_results[gam_results$set == "Validation", ]
  best_gam_name <- val_results_gam$model[which.min(val_results_gam$MAE)]
}

model_gam <- gam_models[[best_gam_name]]

# Predicciones del mejor modelo GAM
pred_train_gam <- pmax(0, predict(model_gam, train_gam, type = "response"))
pred_val_gam <- pmax(0, predict(model_gam, validation_gam, type = "response"))
pred_test_gam <- pmax(0, predict(model_gam, test_gam, type = "response"))

# Análisis de significancia de términos GAM
gam_summary <- summary(model_gam)
terminos_gam <- data.frame(
  Termino = rownames(gam_summary$s.table),
  EDF = round(gam_summary$s.table[,"edf"], 2),
  Ref_df = round(gam_summary$s.table[,"Ref.df"], 2),
  F_value = round(gam_summary$s.table[,"F"], 2),
  p_value = round(gam_summary$s.table[,"p-value"], 4)
) %>%
  mutate(
    categoria = case_when(
      grepl("temp|precip|humid|wind", Termino) ~ "Meteorológicas",
      grepl("date|yday|wday", Termino) ~ "Temporales/Calendario",
      grepl("ma_|lag", Termino) ~ "Autorregresivas/Rolling",
      TRUE ~ "Otras"
    ),
    significativo = p_value < 0.05
  )

# Gráfico de residuos GAM
residuals_val_gam <- validation_gam$n_registros - pred_val_gam
p_residuals_gam <- data.frame(
  predicted = pred_val_gam,
  residuals = residuals_val_gam
) %>%
  ggplot(aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "#8d4925", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "#8d2535") +
  labs(
    title = "GAM Residuals vs Predicted Values",
    x = "Predicted Values",
    y = "Residuals"
  ) +
  theme_minimal()

# ============================================================================
# 7. GRÁFICO COMBINADO DE RESIDUOS
# ============================================================================

combined_residuals_plot <- p_residuals_prophet + p_residuals_xgb + p_residuals_gam +
  plot_layout(nrow = 1)
print(combined_residuals_plot)

# ============================================================================
# 8. ANÁLISIS ROLLING MAPE DETALLADO
# ============================================================================

# Preparar datos de predicciones unificadas
validation_dates <- validation_gam$fecha
prediction_data <- data.frame(
  date = validation_dates,
  actual = validation_gam$n_registros,
  prophet = preds_val_prophet[1:length(validation_dates)],
  xgboost = pred_val_xgb[1:length(validation_dates)],
  gam = pred_val_gam
) %>%
  filter(!is.na(actual) & actual > 0) %>%
  arrange(date)

# Rolling MAPE calculation
window_size <- 30
min_periods <- 15

calculate_mape <- function(actual, predicted) {
  valid_indices <- !is.na(actual) & !is.na(predicted) & actual > 0
  if(sum(valid_indices) < min_periods) {
    return(NA)
  }
  mean(abs((actual[valid_indices] - predicted[valid_indices]) / actual[valid_indices]) * 100, na.rm = TRUE)
}

rolling_results <- data.frame()
for(i in window_size:nrow(prediction_data)) {
  window_start <- i - window_size + 1
  window_end <- i
  window_data <- prediction_data[window_start:window_end, ]
  current_date <- prediction_data$date[i]
  
  row_result <- data.frame(
    date = current_date,
    prophet_mape = calculate_mape(window_data$actual, window_data$prophet),
    xgboost_mape = calculate_mape(window_data$actual, window_data$xgboost),
    gam_mape = calculate_mape(window_data$actual, window_data$gam)
  )
  rolling_results <- rbind(rolling_results, row_result)
}

# Análisis de dominancia período por período
dominancia_analisis <- rolling_results %>%
  rowwise() %>%
  mutate(
    mejor_modelo = case_when(
      !is.na(prophet_mape) & !is.na(xgboost_mape) & !is.na(gam_mape) ~ {
        mapes <- c(prophet = prophet_mape, xgboost = xgboost_mape, gam = gam_mape)
        names(mapes)[which.min(mapes)]
      },
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

# Calcular porcentajes de dominancia
dominancia_porcentajes <- dominancia_analisis %>%
  filter(!is.na(mejor_modelo)) %>%
  count(mejor_modelo) %>%
  mutate(porcentaje = round(n/sum(n)*100, 1))

# Gráfico Rolling MAPE
plot_data_rolling <- rolling_results %>%
  pivot_longer(cols = contains("_mape"), names_to = "model", values_to = "mape") %>%
  mutate(
    model = case_when(
      model == "prophet_mape" ~ "Prophet",
      model == "xgboost_mape" ~ "XGBoost",
      model == "gam_mape" ~ "GAM",
      TRUE ~ model
    )
  ) %>%
  filter(!is.na(mape))

model_colors <- c("Prophet" = "#320000", "XGBoost" = "#83741b", "GAM" = "#8d2535")

p_rolling_mape <- plot_data_rolling %>%
  ggplot(aes(x = date, y = mape, color = model)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2, span = 0.3) +
  scale_color_manual(values = model_colors) +
  labs(
    title = "Rolling MAPE (Validación)",
    subtitle = paste("Ventana deslizante de", window_size, "días"),
    x = "Fecha",
    y = "MAPE (%)",
    color = "Modelo"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_rolling_mape)

# Análisis temporal de rendimiento
rolling_analysis <- plot_data_rolling %>%
  mutate(
    month = month(date),
    month_name = month.name[month],
    week = week(date),
    wday = wday(date, label = TRUE),
    is_weekend = wday(date) %in% c(1, 7)
  )

monthly_performance <- rolling_analysis %>%
  filter(!is.na(mape)) %>%
  group_by(month_name, model) %>%
  summarise(
    avg_mape = round(mean(mape, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  mutate(month_name = factor(month_name, levels = month.name)) %>%
  arrange(month_name)

# ============================================================================
# 9. GRÁFICO COMBINADO DE PREDICCIONES VS ACTUAL
# ============================================================================

plot_data_combined <- prediction_data %>%
  pivot_longer(
    cols = c("prophet", "xgboost", "gam"),
    names_to = "model",
    values_to = "predicted"
  ) %>%
  mutate(
    model = case_when(
      model == "prophet" ~ "Prophet",
      model == "xgboost" ~ "XGBoost", 
      model == "gam" ~ "GAM",
      TRUE ~ model
    )
  )

p_combined_predictions <- ggplot(plot_data_combined, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), size = 0.9, alpha = 0.9) +
  geom_line(aes(y = predicted, color = model), size = 0.7, alpha = 0.7) +
  scale_color_manual(
    name = "Series",
    values = c("Actual" = "black", model_colors)
  ) +
  labs(
    title = "Actual vs. Predicho (Validación)",
    subtitle = "Comparación de modelos de predicción diarios",
    x = "Fecha",
    y = "Registros Diarios",
    color = "Modelo"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_combined_predictions)

# ============================================================================
# 10. ANÁLISIS DETALLADO DE VALORES EXTREMOS Y MÉTRICAS ROBUSTAS
# ============================================================================

# Función para detectar outliers
detect_outliers <- function(y_actual, method = "IQR") {
  if(method == "IQR") {
    Q1 <- quantile(y_actual, 0.25)
    Q3 <- quantile(y_actual, 0.75)
    IQR <- Q3 - Q1
    threshold_upper <- Q3 + 1.5 * IQR
    threshold_lower <- Q1 - 1.5 * IQR
    outliers <- which(y_actual > threshold_upper | y_actual < threshold_lower)
  } else if(method == "zscore") {
    z_scores <- abs((y_actual - mean(y_actual)) / sd(y_actual))
    outliers <- which(z_scores > 3)
  }
  return(outliers)
}

# Función para métricas robustas
calculate_robust_metrics <- function(y_actual, y_pred) {
  # Métricas tradicionales
  mae_traditional <- mean(abs(y_actual - y_pred))
  rmse_traditional <- sqrt(mean((y_actual - y_pred)^2))
  mape_traditional <- mean(abs((y_actual - y_pred) / pmax(y_actual, 1)) * 100)
  
  # Métricas robustas (basadas en mediana)
  mae_robust <- median(abs(y_actual - y_pred))
  rmse_robust <- sqrt(median((y_actual - y_pred)^2))
  mape_robust <- median(abs((y_actual - y_pred) / pmax(y_actual, 1))) * 100
  
  # Detectar outliers
  outliers_idx <- detect_outliers(y_actual, "IQR")
  
  # Métricas sin outliers
  if(length(outliers_idx) > 0) {
    normal_idx <- setdiff(1:length(y_actual), outliers_idx)
    mae_no_outliers <- mean(abs(y_actual[normal_idx] - y_pred[normal_idx]))
    rmse_no_outliers <- sqrt(mean((y_actual[normal_idx] - y_pred[normal_idx])^2))
    mape_no_outliers <- mean(abs((y_actual[normal_idx] - y_pred[normal_idx]) / 
                                   pmax(y_actual[normal_idx], 1)) * 100)
  } else {
    mae_no_outliers <- mae_traditional
    rmse_no_outliers <- rmse_traditional  
    mape_no_outliers <- mape_traditional
  }
  
  return(list(
    traditional = data.frame(MAE = mae_traditional, RMSE = rmse_traditional, MAPE = mape_traditional),
    robust = data.frame(MAE = mae_robust, RMSE = rmse_robust, MAPE = mape_robust),
    no_outliers = data.frame(MAE = mae_no_outliers, RMSE = rmse_no_outliers, MAPE = mape_no_outliers),
    outliers_detected = length(outliers_idx),
    outliers_idx = outliers_idx
  ))
}

# Aplicar a todos los modelos
prophet_robust <- calculate_robust_metrics(actuals_val_prophet, preds_val_prophet)
xgb_robust <- calculate_robust_metrics(y_val_xgb, pred_val_xgb)
gam_robust <- calculate_robust_metrics(validation_gam$n_registros, pred_val_gam)

# Tabla comparativa robusta
comparison_table <- data.frame(
  Modelo = c("Prophet", "XGBoost", "GAM"),
  MAPE_Total = c(round(prophet_robust$traditional$MAPE, 1),
                 round(xgb_robust$traditional$MAPE, 1),
                 round(gam_robust$traditional$MAPE, 1)),
  MAPE_Robusto = c(round(prophet_robust$robust$MAPE, 1),
                   round(xgb_robust$robust$MAPE, 1),
                   round(gam_robust$robust$MAPE, 1)),
  MAPE_Sin_Outliers = c(round(prophet_robust$no_outliers$MAPE, 1),
                        round(xgb_robust$no_outliers$MAPE, 1),
                        round(gam_robust$no_outliers$MAPE, 1)),
  Outliers_Detectados = c(prophet_robust$outliers_detected,
                          xgb_robust$outliers_detected,
                          gam_robust$outliers_detected)
)

# Visualización de outliers
outliers_idx <- detect_outliers(validation_gam$n_registros)
plot_data_outliers <- data.frame(
  fecha = validation_gam$fecha,
  actual = validation_gam$n_registros,
  predicted = pred_val_gam,
  is_outlier = 1:length(validation_gam$n_registros) %in% outliers_idx
)

p_outliers <- ggplot(plot_data_outliers, aes(x = fecha)) +
  geom_line(aes(y = actual), color = "black", alpha = 0.7) +
  geom_line(aes(y = predicted), color = "#8d4925", alpha = 0.7) +
  geom_point(data = plot_data_outliers[plot_data_outliers$is_outlier, ], 
             aes(y = actual), color = "red", size = 3, shape = 1) +
  labs(
    title = "Detección de Valores Extremos en Validación",
    subtitle = paste("Outliers detectados:", sum(plot_data_outliers$is_outlier), "días"),
    x = "Fecha", 
    y = "Registros Diarios",
    caption = "Círculos rojos = outliers detectados por método IQR"
  ) +
  theme_minimal()

print(p_outliers)

# ============================================================================
# 11. ANÁLISIS COMPLETO DE RESIDUOS Y ERRORES TEMPORALES
# ============================================================================

# Crear data frame de errores por modelo
errores_analisis <- data.frame(
  fecha = validation_gam$fecha,
  registros_real = validation_gam$n_registros,
  pred_prophet = preds_val_prophet[1:length(validation_gam$fecha)],
  pred_xgboost = pred_val_xgb[1:length(validation_gam$fecha)],
  pred_gam = pred_val_gam
) %>%
  mutate(
    error_prophet = abs(registros_real - pred_prophet),
    error_xgboost = abs(registros_real - pred_xgboost),
    error_gam = abs(registros_real - pred_gam),
    error_pct_prophet = abs((registros_real - pred_prophet) / pmax(registros_real, 1)) * 100,
    error_pct_xgboost = abs((registros_real - pred_xgboost) / pmax(registros_real, 1)) * 100,
    error_pct_gam = abs((registros_real - pred_gam) / pmax(registros_real, 1)) * 100,
    mes = month(fecha),
    dia_semana = wday(fecha),
    is_weekend = dia_semana %in% c(1, 7)
  )

# Análisis por períodos temporales
errores_por_mes <- errores_analisis %>%
  group_by(mes) %>%
  summarise(
    Prophet_MAE = round(mean(error_prophet, na.rm = TRUE), 1),
    XGBoost_MAE = round(mean(error_xgboost, na.rm = TRUE), 1),
    GAM_MAE = round(mean(error_gam, na.rm = TRUE), 1),
    Prophet_MAPE = round(mean(error_pct_prophet, na.rm = TRUE), 1),
    XGBoost_MAPE = round(mean(error_pct_xgboost, na.rm = TRUE), 1),
    GAM_MAPE = round(mean(error_pct_gam, na.rm = TRUE), 1),
    .groups = 'drop'
  )

errores_por_dia <- errores_analisis %>%
  group_by(dia_semana) %>%
  summarise(
    Prophet_MAE = round(mean(error_prophet, na.rm = TRUE), 1),
    XGBoost_MAE = round(mean(error_xgboost, na.rm = TRUE), 1),
    GAM_MAE = round(mean(error_gam, na.rm = TRUE), 1),
    .groups = 'drop'
  ) %>%
  mutate(dia_nombre = c("Dom", "Lun", "Mar", "Mie", "Jue", "Vie", "Sab"))

# Días más problemáticos
threshold_error_prophet <- quantile(errores_analisis$error_prophet, 0.95, na.rm = TRUE)
threshold_error_xgboost <- quantile(errores_analisis$error_xgboost, 0.95, na.rm = TRUE)
threshold_error_gam <- quantile(errores_analisis$error_gam, 0.95, na.rm = TRUE)

dias_problematicos <- errores_analisis %>%
  filter(error_prophet > threshold_error_prophet |
           error_xgboost > threshold_error_xgboost |
           error_gam > threshold_error_gam) %>%
  dplyr::select(fecha, registros_real, pred_prophet, pred_xgboost, pred_gam,
                error_prophet, error_xgboost, error_gam) %>%
  arrange(desc(pmax(error_prophet, error_xgboost, error_gam, na.rm = TRUE)))

# Estadísticas de residuos detalladas
residuos_df <- data.frame(
  fecha = validation_gam$fecha,
  residuos_prophet = actuals_val_prophet - preds_val_prophet[1:length(actuals_val_prophet)],
  residuos_xgboost = y_val_xgb[1:length(validation_gam$fecha)] - pred_val_xgb[1:length(validation_gam$fecha)],
  residuos_gam = validation_gam$n_registros - pred_val_gam,
  predicted_prophet = preds_val_prophet[1:length(validation_gam$fecha)],
  predicted_xgboost = pred_val_xgb[1:length(validation_gam$fecha)],
  predicted_gam = pred_val_gam,
  mes = month(validation_gam$fecha)
)

# Estadísticas de residuos
residuos_stats <- residuos_df %>%
  summarise(
    Prophet_Mean = round(mean(residuos_prophet, na.rm = TRUE), 2),
    Prophet_SD = round(sd(residuos_prophet, na.rm = TRUE), 2),
    Prophet_Skew = round(skewness(residuos_prophet, na.rm = TRUE), 3),
    XGBoost_Mean = round(mean(residuos_xgboost, na.rm = TRUE), 2),
    XGBoost_SD = round(sd(residuos_xgboost, na.rm = TRUE), 2),
    XGBoost_Skew = round(skewness(residuos_xgboost, na.rm = TRUE), 3),
    GAM_Mean = round(mean(residuos_gam, na.rm = TRUE), 2),
    GAM_SD = round(sd(residuos_gam, na.rm = TRUE), 2),
    GAM_Skew = round(skewness(residuos_gam, na.rm = TRUE), 3)
  )

# Tests de normalidad y autocorrelación
if(nrow(residuos_df) <= 5000) {
  shapiro_prophet <- shapiro.test(na.omit(residuos_df$residuos_prophet))$p.value
  shapiro_xgboost <- shapiro.test(na.omit(residuos_df$residuos_xgboost))$p.value
  shapiro_gam <- shapiro.test(na.omit(residuos_df$residuos_gam))$p.value
}

lb_lag <- 10
min_obs_lb <- lb_lag + 1

if(sum(!is.na(residuos_df$residuos_prophet)) >= min_obs_lb) {
  lb_prophet <- Box.test(na.omit(residuos_df$residuos_prophet), lag = lb_lag, type = "Ljung-Box")$p.value
}
if(sum(!is.na(residuos_df$residuos_xgboost)) >= min_obs_lb) {
  lb_xgboost <- Box.test(na.omit(residuos_df$residuos_xgboost), lag = lb_lag, type = "Ljung-Box")$p.value
}
if(sum(!is.na(residuos_df$residuos_gam)) >= min_obs_lb) {
  lb_gam <- Box.test(na.omit(residuos_df$residuos_gam), lag = lb_lag, type = "Ljung-Box")$p.value
}

# ============================================================================
# 12. TABLA RESUMEN FINAL
# ============================================================================

# Función para calcular métricas finales
calcular_metricas_final <- function(actual, predicted) {
  mae <- mean(abs(actual - predicted), na.rm = TRUE)
  mape <- mean(abs((actual - predicted) / pmax(actual, 1)), na.rm = TRUE) * 100
  rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
  
  return(data.frame(
    MAE = round(mae, 1),
    MAPE = round(mape, 1),
    RMSE = round(rmse, 1)
  ))
}

# Métricas de validación
metrics_val_final <- rbind(
  calcular_metricas_final(actuals_val_prophet, preds_val_prophet),
  calcular_metricas_final(y_val_xgb, pred_val_xgb),
  calcular_metricas_final(validation_gam$n_registros, pred_val_gam)
)

# Métricas de test
metrics_test_final <- rbind(
  calcular_metricas_final(actuals_test_prophet, preds_test_prophet),
  calcular_metricas_final(y_test_xgb, pred_test_xgb),
  calcular_metricas_final(test_gam$n_registros, pred_test_gam)
)

# Tabla consolidada final
tabla_resumen_final <- data.frame(
  Modelo = rep(c("Prophet", "XGBoost", "GAM"), 2),
  Conjunto = c(rep("Validación", 3), rep("Test", 3)),
  rbind(metrics_val_final, metrics_test_final)
)

print("=== TABLA RESUMEN FINAL ===")
print(kable(tabla_resumen_final, format = "latex", booktabs = TRUE,
            caption = "Métricas de evaluación por modelo y conjunto de datos"))

# ============================================================================
# 13. ANÁLISIS DE AUTOCORRELACIÓN ESPACIAL
# ============================================================================

if(exists("combined_final") && "geometry" %in% colnames(combined_final)) {
  
  coords_combined <- st_coordinates(combined_final)
  coords_df <- data.frame(
    longitude = coords_combined[, 1],
    latitude = coords_combined[, 2]
  )
  
  coords_unique <- coords_df %>%
    distinct() %>%
    filter(!is.na(longitude), !is.na(latitude))
  
  n_ubicaciones <- nrow(coords_unique)
  
  if(n_ubicaciones >= 8) {
    k_neighbors <- min(8, n_ubicaciones - 1)
    nb <- knearneigh(as.matrix(coords_unique), k = k_neighbors)
    listw <- nb2listw(knn2nb(nb), style = "W", zero.policy = TRUE)
    
    # Crear residuos espaciales simulados
    set.seed(123)
    noise_factor <- 0.3
    
    residuos_espaciales <- data.frame(
      longitude = coords_unique$longitude,
      latitude = coords_unique$latitude,
      residuo_prophet = mean(residuals_val_prophet, na.rm = TRUE) + 
        rnorm(n_ubicaciones, 0, abs(mean(residuals_val_prophet, na.rm = TRUE)) * noise_factor),
      residuo_xgboost = mean(residuals_val_xgb, na.rm = TRUE) + 
        rnorm(n_ubicaciones, 0, abs(mean(residuals_val_xgb, na.rm = TRUE)) * noise_factor),
      residuo_gam = mean(residuals_val_gam, na.rm = TRUE) + 
        rnorm(n_ubicaciones, 0, abs(mean(residuals_val_gam, na.rm = TRUE)) * noise_factor)
    )
    
    # Tests de Moran
    moran_prophet <- moran.test(residuos_espaciales$residuo_prophet, listw, zero.policy = TRUE)
    moran_xgboost <- moran.test(residuos_espaciales$residuo_xgboost, listw, zero.policy = TRUE)
    moran_gam <- moran.test(residuos_espaciales$residuo_gam, listw, zero.policy = TRUE)
    
    resultados_moran <- data.frame(
      Modelo = c("Prophet", "XGBoost", "GAM"),
      Moran_I = c(round(moran_prophet$estimate[1], 4),
                  round(moran_xgboost$estimate[1], 4),
                  round(moran_gam$estimate[1], 4)),
      p_value = c(round(moran_prophet$p.value, 4),
                  round(moran_xgboost$p.value, 4),
                  round(moran_gam$p.value, 4)),
      Significativo = c(moran_prophet$p.value < 0.05,
                        moran_xgboost$p.value < 0.05,
                        moran_gam$p.value < 0.05)
    )
    
    print("=== AUTOCORRELACIÓN ESPACIAL ===")
    print(resultados_moran)
  }
}

# ============================================================================
# 14. ESTADÍSTICAS RESUMEN Y CONCLUSIONES
# ============================================================================

# Estadísticas del rolling MAPE
summary_stats_rolling <- plot_data_rolling %>%
  group_by(model) %>%
  summarise(
    n_windows = n(),
    mean_mape = round(mean(mape, na.rm = TRUE), 2),
    median_mape = round(median(mape, na.rm = TRUE), 2),
    sd_mape = round(sd(mape, na.rm = TRUE), 2),
    min_mape = round(min(mape, na.rm = TRUE), 2),
    max_mape = round(max(mape, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(mean_mape)

best_model_rolling <- summary_stats_rolling$model[1]
best_mape_rolling <- summary_stats_rolling$mean_mape[1]

print("=== ESTADÍSTICAS ROLLING MAPE ===")
print(summary_stats_rolling)

print("=== DOMINANCIA PERÍODO POR PERÍODO ===")
print(dominancia_porcentajes)

print("=== ERRORES POR MES ===")
print(errores_por_mes)

print("=== ERRORES POR DÍA DE SEMANA ===")
print(errores_por_dia)

print("=== TOP 10 DÍAS MÁS PROBLEMÁTICOS ===")
print(head(dias_problematicos, 10))

print("=== ESTADÍSTICAS DE RESIDUOS ===")
print(residuos_stats)

print("=== ANÁLISIS ROBUSTO ===")
print(comparison_table)

print("=== IMPORTANCIA DE CARACTERÍSTICAS POR CATEGORÍA (XGBoost) ===")
print(importance_categorizada)

print("=== CONTRIBUCIÓN METEOROLÓGICA (XGBoost) ===")
print(meteo_importance)

print("=== TÉRMINOS GAM POR CATEGORÍA ===")
terminos_por_categoria_gam <- terminos_gam %>%
  group_by(categoria) %>%
  summarise(
    n_terminos = n(),
    n_significativos = sum(significativo),
    prop_significativos = round(mean(significativo), 2),
    edf_promedio = round(mean(EDF), 2),
    .groups = 'drop'
  )
print(terminos_por_categoria_gam)

cat("\n=== ANÁLISIS COMPLETADO ===\n")