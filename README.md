# Análisis y predicción de avistamientos de aves para el turismo inteligente en la Comunidad Valenciana

Trabajo de Fin de Grado del Grado en Ciencia de Datos (Universitat Politècnica de València, 2025).

Análisis de las observaciones de aves registradas en eBird en la Comunidad Valenciana entre enero de 2021 y
marzo de 2025: patrones temporales y espaciales, relación con la meteorología y con los espacios protegidos,
y modelos para predecir el número de avistamientos.

## Datos

- `data/raw/`: datos originales de eBird. No se pueden subir por su tamaño; en `data/raw/README.md` se explica
  cómo descargarlos.
- `data/new/weather_completo_2021_2025.csv`: datos meteorológicos (temperatura, viento, precipitación y
  humedad) descargados de la API de Open-Meteo con `openMeteo.py`.
- `data/new/combined.RData`: observaciones de eBird preprocesadas y unidas con la meteorología.
- `data/new/combined_final.RData`: el conjunto anterior con la asignación de espacios protegidos.
- `ZEPA/` y `humedales/`: capas cartográficas de las ZEPA y de las zonas húmedas.

## Scripts

1. `carga_preprocesamiento.R`: carga y preprocesamiento de los datos de eBird y unión con la meteorología.
2. `openMeteo.py`: descarga de los datos meteorológicos de Open-Meteo.
3. `caracterizacion_especies.R`: caracterización de las especies observadas.
4. `analisis_temporal.R`: patrones temporales de las observaciones.
5. `analisi_geospacial.R`: análisis geoespacial y asignación de espacios protegidos.
6. `relacion_mtereologia.R`: relación entre las variables meteorológicas y las observaciones.
7. `modelizacion.R`: modelos predictivos (Prophet, GAM con familias Poisson, Binomial Negativa, Tweedie y
   gaussiana, y XGBoost con pérdida Tweedie).

Los scripts 3 a 7 se pueden ejecutar por separado si antes se cargan los datos que usan (`combined.RData` o
`combined_final.RData`).
