# TFG__validacion_eBird
Este repositorio contiene el código y los datos asociados al Trabajo de Fin de Grado titulado: Análisis y Predicción de Avistamientos de Aves para el Turismo Inteligente en la Comunidad Valenciana.

## Estructura del proyecto

###  Datos

#### `/datos/raw/`
Contiene los datos originales sin procesar:
- `ebd_ES-VC_202101_202503_relMay-2025` - Datos originales de eBird

#### `/datos/new/`
Contiene las versiones procesadas y actualizadas:
- `weather_completo_2021_2025` - Datos meteorológicos, resultado de acceder a la API con el script de openMeteo.
- `combined.RData` - Dataset resultado del procesamiento inicial y unión con variables meteorológicas
- `combined_final.RData` - Dataset final con asignación de espacios protegidos mediante capas cartográficas.

## 🔧 Scripts de análisis

El proyecto incluye 7 scripts principales:

### 1. `carga_preprocesamiento.R`
**Función:** Carga y preprocesamiento inicial de los datos
**Dependencias:**
- `ebd_ES-VC_202101_202503_relMay-2025` 
- `weather_completo_2021_2025` 

### 2. `openMeteo.py`
**Función:** Accede a la API de OpenMeteo para conseguir información sobre: temperatura, velocidad del viento, precipitación y humedad.
**Dependecias:**
-  `ebd_ES-VC_202101_202503_relMay-2025` 

### 2. `caracterizacion_especies.R`
**Función:** Caracterización y análisis de especies
**Dependencias:**
- `combined.RData`

### 3. `analisis_temporal.R`
**Función:** Análisis de patrones temporales
**Dependencias:**
- `combined.RData`

### 4. `analisis_geospacial.R`
**Función:** Análisis geoespacial y asignación de espacios protegidos
**Dependencias:**
- `combined.RData`

### 5. `relacion_meteorologia.R`
**Función:** Análisis de la relación entre variables meteorológicas y observaciones
**Dependencias:**
- `combined_final.RData`

### 6. `modelizacion.R`
**Función:** Desarrollo de modelos predictivos
**Dependencias:**
- `combined_final.RData`
## Nota importante
1. Los scripts pueden ejecutarse independientemente siempre que se tengan cargados los datos requeridos. No es necesario ejecutar todo el flujo completo.
