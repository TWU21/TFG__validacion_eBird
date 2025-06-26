import pandas as pd
import requests
import time
import os
from tqdm import tqdm

# ===================== CONFIGURACIÓN =====================
archivo_ebird = "ebd_ES-VC_202101_202503_relMay-2025.txt"
unique_points_file = "unique_points_2021_2025.csv"
partial_path = "weather_partial_bloques_2021_2025.csv"
batch_size = 12000
sleep_seconds = 0.1

# ===================== GENERAR UNIQUE_POINTS SI NO EXISTE =====================
if not os.path.exists(unique_points_file):
    data = pd.read_csv(archivo_ebird, sep='\t', low_memory=False, na_values=['', 'NA'])
    data.columns = data.columns.str.lower().str.replace(" ", "_").str.replace("-", "_")
    
    data["observation_date"] = pd.to_datetime(data["observation_date"])
    
    if 'time_observations_started' in data.columns:
        data["datetime"] = pd.to_datetime(
            data["observation_date"].astype(str) + " " + 
            data["time_observations_started"].astype(str), 
            errors='coerce'
        )
        data["datetime"] = data["datetime"].dt.floor("H")
    else:
        data["datetime"] = data["observation_date"]
    
    data["date"] = data["datetime"].dt.date
    
    if 'decimal_latitude' in data.columns:
        lat_col, lon_col = 'decimal_latitude', 'decimal_longitude'
    else:
        lat_col, lon_col = 'latitude', 'longitude'
    
    unique_points = data[[lat_col, lon_col, "date"]].dropna().drop_duplicates()
    unique_points.columns = ["lat", "lon", "date"]
    unique_points = unique_points[unique_points["date"] < pd.Timestamp.now().date()]
    unique_points.to_csv(unique_points_file, index=False)
    print(f"Generadas {len(unique_points)} combinaciones únicas")

# ===================== CARGA DE TODOS LOS PUNTOS =====================
unique_points = pd.read_csv(unique_points_file)
print(f"Total combinaciones: {len(unique_points)}")

# ===================== CARGA DE PROGRESO =====================
if os.path.exists(partial_path):
    saved = pd.read_csv(partial_path)
    done_keys = set(zip(saved["lat"], saved["lon"], pd.to_datetime(saved["datetime"]).dt.date))
    print(f"{len(done_keys)} combinaciones ya procesadas.")
else:
    saved = pd.DataFrame()
    done_keys = set()

# ===================== FILTRAR COMBINACIONES PENDIENTES =====================
unique_points["key"] = list(zip(unique_points["lat"], unique_points["lon"], pd.to_datetime(unique_points["date"]).dt.date))
pending_points = unique_points[~unique_points["key"].isin(done_keys)].drop("key", axis=1).reset_index(drop=True)

# ===================== SELECCIONAR BLOQUE =====================
if len(pending_points) == 0:
    print("✅ Todo ya procesado.")
else:
    block = pending_points.iloc[:batch_size]
    print(f"Procesando {len(block)} observaciones...")

    results = []

    # ===================== FUNCION METEO =====================
    def fetch_weather(lat, lon, date, retries=0):
        url = (
            f"https://archive-api.open-meteo.com/v1/archive?"
            f"latitude={lat}&longitude={lon}&start_date={date}&end_date={date}"
            f"&hourly=temperature_2m,precipitation,windspeed_10m,relative_humidity_2m"
            f"&timezone=Europe/Madrid"
        )
        try:
            response = requests.get(url, timeout=15)
            if response.status_code == 200:
                data_json = response.json()
                return [
                    {
                        "datetime": data_json["hourly"]["time"][i],
                        "lat": lat,
                        "lon": lon,
                        "temperature": data_json["hourly"]["temperature_2m"][i],
                        "precipitation": data_json["hourly"]["precipitation"][i],
                        "windspeed": data_json["hourly"]["windspeed_10m"][i],
                        "humidity": data_json["hourly"]["relative_humidity_2m"][i],
                    }
                    for i in range(len(data_json["hourly"]["time"]))
                ]
            elif response.status_code == 429:
                if retries < 3:
                    time.sleep(5)
                    return fetch_weather(lat, lon, date, retries + 1)
                else:
                    return []
            else:
                return []
        except Exception:
            return []

    # ===================== BUCLE PRINCIPAL =====================
    for _, row in tqdm(block.iterrows(), total=len(block)):
        lat, lon, date = row["lat"], row["lon"], row["date"]
        rows = fetch_weather(lat, lon, date)
        results.extend(rows)
        time.sleep(sleep_seconds)
        
        if len(results) >= 100:
            df_temp = pd.DataFrame(results)
            try:
                saved = pd.read_csv(partial_path)
            except:
                saved = pd.DataFrame()
            saved = pd.concat([saved, df_temp], ignore_index=True)
            saved.to_csv(partial_path, index=False)
            results = []

    # ===================== GUARDADO FINAL DEL BLOQUE =====================
    if results:
        df_temp = pd.DataFrame(results)
        try:
            saved = pd.read_csv(partial_path)
        except:
            saved = pd.DataFrame()
        saved = pd.concat([saved, df_temp], ignore_index=True)
        saved.to_csv(partial_path, index=False)
        print(f"✅ Guardadas {len(df_temp)} observaciones.")

# ===================== CREAR ARCHIVO FINAL =====================
if os.path.exists(partial_path):
    final_data = pd.read_csv(partial_path)
    final_data.to_csv("weather_completo_2021_2025.csv", index=False)
    print(f"Archivo final: weather_completo_2021_2025.csv ({len(final_data)} registros)")
