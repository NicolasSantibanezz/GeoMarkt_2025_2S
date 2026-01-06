# ============================================================
# TF IDG — PASO 3: DEMANDA vs OFERTA
# Indicadores de brecha y saturación por zona censal
# ============================================================

library(sf)
library(dplyr)

# ----------------------------
# 1) CARGAR INSUMOS
# ----------------------------
demanda_sf <- st_read("demanda_zona_censal.gpkg", quiet = TRUE)
oferta_sf  <- st_read("oferta_zona_censal.gpkg", quiet = TRUE)

# Nos quedamos con columnas necesarias
demanda_df <- demanda_sf %>%
  st_drop_geometry() %>%
  select(geocodigo, demanda_prob_zona)

oferta_df <- oferta_sf %>%
  st_drop_geometry() %>%
  select(geocodigo, oferta_n)

# ----------------------------
# 2) UNIR DEMANDA + OFERTA
# ----------------------------
df_do <- demanda_df %>%
  left_join(oferta_df, by = "geocodigo") %>%
  mutate(
    oferta_n = ifelse(is.na(oferta_n), 0L, oferta_n)
  )

# ----------------------------
# 3) INDICADOR 1: DEMANDA POR LOCAL
# ----------------------------
df_do <- df_do %>%
  mutate(
    demanda_x_local = ifelse(oferta_n > 0,
                             demanda_prob_zona / oferta_n,
                             NA_real_)
  )

# ----------------------------
# 4) INDICADOR 2: CLASIFICACIÓN DEMANDA–OFERTA
#    (usamos mediana para Alta / Baja)
# ----------------------------
med_demanda <- median(df_do$demanda_prob_zona, na.rm = TRUE)
med_oferta  <- median(df_do$oferta_n, na.rm = TRUE)

df_do <- df_do %>%
  mutate(
    demanda_cat = ifelse(demanda_prob_zona >= med_demanda, "Alta", "Baja"),
    oferta_cat  = ifelse(oferta_n >= med_oferta, "Alta", "Baja"),
    
    categoria_do = case_when(
      demanda_cat == "Alta" & oferta_cat == "Baja" ~ "Alta demanda / Baja oferta",
      demanda_cat == "Alta" & oferta_cat == "Alta" ~ "Alta demanda / Alta oferta",
      demanda_cat == "Baja" & oferta_cat == "Alta" ~ "Baja demanda / Alta oferta",
      demanda_cat == "Baja" & oferta_cat == "Baja" ~ "Baja demanda / Baja oferta"
    )
  )

# ----------------------------
# 5) PEGAR INDICADORES A GEOMETRÍA
# ----------------------------
zonas_do <- demanda_sf %>%
  left_join(df_do, by = "geocodigo")

# ----------------------------
# 6) EXPORTAR RESULTADOS
# ----------------------------
# Tabla
write.csv(
  zonas_do %>% st_drop_geometry(),
  "demanda_oferta_zona_censal.csv",
  row.names = FALSE
)

saveRDS(zonas_do %>% st_drop_geometry(), "demanda_oferta_zona_censal.rds")

# Geo
st_write(
  zonas_do,
  "demanda_oferta_zona_censal.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

message("LISTO ✅ Paso 3 completado: demanda_oferta_zona_censal.csv/.gpkg")


#--------------------------------------------------------------
#--------------------Mapas Generados---------------------------
#--------------------------------------------------------------

zonas_do <- st_read("demanda_oferta_zona_censal.gpkg", quiet = TRUE)

# Mapa 1: Demanda por local
plot(zonas_do["demanda_x_local"])

# Mapa 2: Clasificación Demanda–Oferta
plot(zonas_do["categoria_do"])
