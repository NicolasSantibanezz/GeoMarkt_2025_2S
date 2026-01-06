# ============================================================
# TF IDG — Paso 1: DEMANDA por ZONA CENSAL usando PostgreSQL
# POBLACIÓN por zona = COUNT(personas) via personas -> hogares -> viviendas -> zonas
# Geometría = dpa.zonas_censales_rm
# ============================================================

library(DBI)
library(RPostgres)
library(dplyr)
library(sf)

# ----------------------------
# 0) AJUSTA SOLO ESTO
# ----------------------------
ruta_casen_prob  <- "casen_rm_comida_fuera_prob_y_clase.rds"
ruta_casen_gasto <- "casen_rm_comida_fuera_imputado_solo_gastadores.rds"  # opcional

db_host <- "localhost"
db_port <- 5432
db_name <- "censo_rm_2017"
db_user <- "postgres"
db_password <- "lucho98"

# ----------------------------
# 1) CONEXIÓN
# ----------------------------
con <- dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)

# ----------------------------
# 2) HELPERS
# ----------------------------
pick_first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

standardize_comuna_from_estrato <- function(df) {
  if ("comuna" %in% names(df)) {
    df$comuna <- as.character(df$comuna)
    return(df)
  }
  alt <- pick_first_existing(df, c("COMUNA","cod_comuna","codigo_comuna","comuna_cod","Comuna"))
  if (!is.na(alt)) {
    df <- df %>% mutate(comuna = as.character(.data[[alt]]))
    return(df)
  }
  if (!("estrato" %in% names(df))) {
    stop("CASEN no tiene 'comuna' ni 'estrato' para construir comuna. Revisa names(casen).")
  }
  df <- df %>% mutate(comuna = substr(as.character(estrato), 1, 5))
  df$comuna <- as.character(df$comuna)
  return(df)
}

# ============================================================
# 3) CREAR TEMP TABLE: POBLACIÓN POR ZONA (cadena correcta según ERD)
#    personas -> hogares -> viviendas -> zonas
# ============================================================
dbExecute(con, "DROP TABLE IF EXISTS tmp_pob_zona;")

dbExecute(con, "
CREATE TEMP TABLE tmp_pob_zona AS
SELECT
  z.geocodigo::text      AS geocodigo,
  z.codigo_comuna::text  AS comuna_cod,
  COUNT(*)::bigint       AS pob_zona
FROM public.personas p
JOIN public.hogares h
  ON p.hogar_ref_id = h.hogar_ref_id
JOIN public.viviendas v
  ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas z
  ON v.zonaloc_ref_id = z.zonaloc_ref_id
GROUP BY z.geocodigo::text, z.codigo_comuna::text;
")

dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_tmp_pob_zona_geocodigo ON tmp_pob_zona (geocodigo);")

print(dbGetQuery(con, "SELECT COUNT(*) AS n_zonas_tmp FROM tmp_pob_zona;"))
print(dbGetQuery(con, "SELECT * FROM tmp_pob_zona LIMIT 5;"))

# ============================================================
# 4) CARGAR CASEN DEMANDA (prob/clase + opcional gasto)
# ============================================================
casen <- readRDS(ruta_casen_prob)

req_cols <- c("prob_predicha", "clasificacion")
missing_req <- setdiff(req_cols, names(casen))
if (length(missing_req) > 0) stop("Faltan columnas en CASEN: ", paste(missing_req, collapse = ", "))

casen <- standardize_comuna_from_estrato(casen)

col_rango_edad <- if ("rango_edad" %in% names(casen)) "rango_edad" else NA_character_
col_grupo_esc  <- if ("grupo_escolaridad" %in% names(casen)) "grupo_escolaridad" else NA_character_

demanda_comuna <- casen %>%
  group_by(comuna) %>%
  summarise(
    n_personas     = n(),
    prob_media     = mean(prob_predicha, na.rm = TRUE),
    prob_suma      = sum(prob_predicha,  na.rm = TRUE),
    n_consumidores = sum(clasificacion == 1, na.rm = TRUE),
    
    prop_jovenes = if (!is.na(col_rango_edad)) mean(.data[[col_rango_edad]] == "jovenes", na.rm = TRUE) else NA_real_,
    prop_adultos = if (!is.na(col_rango_edad)) mean(.data[[col_rango_edad]] == "adultos", na.rm = TRUE) else NA_real_,
    prop_universitarios = if (!is.na(col_grupo_esc)) mean(.data[[col_grupo_esc]] %in% c("Universitaria","Postgrado"), na.rm = TRUE) else NA_real_,
    
    .groups = "drop"
  ) %>%
  rename(comuna_cod = comuna) %>%
  mutate(comuna_cod = as.character(comuna_cod))

# --- gasto imputado opcional
if (file.exists(ruta_casen_gasto)) {
  casen_gasto <- readRDS(ruta_casen_gasto)
  casen_gasto <- standardize_comuna_from_estrato(casen_gasto)
  
  col_gasto <- pick_first_existing(casen_gasto, c("gasto_estimado_wins","gasto_estimado"))
  if (is.na(col_gasto)) stop("No encontré columna de gasto en casen_gasto (gasto_estimado_wins o gasto_estimado).")
  
  gasto_comuna <- casen_gasto %>%
    group_by(comuna) %>%
    summarise(
      gasto_total = sum(.data[[col_gasto]], na.rm = TRUE),
      gasto_medio = mean(.data[[col_gasto]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(comuna_cod = comuna) %>%
    mutate(comuna_cod = as.character(comuna_cod))
  
  demanda_comuna <- demanda_comuna %>%
    left_join(gasto_comuna, by = "comuna_cod")
} else {
  demanda_comuna <- demanda_comuna %>%
    mutate(gasto_total = NA_real_, gasto_medio = NA_real_)
}

# ============================================================
# 5) TRAER POBLACIÓN POR ZONA + pob por comuna + pesos
# ============================================================
pob_zona <- dbGetQuery(con, "SELECT geocodigo, comuna_cod, pob_zona FROM tmp_pob_zona;") %>%
  mutate(
    geocodigo = as.character(geocodigo),
    comuna_cod = as.character(comuna_cod),
    pob_zona = as.numeric(pob_zona)
  )

pob_comuna <- pob_zona %>%
  group_by(comuna_cod) %>%
  summarise(pob_comuna = sum(pob_zona, na.rm = TRUE), .groups = "drop")

demanda_zona_tabla <- pob_zona %>%
  left_join(pob_comuna, by = "comuna_cod") %>%
  left_join(demanda_comuna, by = "comuna_cod") %>%
  mutate(
    peso_zona = ifelse(!is.na(pob_comuna) & pob_comuna > 0, pob_zona / pob_comuna, NA_real_),
    demanda_prob_zona  = prob_suma * peso_zona,
    demanda_gasto_zona = ifelse(!is.na(gasto_total), gasto_total * peso_zona, NA_real_)
  )

# ============================================================
# 6) TRAER GEOMETRÍA dpa.zonas_censales_rm sin st_read(con, query)
# ============================================================
srid <- dbGetQuery(con, "SELECT ST_SRID(geom) AS srid FROM dpa.zonas_censales_rm WHERE geom IS NOT NULL LIMIT 1;")$srid[1]
if (is.na(srid)) srid <- 4326

zonas_geom <- dbGetQuery(con, "
 SELECT
  z.geocodigo::text AS geocodigo,
  z.nom_comuna::text AS nom_comuna,
  z.urbano,
  ST_AsBinary(z.geom) AS geom_wkb
FROM dpa.zonas_censales_rm z
JOIN dpa.comunas_rm_shp c
  ON z.nom_comuna = c.nom_comuna
WHERE z.urbano = 1
  AND c.nom_provin = 'SANTIAGO';

")

zonas_geom$geom_wkb <- st_as_sfc(structure(zonas_geom$geom_wkb, class = "WKB"), EWKB = TRUE)
zonas_sf <- st_sf(
  zonas_geom %>% select(-geom_wkb),
  geometry = zonas_geom$geom_wkb,
  crs = srid
)

# ============================================================
# 7) UNIR TODO POR geocodigo y EXPORTAR
# ============================================================
demanda_zona_sf <- zonas_sf %>%
  left_join(demanda_zona_tabla, by = "geocodigo")

saveRDS(demanda_zona_tabla, "demanda_zona_censal.rds")
write.csv(demanda_zona_tabla, "demanda_zona_censal.csv", row.names = FALSE)

saveRDS(demanda_zona_sf, "demanda_zona_censal_sf.rds")
st_write(demanda_zona_sf, "demanda_zona_censal.gpkg", delete_dsn = TRUE, quiet = TRUE)

message("LISTO ✅ Exportado: demanda_zona_censal.rds/.csv + demanda_zona_censal_sf.rds + demanda_zona_censal.gpkg")

dbDisconnect(con)
#-----------------------------------------------------
#----------------MAPEADO DE RESULTADOS----------------
#-----------------------------------------------------
m <- st_read("demanda_zona_censal.gpkg", quiet = TRUE)
plot(m["demanda_prob_zona"])


