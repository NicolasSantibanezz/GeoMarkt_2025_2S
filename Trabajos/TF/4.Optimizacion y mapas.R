library(sf)
library(dplyr)

# ============================================================
# 0) RUTAS
# ============================================================
ruta_do_gpkg <- "demanda_oferta_zona_censal.gpkg"
ruta_osm_pts <- "osm_restaurantes_gs.gpkg"

# ============================================================
# 1) PARÁMETROS
# ============================================================
T_min <- 5                # <-- 5 minutos caminando
v_kmh <- 4.5              # velocidad caminando (km/h)
R_m   <- (T_min/60) * v_kmh * 1000   # radio en metros (~375 m)
top_k <- 10

crs_m <- 32719            # UTM 19S (RM)

cat("Umbral:", T_min, "min  |  Radio:", round(R_m), "m\n")

# ============================================================
# 2) CARGAR ZONAS + CONSOLIDAR DEMANDA
# ============================================================
zonas <- st_read(ruta_do_gpkg, quiet = TRUE)

zonas_df <- zonas %>%
  st_drop_geometry() %>%
  mutate(
    demanda_prob_zona = dplyr::coalesce(`demanda_prob_zona.y`, `demanda_prob_zona.x`)
  ) %>%
  transmute(
    geocodigo = as.character(geocodigo),
    demanda_prob_zona = as.numeric(demanda_prob_zona),
    oferta_n = as.integer(oferta_n),
    categoria_do = as.character(categoria_do)
  )

stopifnot(all(c("geocodigo","demanda_prob_zona","oferta_n","categoria_do") %in% names(zonas_df)))

# ============================================================
# 3) PROYECTAR + CENTROS DE ZONA
# ============================================================
zonas_m <- st_transform(zonas, crs_m)

centros <- st_point_on_surface(zonas_m) %>%
  select(geocodigo) %>%
  mutate(geocodigo = as.character(geocodigo))

# ============================================================
# 4) CANDIDATOS (Escenario 2): Alta demanda / Baja oferta + oferta_n == 0
# ============================================================
candidatos <- zonas_df %>%
  filter(categoria_do == "Alta demanda / Baja oferta", oferta_n == 0) %>%
  distinct(geocodigo)

cat("N° candidatos:", nrow(candidatos), "\n")

# Si te quedaran 0 candidatos por oferta_n==0, vuelve al criterio sin esa restricción:
if (nrow(candidatos) == 0) {
  message("⚠️ No hay candidatos con oferta_n==0. Uso solo 'Alta demanda / Baja oferta'.")
  candidatos <- zonas_df %>%
    filter(categoria_do == "Alta demanda / Baja oferta") %>%
    distinct(geocodigo)
  cat("N° candidatos (sin restricción oferta_n==0):", nrow(candidatos), "\n")
}

cand_pts <- centros %>% inner_join(candidatos, by="geocodigo")

# ============================================================
# 5) OPTIMIZACIÓN: MAXIMIZAR DEMANDA CUBIERTA POR BUFFER
# ============================================================
cand_buf <- st_buffer(cand_pts, dist = R_m)

cov <- st_join(
  centros %>% left_join(zonas_df %>% select(geocodigo, demanda_prob_zona), by="geocodigo"),
  cand_buf %>% rename(cand_geocodigo = geocodigo),
  join = st_within,
  left = FALSE
) %>% st_drop_geometry()

score_cand <- cov %>%
  group_by(cand_geocodigo) %>%
  summarise(
    demanda_cubierta = sum(demanda_prob_zona, na.rm = TRUE),
    n_zonas_cubiertas = n(),
    .groups="drop"
  ) %>%
  arrange(desc(demanda_cubierta))

cat("\nTOP candidatos:\n")
print(head(score_cand, top_k))

optimo <- score_cand %>% slice(1)
cat("\n✅ Zona óptima propuesta:", optimo$cand_geocodigo, "\n")

# ============================================================
# 6) ANTES vs DESPUÉS (accesibilidad por distancia)
# ============================================================
osm_pts <- st_read(ruta_osm_pts, quiet = TRUE)
osm_m   <- st_transform(osm_pts, crs_m)

# 6.1 Distancia a restaurante más cercano (oferta actual)
nearest_idx  <- st_nearest_feature(centros, osm_m)
dist_nearest <- st_distance(centros, osm_m[nearest_idx, ], by_element = TRUE)
dist_nearest <- as.numeric(dist_nearest)

base <- centros %>%
  st_drop_geometry() %>%
  left_join(zonas_df %>% select(geocodigo, demanda_prob_zona), by="geocodigo") %>%
  mutate(
    dist_rest_nearest_m = dist_nearest,
    cubierta_actual = ifelse(dist_rest_nearest_m <= R_m, 1L, 0L)
  )

# 6.2 Distancia al nuevo local (óptimo)
opt_pt <- cand_pts %>% filter(geocodigo == optimo$cand_geocodigo)
dist_to_new <- as.numeric(st_distance(centros, opt_pt))

base <- base %>%
  mutate(
    dist_nuevo_m = dist_to_new,
    cubierta_nuevo = ifelse(dist_nuevo_m <= R_m, 1L, 0L),
    cubierta_despues = pmax(cubierta_actual, cubierta_nuevo),
    
    # Mejora binaria (pasa de 0 a 1 por el nuevo local)
    mejora_cobertura = ifelse(cubierta_actual == 0 & cubierta_nuevo == 1, 1L, 0L),
    
    # Mejora continua (reducción distancia)
    dist_mejor_m  = pmin(dist_rest_nearest_m, dist_nuevo_m),
    mejora_dist_m = dist_rest_nearest_m - dist_mejor_m
  ) %>%
  mutate(
    # BLINDAJE: sin NA y sin negativos
    mejora_dist_m = ifelse(is.na(mejora_dist_m), 0, mejora_dist_m),
    mejora_dist_m = pmax(mejora_dist_m, 0)
  )

# ============================================================
# 7) RESÚMENES
# ============================================================
resumen <- base %>%
  summarise(
    demanda_total = sum(demanda_prob_zona, na.rm = TRUE),
    demanda_cubierta_actual = sum(demanda_prob_zona * cubierta_actual, na.rm = TRUE),
    demanda_cubierta_despues = sum(demanda_prob_zona * cubierta_despues, na.rm = TRUE),
    mejora_demanda_cubierta = demanda_cubierta_despues - demanda_cubierta_actual,
    zonas_nuevas_cubiertas = sum(mejora_cobertura, na.rm = TRUE),
    mejora_dist_total_m = sum(mejora_dist_m, na.rm = TRUE),
    mejora_dist_media_m = mean(mejora_dist_m, na.rm = TRUE)
  )

cat("\nRESUMEN (antes vs después):\n")
print(resumen)

cat("\nTabla mejora_cobertura (0=no, 1=si):\n")
print(table(base$mejora_cobertura, useNA="ifany"))

cat("\nResumen mejora_dist_m:\n")
print(summary(base$mejora_dist_m))
cat("\nMin mejora_dist_m (debería ser 0):\n")
print(min(base$mejora_dist_m, na.rm = TRUE))

# ============================================================
# 8) EXPORTS
# ============================================================
write.csv(score_cand, "optimizacion_scores_candidatos_buffer.csv", row.names = FALSE)
write.csv(base, "mejora_por_zona_buffer.csv", row.names = FALSE)

zonas_out <- zonas_m %>%
  mutate(geocodigo = as.character(geocodigo)) %>%
  left_join(
    base %>% select(geocodigo, cubierta_actual, cubierta_nuevo, cubierta_despues,
                    mejora_cobertura, mejora_dist_m,
                    dist_rest_nearest_m, dist_nuevo_m),
    by="geocodigo"
  ) %>%
  mutate(es_optimo = ifelse(geocodigo == optimo$cand_geocodigo, 1L, 0L))

st_write(zonas_out, "zonas_optimizacion_buffer.gpkg", delete_dsn = TRUE, quiet = TRUE)

opt_buf <- st_buffer(opt_pt, dist = R_m)
st_write(opt_buf, "area_servicio_optimo_buffer.gpkg", delete_dsn = TRUE, quiet = TRUE)

cat("\n✅ Exportado:\n- optimizacion_scores_candidatos_buffer.csv\n- mejora_por_zona_buffer.csv\n- zonas_optimizacion_buffer.gpkg\n- area_servicio_optimo_buffer.gpkg\n")

# ============================================================
# 9) MAPAS 
# ============================================================
plot(zonas_out["mejora_cobertura"], main=paste0("Nuevas zonas cubiertas (",T_min," min ~ ", round(R_m)," m)"))
plot(st_geometry(opt_buf), add = TRUE)
plot(st_geometry(opt_pt), add = TRUE, pch = 16)

plot(zonas_out["mejora_dist_m"], main=paste0("Mejora accesibilidad: reducción distancia (",T_min," min ~ ", round(R_m)," m)"))
plot(st_geometry(opt_buf), add = TRUE)
plot(st_geometry(opt_pt), add = TRUE, pch = 16)
