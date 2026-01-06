# ============================
# Trabajo 3 IDG - Geomarketing
# Clustering zonas censales GS
# ============================

library(DBI)
library(RPostgres)
library(sf)
library(dplyr)
library(ggplot2)
library(factoextra)
library(vegan)      # índice de Shannon
library(cowplot)
library(viridis)    
library(GGally)
library(tidyr)
# ----------------------------
# 1. Conexión a la base de datos
# ----------------------------

db_host     = "localhost"
db_port     = 5432
db_name     = "censo_rm_2017"
db_user     = "postgres"
db_password = "lucho98"

con = dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)

# ----------------------------
# 2. Carga de datos T2 y capas
# ----------------------------

# Capa final de zonas de la T2 (tiene prop_trabajo_formal)
zonas_gs_final = readRDS("Data/zonas_gs_final.rds")

# Base microsimulada (por ahora no la usamos directamente)
sim_df = readRDS("Data/sim_df.rds")

# Capa de ingreso microsimulado de clase (mediana_ingreso por zona)
zonas_gs_ingreso = st_read("Data/zonas_gs_ingreso.geojson")

# Asegurar que geocodigo sea carácter para poder hacer join
zonas_gs_ingreso$geocodigo = as.character(zonas_gs_ingreso$geocodigo)
zonas_gs_final$geocodigo   = as.character(zonas_gs_final$geocodigo)

# Unir mediana de ingreso a la capa final de zonas
zonas_gs_final = zonas_gs_final %>%
  left_join(
    zonas_gs_ingreso %>%
      st_drop_geometry() %>%
      select(geocodigo, mediana_ingreso),
    by = "geocodigo"
  )

summary(zonas_gs_final$mediana_ingreso)

# ----------------------------------------------
# 3. Indicadores desde BD: migración y escolaridad
# ----------------------------------------------

sql_indicadores = "
SELECT
  z.geocodigo AS geocodigo,
  c.nom_comuna,

  -- Porcentaje de migrantes
  ROUND(
    COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99)) * 100.0
    / NULLIF(COUNT(*), 0),
  2) AS ptje_migrantes,

  -- Porcentaje de personas con escolaridad >= 16 años
  ROUND(
    COUNT(*) FILTER (WHERE p.escolaridad >= 16) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.escolaridad IS NOT NULL), 0),
  2) AS ptje_esc_mayor_16
  
FROM public.personas   AS p
JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas      AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
JOIN public.comunas    AS c ON z.codigo_comuna   = c.codigo_comuna

GROUP BY z.geocodigo, c.nom_comuna
ORDER BY ptje_esc_mayor_16 DESC;
"

df_indicadores_sql = dbGetQuery(con, sql_indicadores)

# Asegurar tipo texto
df_indicadores_sql$geocodigo = as.character(df_indicadores_sql$geocodigo)

# Unir indicadores a la capa de zonas
zonas_gs_final = zonas_gs_final %>%
  left_join(df_indicadores_sql, by = "geocodigo")

names(zonas_gs_final)
summary(zonas_gs_final$ptje_migrantes)
summary(zonas_gs_final$ptje_esc_mayor_16)

# ----------------------------
# 4. Preparar datos para clustering
# ----------------------------

# Filtrar filas completas en variables de interés
zonas_cluster = zonas_gs_final %>%
  filter(
    !is.na(prop_trabajo_formal),
    !is.na(mediana_ingreso),
    !is.na(ptje_migrantes),
    !is.na(ptje_esc_mayor_16)
  )

# Matriz de variables numéricas (sin geometría)
vars_clusters = zonas_cluster %>%
  st_drop_geometry() %>%
  select(
    prop_trabajo_formal,
    mediana_ingreso,
    ptje_migrantes,
    ptje_esc_mayor_16
  )

# Estandarizar variables
vars_scaled = scale(vars_clusters)
summary(vars_scaled)

# ------------------------------------
# 4.1 Estadísticas descriptivas básicas
# ------------------------------------

# Tasa de respuesta (zonas con datos completos)
n_total   = nrow(zonas_gs_final)
n_usable  = nrow(zonas_cluster)
tasa_resp = round(100 * n_usable / n_total, 1)

tasa_resp
# Esto lo vas a comentar en el informe: "% de zonas censales con información completa"

# Estadísticas descriptivas de cada variable (a nivel zona)
summary(vars_clusters)

#Histograma de las variables
df_univ <- zonas_cluster %>%
  st_drop_geometry() %>%
  select(
    prop_trabajo_formal,
    mediana_ingreso,
    ptje_migrantes,
    ptje_esc_mayor_16
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  )

ggplot(df_univ, aes(x = valor)) +
  geom_histogram(bins = 30, fill = "grey70", color = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  labs(
    title = "Distribución de las variables utilizadas en el clustering",
    x = NULL,
    y = "Frecuencia"
  ) +
  theme_minimal()


# Revisión de rangos de cada una ed las variables
sapply(vars_clusters, range, na.rm = TRUE)

# Matriz de correlaciones numérica
cor_vars = cor(vars_clusters, use = "complete.obs")
cor_vars


# ----------------------------
# 5. Número óptimo de clusters (método del codo)
# ----------------------------

set.seed(123)

graf_codo = fviz_nbclust(vars_scaled, kmeans, method = "wss") +
  labs(
    title = "Método del codo para K-means",
    x = "Número de clusters (k)",
    y = "Suma de cuadrados intra-cluster (WSS)"
  )

print(graf_codo)

# ----------------------------
# 6. K-means y resumen de clusters
# ----------------------------

set.seed(123)
k_optimo = 3   # AJUSTA este valor según lo que veas en el gráfico del codo

km = kmeans(vars_scaled, centers = k_optimo, nstart = 25)

# Agregar el número de cluster a la capa espacial
zonas_cluster$cluster = as.factor(km$cluster)

# Resumen de variables por cluster
resumen_clusters = zonas_cluster %>%
  st_drop_geometry() %>%
  group_by(cluster) %>%
  summarise(
    n_zonas              = n(),
    prop_formal_prom     = mean(prop_trabajo_formal, na.rm = TRUE),
    mediana_ingreso_prom = mean(mediana_ingreso,     na.rm = TRUE),
    ptje_migrantes_prom  = mean(ptje_migrantes,      na.rm = TRUE),
    ptje_esc16_prom      = mean(ptje_esc_mayor_16,   na.rm = TRUE)
  )

print(resumen_clusters)

# Etiquetas para los clusters 
etiquetas_cluster = c(
  "1" = "Clase media diversa con alta migración",
  "2" = "Zonas de alto nivel socioeconómico",
  "3" = "Zonas de menor ingreso y menor escolaridad"
)

zonas_cluster$cluster_lab = factor(
  zonas_cluster$cluster,
  levels = names(etiquetas_cluster),
  labels = etiquetas_cluster
)



# ------------------------------------
# 6.1 Matriz de correlaciones coloreada por cluster
# ------------------------------------

# Armamos un data.frame sin geometría, con el cluster
df_pairs <- zonas_cluster %>%
  st_drop_geometry() %>%
  select(
    cluster_lab,
    prop_trabajo_formal,
    mediana_ingreso,
    ptje_migrantes,
    ptje_esc_mayor_16
  )

graf_pairs <- ggpairs(
  df_pairs,
  columns  = 2:5,  # las variables numéricas
  mapping  = aes(color = cluster_lab),  # color por cluster
  title    = "Relaciones entre formalidad, ingreso, migración y educación\ncoloreadas según cluster",
  upper    = list(continuous = wrap("cor", size = 3)),
  lower    = list(continuous = wrap("points", alpha = 0.5, size = 0.5)),
  diag     = list(continuous = wrap("densityDiag"))
) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

print(graf_pairs)

#
# 6.2 Gráfico de BOXPLOT de variables
#-----------------------------------

# ----------------------------
# 7. Geometría de comunas del Gran Santiago
# ----------------------------

# Comunas que aparecen en las zonas del Gran Santiago
comunas_gs = zonas_cluster %>%
  st_drop_geometry() %>%
  distinct(nom_comuna) %>%
  pull(nom_comuna)

sql_comunas = "
SELECT cut, nom_comuna, geom
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO';
"

sf_comunas_santiago = st_read(con, query = sql_comunas) %>%
  filter(nom_comuna %in% comunas_gs)

# ----------------------------
# 8. Variabilidad interna: tabla zonas x comuna x cluster
# ----------------------------

tabla_clusters_comuna = zonas_cluster %>%
  st_drop_geometry() %>%
  group_by(nom_comuna, cluster) %>%
  summarise(n_zonas = n(), .groups = "drop")

# Cluster dominante por comuna (el más frecuente)
cluster_dom_comuna = tabla_clusters_comuna %>%
  group_by(nom_comuna) %>%
  slice_max(n_zonas, n = 1, with_ties = FALSE) %>%
  ungroup()

# Índice de Shannon por comuna (diversidad interna de clusters)
shannon_comuna = tabla_clusters_comuna %>%
  group_by(nom_comuna) %>%
  summarise(
    shannon = diversity(n_zonas, index = "shannon"),
    .groups = "drop"
  )

# ----------------------------
# 9. Unir info comunal (cluster dominante + Shannon)
# ----------------------------

# Unión con cluster dominante
sf_comunas_cluster = sf_comunas_santiago %>%
  left_join(cluster_dom_comuna, by = "nom_comuna")

sf_comunas_cluster$cluster = as.factor(sf_comunas_cluster$cluster)

sf_comunas_cluster$cluster_lab = factor(
  sf_comunas_cluster$cluster,
  levels = names(etiquetas_cluster),
  labels = etiquetas_cluster
)

# Unión con índice de Shannon
sf_comunas_shannon = sf_comunas_santiago %>%
  left_join(shannon_comuna, by = "nom_comuna")

# ----------------------------
# 10. Contorno urbano a partir de zonas censales
# ----------------------------

# Unión de todas las zonas censales: "mancha urbana"
urb_gs = st_union(st_geometry(zonas_cluster))

# Intersección de comunas con esa mancha urbana
sf_comunas_shannon_urb = st_intersection(sf_comunas_shannon, urb_gs)
sf_comunas_cluster_urb = st_intersection(sf_comunas_cluster, urb_gs)

# ----------------------------
# 11. Transformación a UTM WGS84 / 19S
# ----------------------------

crs_utm <- 32719  # WGS 84 / UTM zone 19S

zonas_cluster_utm          <- st_transform(zonas_cluster,          crs_utm)
sf_comunas_santiago_utm    <- st_transform(sf_comunas_santiago,    crs_utm)
sf_comunas_cluster_urb_utm <- st_transform(sf_comunas_cluster_urb, crs_utm)
sf_comunas_shannon_urb_utm <- st_transform(sf_comunas_shannon_urb, crs_utm)

# Bbox en UTM para usar en coord_sf
bbox_utm <- st_bbox(zonas_cluster_utm)

# ----------------------------
# 12. Mapa de clusters por zona censal (UTM)
# ----------------------------

mapa_clusters_zonas <- ggplot() +
  geom_sf(data = zonas_cluster_utm,
          aes(fill = cluster_lab),
          color = NA) +
  geom_sf(data = sf_comunas_cluster_urb_utm,
          fill = NA, color = "black", size = 0.4) +
  geom_sf_text(
    data = st_point_on_surface(sf_comunas_cluster_urb_utm),
    aes(label = nom_comuna),
    size = 2,
    fontface = "bold"
  ) +
  scale_fill_brewer(
    palette = "Set2",
    name    = "Tipo de zona\n(según cluster)"
  ) +
  labs(
    title    = "Clusters de zonas censales según\nformalidad, ingreso, educación y migración",
    subtitle = "Gran Santiago (nivel zona censal)",
    caption  = "Elaboración propia. Datos: Censo 2017 y CASEN.\nTrabajo 3 Geomarketing."
  ) +
  coord_sf(
    xlim = c(bbox_utm["xmin"], bbox_utm["xmax"]),
    ylim = c(bbox_utm["ymin"], bbox_utm["ymax"]),
    expand = FALSE,
    crs    = st_crs(crs_utm)
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    plot.caption  = element_text(hjust = 0, size = 7, colour = "grey30",
                                 margin = margin(t = 5))
  )

print(mapa_clusters_zonas)

# ----------------------------
# 13. Mapa de cluster dominante por comuna (UTM + recorte)
# ----------------------------

mapa_clusters_comuna <- ggplot() +
  geom_sf(data = sf_comunas_cluster_urb_utm,
          aes(fill = cluster_lab),
          color = "black", size = 0.4) +
  geom_sf_text(
    data = st_point_on_surface(sf_comunas_cluster_urb_utm),
    aes(label = nom_comuna),
    size = 2,
    fontface = "bold"
  ) +
  scale_fill_brewer(
    palette = "Set2",
    name    = "Cluster dominante\n(tipo de zona)"
  ) +
  labs(
    title    = "Cluster dominante por comuna",
    subtitle = "Gran Santiago (recortado al área urbana)",
    caption  = "Elaboración propia. Datos: Censo 2017 y CASEN.\nTrabajo 3 Geomarketing."
  ) +
  coord_sf(
    xlim = c(bbox_utm["xmin"], bbox_utm["xmax"]),
    ylim = c(bbox_utm["ymin"], bbox_utm["ymax"]),
    expand = FALSE,
    crs    = st_crs(crs_utm)
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    plot.caption  = element_text(hjust = 0, size = 7, colour = "grey30",
                                 margin = margin(t = 5))
  )

print(mapa_clusters_comuna)

# ----------------------------
# 14. Mapa del índice de Shannon por comuna (UTM + recorte)
# ----------------------------

# Color del texto según valor de Shannon (para legibilidad)
#sf_comunas_shannon_urb_utm <- sf_comunas_shannon_urb_utm %>%
 # mutate(label_color = ifelse(shannon > 0.4, "black", "white"))

mapa_shannon <- ggplot() +
  geom_sf(data = sf_comunas_shannon_urb_utm,
          aes(fill = shannon),
          color = "grey30", size = 0.3) +
  geom_sf_text(
    data = sf_comunas_shannon_urb_utm,
    aes(label = nom_comuna),
    colour   = "white",
    size     = 2,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_fill_viridis_c(
    option   = "viridis",
    begin    = 0.1,
    end      = 0.95,
    na.value = "white",
    name     = "Índice de Shannon\n(diversidad de clusters)"
  ) +
  labs(
    title    = "Variabilidad interna de clusters por comuna",
    subtitle = "Índice de Shannon (0 = sin diversidad, valores altos = mayor mezcla de tipos de zona)",
    caption  = "Elaboración propia. Datos: Censo 2017 y CASEN.\nTrabajo 3 Geomarketing."
  ) +
  coord_sf(
    xlim = c(bbox_utm["xmin"], bbox_utm["xmax"]),
    ylim = c(bbox_utm["ymin"], bbox_utm["ymax"]),
    expand = FALSE,
    crs    = st_crs(crs_utm)
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5),
    legend.position = "right",
    panel.grid.major = element_line(colour = "grey85", size = 0.2),
    plot.caption    = element_text(hjust = 0, size = 7, colour = "grey30",
                                   margin = margin(t = 5))
  )

print(mapa_shannon)

# ----------------------------
# 15. Cerrar conexión (opcional)
# ----------------------------

dbDisconnect(con) 