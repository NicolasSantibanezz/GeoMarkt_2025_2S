# ============================================================
# TF IDG — Paso 2 (OFERTA): Restaurantes desde OSM -> Zona Censal
# Requiere: demanda_zona_censal.gpkg (del Paso 1)
# Output: oferta_zona_censal.csv + oferta_zona_censal.gpkg + osm_restaurantes_gs.gpkg
# ============================================================

library(sf)
library(dplyr)
library(osmdata)
library(units)

# ----------------------------
# 0) INSUMO PRINCIPAL (del Paso 1)
# ----------------------------
ruta_gpkg_demanda <- "demanda_zona_censal.gpkg"

zonas <- st_read(ruta_gpkg_demanda, quiet = TRUE)

# chequeos mínimos
stopifnot("geocodigo" %in% names(zonas))
if (!("pob_zona" %in% names(zonas))) {
  message("⚠️ No encuentro 'pob_zona' en zonas. Se calculará solo oferta_n (conteo), no por 1000 hab.")
}

# Asegurar CRS válido
if (is.na(st_crs(zonas))) stop("Tus zonas no tienen CRS. Revisa el gpkg del Paso 1.")

# ----------------------------
# 1) DEFINIR ÁREA DE BÚSQUEDA OSM (bbox de tus zonas)
#    (esto asegura que OSM se descargue solo dentro de tu recorte GS)
# ----------------------------
bbox <- st_bbox(zonas)
bbox_vec <- c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])  # xmin, ymin, xmax, ymax

# Overpass trabaja en WGS84; transformamos bbox a 4326 para la query
zonas_wgs <- st_transform(zonas, 4326)
bbox_wgs <- st_bbox(zonas_wgs)
bbox_wgs_vec <- c(bbox_wgs["xmin"], bbox_wgs["ymin"], bbox_wgs["xmax"], bbox_wgs["ymax"])

# ----------------------------
# 2) DESCARGAR OSM: restaurants / fast_food / cafe
# ----------------------------
# Nota: si Overpass se pone mañoso por tamaño, subimos timeout y/o partimos la bbox.
q <- opq(bbox = bbox_wgs_vec, timeout = 180)

# Queremos “servicio comida fuera del hogar”
amenities <- c("restaurant", "fast_food", "cafe")

osm <- q %>%
  add_osm_feature(key = "amenity", value = amenities) %>%
  osmdata_sf()

# ----------------------------
# 3) UNIFICAR GEOMETRÍAS: puntos + polígonos/lines (centroides)
# ----------------------------
pts <- osm$osm_points
pol <- osm$osm_polygons
mp  <- osm$osm_multipolygons
ln  <- osm$osm_lines

# Convertir todo a puntos representativos (para asignar a zona)
to_points <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(NULL)
  x <- st_make_valid(x)
  # usar punto sobre superficie: mejor que centroid para polígonos raros
  st_point_on_surface(x)
}

pts2 <- if (!is.null(pts) && nrow(pts) > 0) st_make_valid(pts) else NULL
pol2 <- to_points(pol)
mp2  <- to_points(mp)
ln2  <- to_points(ln)

osm_all <- bind_rows(
  if (!is.null(pts2)) pts2 else NULL,
  if (!is.null(pol2)) pol2 else NULL,
  if (!is.null(mp2))  mp2  else NULL,
  if (!is.null(ln2))  ln2  else NULL
)

if (is.null(osm_all) || nrow(osm_all) == 0) stop("OSM no devolvió locales. Puede ser caída de Overpass o bbox vacía.")

# Quedarnos con columnas útiles (si existen)
keep_cols <- intersect(names(osm_all), c("osm_id","name","amenity","brand","operator","addr:street","addr:housenumber"))
osm_all <- osm_all %>% select(all_of(keep_cols), geometry)

# Quitar duplicados por osm_id si viene repetido
if ("osm_id" %in% names(osm_all)) osm_all <- osm_all %>% distinct(osm_id, .keep_all = TRUE)

# Transformar a CRS de las zonas para el join
osm_all <- st_transform(osm_all, st_crs(zonas))

# Export opcional: capa de puntos OSM
st_write(osm_all, "osm_restaurantes_gs.gpkg", delete_dsn = TRUE, quiet = TRUE)

# ----------------------------
# 4) ASIGNAR CADA LOCAL A ZONA CENSAL (spatial join)
# ----------------------------
# Para que no explote por geometrías raras, nos aseguramos de validez:
zonas2 <- st_make_valid(zonas)

# Join: cada punto cae en una zona (geocodigo)
# left = puntos, join = zonas
osm_join <- st_join(osm_all, zonas2 %>% select(geocodigo), join = st_within, left = TRUE)

# Filtrar puntos que no cayeron en ninguna zona (fuera del recorte)
osm_join <- osm_join %>% filter(!is.na(geocodigo))

# ----------------------------
# 5) OFERTA POR ZONA: conteo + densidad por 1000 hab
# ----------------------------
oferta_zona <- osm_join %>%
  st_drop_geometry() %>%
  group_by(geocodigo) %>%
  summarise(
    oferta_n = n(),
    oferta_restaurant = sum(amenity == "restaurant", na.rm = TRUE),
    oferta_fastfood   = sum(amenity == "fast_food",  na.rm = TRUE),
    oferta_cafe       = sum(amenity == "cafe",       na.rm = TRUE),
    .groups = "drop"
  )

# Unir con zonas (y pob_zona si existe)
zonas_oferta <- zonas2 %>%
  left_join(oferta_zona, by = "geocodigo") %>%
  mutate(
    oferta_n = ifelse(is.na(oferta_n), 0L, oferta_n),
    oferta_restaurant = ifelse(is.na(oferta_restaurant), 0L, oferta_restaurant),
    oferta_fastfood   = ifelse(is.na(oferta_fastfood), 0L, oferta_fastfood),
    oferta_cafe       = ifelse(is.na(oferta_cafe), 0L, oferta_cafe)
  )

if ("pob_zona" %in% names(zonas_oferta)) {
  zonas_oferta <- zonas_oferta %>%
    mutate(
      oferta_x1000hab = ifelse(!is.na(pob_zona) & pob_zona > 0, (oferta_n / pob_zona) * 1000, NA_real_)
    )
}

# ----------------------------
# 6) EXPORTAR RESULTADOS
# ----------------------------
# Tabla sin geometría
tabla_oferta <- zonas_oferta %>%
  st_drop_geometry() %>%
  select(geocodigo, oferta_n, oferta_restaurant, oferta_fastfood, oferta_cafe,
         any_of(c("pob_zona","oferta_x1000hab")))

write.csv(tabla_oferta, "oferta_zona_censal.csv", row.names = FALSE)
saveRDS(tabla_oferta, "oferta_zona_censal.rds")

# Geo: zonas + oferta (para mapear)
st_write(zonas_oferta, "oferta_zona_censal.gpkg", delete_dsn = TRUE, quiet = TRUE)

message("LISTO ✅ Paso 2 (Oferta) generado: oferta_zona_censal.csv/.rds + oferta_zona_censal.gpkg + osm_restaurantes_gs.gpkg")
