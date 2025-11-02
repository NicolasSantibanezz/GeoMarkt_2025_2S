## =========================================================
## 1. LIBRERÍAS ####
## =========================================================
library(rakeR)       # Microsimulación espacial (raking)
library(RPostgres)   # Conexión con base de datos PostgreSQL
library(DBI)         # Interfaz para manejo de bases de datos
library(sf)          # Manejo de datos espaciales (objetos simple features)
library(dplyr)       # Manipulación de datos tabulares
library(data.table)  # Procesamiento eficiente de grandes volúmenes de datos
library(tmap)        # Cartografía temática
library(biscale)     # Construcción de mapas bivariados
library(ggplot2)     # Visualización gráfica
library(patchwork)   # Composición de múltiples gráficos
library(ggrepel)     # Etiquetado inteligente de textos en ggplot

## =========================================================
## 2. ENTRADAS ####
## =========================================================
ruta_casen = "Data/casen_rm.rds"
ruta_censo = "Data/cons_censo_df.rds"
ruta_indicadores = "Data/df_indicadores.rds"

casen_raw = readRDS(ruta_casen)
cons_censo_df = readRDS(ruta_censo)
df_indicadores = readRDS(ruta_indicadores) #Proveniente de Entrega 1

## =========================================================
## 3. PREPROCESAMIENTO ####
## =========================================================

### 3.1 CENSO
col_cons = sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))
age_levels = grep("^edad", col_cons, value = TRUE)
esc_levels = grep("^esco", col_cons, value = TRUE)
sexo_levels = grep("^sexo", col_cons, value = TRUE)

### 3.2 CASEN
vars_base = c("estrato", "esc", "edad", "sexo", "e6a", "o12")
casen = casen_raw[, vars_base, drop = FALSE]
rm(casen_raw)

# Extraer comuna
casen$Comuna = substr(as.character(casen$estrato), 1, 5)
casen$estrato = NULL

# Limpieza de tipos
casen$esc = as.integer(unclass(casen$esc))
casen$edad = as.integer(unclass(casen$edad))
casen$e6a = as.numeric(unclass(casen$e6a))
casen$sexo = as.integer(unclass(casen$sexo))
casen$o12 = as.numeric(unclass(casen$o12))

# Variable binaria: formalidad laboral
casen$o12_formal = ifelse(casen$o12 %in% c(1,2), 1,
                          ifelse(casen$o12 %in% c(3,4), 0, NA))

# Imputar escolaridad en base a e6a
idx_na = which(is.na(casen$esc))
fit = lm(esc ~ e6a, data = casen[-idx_na,])
pred = predict(fit, newdata = casen[idx_na, ,drop = FALSE])
casen$esc[idx_na] = as.integer(round(pmax(0, pmin(29, pred))))

# ID único
casen$ID = as.character(seq_len(nrow(casen)))

### 3.3 RE-CODIFICACIÓN
casen$edad_cat = cut(
  casen$edad,
  breaks = c(0,30,40,50,60,70,80,Inf),
  labels = age_levels,
  right = FALSE, include.lowest = TRUE
)

casen$esc_cat = factor(
  with(casen,
       ifelse(esc == 0, esc_levels[1],
              ifelse(esc <= 8, esc_levels[2],
                     ifelse(esc <= 12, esc_levels[3],
                            esc_levels[4])))),
  levels = esc_levels
)

casen$sexo_cat = factor(
  ifelse(casen$sexo == 2, sexo_levels[1],
         ifelse(casen$sexo == 1, sexo_levels[2], NA)),
  levels = sexo_levels
)

## =========================================================
## 4. MICROSIMULACIÓN ####
## =========================================================
#SEPARACIÓN DE BASES POR COMUNA
cons_censo_comunas = split(cons_censo_df, cons_censo_df$COMUNA)
inds_list = split(casen, casen$Comuna)

#Ejecución del modelo por comuna
sim_list = lapply(names(cons_censo_comunas), function(zona) {
  cons_i    = cons_censo_comunas[[zona]]
  col_order = sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i    = cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  tmp    = inds_list[[zona]]
  inds_i = tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
  names(inds_i) = c("ID","Edad","Escolaridad","Sexo")
  
  w_frac  = weight(cons = cons_i, inds = inds_i,
                   vars = c("Edad","Escolaridad","Sexo"))
  sim_i   = integerise(weights = w_frac, inds = inds_i, seed = 123)
  
  # Solo mantenemos variable de formalidad laboral
  merge(sim_i, tmp[, c("ID", "o12_formal")], by = "ID", all.x = TRUE)
})

# Unir todos los resultados en un solo data frame
sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")

# --- Agregación por zona censal: FORMALIDAD LABORAL ---
zonas_formal = aggregate(
  o12_formal ~ zone,
  data = sim_df,
  FUN = function(x) mean(x, na.rm = TRUE)
)
names(zonas_formal) <- c("geocodigo", "prop_trabajo_formal")

## =========================================================
## 5. CONEXIÓN Y UNIÓN FINAL ####
## =========================================================

db_host = "localhost"
db_port = 5432
db_name = "censo_rm_2017"
db_user = "postgres"
db_password = "lucho98"

con = dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)

# Guardar tablas intermedias
dbWriteTable(con, name = DBI::SQL("output.zonas_formal_tmp"), value = zonas_formal, row.names = FALSE)

# Leer zonas censales del Gran Santiago
query_gs = "
SELECT *
FROM dpa.zonas_censales_rm
WHERE urbano = 1 
AND nom_comuna IN (
  'SANTIAGO','INDEPENDENCIA','RECOLETA','CONCHALÍ','HUECHURABA',
  'VITACURA','LAS CONDES','LO BARNECHEA','PROVIDENCIA','ÑUÑOA',
  'MACUL','PEÑALOLÉN','LA REINA','LA FLORIDA','PUENTE ALTO',
  'LA GRANJA','SAN RAMÓN','LA PINTANA','EL BOSQUE','SAN BERNARDO',
  'CERRILLOS','CERRO NAVIA','LO PRADO','QUINTA NORMAL',
  'ESTACIÓN CENTRAL','PEDRO AGUIRRE CERDA','SAN MIGUEL','LO ESPEJO',
  'MAIPÚ','SAN JOAQUÍN','LA CISTERNA','PUDAHUEL','RENCA','QUILICURA',
  'LO BARNECHEA','PROVIDENCIA','VITACURA'
);
"
sql_comunas = "
SELECT nom_comuna, geom
FROM dpa.comunas_rm_shp;
"

sf_comunas = st_read(con, query = sql_comunas)
zonas_gs = st_read(con, query = query_gs)

#Unificación de geocodigo 
zonas_gs$geocodigo <- as.character(zonas_gs$geocodigo)
zonas_formal$geocodigo <- as.character(zonas_formal$geocodigo)
df_indicadores$geocodigo <- as.character(df_indicadores$geocodigo)

# Unión de capas
zonas_gs_final <- zonas_gs %>%
  left_join(zonas_formal, by = "geocodigo") %>%
  left_join(st_drop_geometry(df_indicadores), by = "geocodigo") %>%
  filter(!is.na(prop_trabajo_formal))


## =========================================================
## 6. MAPAS (tmap + ggplot2 bivariado)
## =========================================================
#Definición de ruta de salida para mapas en formato .png
ruta_salida <- "Trabajos/T2"
if(!dir.exists(ruta_salida)) dir.create(ruta_salida, recursive = TRUE)

# Filtrar solo las comunas del Gran Santiago
sf_comunas_gs <- sf_comunas %>%
  dplyr::filter(nom_comuna %in% zonas_gs$nom_comuna)
# --- Mapa 1: Empleo formal ---

# Crear centroides fuera del mapa
centroides_comunas <- sf_comunas_gs %>%
  sf::st_centroid() %>%
  cbind(sf::st_coordinates(.))

# --- Mapa 1: Empleo formal ---
map_formal <- tm_shape(zonas_gs_final) +
  tm_polygons(
    fill = "prop_trabajo_formal",
    fill.scale = tm_scale_intervals(style = "quantile", n = 5, values = "Blues"),
    fill.legend = tm_legend(title = "Proporción empleo formal")
  ) +
  
  # Límites comunales
  tm_shape(sf_comunas_gs) +
  tm_borders(col = "black", lwd = 2) +
  
  # Fondo blanco (rectángulo)
  tm_shape(centroides_comunas) +
  tm_symbols(
    size = 0.08,            # tamaño del rectángulo
    shape = 22,             # cuadrado relleno
    col = "white",          # color del fondo
    border.col = "grey30",  # contorno del fondo
    border.lwd = 0.3
  ) +
  
  # Texto encima del rectángulo
  tm_shape(centroides_comunas) +
  tm_text(
    text = "nom_comuna",
    size = 0.5,
    col = "black",
    just = "center",
    auto.placement = TRUE
  ) +
  
  tm_title("Empleo formal en el Gran Santiago (Microsimulación)") +
  tm_legend(outside = TRUE)

# Guardar el mapa
tmap_save(
  map_formal,
  file.path(ruta_salida, "mapa_formalidad_gran_santiago.png"),
  dpi = 300
)
print(map_formal)
# --- Mapa 2: Formalidad vs Hacinamiento ---

# --- Crear variable bivariada ---
biv_formal_hac <- bi_class(
  zonas_gs_final |> dplyr::filter(!is.na(prop_trabajo_formal), !is.na(ptje_hacinamiento)),
  x = prop_trabajo_formal,
  y = ptje_hacinamiento,
  style = "quantile",
  dim = 3
)
# --- 3. Crear mapa ---
map_bivar_formal_hac <- ggplot() +
  geom_sf(data = biv_formal_hac, aes(fill = bi_class), color = "white", size = 0.1) +
  bi_scale_fill(pal = "DkViolet", dim = 3) +
  geom_label_repel(
    data = centroides_comunas,
    aes(X, Y, label = nom_comuna),
    size = 2.3, color = "black",
    fill = "white", label.size = 0.2,
    label.padding = unit(0.12, "lines"),
    max.overlaps = Inf
  ) +
  bi_theme() +
  labs(
    title = "Formalidad laboral y Hacinamiento en el Gran Santiago",
    subtitle = "Microsimulación CASEN 2022 + CENSO 2017",
    caption = "Elaboración propia con datos CASEN (2022) y CENSO (2017)"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 10)),
    plot.caption = element_text(size = 9, color = "grey40", hjust = 1)
  )

# --- Leyenda bivariada ---
legend_bivar <- bi_legend(
  pal = "DkViolet",
  dim = 3,
  xlab = "→ Mayor formalidad laboral",
  ylab = "↑ Mayor hacinamiento",
  size = 6
)

# --- Combinar mapa y leyenda ---
library(cowplot)
final_bivar_formal_hac <- ggdraw() +
  draw_plot(map_bivar_formal_hac, 0, 0, 1, 1) +
  draw_plot(legend_bivar, 0.73, 0.07, 0.25, 0.25)

# ---Exportar ---
ggsave(
  filename = file.path(ruta_salida, "mapa_bivariado_formalidad_hacinamiento.png"),
  plot = final_bivar_formal_hac,
  width = 10, height = 8, dpi = 300
)
cat("✅ Mapa bivariado: Formalidad laboral vs Hacinamiento generado correctamente\n")
print(final_bivar_formal_hac)

# --- Mapa 3: Bivariado (Formalidad vs Migración) con ggplot2 ---

# Crear variable bivariada
biv_formal_extranj <- bi_class(
  zonas_gs_final |> dplyr::filter(!is.na(prop_trabajo_formal), !is.na(ptje_extranjeros)),
  x = prop_trabajo_formal,
  y = ptje_extranjeros,
  style = "quantile",
  dim = 3
)
# ---Crear mapa bivariado ---
map_bivar_formal_extranj <- ggplot() +
  geom_sf(data = biv_formal_extranj, aes(fill = bi_class), color = "white", size = 0.1) +
  bi_scale_fill(pal = "DkCyan", dim = 3) +  # paleta diferente para distinguirlo del mapa 2
  geom_label_repel(
    data = centroides_comunas,
    aes(X, Y, label = nom_comuna),
    size = 2.3,
    color = "black",
    fill = "white",
    label.size = 0.2,
    label.padding = unit(0.12, "lines"),
    max.overlaps = Inf
  ) +
  bi_theme() +
  labs(
    title = "Formalidad laboral y Población extranjera en el Gran Santiago",
    subtitle = "Microsimulación CASEN 2022 + CENSO 2017",
    caption = "Elaboración propia con datos CASEN (2022) y CENSO (2017)"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 10)),
    plot.caption = element_text(size = 9, color = "grey40", hjust = 1)
  )

# --- Leyenda bivariada ---
legend_bivar_extranj <- bi_legend(
  pal = "DkCyan",
  dim = 3,
  xlab = "→ Mayor formalidad laboral",
  ylab = "↑ Mayor población extranjera",
  size = 6
)

# --- Combinar mapa y leyenda ---
final_bivar_formal_extranj <- ggdraw() +
  draw_plot(map_bivar_formal_extranj, 0, 0, 1, 1) +
  draw_plot(legend_bivar_extranj, 0.73, 0.07, 0.25, 0.25)

# --- Exportar ---
ggsave(
  filename = file.path(ruta_salida, "mapa_bivariado_formalidad_extranjeros.png"),
  plot = final_bivar_formal_extranj,
  width = 10, height = 8, dpi = 300
)
cat("✅ Mapa 3: 'mapa_bivariado_formalidad_extranjeros.png' generado correctamente\n")
print(final_bivar_formal_extranj)


