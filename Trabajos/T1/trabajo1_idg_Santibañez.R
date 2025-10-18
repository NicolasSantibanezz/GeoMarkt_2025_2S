#####################
## 1) Librerías #####
#####################

library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(biscale)


############################
## 2) Configuración BD #####
############################

db_host = "localhost"
db_port = 5432
db_name = "censo_rm_2017"
db_user = "postgres"
db_password = "lucho98"

# Conexión
con = dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)
########################
## 3) Consulta SQL #####
########################

sql_indicadores = "

WITH agg AS (

    SELECT 
        z.geocodigo::double precision AS geocodigo,
        c.nom_comuna,

        -- % población extranjera
        ROUND(
            COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99)) * 100.0 / COUNT(*), 
            2
        ) AS ptje_extranjeros,

        -- % viviendas con hacinamiento (categorías 2 o 3)
        ROUND(
            COUNT(*) FILTER (
              WHERE v.ind_hacin_rec >= 2 AND v.ind_hacin_rec NOT IN (98, 99)
              ) * 100.0 /
              COUNT(*) FILTER (WHERE v.ind_hacin_rec NOT IN (98, 99)),
              2
         ) AS ptje_hacinamiento

    FROM public.personas   AS p
    JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
    JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
    JOIN public.zonas      AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
    JOIN public.comunas    AS c ON z.codigo_comuna   = c.codigo_comuna
    JOIN public.provincias AS pr ON pr.provincia_ref_id = c.provincia_ref_id

    GROUP BY z.geocodigo, c.nom_comuna
)

SELECT 
    a.geocodigo,
    shp.geom,
    a.nom_comuna,
    a.ptje_extranjeros,
    a.ptje_hacinamiento
FROM agg AS a
JOIN dpa.zonas_censales_rm AS shp ON shp.geocodigo = a.geocodigo;

"

# Almacenar resultado en R como objeto sf
df_indicadores = st_read(con, query = sql_indicadores)


########################
### 4) Marco comunal ###
########################

sql_comunas = "
SELECT nom_comuna, geom
FROM dpa.comunas_rm_shp;
"

sf_comunas = st_read(con, query = sql_comunas)


##########################################
## 5) Mapas individuales (ajustados) #####
##########################################

# 1️⃣ Mapa: % población extranjera
mapa_extranjeros <- ggplot() +
  geom_sf(data = df_indicadores, aes(fill = ptje_extranjeros), color = NA) +
  geom_sf(data = sf_comunas, fill = NA, color = "grey40", linewidth = 0.3) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    name = "% población extranjera"
  ) +
  labs(title = "Distribución de la población extranjera - RM (2017)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    legend.position = "right",
    axis.text = element_blank(),       # 🔹 Quita texto de coordenadas
    axis.ticks = element_blank(),      # 🔹 Quita ticks
    panel.grid = element_line(color = "grey90", linewidth = 0.3) # 🔹 Mantiene grilla tenue
  )

# 2️⃣ Mapa: % viviendas con hacinamiento
mapa_hacinamiento <- ggplot() +
  geom_sf(data = df_indicadores, aes(fill = ptje_hacinamiento), color = NA) +
  geom_sf(data = sf_comunas, fill = NA, color = "grey40", linewidth = 0.3) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "% viviendas con hacinamiento"
  ) +
  labs(title = "Distribución del hacinamiento - RM (2017)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(color = "grey90", linewidth = 0.3)
  )

# 🔹 Ordenar uno arriba y otro abajo
cowplot::plot_grid(
  mapa_extranjeros,
  mapa_hacinamiento,
  ncol = 1,          # vertical
  align = "v",
  rel_heights = c(1, 1.05) # pequeño ajuste por leyenda
)

#########################################
## 6) Mapa Bivariado: extranjeros x hacinamiento
#########################################

# Crear la variable bivariada (clasificación 3x3)
bivar_data <- bi_class(
  df_indicadores,
  x = ptje_extranjeros,
  y = ptje_hacinamiento,
  style = "quantile",  # divide ambas variables en 3 cuantiles
  dim = 3              # genera 3x3 = 9 combinaciones de color
)

# Crear mapa base
mapa_bivariado <- ggplot() +
  geom_sf(data = bivar_data, aes(fill = bi_class), color = NA) +
  geom_sf(data = sf_comunas, fill = NA, color = "grey30", linewidth = 0.3) +
  bi_scale_fill(pal = "DkBlue", dim = 3) + # paleta recomendada
  bi_theme() +
  labs(
    title = "Mapa bivariado: Porcentaje población extranjera vs. Porcentaje viviendas con hacinamiento (RM, 2017)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5)
  )

# Crear leyenda bivariada
leyenda_bivariada <- bi_legend(
  pal = "DkBlue",
  dim = 3,
  xlab = "% pob. extranjera ↑",
  ylab = "% hacinamiento ↑",
  size = 8
)

# Combinar mapa + leyenda (en la esquina inferior derecha)
cowplot::ggdraw() +
  draw_plot(mapa_bivariado, 0, 0, 1, 1) +
  draw_plot(leyenda_bivariada, 0.67, 0.05, 0.3, 0.3)

#Guardar Mapa Bivariado como imagen PNG en el repositorio
# Crear la ruta del archivo
ruta_archivo <- "Trabajos/T1/mapa_bivariado_extranjeros_hacinamiento.png"

# Guardar el gráfico
ggsave(ruta_archivo, width = 8, height = 6, dpi = 300)