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
db_port = 5433
db_name = "censo_v_2017"
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

WITH agg AS 
(

SELECT 

z.geocodigo::double precision AS geocodigo,
c.nom_comuna,

-- % migrantes

ROUND(
    COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99)) * 100.0 / COUNT(*), 2
    ) AS ptje_migrantes,
    
-- % escolaridad

ROUND(
    COUNT(*) FILTER (WHERE p.escolaridad >= 16) * 100.0 / COUNT(*), 2
    ) AS ptje_esc_mayor_16

FROM public.personas   AS p
JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas      AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
JOIN public.comunas    AS c ON z.codigo_comuna   = c.codigo_comuna
JOIN public.provincias AS pr ON pr.provincia_ref_id = c.provincia_ref_id
WHERE pr.nom_provincia = 'SAN ANTONIO'
GROUP BY z.geocodigo, c.nom_comuna

)

SELECT a.geocodigo,
    shp.geom,
    a.nom_comuna,
    a.ptje_migrantes,
    a.ptje_esc_mayor_16
FROM agg AS a
JOIN dpa.zonas_censales_v AS shp ON shp.geocodigo = a.geocodigo;



"

# Almacenar DF

df_indicadores = st_read(con, query = sql_indicadores)


########################
### 4) Marco comunal ###
########################

sql_comunas = "

SELECT nom_comuna, geom
FROM dpa.comunas_v
WHERE nom_provin = 'SAN ANTONIO';

"

sf_comunas = st_read(con, query = sql_comunas)




#######################
## 5) Pequeño EDA #####
#######################

ggplot(df_indicadores, aes(x = ptje_migrantes)) +
  geom_histogram(bins = 30, fill = '#226e6e', color = 'white') +
  labs(title = "Distribución de % Migrantes",
       x = "% Migrantes",
       y = "Frecuencia")

