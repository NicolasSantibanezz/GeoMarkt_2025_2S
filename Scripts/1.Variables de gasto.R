#---Librerias
library(haven)
library(pROC)
library(mgcv)
library(ggplot2)
library(dplyr)


#---Carga de Archivos
personas <- read_dta("Data/EPF/base-personas-ix-epf-stata.dta")
gastos      <- read_dta("Data/EPF/base-gastos-ix-epf-stata.dta") 
cantidades  <- read_dta("Data/EPF/base-cantidades-ix-epf-stata.dta") 
ccif        <- read_dta("Data/EPF/ccif-ix-epf-stata.dta")

# ============================================================
# 1. Filtrar gastos en comidas fuera del hogar
#    CCIF:
#    - 11.1.1.01.xx (servicio completo)
#    - 11.1.1.02.xx (servicio limitado, comida rápida, ambulante, delivery)
# ============================================================

gastos_comida_fuera <- gastos %>%
  filter(
    grepl("^11\\.1\\.1\\.01", ccif) |
      grepl("^11\\.1\\.1\\.02", ccif)
  )

# ============================================================
# 2. Agregar gasto total por hogar
# ============================================================

gasto_hogar <- gastos_comida_fuera %>%
  group_by(folio) %>%
  summarise(
    gasto_comida_fuera = sum(gasto, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 3. Crear variable dummy de gasto (modelo de dos partes)
# ============================================================

gasto_hogar <- gasto_hogar %>%
  mutate(
    gasta_comida_fuera = ifelse(gasto_comida_fuera > 0, 1, 0)
  )

# ============================================================
# 4. Unir con base de personas
#    (base final para modelación)
# ============================================================

epf_modelo <- personas %>%
  left_join(gasto_hogar, by = "folio") %>%
  mutate(
    gasto_comida_fuera = ifelse(is.na(gasto_comida_fuera), 0, gasto_comida_fuera),
    gasta_comida_fuera = ifelse(is.na(gasta_comida_fuera), 0, gasta_comida_fuera)
  )

# ============================================================
# 5. Estadísticos descriptivos básicos 
# ============================================================

# Proporción de hogares que gastan
prop_hogares_gastan <- mean(epf_modelo$gasta_comida_fuera)

# Gasto promedio entre hogares que gastan
promedio_gasto <- mean(
  epf_modelo$gasto_comida_fuera[epf_modelo$gasto_comida_fuera > 0]
)

prop_hogares_gastan
promedio_gasto

# ============================================================
# 6. Gráfico: distribución del gasto (solo positivos)
# ============================================================

ggplot(
  epf_modelo %>% filter(gasto_comida_fuera > 0),
  aes(x = gasto_comida_fuera)
) +
  geom_histogram(bins = 40) +
  scale_x_log10() +
  labs(
    title = "Distribución del gasto en comidas fuera del hogar",
    x = "Gasto mensual (escala log)",
    y = "Frecuencia"
  )