library(dplyr)
library(ggplot2)

# ============================================================
# 6. IMPUTACIÓN EN CASEN 2022 (gasto esperado)
# ============================================================

# ============================================================
# 1) Cargar CASEN RM
# ============================================================
casen_rm <- readRDS("Data/casen_rm.rds")

# ============================================================
# 2) Construir CASEN a nivel hogar (casen_hogar)
#    - ID hogar: folio
#    - jefe/a: pco1 == 1
#    - tamaño hogar: n() por folio
# ============================================================

# Conteo personas por hogar
casen_n <- casen_rm %>%
  count(folio, name = "n_personas")

# Jefe de hogar
casen_jefe <- casen_rm %>%
  filter(pco1 == 1)

casen_hogar <- casen_jefe %>%
  left_join(casen_n, by = "folio") %>%
  transmute(
    folio      = folio,
    edad       = as.numeric(edad),
    sexo       = sexo,
    educacion  = educ,   # 👈 educación
    cse        = nse,    # 👈 clase socioeconómica
    n_personas = as.numeric(n_personas),
    macrozona  = region
  )

# ============================================================
# 3) Armonizar tipos con EPF para poder predecir (factores)
# ============================================================
casen_hogar <- casen_hogar %>%
  mutate(
    sexo       = factor(sexo, levels = levels(epf_hogar$sexo)),
    educacion = factor(educacion, levels = levels(epf_hogar$educacion)),
    cse        = factor(cse, levels = levels(epf_hogar$cse)),
    macrozona  = factor(macrozona, levels = levels(epf_hogar$macrozona))
  )


# (Chequeo rápido de NA por niveles no compatibles)
na_check <- sapply(casen_hogar, function(x) sum(is.na(x)))
print(na_check)

# ============================================================
# IMPUTACIÓN ROBUSTA DEL GASTO ESPERADO
# ============================================================

# 1) Probabilidad de gastar (Logit)
prob_casen <- predict(modelo_logit, newdata = casen_hogar, type = "response")

summary(prob_casen)  # chequeo

# 2) Predicción del monto (solo estructura compatible)
log_hat_casen <- predict(modelo_monto, newdata = casen_hogar)

# 3) Reemplazar NA por la media del log_gasto observado en EPF
media_log <- mean(epf_pos$log_gasto, na.rm = TRUE)

log_hat_casen[is.na(log_hat_casen)] <- media_log

summary(log_hat_casen)  # chequeo

# 4) Retransformar a pesos
smear <- mean(exp(residuals(modelo_monto)), na.rm = TRUE)

gasto_cond_casen <- (exp(log_hat_casen) * smear) - 1
gasto_cond_casen <- pmax(gasto_cond_casen, 0)

# 5) Gasto esperado
gasto_esp_casen <- prob_casen * gasto_cond_casen

summary(gasto_esp_casen)  # chequeo FINAL

# 6) Guardar
casen_hogar <- casen_hogar %>%
  mutate(
    prob_gastar_comida_fuera = prob_casen,
    gasto_cond_comida_fuera  = gasto_cond_casen,
    gasto_esp_comida_fuera   = gasto_esp_casen
  )


# ============================================================
# 5) Estadísticas básicas del gasto imputado. Análisis exploratorio
# ============================================================
stats_imp <- casen_hogar %>%
  summarise(
    n_hogares = n(),
    promedio  = mean(gasto_esp_comida_fuera, na.rm = TRUE),
    mediana   = median(gasto_esp_comida_fuera, na.rm = TRUE),
    p25       = quantile(gasto_esp_comida_fuera, 0.25, na.rm = TRUE),
    p75       = quantile(gasto_esp_comida_fuera, 0.75, na.rm = TRUE),
    p90       = quantile(gasto_esp_comida_fuera, 0.90, na.rm = TRUE),
    p95       = quantile(gasto_esp_comida_fuera, 0.95, na.rm = TRUE),
    maximo    = max(gasto_esp_comida_fuera, na.rm = TRUE),
    prop_casi_cero = mean(gasto_esp_comida_fuera < 1, na.rm = TRUE)
  )

print(stats_imp)

# ============================================================
#  Exportar resultados para seguir con trabajo final
# ============================================================
saveRDS(casen_hogar, "casen_rm_con_gasto_imputado.rds")
write.csv(casen_hogar, "casen_rm_con_gasto_imputado.csv", row.names = FALSE)
