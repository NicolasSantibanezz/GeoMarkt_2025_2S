# ============================================================
# TAREA 2 IDG – Modelo de dos partes (EPF IX)
# ============================================================

library(dplyr)
library(pROC)

# ============================================================
# 1. Quedarse con una observación por hogar
#    (persona sustentadora principal)
# ============================================================

epf_hogar <- epf_modelo %>%
  filter(sprincipal == 1)

# ============================================================
# 2. Preparar variables explicativas
# ============================================================

epf_hogar <- epf_hogar %>%
  mutate(
    # Variables sociodemográficas
    edad          = as.numeric(edad),
    sexo          = factor(sexo),
    educacion     = factor(edunivel),
    cse           = factor(cse),
    n_personas    = as.numeric(npersonas),
    macrozona     = factor(macrozona),
    
    
    # Variable dependiente continua en log
    log_gasto     = log1p(gasto_comida_fuera)
  )

# ============================================================
# 3. PARTE 1: Modelo Logit (probabilidad de gastar)
# ============================================================

modelo_logit <- glm(
  gasta_comida_fuera ~ cse + edad + sexo + educacion + n_personas + macrozona,
  data   = epf_hogar,
  family = binomial()
)

summary(modelo_logit)

# Métrica simple para PPT
prob_hat <- predict(modelo_logit, type = "response")
auc_logit <- as.numeric(
  pROC::auc(epf_hogar$gasta_comida_fuera, prob_hat)
)
auc_logit

# ============================================================
# 4. PARTE 2: Modelo de monto (solo hogares que gastan)
# ============================================================

epf_pos <- epf_hogar %>%
  filter(gasto_comida_fuera > 0)

modelo_monto <- lm(
  log_gasto ~ cse + edad + sexo + educacion + n_personas + macrozona,
  data = epf_pos
)

summary(modelo_monto)

# ============================================================
# 5. Gasto esperado (solo para chequeo interno)
# ============================================================

log_gasto_hat <- predict(modelo_monto, newdata = epf_hogar)
gasto_cond_hat <- expm1(log_gasto_hat)

gasto_esperado <- prob_hat * gasto_cond_hat

summary(gasto_esperado)
