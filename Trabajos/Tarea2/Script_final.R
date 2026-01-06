# ============================================================
# TAREA 2 IDG — Comidas fuera del hogar
# Enfoque estilo profe: EPF (Gran Santiago) -> Modelos -> Imputación CASEN RM
# ============================================================

# --- LIBRERÍAS ---
library(haven)
library(pROC)
library(ggplot2)
library(dplyr)

# --- CARGA EPF ---
personas   <- read_dta("Data/EPF/base-personas-ix-epf-stata.dta")
gastos     <- read_dta("Data/EPF/base-gastos-ix-epf-stata.dta")
cantidades <- read_dta("Data/EPF/base-cantidades-ix-epf-stata.dta")
ccif_dic   <- read_dta("Data/EPF/ccif-ix-epf-stata.dta") # (no imprescindible para el modelo)

# ============================================================
# 0) Helpers para elegir columnas sin adivinar
# ============================================================
pick_first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

# ============================================================
# 1) FILTRO EPF: Gran Santiago + limpieza (como profe)
# ============================================================
valores_invalidos <- c(-99, -88, -77)

# OJO: en tu EPF, educación aparece como edunivel o edue (depende versión)
col_edue <- pick_first_existing(personas, c("edue", "edunivel", "educ", "educacion"))
stopifnot(!is.na(col_edue))

# ingreso disponible hogar (profe usa ing_disp_hog_hd_ai)
col_inghog <- pick_first_existing(personas, c("ing_disp_hog_hd_ai", "ing_disp_hog", "ing_hog_disp", "ingreso_hogar"))
stopifnot(!is.na(col_inghog))

# npersonas
col_npers <- pick_first_existing(personas, c("npersonas", "n_personas", "cant_personas"))
stopifnot(!is.na(col_npers))

# sexo
col_sexo <- pick_first_existing(personas, c("sexo", "sexo_b", "sexo_a"))
stopifnot(!is.na(col_sexo))

# macrozona (EPF)
col_macro <- pick_first_existing(personas, c("macrozona"))
stopifnot(!is.na(col_macro))

# edad
col_edad <- pick_first_existing(personas, c("edad"))
stopifnot(!is.na(col_edad))

# filtro GS = macrozona 2
personas_gs <- personas %>%
  filter(
    .data[[col_macro]] == 2,
    !(.data[[col_edad]] %in% valores_invalidos),
    !(.data[[col_edue]] %in% valores_invalidos),
    .data[[col_inghog]] >= 0,
    .data[[col_npers]] > 0
  ) %>%
  mutate(
    ing_pc = .data[[col_inghog]] / .data[[col_npers]],
    id_persona = paste(folio, n_linea, sep = "_")
  )

# crear id_persona también en cantidades
cantidades <- cantidades %>%
  mutate(id_persona = paste(folio, n_linea, sep = "_"))

# ============================================================
# 2) GASTO EN COMIDAS FUERA DEL HOGAR (por persona, estilo profe)
#    CCIF:
#      11.1.1.01.xx  (servicio completo)
#      11.1.1.02.xx  (servicio limitado, rápida, ambulante, delivery, etc.)
# ============================================================
# En cantidades normalmente ccif es string y existe "gasto"
stopifnot("ccif" %in% names(cantidades))

col_gasto_cant <- pick_first_existing(cantidades, c("gasto", "gasto_total", "monto"))
stopifnot(!is.na(col_gasto_cant))

cantidades_cfh <- cantidades %>%
  filter(
    grepl("^11\\.1\\.1\\.01", ccif) | grepl("^11\\.1\\.1\\.02", ccif),
    macrozona == 2
  )

# sumar gasto por persona
gasto_cfh_por_persona <- cantidades_cfh %>%
  group_by(id_persona) %>%
  summarise(gasto_comida_fuera = sum(.data[[col_gasto_cant]], na.rm = TRUE), .groups = "drop")

# merge con personas_gs
personas_gs <- personas_gs %>%
  left_join(gasto_cfh_por_persona, by = "id_persona") %>%
  mutate(
    gasto_comida_fuera = ifelse(is.na(gasto_comida_fuera), 0, gasto_comida_fuera),
    incurre_gasto      = ifelse(gasto_comida_fuera > 0, 1, 0)
  )

# ============================================================
# 3) VARIABLES DERIVADAS (idéntico estilo profe)
# ============================================================

# grupo escolaridad desde variable educacional EPF (edue/edunivel/etc.)
personas_gs <- personas_gs %>%
  mutate(
    grupo_escolaridad = cut(
      .data[[col_edue]],
      breaks = c(-Inf, 12, 14, 16, Inf),
      labels = c("Escolar", "Tecnico", "Universitaria", "Postgrado"),
      right = TRUE
    ),
    rango_edad = cut(
      .data[[col_edad]],
      breaks = c(0, 29, 44, 64, Inf),
      labels = c("jovenes", "adultos_jovenes", "adultos", "adultos_mayores")
    ),
    sexo_fac = factor(.data[[col_sexo]], labels = c("Hombre", "Mujer")),
    log_ing_pc = log(pmax(ing_pc, 1)) # evita log(0)
  )

# base monto (solo quienes gastan)
tabla_gasto <- personas_gs %>%
  filter(gasto_comida_fuera > 0) %>%
  transmute(
    sexo_fac,
    edad = .data[[col_edad]],
    ing_pc,
    gasto_comida_fuera,
    grupo_escolaridad,
    rango_edad,
    log_ing_pc,
    log_gasto = log(gasto_comida_fuera + 1)
  )

# filtro outliers (p1–p99) como profe
q_ing   <- quantile(tabla_gasto$ing_pc, probs = c(0.01, 0.99), na.rm = TRUE)
q_gasto <- quantile(tabla_gasto$gasto_comida_fuera, probs = c(0.01, 0.99), na.rm = TRUE)

tabla_gasto <- tabla_gasto %>%
  filter(
    ing_pc >= q_ing[1] & ing_pc <= q_ing[2],
    gasto_comida_fuera >= q_gasto[1] & gasto_comida_fuera <= q_gasto[2]
  )

# ============================================================
# 4) MODELO LINEAL (monto) — en log(gasto+1)
# ============================================================
modelo_lineal <- lm(
  log_gasto ~ grupo_escolaridad + ing_pc + rango_edad + sexo_fac,
  data = tabla_gasto
)
summary(modelo_lineal)

# ============================================================
# 5) MODELO LOGIT (incurre gasto) + métricas + umbral óptimo
# ============================================================
modelo_data <- personas_gs %>%
  filter(!is.na(.data[[col_edad]]), !is.na(grupo_escolaridad), !is.na(sexo_fac), !is.na(ing_pc)) %>%
  transmute(
    incurre_gasto,
    sexo_fac,
    edad = .data[[col_edad]],
    grupo_escolaridad,
    ing_pc
  )

modelo_logit <- glm(
  incurre_gasto ~ sexo_fac + edad + grupo_escolaridad + ing_pc,
  data = modelo_data,
  family = binomial()
)

# prob predicha
modelo_data$prob_predicha <- predict(modelo_logit, type = "response")

# ROC + AUC
roc_obj <- roc(modelo_data$incurre_gasto, modelo_data$prob_predicha)
auc_val <- as.numeric(auc(roc_obj))
print(auc_val)

# Umbral óptimo (Youden)
coords_opt <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
umbral_optimo <- as.numeric(coords_opt["threshold"])
print(coords_opt)

# Matriz confusión con umbral óptimo
modelo_data$clasificacion <- ifelse(modelo_data$prob_predicha >= umbral_optimo, 1, 0)
conf_opt <- table(Real = modelo_data$incurre_gasto, Predicha = modelo_data$clasificacion)
print(conf_opt)

# ============================================================
# 6) CASEN RM — IMPUTACIÓN (misma ingeniería de variables)
# ============================================================

casen <- readRDS("Data/casen_rm.rds")

# Detectar columnas CASEN (porque varía según base)
col_edad_casen <- pick_first_existing(casen, c("edad"))
col_sexo_casen <- pick_first_existing(casen, c("sexo", "sexo_b", "sexo_a"))
col_esc_casen  <- pick_first_existing(casen, c("esc", "escolaridad"))
col_ingpc_casen <- pick_first_existing(casen, c("ypc", "ing_pc", "ingreso_pc"))

# Si no existe ypc, intenta construirlo desde ingreso hogar / n_personas
if (is.na(col_ingpc_casen)) {
  col_inghog_casen <- pick_first_existing(casen, c("ing_disp_hog_hd_ai", "ing_hog", "y_hog"))
  col_npers_casen  <- pick_first_existing(casen, c("npersonas", "n_personas"))
  stopifnot(!is.na(col_inghog_casen), !is.na(col_npers_casen))
  casen$ing_pc <- casen[[col_inghog_casen]] / casen[[col_npers_casen]]
  col_ingpc_casen <- "ing_pc"
}

stopifnot(!is.na(col_edad_casen), !is.na(col_sexo_casen), !is.na(col_esc_casen), !is.na(col_ingpc_casen))

# Crear variables derivadas (igual profe)
casen <- casen %>%
  mutate(
    grupo_escolaridad = cut(
      .data[[col_esc_casen]],
      breaks = c(-Inf, 12, 14, 16, Inf),
      labels = c("Escolar", "Tecnico", "Universitaria", "Postgrado"),
      right = TRUE
    ),
    rango_edad = cut(
      .data[[col_edad_casen]],
      breaks = c(0, 29, 44, 64, Inf),
      labels = c("jovenes", "adultos_jovenes", "adultos", "adultos_mayores")
    ),
    sexo_fac = factor(as.character(.data[[col_sexo_casen]]), levels = c("1","2"), labels = c("Hombre","Mujer")),
    ing_pc = as.numeric(.data[[col_ingpc_casen]])
  ) %>%
  filter(!is.na(ing_pc), ing_pc >= 0, !is.na(grupo_escolaridad), !is.na(rango_edad), !is.na(sexo_fac), !is.na(.data[[col_edad_casen]]))

# --- Parte 1: predecir prob y clasificar con umbral óptimo
casen$prob_predicha <- predict(modelo_logit, newdata = casen, type = "response")
casen$clasificacion <- ifelse(casen$prob_predicha >= umbral_optimo, 1, 0)

# --- Parte 2: predecir monto SOLO para quienes clasifica como gasto=1
casen_pred <- casen %>% filter(clasificacion == 1)

casen_pred$log_gasto_estimado <- predict(modelo_lineal, newdata = casen_pred)
casen_pred$gasto_estimado <- exp(casen_pred$log_gasto_estimado) - 1

# Winzorizar outliers (como profe)
casen_pred$gasto_estimado_wins <- pmin(casen_pred$gasto_estimado,
                                       quantile(casen_pred$gasto_estimado, 0.999, na.rm = TRUE))

# ============================================================
# 7) ESTADÍSTICAS BÁSICAS (pauta)
# ============================================================

# stats del gasto observado EPF (solo gastadores, tabla_gasto)
stats_epf <- tabla_gasto %>%
  summarise(
    n = n(),
    mean = mean(gasto_comida_fuera, na.rm = TRUE),
    median = median(gasto_comida_fuera, na.rm = TRUE),
    p25 = quantile(gasto_comida_fuera, 0.25, na.rm = TRUE),
    p75 = quantile(gasto_comida_fuera, 0.75, na.rm = TRUE),
    p90 = quantile(gasto_comida_fuera, 0.90, na.rm = TRUE),
    p95 = quantile(gasto_comida_fuera, 0.95, na.rm = TRUE),
    max = max(gasto_comida_fuera, na.rm = TRUE)
  )

# stats gasto imputado CASEN (solo clasificados como gastadores)
stats_casen <- casen_pred %>%
  summarise(
    n = n(),
    mean = mean(gasto_estimado_wins, na.rm = TRUE),
    median = median(gasto_estimado_wins, na.rm = TRUE),
    p25 = quantile(gasto_estimado_wins, 0.25, na.rm = TRUE),
    p75 = quantile(gasto_estimado_wins, 0.75, na.rm = TRUE),
    p90 = quantile(gasto_estimado_wins, 0.90, na.rm = TRUE),
    p95 = quantile(gasto_estimado_wins, 0.95, na.rm = TRUE),
    max = max(gasto_estimado_wins, na.rm = TRUE)
  )

print(stats_epf)
print(stats_casen)

# ============================================================
# 8) (Opcional) chequeo de densidades EPF vs CASEN imputado (como profe)
# ============================================================
plot(density(tabla_gasto$gasto_comida_fuera), col = "blue", lwd = 2,
     main = "Densidad: EPF observado vs CASEN imputado",
     xlab = "Gasto comidas fuera del hogar")
lines(density(casen_pred$gasto_estimado_wins), col = "red", lwd = 2)
legend("topright", legend = c("EPF observado", "CASEN imputado"),
       col = c("blue", "red"), lwd = 2)

# ============================================================
# 9) Exportar resultados
# ============================================================
saveRDS(casen_pred, "casen_rm_comida_fuera_imputado_solo_gastadores.rds")
write.csv(casen_pred, "casen_rm_comida_fuera_imputado_solo_gastadores.csv", row.names = FALSE)

saveRDS(casen, "casen_rm_comida_fuera_prob_y_clase.rds")  # incluye prob y clasificación
write.csv(casen, "casen_rm_comida_fuera_prob_y_clase.csv", row.names = FALSE)
