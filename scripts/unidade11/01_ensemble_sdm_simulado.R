# ============================================================
# Unidade 11 - Modelagem Ensemble em SDM
# Script 01: Ensemble com GLM, GAM, RF, BRT e Maxent
# ============================================================

setwd("C:/Users/rblfl/OneDrive/Documentos/Playground/Species-Distribution-Modeling")

pacotes <- c(
  "dplyr",
  "readr",
  "ggplot2",
  "terra",
  "mgcv",
  "ranger",
  "gbm",
  "maxnet",
  "pROC"
)

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("dados/unidade11/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade11", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade11", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade11", recursive = TRUE, showWarnings = FALSE)

set.seed(123)

# ------------------------------------------------------------
# 1. Criar grade espacial simulada
# ------------------------------------------------------------

r_base <- terra::rast(
  ncols = 120,
  nrows = 100,
  xmin = -80,
  xmax = -40,
  ymin = -30,
  ymax = 10,
  crs = "EPSG:4326"
)

xy <- as.data.frame(terra::xyFromCell(r_base, 1:terra::ncell(r_base)))
names(xy) <- c("lon", "lat")

# ------------------------------------------------------------
# 2. Simular variáveis ambientais
# ------------------------------------------------------------

amb <- xy %>%
  mutate(
    temperatura = 25 + 0.20 * lat + sin(lon / 6) * 2 + rnorm(n(), 0, 1.2),
    precipitacao = 1800 - 10 * abs(lon + 60) + cos(lat / 4) * 200 + rnorm(n(), 0, 100),
    elevacao = 600 + 250 * sin((lon + 70) / 5) + 120 * cos(lat / 6),
    solo = 5.5 + 0.3 * sin(lon / 5) + rnorm(n(), 0, 0.2)
  )

amb <- amb %>%
  mutate(
    eta =
      -5 +
      2.8 * exp(-((temperatura - 25)^2) / (2 * 3^2)) +
      2.4 * exp(-((precipitacao - 1700)^2) / (2 * 350^2)) -
      0.0012 * elevacao +
      0.7 * exp(-((solo - 5.6)^2) / (2 * 0.35^2)),
    adequabilidade_real = plogis(eta),
    peso_presenca = adequabilidade_real / max(adequabilidade_real)
  )

# ------------------------------------------------------------
# 3. Gerar presenças e background
# ------------------------------------------------------------

presencas <- amb %>%
  sample_frac(size = 1, weight = peso_presenca) %>%
  slice(1:350) %>%
  mutate(pa = 1)

background <- amb %>%
  slice_sample(n = 3500) %>%
  mutate(pa = 0)

dados_modelo <- bind_rows(presencas, background) %>%
  select(pa, lon, lat, temperatura, precipitacao, elevacao, solo)

write_csv(
  dados_modelo,
  "dados/unidade11/processados/dados_ensemble_presenca_background.csv"
)

preditores <- c("temperatura", "precipitacao", "elevacao", "solo")

# ------------------------------------------------------------
# 4. Ajustar modelos individuais
# ------------------------------------------------------------

modelo_glm <- glm(
  pa ~ temperatura + I(temperatura^2) +
    precipitacao + I(precipitacao^2) +
    elevacao + solo,
  data = dados_modelo,
  family = binomial(link = "logit")
)

modelo_gam <- mgcv::gam(
  pa ~
    s(temperatura, k = 6) +
    s(precipitacao, k = 6) +
    s(elevacao, k = 5) +
    s(solo, k = 5),
  data = dados_modelo,
  family = binomial(link = "logit"),
  method = "REML"
)

dados_rf <- dados_modelo %>%
  mutate(pa_factor = factor(
    ifelse(pa == 1, "presenca", "background"),
    levels = c("background", "presenca")
  ))

modelo_rf <- ranger::ranger(
  pa_factor ~ temperatura + precipitacao + elevacao + solo,
  data = dados_rf,
  probability = TRUE,
  num.trees = 800,
  mtry = 2,
  min.node.size = 10,
  importance = "permutation",
  seed = 123
)

modelo_brt <- gbm::gbm(
  formula = pa ~ temperatura + precipitacao + elevacao + solo,
  data = dados_modelo,
  distribution = "bernoulli",
  n.trees = 2500,
  interaction.depth = 3,
  shrinkage = 0.01,
  bag.fraction = 0.6,
  cv.folds = 5,
  n.minobsinnode = 10,
  verbose = FALSE
)

melhor_brt <- gbm::gbm.perf(
  modelo_brt,
  method = "cv",
  plot.it = FALSE
)

x_max <- dados_modelo %>% select(all_of(preditores))
p_max <- dados_modelo$pa

formula_max <- maxnet::maxnet.formula(
  p = p_max,
  data = x_max,
  classes = "lqph"
)

modelo_max <- maxnet::maxnet(
  p = p_max,
  data = x_max,
  f = formula_max,
  regmult = 1
)

# ------------------------------------------------------------
# 5. Predições espaciais
# ------------------------------------------------------------

pred_glm <- predict(
  modelo_glm,
  newdata = amb,
  type = "response"
)

pred_gam <- predict(
  modelo_gam,
  newdata = amb,
  type = "response"
)

pred_rf <- predict(
  modelo_rf,
  data = amb
)$predictions[, "presenca"]

pred_brt <- predict(
  modelo_brt,
  newdata = amb,
  n.trees = melhor_brt,
  type = "response"
)

pred_max <- predict(
  modelo_max,
  newdata = amb %>% select(all_of(preditores)),
  type = "cloglog"
)

predicoes <- data.frame(
  glm = pred_glm,
  gam = pred_gam,
  rf = pred_rf,
  brt = pred_brt,
  maxent = pred_max
)

predicoes <- predicoes %>%
  mutate(
    ensemble_medio = rowMeans(across(everything())),
    ensemble_sd = apply(select(., glm, gam, rf, brt, maxent), 1, sd)
  )

write_csv(
  predicoes,
  "dados/unidade11/processados/predicoes_modelos_ensemble.csv"
)

saveRDS(
  list(
    amb = amb,
    dados_modelo = dados_modelo,
    predicoes = predicoes,
    melhor_brt = melhor_brt,
    preditores = preditores
  ),
  "resultados/unidade11/objetos_ensemble.rds"
)

# ------------------------------------------------------------
# 6. Exportar rasters e figuras
# ------------------------------------------------------------

criar_raster <- function(valores, nome_tif, nome_png, titulo) {
  r <- terra::rast(
    ncols = 120,
    nrows = 100,
    xmin = -80,
    xmax = -40,
    ymin = -30,
    ymax = 10,
    crs = "EPSG:4326"
  )
  
  terra::values(r) <- valores
  
  terra::writeRaster(
    r,
    paste0("resultados/unidade11/", nome_tif),
    overwrite = TRUE
  )
  
  png(
    paste0("figuras/unidade11/", nome_png),
    width = 1800,
    height = 1400,
    res = 220
  )
  
  plot(r, main = titulo)
  points(
    dados_modelo$lon[dados_modelo$pa == 1],
    dados_modelo$lat[dados_modelo$pa == 1],
    pch = 16,
    cex = 0.4
  )
  
  dev.off()
}

criar_raster(
  predicoes$ensemble_medio,
  "ensemble_medio.tif",
  "unidade11_ensemble_medio.png",
  "Ensemble médio de adequabilidade ambiental"
)

criar_raster(
  predicoes$ensemble_sd,
  "ensemble_incerteza_sd.tif",
  "unidade11_ensemble_incerteza.png",
  "Incerteza entre algoritmos"
)

message("Script 01 concluído.")
