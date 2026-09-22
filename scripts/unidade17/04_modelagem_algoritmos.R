# ============================================================
# Unidade 17 - Projeto Integrador
# Script 04: Modelagem com múltiplos algoritmos
# ============================================================

pacotes <- c("dplyr", "readr", "mgcv", "ranger", "gbm")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dados <- read_csv(
  "dados/unidade17/processados/dados_modelo_integrador.csv",
  show_col_types = FALSE
)

amb <- read_csv(
  "dados/unidade17/processados/ambiente_integrador.csv",
  show_col_types = FALSE
)

# GLM
modelo_glm <- glm(
  pa ~ temperatura + I(temperatura^2) +
    precipitacao + I(precipitacao^2) +
    elevacao + solo,
  data = dados,
  family = binomial(link = "logit")
)

# GAM
modelo_gam <- mgcv::gam(
  pa ~
    s(temperatura, k = 6) +
    s(precipitacao, k = 6) +
    s(elevacao, k = 5) +
    s(solo, k = 5),
  data = dados,
  family = binomial(link = "logit"),
  method = "REML"
)

# Random Forest
dados_rf <- dados %>%
  mutate(pa_factor = factor(ifelse(pa == 1, "presenca", "background")))

modelo_rf <- ranger::ranger(
  pa_factor ~ temperatura + precipitacao + elevacao + solo,
  data = dados_rf,
  probability = TRUE,
  num.trees = 700,
  mtry = 2,
  min.node.size = 10,
  seed = 123
)

# BRT
modelo_brt <- gbm::gbm(
  pa ~ temperatura + precipitacao + elevacao + solo,
  data = dados,
  distribution = "bernoulli",
  n.trees = 2200,
  interaction.depth = 3,
  shrinkage = 0.01,
  bag.fraction = 0.6,
  cv.folds = 5,
  n.minobsinnode = 10,
  verbose = FALSE
)

melhor_brt <- gbm::gbm.perf(modelo_brt, method = "cv", plot.it = FALSE)

saveRDS(
  list(
    glm = modelo_glm,
    gam = modelo_gam,
    rf = modelo_rf,
    brt = modelo_brt,
    melhor_brt = melhor_brt
  ),
  "resultados/unidade17/modelos_integrador.rds"
)

message("Modelos GLM, GAM, RF e BRT ajustados.")
