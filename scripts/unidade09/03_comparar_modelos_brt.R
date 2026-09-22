# ============================================================
# Unidade 9 - Boosted Regression Trees em SDM
# Script 03: Comparação GLM, GAM, RF e BRT
# ============================================================

pacotes <- c(
  "dplyr",
  "readr",
  "ggplot2",
  "pROC",
  "mgcv",
  "ranger",
  "gbm"
)

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("figuras/unidade09", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade09", recursive = TRUE, showWarnings = FALSE)

dados <- read_csv(
  "dados/unidade09/processados/dados_brt_presenca_background.csv",
  show_col_types = FALSE
)

# ------------------------------------------------------------
# GLM quadrático
# ------------------------------------------------------------

modelo_glm <- glm(
  pa ~ temperatura + I(temperatura^2) +
    precipitacao + I(precipitacao^2) +
    elevacao + solo,
  data = dados,
  family = binomial(link = "logit")
)

pred_glm <- predict(
  modelo_glm,
  newdata = dados,
  type = "response"
)

# ------------------------------------------------------------
# GAM
# ------------------------------------------------------------

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

pred_gam <- predict(
  modelo_gam,
  newdata = dados,
  type = "response"
)

# ------------------------------------------------------------
# Random Forest
# ------------------------------------------------------------

dados_rf <- dados %>%
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

pred_rf <- predict(
  modelo_rf,
  data = dados_rf
)$predictions[, "presenca"]

# ------------------------------------------------------------
# BRT
# ------------------------------------------------------------

modelo_brt <- readRDS("resultados/unidade09/modelo_brt.rds")

melhor_iter <- read_csv(
  "tabelas/unidade09/melhor_iteracao_brt.csv",
  show_col_types = FALSE
)$melhor_iteracao[1]

pred_brt <- predict(
  modelo_brt,
  newdata = dados,
  n.trees = melhor_iter,
  type = "response"
)

# ------------------------------------------------------------
# AUC dos modelos
# ------------------------------------------------------------

auc_glm <- as.numeric(pROC::auc(pROC::roc(dados$pa, pred_glm, quiet = TRUE)))
auc_gam <- as.numeric(pROC::auc(pROC::roc(dados$pa, pred_gam, quiet = TRUE)))
auc_rf  <- as.numeric(pROC::auc(pROC::roc(dados$pa, pred_rf, quiet = TRUE)))
auc_brt <- as.numeric(pROC::auc(pROC::roc(dados$pa, pred_brt, quiet = TRUE)))

comparacao <- data.frame(
  modelo = c("GLM", "GAM", "Random Forest", "BRT"),
  AUC = c(auc_glm, auc_gam, auc_rf, auc_brt)
)

write_csv(
  comparacao,
  "tabelas/unidade09/comparacao_auc_glm_gam_rf_brt.csv"
)

g_auc <- ggplot(
  comparacao,
  aes(x = modelo, y = AUC)
) +
  geom_col() +
  ylim(0, 1) +
  labs(
    title = "Comparação de desempenho entre modelos",
    subtitle = "GLM, GAM, Random Forest e BRT",
    x = NULL,
    y = "AUC"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade09/unidade09_comparacao_modelos_auc.png",
  plot = g_auc,
  width = 8,
  height = 5,
  dpi = 600
)

message("Comparação entre modelos concluída.")
print(comparacao)
