# ============================================================
# Unidade 8 - Random Forest em SDM
# Script 03: Comparação GLM, GAM e Random Forest
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "pROC", "mgcv", "ranger")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("figuras/unidade08", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade08", recursive = TRUE, showWarnings = FALSE)

dados <- read_csv(
  "dados/unidade08/processados/dados_rf_presenca_background.csv",
  show_col_types = FALSE
) %>%
  mutate(
    pa_num = ifelse(pa == "presenca", 1, 0),
    pa = factor(pa, levels = c("background", "presenca"))
  )

# ------------------------------------------------------------
# GLM
# ------------------------------------------------------------

modelo_glm <- glm(
  pa_num ~ temperatura + I(temperatura^2) +
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
  pa_num ~
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

modelo_rf <- readRDS("resultados/unidade08/modelo_random_forest.rds")

pred_rf <- predict(
  modelo_rf,
  data = dados
)$predictions[, "presenca"]

# ------------------------------------------------------------
# Calcular AUC
# ------------------------------------------------------------

auc_glm <- as.numeric(pROC::auc(pROC::roc(dados$pa_num, pred_glm, quiet = TRUE)))
auc_gam <- as.numeric(pROC::auc(pROC::roc(dados$pa_num, pred_gam, quiet = TRUE)))
auc_rf  <- as.numeric(pROC::auc(pROC::roc(dados$pa_num, pred_rf, quiet = TRUE)))

comparacao <- data.frame(
  modelo = c("GLM", "GAM", "Random Forest"),
  AUC = c(auc_glm, auc_gam, auc_rf)
)

write_csv(
  comparacao,
  "tabelas/unidade08/comparacao_auc_glm_gam_rf.csv"
)

g_auc <- ggplot(
  comparacao,
  aes(x = modelo, y = AUC)
) +
  geom_col() +
  ylim(0, 1) +
  labs(
    title = "Comparação de desempenho entre modelos",
    subtitle = "Métrica AUC em dados simulados",
    x = NULL,
    y = "AUC"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade08/unidade08_comparacao_modelos_auc.png",
  plot = g_auc,
  width = 7,
  height = 5,
  dpi = 600
)

message("Comparação GLM, GAM e Random Forest concluída.")
print(comparacao)