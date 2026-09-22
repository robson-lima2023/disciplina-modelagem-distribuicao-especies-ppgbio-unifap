# ============================================================
# Unidade 8 - Random Forest em SDM
# Script 02: Avaliação básica do Random Forest
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "pROC", "ranger")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("figuras/unidade08", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade08", recursive = TRUE, showWarnings = FALSE)

dados <- read_csv(
  "dados/unidade08/processados/dados_rf_presenca_background.csv",
  show_col_types = FALSE
) %>%
  mutate(pa = factor(pa, levels = c("background", "presenca")))

modelo <- readRDS("resultados/unidade08/modelo_random_forest.rds")

pred <- predict(
  modelo,
  data = dados
)$predictions[, "presenca"]

dados$pred <- pred
dados$pa_num <- ifelse(dados$pa == "presenca", 1, 0)

roc_obj <- pROC::roc(
  response = dados$pa_num,
  predictor = dados$pred,
  quiet = TRUE
)

auc_valor <- as.numeric(pROC::auc(roc_obj))

roc_df <- data.frame(
  sensibilidade = roc_obj$sensitivities,
  especificidade = roc_obj$specificities
) %>%
  mutate(fpr = 1 - especificidade)

g_roc <- ggplot(
  roc_df,
  aes(x = fpr, y = sensibilidade)
) +
  geom_line(linewidth = 1) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  labs(
    title = "Curva ROC - Random Forest",
    subtitle = paste0("AUC = ", round(auc_valor, 3)),
    x = "1 - Especificidade",
    y = "Sensibilidade"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade08/unidade08_roc_rf.png",
  plot = g_roc,
  width = 6,
  height = 6,
  dpi = 600
)

metricas <- data.frame(
  metrica = "AUC",
  valor = auc_valor
)

write_csv(
  metricas,
  "tabelas/unidade08/metricas_random_forest.csv"
)

message("Avaliação do Random Forest concluída.")
print(metricas)
