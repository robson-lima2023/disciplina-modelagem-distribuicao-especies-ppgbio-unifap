# ============================================================
# Unidade 9 - Boosted Regression Trees em SDM
# Script 02: Avaliação básica do BRT
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "pROC", "gbm")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("figuras/unidade09", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade09", recursive = TRUE, showWarnings = FALSE)

dados <- read_csv(
  "dados/unidade09/processados/dados_brt_presenca_background.csv",
  show_col_types = FALSE
)

modelo <- readRDS("resultados/unidade09/modelo_brt.rds")

melhor_iter <- read_csv(
  "tabelas/unidade09/melhor_iteracao_brt.csv",
  show_col_types = FALSE
)$melhor_iteracao[1]

dados$pred <- predict(
  modelo,
  newdata = dados,
  n.trees = melhor_iter,
  type = "response"
)

roc_obj <- pROC::roc(
  response = dados$pa,
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
    title = "Curva ROC - BRT",
    subtitle = paste0("AUC = ", round(auc_valor, 3)),
    x = "1 - Especificidade",
    y = "Sensibilidade"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade09/unidade09_roc_brt.png",
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
  "tabelas/unidade09/metricas_brt.csv"
)

message("Avaliação do BRT concluída.")
print(metricas)
