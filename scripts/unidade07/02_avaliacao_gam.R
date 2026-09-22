# ============================================================
# Unidade 7 - GAM em SDM
# Script 02: Avaliação básica do GAM
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "pROC", "mgcv")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("figuras/unidade07", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade07", recursive = TRUE, showWarnings = FALSE)

dados <- read_csv(
  "dados/unidade07/processados/dados_gam_presenca_background.csv",
  show_col_types = FALSE
)

modelo <- readRDS("resultados/unidade07/modelo_gam.rds")

dados$pred <- predict(
  modelo,
  newdata = dados,
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
    title = "Curva ROC do GAM",
    subtitle = paste0("AUC = ", round(auc_valor, 3)),
    x = "1 - Especificidade",
    y = "Sensibilidade"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade07/unidade07_roc_gam.png",
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
  "tabelas/unidade07/metricas_gam.csv"
)

message("Avaliação do GAM concluída.")
print(metricas)
