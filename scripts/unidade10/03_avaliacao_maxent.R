# ============================================================
# Unidade 10 - Maxent em SDM
# Script 03: Avaliação do modelo final
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "pROC", "maxnet")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

objetos <- readRDS("resultados/unidade10/objetos_base_maxent.rds")
modelo <- readRDS("resultados/unidade10/modelo_maxent_final.rds")

dados_teste <- objetos$dados_teste
preditores <- objetos$preditores

x_teste <- dados_teste %>% select(all_of(preditores))
p_teste <- dados_teste$pa

pred <- predict(
  modelo,
  newdata = x_teste,
  type = "cloglog"
)

roc_obj <- pROC::roc(
  response = p_teste,
  predictor = pred,
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
    title = "Curva ROC - Maxent",
    subtitle = paste0("AUC = ", round(auc_valor, 3)),
    x = "1 - Especificidade",
    y = "Sensibilidade"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade10/unidade10_roc_maxent.png",
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
  "tabelas/unidade10/metricas_maxent.csv"
)

message("Avaliação do Maxent concluída.")
print(metricas)
