source("scripts/_bootstrap.R")

# ============================================================
# Unidade 6 - GLM em SDM
# Script 04: Avaliar GLM
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr", "readr", "ggplot2",
  "pROC", "tibble", "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 2. Diretório raiz do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios de saída
# ------------------------------------------------------------

dir.create("figuras/unidade06", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade06", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/unidade06/processados", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_modelo <- "resultados/unidade06/modelo_glm_dinizia.rds"
arquivo_teste  <- "dados/unidade06/processados/teste_glm_dinizia.csv"

if (!file.exists(arquivo_modelo)) {
  stop("Modelo GLM não encontrado. Execute o Script 03 da Unidade 6.")
}

if (!file.exists(arquivo_teste)) {
  stop("Arquivo de teste não encontrado. Execute o Script 02 da Unidade 6.")
}

# ------------------------------------------------------------
# 5. Leitura dos dados
# ------------------------------------------------------------

modelo <- readRDS(arquivo_modelo)

teste <- read_csv(
  arquivo_teste,
  show_col_types = FALSE
)

if (!"pa" %in% names(teste)) {
  stop("A coluna 'pa' não foi encontrada no conjunto de teste.")
}

teste <- teste |>
  mutate(pa = as.integer(pa))

if (!all(teste$pa %in% c(0, 1))) {
  stop("A variável 'pa' deve conter apenas 0 = background e 1 = presença.")
}

if (length(unique(teste$pa)) < 2) {
  stop("O conjunto de teste precisa conter presença e background.")
}

# ------------------------------------------------------------
# 6. Predição no conjunto de teste
# ------------------------------------------------------------

teste$pred_glm <- predict(
  modelo,
  newdata = teste,
  type = "response"
)

teste <- teste |>
  filter(
    !is.na(pa),
    !is.na(pred_glm),
    is.finite(pred_glm)
  )

if (nrow(teste) == 0) {
  stop("Nenhuma predição válida foi gerada para o conjunto de teste.")
}

# ------------------------------------------------------------
# 7. Curva ROC e AUC
# ------------------------------------------------------------

roc_obj <- pROC::roc(
  response = teste$pa,
  predictor = teste$pred_glm,
  levels = c(0, 1),
  direction = "<",
  quiet = TRUE
)

auc_val <- as.numeric(pROC::auc(roc_obj))

roc_df <- tibble(
  especificidade = rev(roc_obj$specificities),
  sensibilidade = rev(roc_obj$sensitivities)
) |>
  mutate(
    fpr = 1 - especificidade
  )

# ------------------------------------------------------------
# 8. Melhor limiar pelo TSS
# ------------------------------------------------------------

calcular_metricas <- function(obs, pred, th) {
  
  pred_bin <- ifelse(pred >= th, 1, 0)
  
  TP <- sum(pred_bin == 1 & obs == 1, na.rm = TRUE)
  TN <- sum(pred_bin == 0 & obs == 0, na.rm = TRUE)
  FP <- sum(pred_bin == 1 & obs == 0, na.rm = TRUE)
  FN <- sum(pred_bin == 0 & obs == 1, na.rm = TRUE)
  
  sens <- ifelse((TP + FN) > 0, TP / (TP + FN), NA_real_)
  esp  <- ifelse((TN + FP) > 0, TN / (TN + FP), NA_real_)
  acc  <- ifelse((TP + TN + FP + FN) > 0, (TP + TN) / (TP + TN + FP + FN), NA_real_)
  
  tss <- sens + esp - 1
  
  tibble(
    limiar = th,
    sensibilidade = sens,
    especificidade = esp,
    acuracia = acc,
    TSS = tss,
    TP = TP,
    TN = TN,
    FP = FP,
    FN = FN
  )
}

limiares <- seq(0.01, 0.99, by = 0.01)

metricas_limiar <- lapply(
  limiares,
  function(th) calcular_metricas(teste$pa, teste$pred_glm, th)
) |>
  bind_rows() |>
  filter(!is.na(TSS))

melhor <- metricas_limiar |>
  arrange(desc(TSS), desc(sensibilidade), desc(especificidade)) |>
  slice(1)

# ------------------------------------------------------------
# 9. Matriz de confusão com melhor limiar
# ------------------------------------------------------------

teste$pred_bin_glm <- ifelse(
  teste$pred_glm >= melhor$limiar,
  1,
  0
)

matriz_confusao <- tibble(
  observado = c("Presença", "Presença", "Background", "Background"),
  predito = c("Presença", "Background", "Presença", "Background"),
  n = c(
    melhor$TP,
    melhor$FN,
    melhor$FP,
    melhor$TN
  )
)

# ------------------------------------------------------------
# 10. Resumo das predições
# ------------------------------------------------------------

resumo_predicoes <- teste |>
  group_by(pa) |>
  summarise(
    n = n(),
    media = mean(pred_glm, na.rm = TRUE),
    mediana = median(pred_glm, na.rm = TRUE),
    minimo = min(pred_glm, na.rm = TRUE),
    maximo = max(pred_glm, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    classe = ifelse(pa == 1, "Presença", "Background")
  ) |>
  select(classe, everything(), -pa)

# ------------------------------------------------------------
# 11. Salvar tabelas
# ------------------------------------------------------------

write_csv(
  metricas_limiar,
  "tabelas/unidade06/metricas_limiares_glm.csv"
)

avaliacao <- tibble(
  modelo = "GLM",
  dominio_calibracao = "Bioma Amazônia",
  AUC = auc_val,
  limiar_TSS = melhor$limiar,
  TSS = melhor$TSS,
  sensibilidade = melhor$sensibilidade,
  especificidade = melhor$especificidade,
  acuracia = melhor$acuracia,
  TP = melhor$TP,
  TN = melhor$TN,
  FP = melhor$FP,
  FN = melhor$FN,
  n_teste = nrow(teste),
  n_presencas_teste = sum(teste$pa == 1),
  n_background_teste = sum(teste$pa == 0)
)

write_csv(
  avaliacao,
  "tabelas/unidade06/avaliacao_glm_dinizia.csv"
)

write_csv(
  matriz_confusao,
  "tabelas/unidade06/matriz_confusao_glm.csv"
)

write_csv(
  resumo_predicoes,
  "tabelas/unidade06/resumo_predicoes_glm.csv"
)

write_csv(
  teste,
  "dados/unidade06/processados/teste_predicoes_glm_dinizia.csv"
)

# ------------------------------------------------------------
# 12. Gráfico ROC
# ------------------------------------------------------------

g_roc <- ggplot(
  roc_df,
  aes(x = fpr, y = sensibilidade)
) +
  geom_abline(
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_line(
    linewidth = 1.1,
    color = "black"
  ) +
  coord_equal() +
  labs(
    title = expression("Curva ROC do GLM para " * italic("Dinizia excelsa")),
    subtitle = paste0("AUC = ", round(auc_val, 3)),
    x = "1 - Especificidade",
    y = "Sensibilidade"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade06/unidade06_roc_glm.png",
  plot = g_roc,
  width = 6.5,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Gráfico TSS por limiar
# ------------------------------------------------------------

g_tss <- ggplot(
  metricas_limiar,
  aes(x = limiar, y = TSS)
) +
  geom_line(
    linewidth = 1.0,
    color = "black"
  ) +
  geom_vline(
    xintercept = melhor$limiar,
    linetype = "dashed",
    color = "grey30"
  ) +
  geom_point(
    data = melhor,
    aes(x = limiar, y = TSS),
    size = 2.5,
    color = "black"
  ) +
  labs(
    title = "Seleção do limiar pelo TSS",
    subtitle = paste0(
      "Melhor limiar = ",
      round(melhor$limiar, 2),
      " | TSS = ",
      round(melhor$TSS, 3)
    ),
    x = "Limiar de adequabilidade",
    y = "TSS"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade06/unidade06_tss_limiar_glm.png",
  plot = g_tss,
  width = 7,
  height = 5,
  dpi = 600
)

# ------------------------------------------------------------
# 14. Boxplot das predições por classe
# ------------------------------------------------------------

teste_plot <- teste |>
  mutate(
    classe = ifelse(pa == 1, "Presença", "Background")
  )

g_box <- ggplot(
  teste_plot,
  aes(x = classe, y = pred_glm)
) +
  geom_boxplot(outlier.alpha = 0.35) +
  geom_hline(
    yintercept = melhor$limiar,
    linetype = "dashed",
    color = "grey30"
  ) +
  labs(
    title = "Distribuição das predições do GLM",
    subtitle = "Linha tracejada indica o limiar ótimo por TSS",
    x = NULL,
    y = "Adequabilidade predita"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade06/unidade06_boxplot_predicoes_glm.png",
  plot = g_box,
  width = 6.5,
  height = 5,
  dpi = 600
)

# ------------------------------------------------------------
# 15. Figura composta com patchwork
# ------------------------------------------------------------

g_patch <- (g_roc + g_tss) / g_box +
  patchwork::plot_annotation(
    title = expression("Avaliação do GLM para " * italic("Dinizia excelsa")),
    subtitle = "Modelo calibrado com presença-background no bioma Amazônia"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade06/unidade06_avaliacao_glm_patchwork.png",
  plot = g_patch,
  width = 12,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 16. Mensagem final
# ------------------------------------------------------------

message("Avaliação do GLM concluída.")
message("AUC = ", round(auc_val, 3))
message("Melhor limiar TSS = ", round(melhor$limiar, 2))
message("TSS = ", round(melhor$TSS, 3))
message("Sensibilidade = ", round(melhor$sensibilidade, 3))
message("Especificidade = ", round(melhor$especificidade, 3))

print(avaliacao)
