source("scripts/_bootstrap.R")

# ============================================================
# Unidade 7 - GAM em SDM
# Script 04: Avaliação do GAM
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
# 2. Diretório do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios de saída
# ------------------------------------------------------------

dir.create("figuras/unidade07", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade07", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/unidade07/processados", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_modelo <- "resultados/unidade07/modelo_gam_dinizia.rds"
arquivo_teste  <- "dados/unidade07/processados/teste_gam_dinizia.csv"

if (!file.exists(arquivo_modelo)) {
  stop("Modelo GAM não encontrado. Execute o Script 03 da Unidade 7.")
}

if (!file.exists(arquivo_teste)) {
  stop("Arquivo de teste não encontrado. Execute o Script 02 da Unidade 7.")
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
# 6. Predições no conjunto de teste
# ------------------------------------------------------------

teste$pred_gam <- as.numeric(
  predict(
    modelo,
    newdata = teste,
    type = "response"
  )
)

teste <- teste |>
  filter(
    !is.na(pa),
    !is.na(pred_gam),
    is.finite(pred_gam)
  )

if (nrow(teste) == 0) {
  stop("Nenhuma predição válida foi gerada para o conjunto de teste.")
}

# ------------------------------------------------------------
# 7. Curva ROC e AUC
# ------------------------------------------------------------

roc_obj <- pROC::roc(
  response = teste$pa,
  predictor = teste$pred_gam,
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
# 8. Função para métricas por limiar
# ------------------------------------------------------------

calcular_metricas <- function(obs, pred, th) {
  
  pred_bin <- ifelse(pred >= th, 1, 0)
  
  TP <- sum(pred_bin == 1 & obs == 1, na.rm = TRUE)
  TN <- sum(pred_bin == 0 & obs == 0, na.rm = TRUE)
  FP <- sum(pred_bin == 1 & obs == 0, na.rm = TRUE)
  FN <- sum(pred_bin == 0 & obs == 1, na.rm = TRUE)
  
  total <- TP + TN + FP + FN
  
  sens <- ifelse((TP + FN) > 0, TP / (TP + FN), NA_real_)
  esp  <- ifelse((TN + FP) > 0, TN / (TN + FP), NA_real_)
  acc  <- ifelse(total > 0, (TP + TN) / total, NA_real_)
  tss  <- sens + esp - 1
  
  pe <- ifelse(
    total > 0,
    (((TP + FP) * (TP + FN)) + ((FN + TN) * (FP + TN))) / total^2,
    NA_real_
  )
  
  kappa <- ifelse(
    !is.na(pe) && (1 - pe) != 0,
    (acc - pe) / (1 - pe),
    NA_real_
  )
  
  tibble(
    limiar = th,
    sensibilidade = sens,
    especificidade = esp,
    acuracia = acc,
    TSS = tss,
    Kappa = kappa,
    TP = TP,
    TN = TN,
    FP = FP,
    FN = FN
  )
}

# ------------------------------------------------------------
# 9. Melhor limiar por TSS
# ------------------------------------------------------------

limiares <- seq(0.01, 0.99, by = 0.01)

metricas_limiar <- lapply(
  limiares,
  function(th) calcular_metricas(teste$pa, teste$pred_gam, th)
) |>
  bind_rows() |>
  filter(!is.na(TSS))

melhor <- metricas_limiar |>
  arrange(desc(TSS), desc(sensibilidade), desc(especificidade)) |>
  slice(1)

# ------------------------------------------------------------
# 10. Classificação do desempenho
# ------------------------------------------------------------

classe_auc <- case_when(
  auc_val > 0.90 ~ "Excelente",
  auc_val > 0.80 ~ "Muito boa",
  auc_val > 0.70 ~ "Aceitável",
  TRUE ~ "Fraca"
)

classe_tss <- case_when(
  melhor$TSS > 0.70 ~ "Excelente",
  melhor$TSS > 0.50 ~ "Boa",
  melhor$TSS > 0.30 ~ "Moderada",
  TRUE ~ "Baixa"
)

# ------------------------------------------------------------
# 11. Predição binária com melhor limiar
# ------------------------------------------------------------

teste$pred_bin_gam <- ifelse(
  teste$pred_gam >= melhor$limiar,
  1,
  0
)

# ------------------------------------------------------------
# 12. Avaliação final
# ------------------------------------------------------------

avaliacao <- tibble(
  modelo = "GAM",
  dominio_calibracao = "Bioma Amazônia",
  AUC = auc_val,
  interpretacao_AUC = classe_auc,
  limiar_TSS = melhor$limiar,
  TSS = melhor$TSS,
  interpretacao_TSS = classe_tss,
  Kappa = melhor$Kappa,
  acuracia = melhor$acuracia,
  sensibilidade = melhor$sensibilidade,
  especificidade = melhor$especificidade,
  TP = melhor$TP,
  TN = melhor$TN,
  FP = melhor$FP,
  FN = melhor$FN,
  n_teste = nrow(teste),
  n_presencas_teste = sum(teste$pa == 1),
  n_background_teste = sum(teste$pa == 0)
)

# ------------------------------------------------------------
# 13. Matriz de confusão
# ------------------------------------------------------------

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
# 14. Resumo das predições
# ------------------------------------------------------------

resumo_predicoes <- teste |>
  group_by(pa) |>
  summarise(
    n = n(),
    media = mean(pred_gam, na.rm = TRUE),
    mediana = median(pred_gam, na.rm = TRUE),
    minimo = min(pred_gam, na.rm = TRUE),
    maximo = max(pred_gam, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    classe = ifelse(pa == 1, "Presença", "Background")
  ) |>
  select(classe, everything(), -pa)

# ------------------------------------------------------------
# 15. Limpar colunas problemáticas antes de exportar
# ------------------------------------------------------------

teste_export <- as.data.frame(teste)

teste_export[] <- lapply(teste_export, function(x) {
  
  if (is.matrix(x) || is.array(x)) {
    return(as.numeric(x))
  }
  
  if (is.list(x)) {
    return(as.character(unlist(x)))
  }
  
  x
})

# ------------------------------------------------------------
# 16. Exportar resultados
# ------------------------------------------------------------

write_csv(
  metricas_limiar,
  "tabelas/unidade07/metricas_limiares_gam.csv"
)

write_csv(
  avaliacao,
  "tabelas/unidade07/avaliacao_gam_dinizia.csv"
)

write_csv(
  matriz_confusao,
  "tabelas/unidade07/matriz_confusao_gam.csv"
)

write_csv(
  resumo_predicoes,
  "tabelas/unidade07/resumo_predicoes_gam.csv"
)

write_csv(
  teste_export,
  "dados/unidade07/processados/teste_predicoes_gam_dinizia.csv"
)

# ------------------------------------------------------------
# 17. Curva ROC
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
    title = expression("Curva ROC do GAM para " * italic("Dinizia excelsa")),
    subtitle = paste0("AUC = ", round(auc_val, 3), " (", classe_auc, ")"),
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
  "figuras/unidade07/unidade07_roc_gam.png",
  plot = g_roc,
  width = 6.5,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 18. Curva TSS
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
    size = 2.8,
    color = "black"
  ) +
  labs(
    title = "Seleção do limiar pelo TSS - GAM",
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
  "figuras/unidade07/unidade07_tss_limiar_gam.png",
  plot = g_tss,
  width = 7,
  height = 5,
  dpi = 600
)

# ------------------------------------------------------------
# 19. Boxplot das predições por classe
# ------------------------------------------------------------

teste_plot <- teste |>
  mutate(
    classe = ifelse(pa == 1, "Presença", "Background")
  )

g_box <- ggplot(
  teste_plot,
  aes(x = classe, y = pred_gam)
) +
  geom_boxplot(outlier.alpha = 0.35) +
  geom_hline(
    yintercept = melhor$limiar,
    linetype = "dashed",
    color = "grey30"
  ) +
  labs(
    title = "Distribuição das predições do GAM",
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
  "figuras/unidade07/unidade07_boxplot_predicoes_gam.png",
  plot = g_box,
  width = 6.5,
  height = 5,
  dpi = 600
)

# ------------------------------------------------------------
# 20. Figura composta com patchwork
# ------------------------------------------------------------

g_patch <- (g_roc + g_tss) / g_box +
  patchwork::plot_annotation(
    title = expression("Avaliação do GAM para " * italic("Dinizia excelsa")),
    subtitle = "Modelo calibrado com presença-background no bioma Amazônia"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade07/unidade07_avaliacao_gam_patchwork.png",
  plot = g_patch,
  width = 12,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 21. Mensagem final
# ------------------------------------------------------------

message("Avaliação do GAM concluída.")
message("AUC = ", round(auc_val, 3))
message("Melhor limiar TSS = ", round(melhor$limiar, 2))
message("TSS = ", round(melhor$TSS, 3))
message("Sensibilidade = ", round(melhor$sensibilidade, 3))
message("Especificidade = ", round(melhor$especificidade, 3))

print(avaliacao)
