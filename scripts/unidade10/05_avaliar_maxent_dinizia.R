source("scripts/_bootstrap.R")

# ============================================================
# Unidade 10 - Maxent em SDM
# Script 05: Avaliar MaxEnt
# ============================================================

pacotes <- c(
  "dplyr", "readr", "tidyr", "maxnet",
  "ggplot2", "pROC", "tibble", "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))
dir.create("figuras/unidade10", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade10", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/unidade10/processados", recursive = TRUE, showWarnings = FALSE)

arquivo_modelo <- "resultados/unidade10/modelo_maxent_dinizia.rds"
arquivo_treino <- "dados/unidade10/processados/treino_maxent_dinizia.csv"
arquivo_teste  <- "dados/unidade10/processados/teste_maxent_dinizia.csv"
arquivo_vars   <- "resultados/unidade10/variaveis_maxent.csv"

arquivos <- c(arquivo_modelo, arquivo_treino, arquivo_teste, arquivo_vars)

if (any(!file.exists(arquivos))) {
  stop(
    "Arquivos ausentes:\n",
    paste(arquivos[!file.exists(arquivos)], collapse = "\n")
  )
}

modelo <- readRDS(arquivo_modelo)

treino <- read_csv(arquivo_treino, show_col_types = FALSE) |>
  mutate(pa = as.numeric(pa))

teste <- read_csv(arquivo_teste, show_col_types = FALSE) |>
  mutate(pa = as.numeric(pa))

vars <- read_csv(
  arquivo_vars,
  show_col_types = FALSE
)$variavel |>
  unique()

vars <- vars[vars %in% names(treino)]

if (length(vars) < 2) {
  stop("Número insuficiente de variáveis ambientais para avaliar o MaxEnt.")
}

if (!all(treino$pa %in% c(0, 1)) || !all(teste$pa %in% c(0, 1))) {
  stop("A variável 'pa' deve conter apenas 0 = background e 1 = presença.")
}

x_treino <- treino |>
  select(all_of(vars)) |>
  as.data.frame()

x_teste <- teste |>
  select(all_of(vars)) |>
  as.data.frame()

for (v in vars) {
  x_treino[[v]] <- as.numeric(x_treino[[v]])
  x_teste[[v]] <- as.numeric(x_teste[[v]])
}

treino$pred_maxent <- as.numeric(
  predict(
    modelo,
    newdata = x_treino,
    type = "cloglog",
    clamp = FALSE
  )
)

teste$pred_maxent <- as.numeric(
  predict(
    modelo,
    newdata = x_teste,
    type = "cloglog",
    clamp = FALSE
  )
)

treino <- treino |>
  filter(!is.na(pa), !is.na(pred_maxent), is.finite(pred_maxent))

teste <- teste |>
  filter(!is.na(pa), !is.na(pred_maxent), is.finite(pred_maxent))

if (length(unique(teste$pa)) < 2) {
  stop("O conjunto de teste precisa conter presenças e background.")
}

roc_treino <- pROC::roc(
  response = treino$pa,
  predictor = treino$pred_maxent,
  levels = c(0, 1),
  direction = "<",
  quiet = TRUE
)

roc_teste <- pROC::roc(
  response = teste$pa,
  predictor = teste$pred_maxent,
  levels = c(0, 1),
  direction = "<",
  quiet = TRUE
)

auc_treino <- as.numeric(pROC::auc(roc_treino))
auc_teste <- as.numeric(pROC::auc(roc_teste))

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

limiares <- seq(0.01, 0.99, by = 0.01)

metricas_limiar <- lapply(
  limiares,
  function(th) calcular_metricas(teste$pa, teste$pred_maxent, th)
) |>
  bind_rows() |>
  filter(!is.na(TSS))

melhor <- metricas_limiar |>
  arrange(desc(TSS), desc(sensibilidade), desc(especificidade)) |>
  slice(1)

teste$pred_bin_maxent <- ifelse(
  teste$pred_maxent >= melhor$limiar,
  1,
  0
)

brier_teste <- mean(
  (teste$pa - teste$pred_maxent)^2,
  na.rm = TRUE
)

classe_auc <- case_when(
  auc_teste > 0.90 ~ "Excelente",
  auc_teste > 0.80 ~ "Muito boa",
  auc_teste > 0.70 ~ "Aceitável",
  TRUE ~ "Fraca"
)

classe_sobreajuste <- case_when(
  (auc_treino - auc_teste) <= 0.05 ~ "Baixo",
  (auc_treino - auc_teste) <= 0.10 ~ "Moderado",
  TRUE ~ "Elevado"
)

avaliacao <- tibble(
  modelo = "MaxEnt_maxnet",
  dominio_calibracao = "Bioma Amazônia",
  AUC_treino = auc_treino,
  AUC_teste_random_split = auc_teste,
  diferenca_treino_teste = auc_treino - auc_teste,
  diagnostico_sobreajuste = classe_sobreajuste,
  interpretacao_AUC_teste = classe_auc,
  limiar_TSS = melhor$limiar,
  TSS = melhor$TSS,
  Kappa = melhor$Kappa,
  Brier_teste = brier_teste,
  sensibilidade = melhor$sensibilidade,
  especificidade = melhor$especificidade,
  acuracia = melhor$acuracia,
  TP = melhor$TP,
  TN = melhor$TN,
  FP = melhor$FP,
  FN = melhor$FN,
  n_teste = nrow(teste),
  n_presencas_teste = sum(teste$pa == 1),
  n_background_teste = sum(teste$pa == 0),
  n_variaveis = length(vars)
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

resumo_predicoes <- teste |>
  group_by(pa) |>
  summarise(
    n = n(),
    media = mean(pred_maxent, na.rm = TRUE),
    mediana = median(pred_maxent, na.rm = TRUE),
    minimo = min(pred_maxent, na.rm = TRUE),
    maximo = max(pred_maxent, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    classe = ifelse(pa == 1, "Presença", "Background")
  ) |>
  select(classe, everything(), -pa)

write_csv(avaliacao, "tabelas/unidade10/avaliacao_maxent_dinizia.csv")
write_csv(metricas_limiar, "tabelas/unidade10/metricas_limiares_maxent.csv")
write_csv(matriz_confusao, "tabelas/unidade10/matriz_confusao_maxent.csv")
write_csv(resumo_predicoes, "tabelas/unidade10/resumo_predicoes_maxent.csv")

teste_export <- teste |>
  select(lon, lat, pa, tipo, pred_maxent, pred_bin_maxent)

write_csv(
  teste_export,
  "dados/unidade10/processados/teste_predicoes_maxent_dinizia.csv"
)

roc_df <- tibble(
  especificidade = rev(roc_teste$specificities),
  sensibilidade = rev(roc_teste$sensitivities)
) |>
  mutate(
    fpr = 1 - especificidade
  )

g_roc <- ggplot(
  roc_df,
  aes(x = fpr, y = sensibilidade)
) +
  geom_abline(linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1.1, color = "black") +
  coord_equal() +
  labs(
    title = expression("Curva ROC do MaxEnt para " * italic("Dinizia excelsa")),
    subtitle = paste0("AUC teste = ", round(auc_teste, 3), " (", classe_auc, ")"),
    x = "1 - Especificidade",
    y = "Sensibilidade"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

g_tss <- ggplot(
  metricas_limiar,
  aes(x = limiar, y = TSS)
) +
  geom_line(linewidth = 1, color = "black") +
  geom_vline(
    xintercept = melhor$limiar,
    linetype = "dashed",
    color = "grey30"
  ) +
  geom_point(
    data = melhor,
    aes(x = limiar, y = TSS),
    size = 2.8
  ) +
  labs(
    title = "Seleção do limiar pelo TSS - MaxEnt",
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
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

teste_plot <- teste |>
  mutate(
    classe = ifelse(pa == 1, "Presença", "Background")
  )

g_box <- ggplot(
  teste_plot,
  aes(x = classe, y = pred_maxent)
) +
  geom_boxplot(outlier.alpha = 0.35) +
  geom_hline(
    yintercept = melhor$limiar,
    linetype = "dashed",
    color = "grey30"
  ) +
  labs(
    title = "Distribuição das predições do MaxEnt",
    subtitle = "Linha tracejada indica o limiar ótimo por TSS",
    x = NULL,
    y = "Adequabilidade predita"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

g_diag <- ggplot(
  tibble(
    conjunto = c("Treino", "Teste aleatório"),
    AUC = c(auc_treino, auc_teste)
  ),
  aes(x = conjunto, y = AUC)
) +
  geom_col(color = "grey25", linewidth = 0.2) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Diagnóstico de sobreajuste",
    subtitle = paste0("Diagnóstico: ", classe_sobreajuste),
    x = NULL,
    y = "AUC"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("figuras/unidade10/unidade10_roc_maxent.png", g_roc, width = 6.5, height = 6, dpi = 600)
ggsave("figuras/unidade10/unidade10_tss_limiar_maxent.png", g_tss, width = 7, height = 5, dpi = 600)
ggsave("figuras/unidade10/unidade10_boxplot_predicoes_maxent.png", g_box, width = 6.5, height = 5, dpi = 600)
ggsave("figuras/unidade10/unidade10_diagnostico_overfitting_maxent.png", g_diag, width = 7, height = 5, dpi = 600)

g_patch <- (g_roc | g_tss) / (g_diag | g_box) +
  patchwork::plot_annotation(
    title = expression("Avaliação crítica do MaxEnt para " * italic("Dinizia excelsa")),
    subtitle = "Inclui desempenho preditivo, limiar ótimo, diagnóstico de sobreajuste e distribuição das predições"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade10/unidade10_avaliacao_maxent_patchwork.png",
  plot = g_patch,
  width = 13,
  height = 9,
  dpi = 600
)

message("Avaliação crítica do MaxEnt concluída.")
message("AUC treino = ", round(auc_treino, 3))
message("AUC teste = ", round(auc_teste, 3))
message("Diagnóstico de sobreajuste: ", classe_sobreajuste)

print(avaliacao)

