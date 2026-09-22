source("scripts/_bootstrap.R")

# ============================================================
# Unidade 9 - BRT em SDM
# Script 04: Avaliar BRT e diagnosticar sobreajuste
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr", "readr", "tidyr", "gbm",
  "ggplot2", "pROC", "tibble", "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 2. Diretório raiz do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios de saída
# ------------------------------------------------------------

dir.create("figuras/unidade09", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade09", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/unidade09/processados", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_modelo <- "resultados/unidade09/modelo_brt_dinizia.rds"
arquivo_parametros <- "tabelas/unidade09/parametros_brt.csv"
arquivo_treino <- "dados/unidade09/processados/treino_brt_dinizia.csv"
arquivo_teste <- "dados/unidade09/processados/teste_brt_dinizia.csv"
arquivo_dados <- "dados/unidade09/processados/dados_brt_dinizia.csv"
arquivo_vars <- "resultados/unidade09/variaveis_brt.csv"

arquivos <- c(
  arquivo_modelo,
  arquivo_parametros,
  arquivo_treino,
  arquivo_teste,
  arquivo_dados,
  arquivo_vars
)

if (any(!file.exists(arquivos))) {
  stop(
    "Arquivos ausentes:\n",
    paste(arquivos[!file.exists(arquivos)], collapse = "\n")
  )
}

# ------------------------------------------------------------
# 5. Leitura dos dados
# ------------------------------------------------------------

modelo <- readRDS(arquivo_modelo)

parametros <- read_csv(
  arquivo_parametros,
  show_col_types = FALSE
)

melhor_iter <- as.numeric(
  parametros$valor[parametros$parametro == "best.trees"]
)

if (length(melhor_iter) == 0 || is.na(melhor_iter)) {
  stop("Número ótimo de árvores não encontrado em parametros_brt.csv.")
}

treino <- read_csv(
  arquivo_treino,
  show_col_types = FALSE
) |>
  mutate(pa = as.numeric(pa))

teste <- read_csv(
  arquivo_teste,
  show_col_types = FALSE
) |>
  mutate(pa = as.numeric(pa))

dados <- read_csv(
  arquivo_dados,
  show_col_types = FALSE
) |>
  mutate(pa = as.numeric(pa))

vars <- read_csv(
  arquivo_vars,
  show_col_types = FALSE
)$variavel |>
  unique()

vars <- vars[vars %in% names(dados)]

if (length(vars) < 2) {
  stop("Número insuficiente de variáveis ambientais para avaliar o BRT.")
}

if (!all(treino$pa %in% c(0, 1)) || !all(teste$pa %in% c(0, 1))) {
  stop("A variável 'pa' deve conter apenas 0 = background e 1 = presença.")
}

# ------------------------------------------------------------
# 6. Predições treino e teste
# ------------------------------------------------------------

treino$pred_brt <- as.numeric(
  predict(
    modelo,
    newdata = treino,
    n.trees = melhor_iter,
    type = "response"
  )
)

teste$pred_brt <- as.numeric(
  predict(
    modelo,
    newdata = teste,
    n.trees = melhor_iter,
    type = "response"
  )
)

treino <- treino |>
  filter(!is.na(pa), !is.na(pred_brt), is.finite(pred_brt))

teste <- teste |>
  filter(!is.na(pa), !is.na(pred_brt), is.finite(pred_brt))

if (length(unique(teste$pa)) < 2) {
  stop("O conjunto de teste precisa conter presença e background.")
}

auc_treino <- as.numeric(
  pROC::auc(
    pROC::roc(
      response = treino$pa,
      predictor = treino$pred_brt,
      levels = c(0, 1),
      direction = "<",
      quiet = TRUE
    )
  )
)

auc_teste <- as.numeric(
  pROC::auc(
    pROC::roc(
      response = teste$pa,
      predictor = teste$pred_brt,
      levels = c(0, 1),
      direction = "<",
      quiet = TRUE
    )
  )
)

# ------------------------------------------------------------
# 7. Métricas por limiar
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

limiares <- seq(0.01, 0.99, by = 0.01)

metricas_limiar <- lapply(
  limiares,
  function(th) calcular_metricas(teste$pa, teste$pred_brt, th)
) |>
  bind_rows() |>
  filter(!is.na(TSS))

melhor <- metricas_limiar |>
  arrange(desc(TSS), desc(sensibilidade), desc(especificidade)) |>
  slice(1)

teste$pred_bin_brt <- ifelse(
  teste$pred_brt >= melhor$limiar,
  1,
  0
)

# ------------------------------------------------------------
# 8. Diagnóstico de calibração e sobreajuste
# ------------------------------------------------------------

brier_teste <- mean(
  (teste$pa - teste$pred_brt)^2,
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

# ------------------------------------------------------------
# 9. Validação espacial por blocos
# ------------------------------------------------------------

set.seed(123)

dados_blocos <- dados |>
  drop_na(lon, lat, pa, all_of(vars))

n_blocos <- 5

coords_scaled <- scale(dados_blocos[, c("lon", "lat")])

dados_blocos$bloco_espacial <- kmeans(
  coords_scaled,
  centers = n_blocos,
  nstart = 50
)$cluster

termos_brt <- paste0("`", vars, "`")

formula_brt <- as.formula(
  paste(
    "pa ~",
    paste(termos_brt, collapse = " + ")
  )
)

n_pres_total <- sum(dados_blocos$pa == 1)

n_minobsinnode <- max(10, floor(0.03 * n_pres_total))

auc_blocos <- lapply(
  sort(unique(dados_blocos$bloco_espacial)),
  function(b) {
    
    treino_b <- dados_blocos |>
      filter(bloco_espacial != b)
    
    teste_b <- dados_blocos |>
      filter(bloco_espacial == b)
    
    if (
      length(unique(teste_b$pa)) < 2 ||
      length(unique(treino_b$pa)) < 2
    ) {
      return(
        tibble(
          bloco = b,
          n_teste = nrow(teste_b),
          presencas = sum(teste_b$pa == 1),
          background = sum(teste_b$pa == 0),
          AUC_espacial = NA_real_
        )
      )
    }
    
    mod_b <- gbm::gbm(
      formula = formula_brt,
      data = treino_b,
      distribution = "bernoulli",
      n.trees = 3000,
      interaction.depth = 2,
      shrinkage = 0.005,
      bag.fraction = 0.65,
      n.minobsinnode = n_minobsinnode,
      cv.folds = 3,
      train.fraction = 1.0,
      keep.data = FALSE,
      verbose = FALSE
    )
    
    best_b <- gbm::gbm.perf(
      mod_b,
      method = "cv",
      plot.it = FALSE
    )
    
    if (is.null(best_b) || is.na(best_b)) {
      best_b <- 3000
    }
    
    pred_b <- predict(
      mod_b,
      newdata = teste_b,
      n.trees = best_b,
      type = "response"
    )
    
    auc_b <- as.numeric(
      pROC::auc(
        pROC::roc(
          response = teste_b$pa,
          predictor = pred_b,
          levels = c(0, 1),
          direction = "<",
          quiet = TRUE
        )
      )
    )
    
    tibble(
      bloco = b,
      n_teste = nrow(teste_b),
      presencas = sum(teste_b$pa == 1),
      background = sum(teste_b$pa == 0),
      best_trees = best_b,
      AUC_espacial = auc_b
    )
  }
) |>
  bind_rows()

auc_espacial_medio <- mean(auc_blocos$AUC_espacial, na.rm = TRUE)
auc_espacial_sd <- sd(auc_blocos$AUC_espacial, na.rm = TRUE)

diferenca_random_spatial <- auc_teste - auc_espacial_medio

diagnostico_espacial <- case_when(
  is.na(diferenca_random_spatial) ~ "Não avaliado",
  diferenca_random_spatial <= 0.05 ~ "Baixa evidência de otimismo espacial",
  diferenca_random_spatial <= 0.10 ~ "Otimismo espacial moderado",
  TRUE ~ "Forte evidência de otimismo espacial"
)

# ------------------------------------------------------------
# 10. Tabelas de avaliação
# ------------------------------------------------------------

avaliacao <- tibble(
  modelo = "BRT",
  dominio_calibracao = "Bioma Amazônia",
  AUC_treino = auc_treino,
  AUC_teste_random_split = auc_teste,
  AUC_espacial_medio = auc_espacial_medio,
  AUC_espacial_sd = auc_espacial_sd,
  diferenca_treino_teste = auc_treino - auc_teste,
  diferenca_random_spatial = diferenca_random_spatial,
  diagnostico_sobreajuste = classe_sobreajuste,
  diagnostico_espacial = diagnostico_espacial,
  interpretacao_AUC_teste = classe_auc,
  best_trees = melhor_iter,
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
    media = mean(pred_brt, na.rm = TRUE),
    mediana = median(pred_brt, na.rm = TRUE),
    minimo = min(pred_brt, na.rm = TRUE),
    maximo = max(pred_brt, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    classe = ifelse(pa == 1, "Presença", "Background")
  ) |>
  select(classe, everything(), -pa)

write_csv(
  avaliacao,
  "tabelas/unidade09/avaliacao_brt_dinizia.csv"
)

write_csv(
  metricas_limiar,
  "tabelas/unidade09/metricas_limiares_brt.csv"
)

write_csv(
  auc_blocos,
  "tabelas/unidade09/validacao_espacial_blocos_brt.csv"
)

write_csv(
  matriz_confusao,
  "tabelas/unidade09/matriz_confusao_brt.csv"
)

write_csv(
  resumo_predicoes,
  "tabelas/unidade09/resumo_predicoes_brt.csv"
)

teste_export <- teste |>
  select(lon, lat, pa, tipo, pred_brt, pred_bin_brt)

write_csv(
  teste_export,
  "dados/unidade09/processados/teste_predicoes_brt_dinizia.csv"
)

# ------------------------------------------------------------
# 11. Gráficos
# ------------------------------------------------------------

roc_teste <- pROC::roc(
  response = teste$pa,
  predictor = teste$pred_brt,
  levels = c(0, 1),
  direction = "<",
  quiet = TRUE
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
    title = expression("Curva ROC do BRT para " * italic("Dinizia excelsa")),
    subtitle = paste0(
      "AUC teste = ", round(auc_teste, 3),
      " | AUC espacial média = ", round(auc_espacial_medio, 3)
    ),
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
    title = "Seleção do limiar pelo TSS - BRT",
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

diag_df <- tibble(
  conjunto = c("Treino", "Teste aleatório", "Validação espacial"),
  AUC = c(auc_treino, auc_teste, auc_espacial_medio)
)

g_diag <- ggplot(
  diag_df,
  aes(x = conjunto, y = AUC)
) +
  geom_col(color = "grey25", linewidth = 0.2) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  labs(
    title = "Diagnóstico de sobreajuste e otimismo espacial",
    subtitle = diagnostico_espacial,
    x = NULL,
    y = "AUC"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 20, hjust = 1),
    panel.grid.minor = element_blank()
  )

teste_plot <- teste |>
  mutate(
    classe = ifelse(pa == 1, "Presença", "Background")
  )

g_box <- ggplot(
  teste_plot,
  aes(x = classe, y = pred_brt)
) +
  geom_boxplot(outlier.alpha = 0.35) +
  geom_hline(
    yintercept = melhor$limiar,
    linetype = "dashed",
    color = "grey30"
  ) +
  labs(
    title = "Distribuição das predições do BRT",
    subtitle = "Linha tracejada indica o limiar ótimo por TSS",
    x = NULL,
    y = "Adequabilidade predita"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade09/unidade09_roc_brt.png",
  g_roc,
  width = 6.5,
  height = 6,
  dpi = 600
)

ggsave(
  "figuras/unidade09/unidade09_tss_limiar_brt.png",
  g_tss,
  width = 7,
  height = 5,
  dpi = 600
)

ggsave(
  "figuras/unidade09/unidade09_diagnostico_overfitting_brt.png",
  g_diag,
  width = 8,
  height = 5.5,
  dpi = 600
)

ggsave(
  "figuras/unidade09/unidade09_boxplot_predicoes_brt.png",
  g_box,
  width = 6.5,
  height = 5,
  dpi = 600
)

g_patch <- (g_roc | g_tss) / (g_diag | g_box) +
  patchwork::plot_annotation(
    title = expression("Avaliação crítica do BRT para " * italic("Dinizia excelsa")),
    subtitle = "Inclui diagnóstico de sobreajuste, validação espacial por blocos e distribuição das predições"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade09/unidade09_avaliacao_brt_patchwork.png",
  plot = g_patch,
  width = 13,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Mensagem final
# ------------------------------------------------------------

message("Avaliação crítica do BRT concluída.")
message("AUC treino = ", round(auc_treino, 3))
message("AUC teste aleatório = ", round(auc_teste, 3))
message("AUC espacial média = ", round(auc_espacial_medio, 3))
message("Diagnóstico de sobreajuste: ", classe_sobreajuste)
message("Diagnóstico espacial: ", diagnostico_espacial)

print(avaliacao)
