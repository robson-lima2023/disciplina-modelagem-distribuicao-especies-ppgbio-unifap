source("scripts/_bootstrap.R")

# ============================================================
# Unidade 8 - Random Forest em SDM
# Script 04: Avaliar Random Forest e diagnosticar sobreajuste
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr", "readr", "ranger", "ggplot2",
  "pROC", "tibble", "patchwork", "tidyr"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 2. Diretório raiz do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios de saída
# ------------------------------------------------------------

dir.create("figuras/unidade08", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade08", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/unidade08/processados", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_modelo <- "resultados/unidade08/modelo_rf_dinizia.rds"
arquivo_treino <- "dados/unidade08/processados/treino_rf_dinizia.csv"
arquivo_teste  <- "dados/unidade08/processados/teste_rf_dinizia.csv"
arquivo_dados  <- "dados/unidade08/processados/dados_rf_dinizia.csv"
arquivo_vars   <- "resultados/unidade08/variaveis_rf.csv"

arquivos <- c(
  arquivo_modelo,
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

treino <- read_csv(arquivo_treino, show_col_types = FALSE) |>
  mutate(
    pa = as.integer(pa),
    pa_factor = factor(pa_factor, levels = c("background", "presenca"))
  )

teste <- read_csv(arquivo_teste, show_col_types = FALSE) |>
  mutate(
    pa = as.integer(pa),
    pa_factor = factor(pa_factor, levels = c("background", "presenca"))
  )

dados <- read_csv(arquivo_dados, show_col_types = FALSE) |>
  mutate(
    pa = as.integer(pa),
    pa_factor = factor(pa_factor, levels = c("background", "presenca"))
  )

vars <- read_csv(arquivo_vars, show_col_types = FALSE)$variavel
vars <- unique(vars)
vars <- vars[vars %in% names(dados)]

if (length(vars) < 2) {
  stop("Número insuficiente de variáveis ambientais para avaliar o Random Forest.")
}

if (!all(c("pa", "pa_factor") %in% names(teste))) {
  stop("As colunas 'pa' e 'pa_factor' precisam existir no conjunto de teste.")
}

# ------------------------------------------------------------
# 6. Predições em treino e teste
# ------------------------------------------------------------

treino$pred_rf <- as.numeric(
  predict(modelo, data = treino)$predictions[, "presenca"]
)

teste$pred_rf <- as.numeric(
  predict(modelo, data = teste)$predictions[, "presenca"]
)

treino <- treino |>
  filter(!is.na(pa), !is.na(pred_rf), is.finite(pred_rf))

teste <- teste |>
  filter(!is.na(pa), !is.na(pred_rf), is.finite(pred_rf))

if (length(unique(teste$pa)) < 2) {
  stop("O conjunto de teste precisa conter presença e background.")
}

auc_treino <- as.numeric(
  pROC::auc(
    pROC::roc(
      response = treino$pa,
      predictor = treino$pred_rf,
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
      predictor = teste$pred_rf,
      levels = c(0, 1),
      direction = "<",
      quiet = TRUE
    )
  )
)

auc_oob <- NA_real_

if (!is.null(modelo$predictions)) {
  
  pred_oob <- modelo$predictions[, "presenca"]
  
  if (length(pred_oob) == nrow(treino)) {
    auc_oob <- as.numeric(
      pROC::auc(
        pROC::roc(
          response = treino$pa,
          predictor = pred_oob,
          levels = c(0, 1),
          direction = "<",
          quiet = TRUE
        )
      )
    )
  }
}

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
  function(th) calcular_metricas(teste$pa, teste$pred_rf, th)
) |>
  bind_rows() |>
  filter(!is.na(TSS))

melhor <- metricas_limiar |>
  arrange(desc(TSS), desc(sensibilidade), desc(especificidade)) |>
  slice(1)

teste$pred_bin_rf <- ifelse(
  teste$pred_rf >= melhor$limiar,
  1,
  0
)

# ------------------------------------------------------------
# 8. Diagnóstico de calibração e sobreajuste
# ------------------------------------------------------------

brier_teste <- mean(
  (teste$pa - teste$pred_rf)^2,
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
  drop_na(lon, lat, pa, pa_factor, all_of(vars))

n_blocos <- 5

coords_scaled <- scale(dados_blocos[, c("lon", "lat")])

dados_blocos$bloco_espacial <- kmeans(
  coords_scaled,
  centers = n_blocos,
  nstart = 50
)$cluster

termos_rf <- paste0("`", vars, "`")

formula_rf <- as.formula(
  paste(
    "pa_factor ~",
    paste(termos_rf, collapse = " + ")
  )
)

p <- length(vars)
mtry_rf <- max(1, floor(sqrt(p)))
min_node <- max(10, floor(0.03 * sum(dados_blocos$pa == 1)))

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
    
    mod_b <- ranger::ranger(
      formula = formula_rf,
      data = treino_b,
      probability = TRUE,
      num.trees = 500,
      mtry = mtry_rf,
      min.node.size = min_node,
      sample.fraction = 0.70,
      replace = TRUE,
      classification = TRUE,
      seed = 123
    )
    
    pred_b <- predict(
      mod_b,
      data = teste_b
    )$predictions[, "presenca"]
    
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
  modelo = "Random Forest",
  dominio_calibracao = "Bioma Amazônia",
  AUC_treino = auc_treino,
  AUC_OOB = auc_oob,
  AUC_teste_random_split = auc_teste,
  AUC_espacial_medio = auc_espacial_medio,
  AUC_espacial_sd = auc_espacial_sd,
  diferenca_treino_teste = auc_treino - auc_teste,
  diferenca_random_spatial = diferenca_random_spatial,
  diagnostico_sobreajuste = classe_sobreajuste,
  diagnostico_espacial = diagnostico_espacial,
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
  n_variaveis = p
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
    media = mean(pred_rf, na.rm = TRUE),
    mediana = median(pred_rf, na.rm = TRUE),
    minimo = min(pred_rf, na.rm = TRUE),
    maximo = max(pred_rf, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    classe = ifelse(pa == 1, "Presença", "Background")
  ) |>
  select(classe, everything(), -pa)

write_csv(avaliacao, "tabelas/unidade08/avaliacao_rf_dinizia.csv")
write_csv(metricas_limiar, "tabelas/unidade08/metricas_limiares_rf.csv")
write_csv(auc_blocos, "tabelas/unidade08/validacao_espacial_blocos_rf.csv")
write_csv(matriz_confusao, "tabelas/unidade08/matriz_confusao_rf.csv")
write_csv(resumo_predicoes, "tabelas/unidade08/resumo_predicoes_rf.csv")

teste_export <- teste |>
  select(lon, lat, pa, tipo, pred_rf, pred_bin_rf)

write_csv(
  teste_export,
  "dados/unidade08/processados/teste_predicoes_rf_dinizia.csv"
)

# ------------------------------------------------------------
# 11. Gráficos
# ------------------------------------------------------------

roc_teste <- pROC::roc(
  response = teste$pa,
  predictor = teste$pred_rf,
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
    title = expression("Curva ROC do Random Forest para " * italic("Dinizia excelsa")),
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
    title = "Seleção do limiar pelo TSS - Random Forest",
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
  conjunto = c("Treino", "OOB", "Teste aleatório", "Validação espacial"),
  AUC = c(auc_treino, auc_oob, auc_teste, auc_espacial_medio)
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
  aes(x = classe, y = pred_rf)
) +
  geom_boxplot(outlier.alpha = 0.35) +
  geom_hline(
    yintercept = melhor$limiar,
    linetype = "dashed",
    color = "grey30"
  ) +
  labs(
    title = "Distribuição das predições do Random Forest",
    subtitle = "Linha tracejada indica o limiar ótimo por TSS",
    x = NULL,
    y = "Adequabilidade predita"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("figuras/unidade08/unidade08_roc_rf.png", g_roc, width = 6.5, height = 6, dpi = 600)
ggsave("figuras/unidade08/unidade08_tss_limiar_rf.png", g_tss, width = 7, height = 5, dpi = 600)
ggsave("figuras/unidade08/unidade08_diagnostico_overfitting_rf.png", g_diag, width = 8, height = 5.5, dpi = 600)
ggsave("figuras/unidade08/unidade08_boxplot_predicoes_rf.png", g_box, width = 6.5, height = 5, dpi = 600)

g_patch <- (g_roc | g_tss) / (g_diag | g_box) +
  patchwork::plot_annotation(
    title = expression("Avaliação crítica do Random Forest para " * italic("Dinizia excelsa")),
    subtitle = "Inclui diagnóstico de sobreajuste, validação espacial por blocos e distribuição das predições"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade08/unidade08_avaliacao_rf_patchwork.png",
  plot = g_patch,
  width = 13,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Mensagem final
# ------------------------------------------------------------

message("Avaliação crítica do Random Forest concluída.")
message("AUC treino = ", round(auc_treino, 3))
message("AUC teste aleatório = ", round(auc_teste, 3))
message("AUC espacial média = ", round(auc_espacial_medio, 3))
message("Diagnóstico de sobreajuste: ", classe_sobreajuste)
message("Diagnóstico espacial: ", diagnostico_espacial)

print(avaliacao)
