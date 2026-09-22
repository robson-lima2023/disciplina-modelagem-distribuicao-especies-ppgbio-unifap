source("scripts/_bootstrap.R")

# ============================================================
# Unidade 12 - Avaliação de modelos em SDM
# Script 03: Calcular métricas integradas
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr",
  "readr",
  "tidyr",
  "ggplot2",
  "pROC",
  "tibble",
  "patchwork",
  "stringr"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 2. Diretório raiz do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios de saída
# ------------------------------------------------------------

dir.create("figuras/unidade12", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade12", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade12", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Entrada
# ------------------------------------------------------------

arquivo_pred <- "dados/unidade12/processados/predicoes_teste_modelos.csv"

if (!file.exists(arquivo_pred)) {
  stop("Arquivo de predições não encontrado. Execute antes o Script 02 da Unidade 12.")
}

predicoes <- read_csv(
  arquivo_pred,
  show_col_types = FALSE
) |>
  mutate(
    modelo = as.character(modelo),
    pa = as.numeric(pa),
    pred = as.numeric(pred)
  ) |>
  filter(
    !is.na(modelo),
    !is.na(pa),
    !is.na(pred),
    is.finite(pred),
    pa %in% c(0, 1)
  ) |>
  mutate(
    pred = pmin(pmax(pred, 0), 1)
  )

if (nrow(predicoes) == 0) {
  stop("A tabela de predições está vazia após remover valores ausentes.")
}

# ------------------------------------------------------------
# 5. Função opcional para calcular Boyce Index
# ------------------------------------------------------------

calcular_boyce <- function(df) {
  
  if (!requireNamespace("ecospat", quietly = TRUE)) {
    return(NA_real_)
  }
  
  obs <- df$pred[df$pa == 1]
  fit <- df$pred
  
  if (length(obs) < 5) {
    return(NA_real_)
  }
  
  if (length(unique(obs)) < 3 || length(unique(fit)) < 5) {
    return(NA_real_)
  }
  
  boyce <- tryCatch(
    {
      b <- ecospat::ecospat.boyce(
        fit = fit,
        obs = obs,
        nclass = 0,
        window.w = "default",
        res = 100,
        PEplot = FALSE
      )
      
      if ("Spearman.cor" %in% names(b)) {
        as.numeric(b$Spearman.cor)
      } else {
        NA_real_
      }
    },
    error = function(e) {
      NA_real_
    }
  )
  
  boyce
}

# ------------------------------------------------------------
# 6. Função para métricas por limiar
# ------------------------------------------------------------

calcular_por_limiar <- function(obs, pred, th) {
  
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
  
  precision <- ifelse((TP + FP) > 0, TP / (TP + FP), NA_real_)
  f1 <- ifelse(
    !is.na(precision) && !is.na(sens) && (precision + sens) > 0,
    2 * precision * sens / (precision + sens),
    NA_real_
  )
  
  tibble(
    limiar = th,
    TP = TP,
    TN = TN,
    FP = FP,
    FN = FN,
    sensibilidade = sens,
    especificidade = esp,
    acuracia = acc,
    TSS = tss,
    Kappa = kappa,
    precisao = precision,
    F1 = f1
  )
}

# ------------------------------------------------------------
# 7. Função para calcular métricas por modelo
# ------------------------------------------------------------

calcular_metricas_modelo <- function(df) {
  
  df <- df |>
    filter(
      !is.na(pa),
      !is.na(pred),
      pa %in% c(0, 1)
    )
  
  if (nrow(df) == 0 || length(unique(df$pa)) < 2 || length(unique(df$pred)) < 2) {
    return(
      tibble(
        AUC = NA_real_,
        limiar_TSS = NA_real_,
        TSS = NA_real_,
        Kappa = NA_real_,
        Acuracia = NA_real_,
        Sensibilidade = NA_real_,
        Especificidade = NA_real_,
        Precisao = NA_real_,
        F1 = NA_real_,
        Brier = NA_real_,
        Boyce = NA_real_,
        TP = NA_integer_,
        TN = NA_integer_,
        FP = NA_integer_,
        FN = NA_integer_,
        n = nrow(df),
        presencas = sum(df$pa == 1),
        background = sum(df$pa == 0)
      )
    )
  }
  
  roc_obj <- pROC::roc(
    response = df$pa,
    predictor = df$pred,
    levels = c(0, 1),
    direction = "<",
    quiet = TRUE
  )
  
  auc_val <- as.numeric(pROC::auc(roc_obj))
  
  lim_min <- max(0.001, min(df$pred, na.rm = TRUE))
  lim_max <- min(0.999, max(df$pred, na.rm = TRUE))
  
  if (lim_min >= lim_max) {
    limiares <- 0.5
  } else {
    limiares <- seq(lim_min, lim_max, length.out = 200)
  }
  
  tab_limiar <- lapply(
    limiares,
    function(th) calcular_por_limiar(df$pa, df$pred, th)
  ) |>
    bind_rows()
  
  melhor <- tab_limiar |>
    filter(!is.na(TSS)) |>
    arrange(desc(TSS), desc(sensibilidade), desc(especificidade)) |>
    slice(1)
  
  if (nrow(melhor) == 0) {
    melhor <- tibble(
      limiar = NA_real_,
      TP = NA_integer_,
      TN = NA_integer_,
      FP = NA_integer_,
      FN = NA_integer_,
      sensibilidade = NA_real_,
      especificidade = NA_real_,
      acuracia = NA_real_,
      TSS = NA_real_,
      Kappa = NA_real_,
      precisao = NA_real_,
      F1 = NA_real_
    )
  }
  
  brier <- mean(
    (df$pa - df$pred)^2,
    na.rm = TRUE
  )
  
  boyce <- calcular_boyce(df)
  
  tibble(
    AUC = auc_val,
    limiar_TSS = melhor$limiar,
    TSS = melhor$TSS,
    Kappa = melhor$Kappa,
    Acuracia = melhor$acuracia,
    Sensibilidade = melhor$sensibilidade,
    Especificidade = melhor$especificidade,
    Precisao = melhor$precisao,
    F1 = melhor$F1,
    Brier = brier,
    Boyce = boyce,
    TP = melhor$TP,
    TN = melhor$TN,
    FP = melhor$FP,
    FN = melhor$FN,
    n = nrow(df),
    presencas = sum(df$pa == 1),
    background = sum(df$pa == 0)
  )
}

# ------------------------------------------------------------
# 8. Calcular métricas integradas
# ------------------------------------------------------------

metricas <- predicoes |>
  group_by(modelo) |>
  group_modify(~ calcular_metricas_modelo(.x)) |>
  ungroup() |>
  mutate(
    classe_AUC = case_when(
      AUC >= 0.90 ~ "Excelente",
      AUC >= 0.80 ~ "Muito boa",
      AUC >= 0.70 ~ "Aceitável",
      TRUE ~ "Fraca"
    ),
    classe_TSS = case_when(
      TSS >= 0.70 ~ "Excelente",
      TSS >= 0.50 ~ "Boa",
      TSS >= 0.30 ~ "Moderada",
      TRUE ~ "Baixa"
    ),
    classe_Kappa = case_when(
      Kappa >= 0.80 ~ "Quase perfeita",
      Kappa >= 0.60 ~ "Substancial",
      Kappa >= 0.40 ~ "Moderada",
      Kappa >= 0.20 ~ "Fraca",
      TRUE ~ "Muito baixa"
    )
  ) |>
  arrange(desc(AUC))

write_csv(
  metricas,
  "tabelas/unidade12/metricas_integradas_modelos.csv"
)

# ------------------------------------------------------------
# 9. Exportar limiares e matriz de confusão
# ------------------------------------------------------------

limiares_integrados <- metricas |>
  select(
    modelo,
    limiar_TSS,
    AUC,
    TSS,
    Sensibilidade,
    Especificidade
  ) |>
  arrange(desc(AUC))

write_csv(
  limiares_integrados,
  "tabelas/unidade12/limiares_integrados_modelos.csv"
)

matriz_confusao <- metricas |>
  select(modelo, TP, TN, FP, FN) |>
  pivot_longer(
    cols = c(TP, TN, FP, FN),
    names_to = "classe",
    values_to = "n"
  )

write_csv(
  matriz_confusao,
  "tabelas/unidade12/matriz_confusao_integrada_modelos.csv"
)

# ------------------------------------------------------------
# 10. Tabela longa para figuras
# ------------------------------------------------------------

metricas_para_plot <- metricas |>
  select(
    modelo,
    AUC,
    TSS,
    Kappa,
    Acuracia,
    Sensibilidade,
    Especificidade,
    Precisao,
    F1,
    Brier,
    Boyce
  )

if (all(is.na(metricas_para_plot$Boyce))) {
  metricas_para_plot <- metricas_para_plot |>
    select(-Boyce)
}

metricas_long <- metricas_para_plot |>
  pivot_longer(
    cols = -modelo,
    names_to = "metrica",
    values_to = "valor"
  ) |>
  filter(!is.na(valor)) |>
  mutate(
    metrica = factor(
      metrica,
      levels = c(
        "AUC",
        "TSS",
        "Kappa",
        "Acuracia",
        "Sensibilidade",
        "Especificidade",
        "Precisao",
        "F1",
        "Brier",
        "Boyce"
      )
    )
  )

write_csv(
  metricas_long,
  "tabelas/unidade12/metricas_integradas_modelos_long.csv"
)

# ------------------------------------------------------------
# 11. Figura comparativa das métricas
# ------------------------------------------------------------

g_metricas <- ggplot(
  metricas_long,
  aes(
    x = reorder(modelo, valor),
    y = valor
  )
) +
  geom_col(
    fill = "grey55",
    color = "grey25",
    linewidth = 0.15
  ) +
  coord_flip() +
  facet_wrap(
    ~ metrica,
    scales = "free_x",
    ncol = 3
  ) +
  labs(
    title = expression("Avaliação comparativa dos modelos para " * italic("Dinizia excelsa")),
    subtitle = "Métricas calculadas no conjunto de teste; Brier deve ser interpretado como menor = melhor",
    x = NULL,
    y = "Valor da métrica"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade12/unidade12_comparacao_metricas.png",
  plot = g_metricas,
  width = 12,
  height = 8,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Figura sintética com métricas principais
# ------------------------------------------------------------

metricas_principais <- metricas_long |>
  filter(
    metrica %in% c("AUC", "TSS", "Kappa", "Brier")
  )

g_principal <- ggplot(
  metricas_principais,
  aes(
    x = reorder(modelo, valor),
    y = valor
  )
) +
  geom_col(
    fill = "grey55",
    color = "grey25",
    linewidth = 0.15
  ) +
  coord_flip() +
  facet_wrap(
    ~ metrica,
    scales = "free_x",
    ncol = 2
  ) +
  labs(
    title = expression("Métricas principais de avaliação para " * italic("Dinizia excelsa")),
    subtitle = "AUC, TSS e Kappa: maior = melhor; Brier: menor = melhor",
    x = NULL,
    y = "Valor"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade12/unidade12_metricas_principais.png",
  plot = g_principal,
  width = 10,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Ranking composto
# ------------------------------------------------------------

ranking <- metricas |>
  mutate(
    AUC_rank = rank(-AUC, ties.method = "average"),
    TSS_rank = rank(-TSS, ties.method = "average"),
    Kappa_rank = rank(-Kappa, ties.method = "average"),
    Brier_rank = rank(Brier, ties.method = "average"),
    ranking_medio = rowMeans(
      cbind(AUC_rank, TSS_rank, Kappa_rank, Brier_rank),
      na.rm = TRUE
    )
  ) |>
  arrange(ranking_medio)

write_csv(
  ranking,
  "tabelas/unidade12/ranking_integrado_modelos.csv"
)

g_ranking <- ggplot(
  ranking,
  aes(
    x = reorder(modelo, -ranking_medio),
    y = ranking_medio
  )
) +
  geom_col(
    fill = "grey55",
    color = "grey25",
    linewidth = 0.15
  ) +
  coord_flip() +
  labs(
    title = "Ranking integrado dos modelos",
    subtitle = "Menor ranking médio indica melhor desempenho conjunto",
    x = NULL,
    y = "Ranking médio"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade12/unidade12_ranking_integrado_modelos.png",
  plot = g_ranking,
  width = 8,
  height = 5.5,
  dpi = 600
)

# ------------------------------------------------------------
# 14. Mensagem final
# ------------------------------------------------------------

message("Métricas integradas calculadas com sucesso.")
message("Modelos avaliados: ", paste(metricas$modelo, collapse = ", "))

print(metricas)
