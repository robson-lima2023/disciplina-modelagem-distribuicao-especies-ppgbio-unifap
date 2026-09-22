source("scripts/_bootstrap.R")

# ============================================================
# Unidade 12 - Avaliação de modelos em SDM
# Script 04: Curvas ROC comparativas
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr",
  "readr",
  "ggplot2",
  "pROC",
  "purrr",
  "tibble",
  "tidyr",
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
dir.create("dados/unidade12/processados", recursive = TRUE, showWarnings = FALSE)

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
  stop("Nenhuma predição válida foi encontrada.")
}

# ------------------------------------------------------------
# 5. Função para gerar curva ROC por modelo
# ------------------------------------------------------------

gerar_roc <- function(df, nm) {
  
  df <- df |>
    filter(
      !is.na(pa),
      !is.na(pred),
      pa %in% c(0, 1),
      is.finite(pred)
    )
  
  if (length(unique(df$pa)) < 2 || length(unique(df$pred)) < 2) {
    warning("Não foi possível calcular ROC para o modelo: ", nm)
    
    return(
      tibble(
        modelo = nm,
        AUC = NA_real_,
        especificidade = NA_real_,
        sensibilidade = NA_real_,
        fpr = NA_real_,
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
  
  auc_val <- as.numeric(
    pROC::auc(roc_obj)
  )
  
  tibble(
    modelo = nm,
    AUC = auc_val,
    especificidade = rev(roc_obj$specificities),
    sensibilidade = rev(roc_obj$sensitivities),
    fpr = 1 - especificidade,
    n = nrow(df),
    presencas = sum(df$pa == 1),
    background = sum(df$pa == 0)
  )
}

# ------------------------------------------------------------
# 6. Calcular curvas ROC
# ------------------------------------------------------------

roc_df <- split(
  predicoes,
  predicoes$modelo
) |>
  purrr::imap_dfr(
    ~ gerar_roc(.x, .y)
  ) |>
  dplyr::filter(!is.na(AUC))

if (nrow(roc_df) == 0) {
  stop("Nenhuma curva ROC pôde ser calculada.")
}

# ------------------------------------------------------------
# 7. Resumo das curvas ROC
# ------------------------------------------------------------

resumo_auc <- roc_df |>
  distinct(
    modelo,
    AUC,
    n,
    presencas,
    background
  ) |>
  mutate(
    classe_AUC = case_when(
      AUC >= 0.90 ~ "Excelente",
      AUC >= 0.80 ~ "Muito boa",
      AUC >= 0.70 ~ "Aceitável",
      TRUE ~ "Fraca"
    )
  ) |>
  arrange(desc(AUC))

write_csv(
  resumo_auc,
  "tabelas/unidade12/resumo_auc_curvas_roc.csv"
)

# Ordem dos modelos na legenda e nos painéis
ordem_modelos <- resumo_auc$modelo

roc_df <- roc_df |>
  mutate(
    modelo = factor(
      modelo,
      levels = ordem_modelos
    )
  )

labels_auc <- resumo_auc |>
  mutate(
    modelo = factor(
      modelo,
      levels = ordem_modelos
    ),
    rotulo = paste0(
      as.character(modelo),
      " (AUC = ",
      round(AUC, 3),
      ")"
    )
  ) |>
  select(modelo, rotulo)

roc_df <- roc_df |>
  left_join(
    labels_auc,
    by = "modelo"
  )

write_csv(
  roc_df,
  "dados/unidade12/processados/curvas_roc_comparativas.csv"
)

# ------------------------------------------------------------
# 8. Figura ROC geral refinada
# ------------------------------------------------------------

# Paleta Okabe-Ito: segura para daltônicos e adequada para publicação
paleta_modelos <- c(
  "#000000", # preto
  "#E69F00", # laranja
  "#56B4E9", # azul claro
  "#009E73", # verde
  "#F0E442", # amarelo
  "#0072B2", # azul
  "#D55E00", # vermelho
  "#CC79A7"  # roxo
)

nomes_rotulos <- labels_auc$rotulo
names(paleta_modelos) <- nomes_rotulos[seq_along(paleta_modelos)]

g_roc <- ggplot(
  roc_df,
  aes(
    x = fpr,
    y = sensibilidade,
    color = rotulo
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "grey55",
    linewidth = 0.6
  ) +
  geom_line(
    linewidth = 1.15,
    alpha = 0.95
  ) +
  scale_color_manual(
    values = paleta_modelos,
    breaks = nomes_rotulos,
    name = "Modelo"
  ) +
  coord_equal(
    xlim = c(0, 1),
    ylim = c(0, 1),
    expand = FALSE
  ) +
  labs(
    title = expression("Curvas ROC comparativas para " * italic("Dinizia excelsa")),
    subtitle = "Desempenho discriminatório dos modelos no conjunto de teste",
    x = "1 - Especificidade",
    y = "Sensibilidade"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.30),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(
  "figuras/unidade12/unidade12_curvas_roc_comparativas.png",
  plot = g_roc,
  width = 9.5,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 9. Figura ROC facetada refinada
# ------------------------------------------------------------

g_roc_facet <- ggplot(
  roc_df,
  aes(
    x = fpr,
    y = sensibilidade
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "grey55",
    linewidth = 0.5
  ) +
  geom_line(
    linewidth = 1.1,
    color = "black"
  ) +
  coord_equal(
    xlim = c(0, 1),
    ylim = c(0, 1),
    expand = FALSE
  ) +
  facet_wrap(
    ~ rotulo,
    ncol = 3
  ) +
  labs(
    title = expression("Curvas ROC por modelo para " * italic("Dinizia excelsa")),
    subtitle = "Painéis individuais facilitam a comparação quando as curvas se sobrepõem",
    x = "1 - Especificidade",
    y = "Sensibilidade"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 8.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.25)
  )

ggsave(
  "figuras/unidade12/unidade12_curvas_roc_facetadas.png",
  plot = g_roc_facet,
  width = 12,
  height = 8,
  dpi = 600
)

# ------------------------------------------------------------
# 10. Mensagem final
# ------------------------------------------------------------

message("Curvas ROC comparativas geradas com sucesso.")
message("Modelos avaliados: ", paste(as.character(ordem_modelos), collapse = ", "))

print(resumo_auc)
