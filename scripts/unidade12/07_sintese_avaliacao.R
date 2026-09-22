source("scripts/_bootstrap.R")

# ============================================================
# Unidade 12 - Avaliação de modelos em SDM
# Script 07: Síntese e ranking dos modelos
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr",
  "readr",
  "ggplot2",
  "tidyr",
  "tibble",
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

arquivo_metricas <- "tabelas/unidade12/metricas_integradas_modelos.csv"

if (!file.exists(arquivo_metricas)) {
  stop("Tabela de métricas não encontrada. Execute primeiro o Script 03 da Unidade 12.")
}

metricas <- read_csv(
  arquivo_metricas,
  show_col_types = FALSE
) |>
  mutate(
    modelo = as.character(modelo)
  )

if (nrow(metricas) == 0) {
  stop("A tabela de métricas está vazia.")
}

# ------------------------------------------------------------
# 5. Garantir colunas esperadas
# ------------------------------------------------------------

colunas_necessarias <- c(
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

for (cc in colunas_necessarias) {
  if (!cc %in% names(metricas)) {
    metricas[[cc]] <- NA_real_
  }
}

metricas <- metricas |>
  mutate(
    across(
      all_of(colunas_necessarias),
      as.numeric
    )
  )

# ------------------------------------------------------------
# 6. Funções de normalização
# ------------------------------------------------------------

normalizar_maior_melhor <- function(x) {
  
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  
  if (max_x == min_x) {
    return(rep(1, length(x)))
  }
  
  (x - min_x) / (max_x - min_x)
}

normalizar_menor_melhor <- function(x) {
  
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  
  if (max_x == min_x) {
    return(rep(1, length(x)))
  }
  
  1 - ((x - min_x) / (max_x - min_x))
}

# ------------------------------------------------------------
# 7. Pesos das métricas
# ------------------------------------------------------------
# AUC, TSS e Kappa recebem maior peso por serem métricas
# centrais de discriminação e classificação em SDM.
# Brier é invertido, pois menor valor representa melhor calibração.
# Boyce só entra quando disponível.
# ------------------------------------------------------------

pesos_metricas <- tibble(
  metrica_normalizada = c(
    "AUC_n",
    "TSS_n",
    "Kappa_n",
    "Acuracia_n",
    "Sensibilidade_n",
    "Especificidade_n",
    "Precisao_n",
    "F1_n",
    "Brier_n",
    "Boyce_n"
  ),
  metrica_original = c(
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
  ),
  direcao = c(
    "maior_melhor",
    "maior_melhor",
    "maior_melhor",
    "maior_melhor",
    "maior_melhor",
    "maior_melhor",
    "maior_melhor",
    "maior_melhor",
    "menor_melhor",
    "maior_melhor"
  ),
  peso = c(
    0.18,
    0.18,
    0.15,
    0.08,
    0.10,
    0.10,
    0.06,
    0.06,
    0.09,
    0.10
  )
)

# Se Boyce estiver todo ausente, redistribuir o peso para AUC, TSS e Kappa
if (all(is.na(metricas$Boyce))) {
  
  pesos_metricas <- pesos_metricas |>
    mutate(
      peso = case_when(
        metrica_original == "Boyce" ~ 0,
        metrica_original == "AUC" ~ peso + 0.04,
        metrica_original == "TSS" ~ peso + 0.03,
        metrica_original == "Kappa" ~ peso + 0.03,
        TRUE ~ peso
      )
    )
}

pesos_metricas <- pesos_metricas |>
  mutate(
    peso = peso / sum(peso)
  )

write_csv(
  pesos_metricas,
  "tabelas/unidade12/pesos_metricas_ranking.csv"
)

# ------------------------------------------------------------
# 8. Normalizar métricas
# ------------------------------------------------------------

ranking <- metricas |>
  mutate(
    AUC_n = normalizar_maior_melhor(AUC),
    TSS_n = normalizar_maior_melhor(TSS),
    Kappa_n = normalizar_maior_melhor(Kappa),
    Acuracia_n = normalizar_maior_melhor(Acuracia),
    Sensibilidade_n = normalizar_maior_melhor(Sensibilidade),
    Especificidade_n = normalizar_maior_melhor(Especificidade),
    Precisao_n = normalizar_maior_melhor(Precisao),
    F1_n = normalizar_maior_melhor(F1),
    Brier_n = normalizar_menor_melhor(Brier),
    Boyce_n = normalizar_maior_melhor(Boyce)
  )

# ------------------------------------------------------------
# 9. Calcular score integrado ponderado
# ------------------------------------------------------------

metricas_norm <- pesos_metricas$metrica_normalizada

calcular_score <- function(df_linha, pesos_tab) {
  
  valores <- as.numeric(df_linha[, pesos_tab$metrica_normalizada])
  pesos <- pesos_tab$peso
  
  ok <- !is.na(valores)
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  sum(valores[ok] * pesos[ok]) / sum(pesos[ok])
}

ranking$score_integrado <- apply(
  ranking,
  1,
  function(linha) {
    
    linha_df <- as.data.frame(
      t(linha),
      stringsAsFactors = FALSE
    )
    
    names(linha_df) <- names(ranking)
    
    linha_df[metricas_norm] <- lapply(
      linha_df[metricas_norm],
      as.numeric
    )
    
    calcular_score(
      linha_df,
      pesos_metricas
    )
  }
)

ranking <- ranking |>
  arrange(desc(score_integrado)) |>
  mutate(
    posicao = row_number(),
    desempenho_integrado = case_when(
      score_integrado >= 0.80 ~ "Muito alto",
      score_integrado >= 0.60 ~ "Alto",
      score_integrado >= 0.40 ~ "Moderado",
      TRUE ~ "Baixo"
    )
  )

write_csv(
  ranking,
  "tabelas/unidade12/ranking_modelos_avaliacao.csv"
)

# ------------------------------------------------------------
# 10. Tabela longa do ranking
# ------------------------------------------------------------

ranking_long <- ranking |>
  select(
    modelo,
    posicao,
    score_integrado,
    desempenho_integrado,
    AUC,
    TSS,
    Kappa,
    Acuracia,
    Sensibilidade,
    Especificidade,
    Precisao,
    F1,
    Brier,
    Boyce,
    AUC_n,
    TSS_n,
    Kappa_n,
    Acuracia_n,
    Sensibilidade_n,
    Especificidade_n,
    Precisao_n,
    F1_n,
    Brier_n,
    Boyce_n
  ) |>
  pivot_longer(
    cols = -c(modelo, posicao, score_integrado, desempenho_integrado),
    names_to = "metrica",
    values_to = "valor"
  )

write_csv(
  ranking_long,
  "tabelas/unidade12/ranking_modelos_long.csv"
)

# ------------------------------------------------------------
# 11. Figura do ranking integrado
# ------------------------------------------------------------

g_ranking <- ggplot(
  ranking,
  aes(
    x = reorder(modelo, score_integrado),
    y = score_integrado
  )
) +
  geom_col(
    fill = "grey55",
    color = "grey25",
    linewidth = 0.15
  ) +
  geom_text(
    aes(
      label = paste0(
        "#",
        posicao,
        " | ",
        round(score_integrado, 2)
      )
    ),
    hjust = -0.08,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, min(1, max(ranking$score_integrado, na.rm = TRUE) + 0.12)),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = expression("Ranking integrado dos modelos para " * italic("Dinizia excelsa")),
    subtitle = "Score ponderado com AUC, TSS, Kappa, acurácia, sensibilidade, especificidade, precisão, F1 e Brier",
    x = NULL,
    y = "Score integrado"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade12/unidade12_ranking_modelos.png",
  plot = g_ranking,
  width = 9,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Heatmap das métricas normalizadas
# ------------------------------------------------------------

heatmap_df <- ranking |>
  select(
    modelo,
    posicao,
    AUC_n,
    TSS_n,
    Kappa_n,
    Acuracia_n,
    Sensibilidade_n,
    Especificidade_n,
    Precisao_n,
    F1_n,
    Brier_n,
    Boyce_n
  ) |>
  pivot_longer(
    cols = ends_with("_n"),
    names_to = "metrica",
    values_to = "valor_normalizado"
  ) |>
  left_join(
    pesos_metricas |>
      select(metrica_normalizada, metrica_original, peso),
    by = c("metrica" = "metrica_normalizada")
  ) |>
  filter(
    peso > 0,
    !is.na(valor_normalizado)
  ) |>
  mutate(
    modelo = factor(
      modelo,
      levels = ranking$modelo
    ),
    metrica_original = factor(
      metrica_original,
      levels = pesos_metricas$metrica_original[pesos_metricas$peso > 0]
    )
  )

write_csv(
  heatmap_df,
  "tabelas/unidade12/heatmap_metricas_normalizadas.csv"
)

g_heat <- ggplot(
  heatmap_df,
  aes(
    x = metrica_original,
    y = modelo,
    fill = valor_normalizado
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.4
  ) +
  geom_text(
    aes(label = round(valor_normalizado, 2)),
    size = 3
  ) +
  scale_fill_viridis_c(
    name = "Valor\nnormalizado",
    option = "viridis",
    limits = c(0, 1)
  ) +
  labs(
    title = expression("Desempenho normalizado dos modelos para " * italic("Dinizia excelsa")),
    subtitle = "Valores próximos de 1 indicam melhor desempenho relativo em cada métrica",
    x = "Métrica",
    y = "Modelo"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    axis.text.x = element_text(angle = 35, hjust = 1),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade12/unidade12_heatmap_metricas_normalizadas.png",
  plot = g_heat,
  width = 10,
  height = 6.5,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Ranking resumido para uso no texto
# ------------------------------------------------------------

ranking_resumo <- ranking |>
  select(
    posicao,
    modelo,
    score_integrado,
    desempenho_integrado,
    AUC,
    TSS,
    Kappa,
    Brier
  ) |>
  arrange(posicao)

write_csv(
  ranking_resumo,
  "tabelas/unidade12/ranking_modelos_resumo.csv"
)

# ------------------------------------------------------------
# 14. Mensagem final
# ------------------------------------------------------------

message("Síntese de avaliação e ranking concluídos com sucesso.")
message("Melhor modelo pelo score integrado: ", ranking$modelo[1])
message("Score integrado do melhor modelo: ", round(ranking$score_integrado[1], 3))

print(ranking_resumo)
