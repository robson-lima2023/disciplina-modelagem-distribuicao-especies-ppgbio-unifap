source("scripts/_bootstrap.R")

# ============================================================
# Unidade 12 - Avaliação de modelos em SDM
# Script 05: Matrizes de confusão
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
dir.create("dados/unidade12/processados", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_pred <- "dados/unidade12/processados/predicoes_teste_modelos.csv"
arquivo_metricas <- "tabelas/unidade12/metricas_integradas_modelos.csv"

if (!file.exists(arquivo_pred)) {
  stop("Arquivo de predições não encontrado. Execute o Script 02 da Unidade 12.")
}

if (!file.exists(arquivo_metricas)) {
  stop("Arquivo de métricas não encontrado. Execute o Script 03 da Unidade 12.")
}

# ------------------------------------------------------------
# 5. Leitura dos dados
# ------------------------------------------------------------

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

metricas <- read_csv(
  arquivo_metricas,
  show_col_types = FALSE
) |>
  mutate(
    modelo = as.character(modelo),
    limiar_TSS = as.numeric(limiar_TSS)
  )

if (nrow(predicoes) == 0) {
  stop("Nenhuma predição válida foi encontrada.")
}

if (!"limiar_TSS" %in% names(metricas)) {
  stop("A tabela de métricas precisa conter a coluna 'limiar_TSS'.")
}

# ------------------------------------------------------------
# 6. Preparar classificações observadas e preditas
# ------------------------------------------------------------

dados_classificados <- predicoes |>
  left_join(
    metricas |>
      select(modelo, limiar_TSS),
    by = "modelo"
  ) |>
  mutate(
    limiar_TSS = ifelse(
      is.na(limiar_TSS) | !is.finite(limiar_TSS),
      0.5,
      limiar_TSS
    ),
    observado = ifelse(pa == 1, "Presença", "Background"),
    predito = ifelse(pred >= limiar_TSS, "Adequado", "Inadequado"),
    observado = factor(
      observado,
      levels = c("Presença", "Background")
    ),
    predito = factor(
      predito,
      levels = c("Adequado", "Inadequado")
    )
  )

write_csv(
  dados_classificados,
  "dados/unidade12/processados/predicoes_teste_classificadas.csv"
)

# ------------------------------------------------------------
# 7. Matrizes de confusão completas
# ------------------------------------------------------------

confusao <- dados_classificados |>
  count(
    modelo,
    observado,
    predito,
    name = "n"
  ) |>
  group_by(modelo) |>
  complete(
    observado = factor(
      c("Presença", "Background"),
      levels = c("Presença", "Background")
    ),
    predito = factor(
      c("Adequado", "Inadequado"),
      levels = c("Adequado", "Inadequado")
    ),
    fill = list(n = 0)
  ) |>
  mutate(
    total_modelo = sum(n),
    proporcao_total = ifelse(total_modelo > 0, n / total_modelo, NA_real_),
    proporcao_linha = ifelse(sum(n) > 0, n / sum(n), NA_real_)
  ) |>
  ungroup() |>
  mutate(
    classe_confusao = case_when(
      observado == "Presença" & predito == "Adequado" ~ "TP",
      observado == "Presença" & predito == "Inadequado" ~ "FN",
      observado == "Background" & predito == "Adequado" ~ "FP",
      observado == "Background" & predito == "Inadequado" ~ "TN",
      TRUE ~ NA_character_
    ),
    rotulo = paste0(
      n,
      "\n",
      round(100 * proporcao_total, 1),
      "%"
    )
  )

write_csv(
  confusao,
  "tabelas/unidade12/matrizes_confusao_modelos.csv"
)

# ------------------------------------------------------------
# 8. Métricas derivadas das matrizes de confusão
# ------------------------------------------------------------

confusao_wide <- confusao |>
  select(modelo, classe_confusao, n) |>
  pivot_wider(
    names_from = classe_confusao,
    values_from = n,
    values_fill = 0
  )

metricas_confusao <- confusao_wide |>
  mutate(
    total = TP + TN + FP + FN,
    sensibilidade = ifelse((TP + FN) > 0, TP / (TP + FN), NA_real_),
    especificidade = ifelse((TN + FP) > 0, TN / (TN + FP), NA_real_),
    acuracia = ifelse(total > 0, (TP + TN) / total, NA_real_),
    precisao = ifelse((TP + FP) > 0, TP / (TP + FP), NA_real_),
    F1 = ifelse(
      !is.na(precisao) & !is.na(sensibilidade) & (precisao + sensibilidade) > 0,
      2 * precisao * sensibilidade / (precisao + sensibilidade),
      NA_real_
    ),
    taxa_falso_positivo = ifelse((FP + TN) > 0, FP / (FP + TN), NA_real_),
    taxa_falso_negativo = ifelse((FN + TP) > 0, FN / (FN + TP), NA_real_)
  ) |>
  arrange(desc(acuracia))

write_csv(
  metricas_confusao,
  "tabelas/unidade12/metricas_matrizes_confusao.csv"
)

# ------------------------------------------------------------
# 9. Ordenar modelos pela AUC
# ------------------------------------------------------------

ordem_modelos <- metricas |>
  arrange(desc(AUC)) |>
  pull(modelo)

confusao <- confusao |>
  mutate(
    modelo = factor(
      modelo,
      levels = ordem_modelos
    )
  )

# ------------------------------------------------------------
# 10. Figura das matrizes de confusão
# ------------------------------------------------------------

g_confusao <- ggplot(
  confusao,
  aes(
    x = predito,
    y = observado,
    fill = proporcao_total
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.8
  ) +
  geom_text(
    aes(label = rotulo),
    size = 3.8,
    lineheight = 0.9,
    fontface = "bold"
  ) +
  facet_wrap(
    ~ modelo,
    ncol = 3
  ) +
  scale_fill_viridis_c(
    name = "Proporção",
    option = "viridis",
    limits = c(0, max(confusao$proporcao_total, na.rm = TRUE)),
    na.value = NA
  ) +
  labs(
    title = "Matrizes de confusão dos modelos",
    subtitle = "Classificação binária usando o limiar ótimo por TSS; valores indicam n e proporção total",
    x = "Classe predita",
    y = "Classe observada"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

ggsave(
  "figuras/unidade12/unidade12_matrizes_confusao.png",
  plot = g_confusao,
  width = 12,
  height = 8,
  dpi = 600
)

# ------------------------------------------------------------
# 11. Figura sintética TP, TN, FP e FN
# ------------------------------------------------------------

g_barras <- ggplot(
  confusao,
  aes(
    x = modelo,
    y = n,
    fill = classe_confusao
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15,
    position = "stack"
  ) +
  coord_flip() +
  labs(
    title = "Componentes das matrizes de confusão por modelo",
    subtitle = "TP = presença correta; TN = background correto; FP/FN = erros de classificação",
    x = NULL,
    y = "Número de pontos",
    fill = "Classe"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade12/unidade12_componentes_confusao.png",
  plot = g_barras,
  width = 9,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Mensagem final
# ------------------------------------------------------------

message("Matrizes de confusão geradas com sucesso.")
message("Modelos avaliados: ", paste(as.character(unique(confusao$modelo)), collapse = ", "))

print(metricas_confusao)
