source("scripts/_bootstrap.R")

# ============================================================
# Unidade 10 - MaxEnt em SDM
# Script 04: Ajustar modelo MaxEnt final
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr",
  "readr",
  "tidyr",
  "maxnet",
  "ggplot2",
  "tibble",
  "pROC",
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

dir.create("figuras/unidade10", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade10", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade10", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_treino <- "dados/unidade10/processados/treino_maxent_dinizia.csv"
arquivo_vars <- "resultados/unidade10/variaveis_maxent.csv"
arquivo_melhor <- "resultados/unidade10/melhor_tuning_maxent.csv"

arquivos <- c(
  arquivo_treino,
  arquivo_vars,
  arquivo_melhor
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

treino <- read_csv(
  arquivo_treino,
  show_col_types = FALSE
)

vars <- read_csv(
  arquivo_vars,
  show_col_types = FALSE
)$variavel |>
  unique()

melhor <- read_csv(
  arquivo_melhor,
  show_col_types = FALSE
)

vars <- vars[vars %in% names(treino)]

if (length(vars) < 2) {
  stop("Número insuficiente de variáveis ambientais para ajustar MaxEnt.")
}

if (!"pa" %in% names(treino)) {
  stop("A variável resposta 'pa' não foi encontrada no treino.")
}

treino <- treino |>
  mutate(pa = as.numeric(pa)) |>
  drop_na(pa, all_of(vars))

if (!all(treino$pa %in% c(0, 1))) {
  stop("A variável 'pa' deve conter apenas 0 = background e 1 = presença.")
}

if (length(unique(treino$pa)) < 2) {
  stop("O conjunto de treino precisa conter presenças e background.")
}

x_treino <- treino |>
  select(all_of(vars)) |>
  as.data.frame()

for (v in vars) {
  x_treino[[v]] <- as.numeric(x_treino[[v]])
}

p_treino <- as.numeric(treino$pa)

fc_final <- melhor$fc[1]
regmult_final <- as.numeric(melhor$regmult[1])

if (length(fc_final) == 0 || is.na(fc_final) || fc_final == "") {
  stop("Feature class final inválida em melhor_tuning_maxent.csv.")
}

if (length(regmult_final) == 0 || is.na(regmult_final)) {
  stop("Regularization multiplier final inválido em melhor_tuning_maxent.csv.")
}

# ------------------------------------------------------------
# 6. Ajustar modelo MaxEnt final
# ------------------------------------------------------------

formula_maxent <- maxnet::maxnet.formula(
  p = p_treino,
  data = x_treino,
  classes = fc_final
)

modelo_maxent <- maxnet::maxnet(
  p = p_treino,
  data = x_treino,
  f = formula_maxent,
  regmult = regmult_final
)

saveRDS(
  modelo_maxent,
  "resultados/unidade10/modelo_maxent_dinizia.rds"
)

saveRDS(
  formula_maxent,
  "resultados/unidade10/formula_maxent_dinizia.rds"
)

# ------------------------------------------------------------
# 7. Predição e AUC no conjunto de treino
# ------------------------------------------------------------

pred_base <- as.numeric(
  predict(
    modelo_maxent,
    newdata = x_treino,
    type = "cloglog",
    clamp = FALSE
  )
)

auc_base <- as.numeric(
  pROC::auc(
    pROC::roc(
      response = p_treino,
      predictor = pred_base,
      levels = c(0, 1),
      direction = "<",
      quiet = TRUE
    )
  )
)

# ------------------------------------------------------------
# 8. Parâmetros do modelo final
# ------------------------------------------------------------

parametros <- tibble(
  parametro = c(
    "feature_classes",
    "regularization_multiplier",
    "n_coeficientes_total",
    "n_coeficientes_nao_zero",
    "auc_treino",
    "n_treino",
    "n_presencas",
    "n_background",
    "n_variaveis"
  ),
  valor = c(
    as.character(fc_final),
    as.character(regmult_final),
    as.character(length(modelo_maxent$betas)),
    as.character(sum(modelo_maxent$betas != 0)),
    as.character(auc_base),
    as.character(nrow(treino)),
    as.character(sum(treino$pa == 1)),
    as.character(sum(treino$pa == 0)),
    as.character(length(vars))
  )
)

write_csv(
  parametros,
  "tabelas/unidade10/parametros_maxent_final.csv"
)

coeficientes <- tibble(
  termo = names(modelo_maxent$betas),
  beta = as.numeric(modelo_maxent$betas)
) |>
  mutate(
    ativo = beta != 0,
    abs_beta = abs(beta)
  ) |>
  arrange(desc(abs_beta))

write_csv(
  coeficientes,
  "tabelas/unidade10/coeficientes_maxent_dinizia.csv"
)

# ------------------------------------------------------------
# 9. Rótulos ecológicos das variáveis
# ------------------------------------------------------------

nome_legivel <- function(x) {
  
  dicionario <- c(
    bio1_temp_media_anual = "BIO1 - Temperatura média anual",
    bio2_amplitude_termica_diaria = "BIO2 - Amplitude térmica média diária",
    bio3_isotermalidade = "BIO3 - Isotermalidade",
    bio4_sazonalidade_temperatura = "BIO4 - Sazonalidade da temperatura",
    bio5_temp_max_mes_quente = "BIO5 - Temperatura máxima do mês mais quente",
    bio6_temp_min_mes_frio = "BIO6 - Temperatura mínima do mês mais frio",
    bio7_amplitude_termica_anual = "BIO7 - Amplitude térmica anual",
    bio8_temp_media_trimestre_umido = "BIO8 - Temperatura média do trimestre mais úmido",
    bio9_temp_media_trimestre_seco = "BIO9 - Temperatura média do trimestre mais seco",
    bio10_temp_media_trimestre_quente = "BIO10 - Temperatura média do trimestre mais quente",
    bio11_temp_media_trimestre_frio = "BIO11 - Temperatura média do trimestre mais frio",
    bio12_precipitacao_anual = "BIO12 - Precipitação anual",
    bio13_precipitacao_mes_umido = "BIO13 - Precipitação do mês mais úmido",
    bio14_precipitacao_mes_seco = "BIO14 - Precipitação do mês mais seco",
    bio15_sazonalidade_precipitacao = "BIO15 - Sazonalidade da precipitação",
    bio16_precipitacao_trimestre_umido = "BIO16 - Precipitação do trimestre mais úmido",
    bio17_precipitacao_trimestre_seco = "BIO17 - Precipitação do trimestre mais seco",
    bio18_precipitacao_trimestre_quente = "BIO18 - Precipitação do trimestre mais quente",
    bio19_precipitacao_trimestre_frio = "BIO19 - Precipitação do trimestre mais frio",
    elevacao = "Elevação",
    declividade = "Declividade",
    orientacao = "Orientação"
  )
  
  if (x %in% names(dicionario)) {
    return(dicionario[[x]])
  }
  
  x |>
    str_replace_all("_", " ") |>
    str_replace_all("bio", "BIO") |>
    str_to_sentence()
}

# ------------------------------------------------------------
# 10. Importância das variáveis por amplitude de resposta
# ------------------------------------------------------------

set.seed(123)

base_media <- x_treino |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(vars),
      mean,
      na.rm = TRUE
    )
  ) |>
  as.data.frame()

imp <- lapply(vars, function(v) {
  
  valores <- x_treino[[v]]
  
  seq_v <- seq(
    stats::quantile(valores, 0.01, na.rm = TRUE),
    stats::quantile(valores, 0.99, na.rm = TRUE),
    length.out = 150
  )
  
  novo <- base_media[rep(1, length(seq_v)), , drop = FALSE]
  novo[[v]] <- as.numeric(seq_v)
  
  for (vv in vars) {
    novo[[vv]] <- as.numeric(novo[[vv]])
  }
  
  pred_v <- as.numeric(
    predict(
      modelo_maxent,
      newdata = novo[, vars, drop = FALSE],
      type = "cloglog",
      clamp = FALSE
    )
  )
  
  tibble(
    variavel = v,
    importancia = max(pred_v, na.rm = TRUE) - min(pred_v, na.rm = TRUE)
  )
}) |>
  bind_rows() |>
  mutate(
    importancia_relativa = ifelse(
      sum(importancia, na.rm = TRUE) > 0,
      100 * importancia / sum(importancia, na.rm = TRUE),
      0
    ),
    variavel_legivel = vapply(
      variavel,
      nome_legivel,
      character(1)
    )
  ) |>
  arrange(desc(importancia_relativa))

write_csv(
  imp,
  "tabelas/unidade10/importancia_variaveis_maxent.csv"
)

# ------------------------------------------------------------
# 11. Figura de importância das variáveis
# ------------------------------------------------------------

g_importancia <- ggplot(
  imp,
  aes(
    x = reorder(variavel_legivel, importancia_relativa),
    y = importancia_relativa
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15,
    fill = "grey55"
  ) +
  coord_flip() +
  labs(
    title = expression("Importância das variáveis no MaxEnt para " * italic("Dinizia excelsa")),
    subtitle = "Importância estimada pela amplitude das curvas de resposta parcial",
    x = NULL,
    y = "Importância relativa (%)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade10/unidade10_importancia_variaveis_maxent.png",
  plot = g_importancia,
  width = 8.5,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Resumo do modelo final
# ------------------------------------------------------------

resumo_maxent <- tibble(
  modelo = "MaxEnt",
  dominio_calibracao = "Bioma Amazônia",
  feature_classes = fc_final,
  regularization_multiplier = regmult_final,
  auc_treino = auc_base,
  n_treino = nrow(treino),
  n_presencas = sum(treino$pa == 1),
  n_background = sum(treino$pa == 0),
  n_variaveis = length(vars),
  n_coeficientes_total = length(modelo_maxent$betas),
  n_coeficientes_nao_zero = sum(modelo_maxent$betas != 0)
)

write_csv(
  resumo_maxent,
  "tabelas/unidade10/resumo_modelo_maxent.csv"
)

variaveis_modelo <- tibble(
  ordem = seq_along(vars),
  variavel = vars,
  variavel_legivel = vapply(
    vars,
    nome_legivel,
    character(1)
  )
)

write_csv(
  variaveis_modelo,
  "tabelas/unidade10/variaveis_usadas_modelo_maxent.csv"
)

# ------------------------------------------------------------
# 13. Mensagem final
# ------------------------------------------------------------

message("Modelo MaxEnt final ajustado com sucesso.")
message("Feature classes: ", fc_final)
message("Regularization multiplier: ", regmult_final)
message("AUC treino: ", round(auc_base, 3))
message("Número de variáveis utilizadas: ", length(vars))
message("Coeficientes ativos: ", sum(modelo_maxent$betas != 0))

print(resumo_maxent)
