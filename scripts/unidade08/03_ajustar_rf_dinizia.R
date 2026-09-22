source("scripts/_bootstrap.R")

# ============================================================
# Unidade 8 - Random Forest em SDM
# Script 03: Ajustar Random Forest com controle de sobreajuste
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr", "readr", "tidyr", "ranger",
  "ggplot2", "tibble", "stringr"
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
dir.create("resultados/unidade08", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_treino <- "dados/unidade08/processados/treino_rf_dinizia.csv"
arquivo_vars   <- "resultados/unidade08/variaveis_rf.csv"

if (!file.exists(arquivo_treino)) {
  stop("Arquivo de treino não encontrado. Execute o Script 02 da Unidade 8.")
}

if (!file.exists(arquivo_vars)) {
  stop("Arquivo de variáveis do RF não encontrado. Execute o Script 02 da Unidade 8.")
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
)$variavel

vars <- vars |> unique()
vars <- vars[vars %in% names(treino)]

if (length(vars) < 2) {
  stop("Número insuficiente de variáveis ambientais para ajustar Random Forest.")
}

if (!"pa_factor" %in% names(treino)) {
  stop("A variável 'pa_factor' não foi encontrada no treino.")
}

treino <- treino |>
  mutate(
    pa_factor = factor(
      pa_factor,
      levels = c("background", "presenca")
    )
  )

if (any(is.na(treino$pa_factor))) {
  stop("A variável 'pa_factor' possui valores fora dos níveis esperados.")
}

treino <- treino |>
  select(
    pa_factor,
    all_of(vars),
    everything()
  ) |>
  tidyr::drop_na(all_of(vars), pa_factor)

n_pres <- sum(treino$pa_factor == "presenca")
n_back <- sum(treino$pa_factor == "background")

if (n_pres == 0 || n_back == 0) {
  stop("O conjunto de treino precisa conter presenças e background.")
}

# ------------------------------------------------------------
# 6. Parâmetros conservadores contra sobreajuste
# ------------------------------------------------------------

p <- length(vars)

mtry_rf <- max(1, floor(sqrt(p)))

min_node <- max(
  10,
  floor(0.03 * n_pres)
)

sample_fraction <- 0.70
num_trees <- 1000

termos_rf <- paste0("`", vars, "`")

formula_rf <- as.formula(
  paste(
    "pa_factor ~",
    paste(termos_rf, collapse = " + ")
  )
)

# ------------------------------------------------------------
# 7. Ajuste do Random Forest
# ------------------------------------------------------------

set.seed(123)

modelo_rf <- ranger::ranger(
  formula = formula_rf,
  data = treino,
  probability = TRUE,
  num.trees = num_trees,
  mtry = mtry_rf,
  min.node.size = min_node,
  sample.fraction = sample_fraction,
  replace = TRUE,
  importance = "permutation",
  classification = TRUE,
  seed = 123,
  oob.error = TRUE
)

# ------------------------------------------------------------
# 8. Salvar modelo e fórmula
# ------------------------------------------------------------

saveRDS(
  modelo_rf,
  "resultados/unidade08/modelo_rf_dinizia.rds"
)

saveRDS(
  formula_rf,
  "resultados/unidade08/formula_rf_dinizia.rds"
)

# ------------------------------------------------------------
# 9. Tabela de parâmetros
# ------------------------------------------------------------

parametros <- tibble(
  parametro = c(
    "num.trees",
    "mtry",
    "min.node.size",
    "sample.fraction",
    "replace",
    "importance",
    "probability",
    "classification",
    "n_treino",
    "n_presencas",
    "n_background",
    "n_variaveis",
    "oob_prediction_error"
  ),
  valor = c(
    as.character(num_trees),
    as.character(mtry_rf),
    as.character(min_node),
    as.character(sample_fraction),
    "TRUE",
    "permutation",
    "TRUE",
    "TRUE",
    as.character(nrow(treino)),
    as.character(n_pres),
    as.character(n_back),
    as.character(p),
    as.character(modelo_rf$prediction.error)
  )
)

write_csv(
  parametros,
  "tabelas/unidade08/parametros_rf.csv"
)

# ------------------------------------------------------------
# 10. Importância das variáveis
# ------------------------------------------------------------

importancia <- tibble(
  variavel = names(modelo_rf$variable.importance),
  importancia = as.numeric(modelo_rf$variable.importance)
) |>
  mutate(
    importancia_relativa = importancia / max(importancia, na.rm = TRUE),
    variavel_legivel = variavel |>
      str_replace_all("_", " ") |>
      str_replace_all("bio", "BIO") |>
      str_to_sentence()
  ) |>
  arrange(desc(importancia))

write_csv(
  importancia,
  "tabelas/unidade08/importancia_variaveis_rf.csv"
)

# ------------------------------------------------------------
# 11. Gráfico de importância
# ------------------------------------------------------------

g <- ggplot(
  importancia,
  aes(
    x = reorder(variavel_legivel, importancia_relativa),
    y = importancia_relativa
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15
  ) +
  coord_flip() +
  labs(
    title = expression("Importância das variáveis no Random Forest para " * italic("Dinizia excelsa")),
    subtitle = "Importância relativa por permutação",
    x = NULL,
    y = "Importância relativa"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade08/unidade08_importancia_variaveis_rf.png",
  plot = g,
  width = 8.5,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Resumo do modelo
# ------------------------------------------------------------

resumo_rf <- tibble(
  modelo = "Random Forest",
  dominio_calibracao = "Bioma Amazônia",
  num_trees = num_trees,
  mtry = mtry_rf,
  min_node_size = min_node,
  sample_fraction = sample_fraction,
  n_treino = nrow(treino),
  n_presencas = n_pres,
  n_background = n_back,
  n_variaveis = p,
  oob_prediction_error = modelo_rf$prediction.error
)

write_csv(
  resumo_rf,
  "tabelas/unidade08/resumo_modelo_rf.csv"
)

variaveis_modelo <- tibble(
  ordem = seq_along(vars),
  variavel = vars
)

write_csv(
  variaveis_modelo,
  "tabelas/unidade08/variaveis_usadas_modelo_rf.csv"
)

# ------------------------------------------------------------
# 13. Mensagem final
# ------------------------------------------------------------

message("Random Forest ajustado com controle de sobreajuste.")
message("Número de variáveis utilizadas: ", p)
message("mtry: ", mtry_rf)
message("min.node.size: ", min_node)
message("Erro OOB: ", round(modelo_rf$prediction.error, 4))

print(resumo_rf)
