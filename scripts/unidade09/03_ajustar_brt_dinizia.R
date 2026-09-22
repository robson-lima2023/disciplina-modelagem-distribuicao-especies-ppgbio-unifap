source("scripts/_bootstrap.R")

# ============================================================
# Unidade 9 - BRT em SDM
# Script 03: Ajustar BRT para Dinizia excelsa
# ============================================================

pacotes <- c(
  "dplyr", "readr", "tidyr", "gbm",
  "ggplot2", "tibble", "stringr"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))
dir.create("figuras/unidade09", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade09", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade09", recursive = TRUE, showWarnings = FALSE)

arquivo_treino <- "dados/unidade09/processados/treino_brt_dinizia.csv"
arquivo_vars <- "resultados/unidade09/variaveis_brt.csv"

if (!file.exists(arquivo_treino)) {
  stop("Arquivo de treino não encontrado. Execute o Script 02 da Unidade 9.")
}

if (!file.exists(arquivo_vars)) {
  stop("Arquivo de variáveis do BRT não encontrado. Execute o Script 02 da Unidade 9.")
}

treino <- read_csv(arquivo_treino, show_col_types = FALSE)

vars <- read_csv(arquivo_vars, show_col_types = FALSE)$variavel
vars <- unique(vars)
vars <- vars[vars %in% names(treino)]

if (length(vars) < 2) {
  stop("Número insuficiente de variáveis ambientais para ajustar BRT.")
}

if (!"pa" %in% names(treino)) {
  stop("A variável resposta 'pa' não foi encontrada no treino.")
}

treino <- treino |>
  mutate(pa = as.numeric(pa)) |>
  select(pa, all_of(vars), everything()) |>
  drop_na(all_of(vars), pa)

if (!all(treino$pa %in% c(0, 1))) {
  stop("A variável 'pa' deve conter apenas 0 = background e 1 = presença.")
}

n_pres <- sum(treino$pa == 1)
n_back <- sum(treino$pa == 0)

if (n_pres == 0 || n_back == 0) {
  stop("O conjunto de treino precisa conter presenças e background.")
}

termos_brt <- paste0("`", vars, "`")

formula_brt <- as.formula(
  paste(
    "pa ~",
    paste(termos_brt, collapse = " + ")
  )
)

# ------------------------------------------------------------
# Hiperparâmetros conservadores
# ------------------------------------------------------------

n_trees_max <- 5000
interaction_depth <- 2
shrinkage <- 0.005
bag_fraction <- 0.65
n_minobsinnode <- max(10, floor(0.03 * n_pres))
cv_folds <- 5

set.seed(123)

modelo_brt <- gbm::gbm(
  formula = formula_brt,
  data = treino,
  distribution = "bernoulli",
  n.trees = n_trees_max,
  interaction.depth = interaction_depth,
  shrinkage = shrinkage,
  bag.fraction = bag_fraction,
  n.minobsinnode = n_minobsinnode,
  cv.folds = cv_folds,
  train.fraction = 1.0,
  keep.data = TRUE,
  verbose = FALSE
)

melhor_iter <- gbm::gbm.perf(
  modelo_brt,
  method = "cv",
  plot.it = FALSE
)

if (is.null(melhor_iter) || is.na(melhor_iter)) {
  warning("gbm.perf não retornou número ótimo de árvores. Usando n.trees máximo.")
  melhor_iter <- n_trees_max
}

saveRDS(
  modelo_brt,
  "resultados/unidade09/modelo_brt_dinizia.rds"
)

saveRDS(
  formula_brt,
  "resultados/unidade09/formula_brt_dinizia.rds"
)

parametros <- tibble(
  parametro = c(
    "n.trees.max",
    "best.trees",
    "interaction.depth",
    "shrinkage",
    "bag.fraction",
    "n.minobsinnode",
    "cv.folds",
    "n_treino",
    "n_presencas",
    "n_background",
    "n_variaveis"
  ),
  valor = c(
    as.character(n_trees_max),
    as.character(melhor_iter),
    as.character(interaction_depth),
    as.character(shrinkage),
    as.character(bag_fraction),
    as.character(n_minobsinnode),
    as.character(cv_folds),
    as.character(nrow(treino)),
    as.character(n_pres),
    as.character(n_back),
    as.character(length(vars))
  )
)

write_csv(
  parametros,
  "tabelas/unidade09/parametros_brt.csv"
)

write_csv(
  tibble(best_trees = melhor_iter),
  "resultados/unidade09/best_trees_brt.csv"
)

importancia_raw <- summary(
  modelo_brt,
  n.trees = melhor_iter,
  plotit = FALSE
)

importancia <- importancia_raw |>
  as_tibble() |>
  rename(
    variavel = var,
    importancia = rel.inf
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
  "tabelas/unidade09/importancia_variaveis_brt.csv"
)

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
    title = expression("Importância relativa das variáveis no BRT para " * italic("Dinizia excelsa")),
    subtitle = paste0("Número ótimo de árvores = ", melhor_iter),
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
  "figuras/unidade09/unidade09_importancia_variaveis_brt.png",
  plot = g,
  width = 8.5,
  height = 6,
  dpi = 600
)

resumo_brt <- tibble(
  modelo = "Boosted Regression Trees",
  dominio_calibracao = "Bioma Amazônia",
  n_trees_max = n_trees_max,
  best_trees = melhor_iter,
  interaction_depth = interaction_depth,
  shrinkage = shrinkage,
  bag_fraction = bag_fraction,
  n_minobsinnode = n_minobsinnode,
  cv_folds = cv_folds,
  n_treino = nrow(treino),
  n_presencas = n_pres,
  n_background = n_back,
  n_variaveis = length(vars)
)

write_csv(
  resumo_brt,
  "tabelas/unidade09/resumo_modelo_brt.csv"
)

variaveis_modelo <- tibble(
  ordem = seq_along(vars),
  variavel = vars
)

write_csv(
  variaveis_modelo,
  "tabelas/unidade09/variaveis_usadas_modelo_brt.csv"
)

message("BRT ajustado com sucesso.")
message("Número de variáveis utilizadas: ", length(vars))
message("Número ótimo de árvores: ", melhor_iter)

print(resumo_brt)

