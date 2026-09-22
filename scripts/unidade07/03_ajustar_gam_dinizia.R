source("scripts/_bootstrap.R")

# ============================================================
# Unidade 7 - GAM em SDM
# Script 03: Ajustar GAM para Dinizia excelsa
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr", "readr", "mgcv", "ggplot2",
  "broom", "tibble", "stringr"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 2. Diretório raiz do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios de saída
# ------------------------------------------------------------

dir.create("figuras/unidade07", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade07", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade07", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_treino <- "dados/unidade07/processados/treino_gam_dinizia.csv"
arquivo_parametros <- "resultados/unidade07/parametros_padronizacao_gam.csv"

if (!file.exists(arquivo_treino)) {
  stop("Arquivo de treino não encontrado. Execute o Script 02 da Unidade 7.")
}

if (!file.exists(arquivo_parametros)) {
  stop("Arquivo de parâmetros não encontrado. Execute o Script 02 da Unidade 7.")
}

# ------------------------------------------------------------
# 5. Leitura dos dados
# ------------------------------------------------------------

treino <- read_csv(arquivo_treino, show_col_types = FALSE)
parametros <- read_csv(arquivo_parametros, show_col_types = FALSE)

# ------------------------------------------------------------
# 6. Checagem das variáveis
# ------------------------------------------------------------

if (!"pa" %in% names(treino)) {
  stop("A variável resposta 'pa' não foi encontrada no arquivo de treino.")
}

if (!"variavel_z" %in% names(parametros)) {
  stop("O arquivo parametros_padronizacao_gam.csv precisa conter a coluna 'variavel_z'.")
}

treino <- treino |>
  mutate(pa = as.integer(pa))

if (!all(treino$pa %in% c(0, 1))) {
  stop("A variável resposta 'pa' deve conter apenas 0 = background e 1 = presença.")
}

vars_z <- parametros$variavel_z |> unique()
vars_z <- vars_z[vars_z %in% names(treino)]

if (length(vars_z) < 2) {
  stop("Número insuficiente de variáveis padronizadas para ajustar o GAM.")
}

variancias <- treino |>
  summarise(
    across(
      all_of(vars_z),
      ~ stats::var(.x, na.rm = TRUE)
    )
  )

vars_z <- vars_z[
  as.numeric(variancias[1, vars_z]) > 0 &
    !is.na(as.numeric(variancias[1, vars_z]))
]

if (length(vars_z) < 2) {
  stop("Após remover variáveis sem variação, restaram menos de duas variáveis.")
}

# ------------------------------------------------------------
# 7. Fórmula GAM
# ------------------------------------------------------------

k_suave <- 5

termos_suaves <- paste0(
  "s(`",
  vars_z,
  "`, k = ",
  k_suave,
  ", bs = 'tp')"
)

formula_gam <- as.formula(
  paste(
    "pa ~",
    paste(termos_suaves, collapse = " + ")
  )
)

# ------------------------------------------------------------
# 8. Ajuste do modelo GAM
# ------------------------------------------------------------

modelo_gam <- mgcv::gam(
  formula_gam,
  data = treino,
  family = binomial(link = "logit"),
  method = "REML",
  select = TRUE
)

# ------------------------------------------------------------
# 9. Salvar modelo e fórmula
# ------------------------------------------------------------

saveRDS(
  modelo_gam,
  "resultados/unidade07/modelo_gam_dinizia.rds"
)

saveRDS(
  formula_gam,
  "resultados/unidade07/formula_gam_dinizia.rds"
)

# ------------------------------------------------------------
# 10. Tabelas de ajuste e suavizadores
# ------------------------------------------------------------

resumo_modelo <- broom::glance(modelo_gam) |>
  mutate(
    n = nobs(modelo_gam),
    aic = AIC(modelo_gam),
    bic = BIC(modelo_gam),
    logLik = as.numeric(logLik(modelo_gam)),
    n_variaveis = length(vars_z),
    k_suave = k_suave,
    convergiu = modelo_gam$converged
  )

write_csv(
  resumo_modelo,
  "tabelas/unidade07/ajuste_gam_dinizia.csv"
)

suaves <- broom::tidy(
  modelo_gam,
  parametric = FALSE
) |>
  mutate(
    variavel_z = term |>
      str_replace_all("s\\(", "") |>
      str_replace_all("\\)", "") |>
      str_replace_all("`", ""),
    variavel_original = str_replace(variavel_z, "_z$", ""),
    significancia = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE ~ "ns"
    )
  ) |>
  arrange(p.value)

write_csv(
  suaves,
  "tabelas/unidade07/suavizadores_gam_dinizia.csv"
)

formula_txt <- tibble(
  modelo = "GAM binomial com suavizadores univariados",
  formula = paste(deparse(formula_gam), collapse = " "),
  k_suave = k_suave,
  n_variaveis = length(vars_z),
  n_suavizadores = length(modelo_gam$smooth),
  n_treino = nrow(treino),
  n_presencas = sum(treino$pa == 1),
  n_background = sum(treino$pa == 0),
  convergiu = modelo_gam$converged,
  aic = AIC(modelo_gam),
  bic = BIC(modelo_gam)
)

write_csv(
  formula_txt,
  "tabelas/unidade07/formula_gam_dinizia.csv"
)

variaveis_gam <- tibble(
  ordem = seq_along(vars_z),
  variavel_z = vars_z,
  variavel_original = str_replace(vars_z, "_z$", "")
)

write_csv(
  variaveis_gam,
  "tabelas/unidade07/variaveis_usadas_modelo_gam.csv"
)

# ------------------------------------------------------------
# 11. Diagnóstico básico do GAM
# ------------------------------------------------------------

sink("tabelas/unidade07/diagnostico_gam_check.txt")
print(mgcv::gam.check(modelo_gam))
sink()

# ------------------------------------------------------------
# 12. Figura dos suavizadores
# ------------------------------------------------------------

png(
  "figuras/unidade07/unidade07_suavizadores_gam.png",
  width = 3600,
  height = 3000,
  res = 300
)

n_smooth <- length(modelo_gam$smooth)
n_col <- ifelse(n_smooth <= 4, 2, 3)
n_row <- ceiling(n_smooth / n_col)

par(
  mfrow = c(n_row, n_col),
  mar = c(4, 4, 3, 1),
  oma = c(0, 0, 3, 0)
)

plot(
  modelo_gam,
  pages = 1,
  shade = TRUE,
  seWithMean = TRUE,
  scale = 0,
  rug = TRUE,
  residuals = FALSE,
  scheme = 1,
  col = "black",
  shade.col = "grey85"
)

mtext(
  expression("Suavizadores do GAM para " * italic("Dinizia excelsa")),
  outer = TRUE,
  cex = 1.4,
  font = 2
)

dev.off()

# ------------------------------------------------------------
# 13. Mensagem final
# ------------------------------------------------------------

message("GAM ajustado com sucesso para o bioma Amazônia.")
message("Número de variáveis utilizadas: ", length(vars_z))
message("Número de suavizadores: ", length(modelo_gam$smooth))
message("Convergência: ", modelo_gam$converged)
message("AIC: ", round(AIC(modelo_gam), 2))
message("BIC: ", round(BIC(modelo_gam), 2))

print(resumo_modelo)
