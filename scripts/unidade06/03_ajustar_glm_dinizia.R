source("scripts/_bootstrap.R")

# ============================================================
# Unidade 6 - GLM em SDM
# Script 03: Ajustar GLM para Dinizia excelsa
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr", "readr", "ggplot2",
  "broom", "stringr", "tibble"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 2. Diretório raiz do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios de saída
# ------------------------------------------------------------

dir.create("figuras/unidade06", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade06", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade06", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_treino <- "dados/unidade06/processados/treino_glm_dinizia.csv"
arquivo_parametros <- "resultados/unidade06/parametros_padronizacao_glm.csv"

if (!file.exists(arquivo_treino)) {
  stop("Arquivo de treino não encontrado. Execute o Script 02 da Unidade 6.")
}

if (!file.exists(arquivo_parametros)) {
  stop("Arquivo de parâmetros não encontrado. Execute o Script 02 da Unidade 6.")
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
  stop("O arquivo parametros_padronizacao_glm.csv precisa conter a coluna 'variavel_z'.")
}

vars_z <- parametros$variavel_z |> unique()
vars_z <- vars_z[vars_z %in% names(treino)]

if (length(vars_z) < 2) {
  stop("Número insuficiente de variáveis padronizadas para ajustar o GLM.")
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
# 7. Fórmula com termos lineares e quadráticos
# ------------------------------------------------------------

termos_lineares <- paste0("`", vars_z, "`")
termos_quadraticos <- paste0("I(`", vars_z, "`^2)")

formula_glm <- as.formula(
  paste(
    "pa ~",
    paste(
      c(termos_lineares, termos_quadraticos),
      collapse = " + "
    )
  )
)

# ------------------------------------------------------------
# 8. Ajuste do modelo GLM
# ------------------------------------------------------------

modelo_glm <- glm(
  formula_glm,
  data = treino,
  family = binomial(link = "logit"),
  control = glm.control(maxit = 100)
)

if (!modelo_glm$converged) {
  warning("O GLM não convergiu. Verifique separação, colinearidade residual ou excesso de termos.")
}

# ------------------------------------------------------------
# 9. Salvar modelo e fórmula
# ------------------------------------------------------------

saveRDS(
  modelo_glm,
  "resultados/unidade06/modelo_glm_dinizia.rds"
)

saveRDS(
  formula_glm,
  "resultados/unidade06/formula_glm_dinizia.rds"
)

# ------------------------------------------------------------
# 10. Tabelas de coeficientes e ajuste
# ------------------------------------------------------------

coeficientes <- broom::tidy(
  modelo_glm,
  conf.int = TRUE
) |>
  mutate(
    significancia = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE ~ "ns"
    ),
    tipo = case_when(
      term == "(Intercept)" ~ "Intercepto",
      stringr::str_detect(term, "\\^2") ~ "Quadrático",
      TRUE ~ "Linear"
    ),
    variavel = term |>
      stringr::str_replace_all("I\\(", "") |>
      stringr::str_replace_all("\\^2\\)", "") |>
      stringr::str_replace_all("`", "") |>
      stringr::str_replace_all("_z", "")
  ) |>
  arrange(p.value)

write_csv(
  coeficientes,
  "tabelas/unidade06/coeficientes_glm_dinizia.csv"
)

ajuste <- broom::glance(modelo_glm) |>
  mutate(
    n = nobs(modelo_glm),
    aic = AIC(modelo_glm),
    bic = BIC(modelo_glm),
    logLik = as.numeric(logLik(modelo_glm)),
    n_variaveis = length(vars_z),
    n_parametros = length(stats::coef(modelo_glm)),
    convergiu = modelo_glm$converged
  )

write_csv(
  ajuste,
  "tabelas/unidade06/ajuste_glm_dinizia.csv"
)

# ------------------------------------------------------------
# 11. Variáveis efetivamente usadas
# ------------------------------------------------------------

variaveis_glm <- tibble(
  ordem = seq_along(vars_z),
  variavel_z = vars_z,
  variavel_original = stringr::str_replace(vars_z, "_z$", "")
)

write_csv(
  variaveis_glm,
  "tabelas/unidade06/variaveis_usadas_modelo_glm.csv"
)

# ------------------------------------------------------------
# 12. Gráfico dos coeficientes
# ------------------------------------------------------------

coef_plot <- coeficientes |>
  filter(term != "(Intercept)") |>
  mutate(
    abs_estimate = abs(estimate),
    label = paste0(variavel, " (", tipo, ")")
  ) |>
  arrange(desc(abs_estimate)) |>
  slice_head(n = 30)

g <- ggplot(
  coef_plot,
  aes(
    x = reorder(label, estimate),
    y = estimate,
    fill = tipo
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15,
    width = 0.75
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.25,
    linewidth = 0.35
  ) +
  coord_flip() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey30"
  ) +
  labs(
    title = expression("Coeficientes do GLM para " * italic("Dinizia excelsa")),
    subtitle = "Modelo binomial com termos lineares e quadráticos; barras indicam IC 95%",
    x = NULL,
    y = "Estimativa padronizada na escala logit",
    fill = "Tipo de termo"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade06/unidade06_coeficientes_glm.png",
  plot = g,
  width = 10,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Salvar resumo textual da fórmula
# ------------------------------------------------------------

formula_txt <- tibble(
  modelo = "GLM binomial com termos lineares e quadráticos",
  formula = paste(deparse(formula_glm), collapse = " "),
  n_variaveis = length(vars_z),
  n_parametros = length(stats::coef(modelo_glm)),
  n_treino = nrow(treino),
  n_presencas = sum(treino$pa == 1),
  n_background = sum(treino$pa == 0),
  convergiu = modelo_glm$converged,
  aic = AIC(modelo_glm),
  bic = BIC(modelo_glm)
)

write_csv(
  formula_txt,
  "tabelas/unidade06/formula_glm_dinizia.csv"
)

# ------------------------------------------------------------
# 14. Mensagem final
# ------------------------------------------------------------

message("GLM ajustado com sucesso para o bioma Amazônia.")
message("Número de variáveis utilizadas: ", length(vars_z))
message("Número de parâmetros estimados: ", length(stats::coef(modelo_glm)))
message("Convergência: ", modelo_glm$converged)
message("AIC: ", round(AIC(modelo_glm), 2))
message("BIC: ", round(BIC(modelo_glm), 2))

print(ajuste)
