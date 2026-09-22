# ============================================================
# Unidade 4 - Multicolinearidade em SDM
# Script 02: Simulação de variáveis ambientais correlacionadas
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("dados/unidade04/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade04", recursive = TRUE, showWarnings = FALSE)

set.seed(123)

n <- 1000

# ------------------------------------------------------------
# Simulação conceitual
# ------------------------------------------------------------
# O objetivo é criar um conjunto de variáveis ambientais em que
# algumas sejam independentes e outras sejam fortemente correlacionadas.
#
# Isso representa um cenário comum em SDM:
# - variáveis térmicas correlacionadas entre si;
# - variáveis de precipitação correlacionadas entre si;
# - elevação associada à temperatura;
# - variáveis edáficas parcialmente independentes.

dados_env <- data.frame(
  temperatura_media = rnorm(n, mean = 24, sd = 4),
  precipitacao_anual = rnorm(n, mean = 1800, sd = 500),
  elevacao = runif(n, min = 0, max = 1800),
  ph_solo = rnorm(n, mean = 5.5, sd = 0.7),
  argila = runif(n, min = 10, max = 70)
)

dados_env <- dados_env %>%
  mutate(
    temperatura_maxima = temperatura_media + rnorm(n, mean = 3, sd = 1),
    temperatura_minima = temperatura_media - rnorm(n, mean = 4, sd = 1),
    sazonalidade_termica = 0.7 * temperatura_maxima -
      0.4 * temperatura_minima + rnorm(n, 0, 1),
    precipitacao_trimestre_umido = precipitacao_anual * 0.45 +
      rnorm(n, 0, 120),
    precipitacao_trimestre_seco = precipitacao_anual * 0.12 +
      rnorm(n, 0, 60),
    areia = 100 - argila + rnorm(n, 0, 8)
  ) %>%
  mutate(
    temperatura_media = temperatura_media - 0.003 * elevacao,
    temperatura_maxima = temperatura_maxima - 0.003 * elevacao,
    temperatura_minima = temperatura_minima - 0.003 * elevacao
  )

write_csv(
  dados_env,
  "dados/unidade04/processados/variaveis_ambientais_simuladas.csv"
)

resumo <- dados_env %>%
  summarise(
    across(
      everything(),
      list(
        media = mean,
        desvio = sd,
        minimo = min,
        maximo = max
      )
    )
  )

write_csv(
  resumo,
  "tabelas/unidade04/resumo_variaveis_ambientais.csv"
)

message("Variáveis ambientais simuladas e exportadas com sucesso.")
