# ============================================================
# Unidade 3 - Variáveis Ambientais em SDM
# Script 05: Exemplo conceitual EarthEnv
# ============================================================

dir.create("tabelas/unidade03", recursive = TRUE, showWarnings = FALSE)

earthenv_variaveis <- data.frame(
  grupo = c(
    "Topografia",
    "Heterogeneidade",
    "Habitat",
    "Clima"
  ),
  exemplo = c(
    "Elevação e rugosidade",
    "Amplitude ambiental local",
    "Cobertura e estrutura do habitat",
    "Gradientes climáticos globais"
  ),
  uso_em_sdm = c(
    "Representar gradientes altitudinais",
    "Identificar ambientes heterogêneos",
    "Caracterizar disponibilidade de habitat",
    "Complementar variáveis climáticas"
  )
)

write.csv(
  earthenv_variaveis,
  "tabelas/unidade03/earthenv_variaveis_exemplo.csv",
  row.names = FALSE
)

message("Tabela conceitual EarthEnv criada.")
