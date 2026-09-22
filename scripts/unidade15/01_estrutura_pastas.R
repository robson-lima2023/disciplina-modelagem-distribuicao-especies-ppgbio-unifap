source("scripts/_bootstrap.R")

# ============================================================
# Unidade 15 - Paleoclima e nicho climático passado
# Script 01: Estrutura de pastas
# ============================================================
pastas <- c(
  "dados/unidade15",
  "dados/unidade15/brutos",
  "dados/unidade15/brutos/paleoclim",
  "dados/unidade15/processados",
  "figuras/unidade15",
  "tabelas/unidade15",
  "resultados/unidade15",
  "resultados/unidade15/modelos_individuais",
  "resultados/unidade15/ensemble",
  "resultados/unidade15/estabilidade"
)

for (p in pastas) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

message("Estrutura de pastas da Unidade 15 criada com sucesso.")
