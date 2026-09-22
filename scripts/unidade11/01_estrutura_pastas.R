source("scripts/_bootstrap.R")

# ============================================================
# Unidade 11 - Modelagem Ensemble em SDM
# Script 01: Estrutura de pastas
# ============================================================
pastas <- c(
  "dados/unidade11",
  "dados/unidade11/processados",
  "figuras/unidade11",
  "tabelas/unidade11",
  "resultados/unidade11"
)

for (p in pastas) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

message("Estrutura de pastas da Unidade 11 criada com sucesso.")
