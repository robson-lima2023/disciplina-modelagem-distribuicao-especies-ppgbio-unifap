source("scripts/_bootstrap.R")

# ============================================================
# Unidade 10 - Maxent em SDM
# Script 01: Estrutura de pastas
# ============================================================
pastas <- c(
  "dados/unidade10",
  "dados/unidade10/processados",
  "figuras/unidade10",
  "tabelas/unidade10",
  "resultados/unidade10"
)

for (p in pastas) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

message("Estrutura de pastas da Unidade 10 criada com sucesso.")
