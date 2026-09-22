source("scripts/_bootstrap.R")

# ============================================================
# Unidade 9 - BRT em SDM
# Script 01: Estrutura de pastas
# ============================================================
pastas <- c(
  "dados/unidade09",
  "dados/unidade09/processados",
  "figuras/unidade09",
  "tabelas/unidade09",
  "resultados/unidade09"
)

for (p in pastas) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

message("Estrutura de pastas da Unidade 9 criada com sucesso.")
