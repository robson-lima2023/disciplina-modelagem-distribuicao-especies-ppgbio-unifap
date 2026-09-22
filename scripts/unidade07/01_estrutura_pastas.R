source("scripts/_bootstrap.R")

# ============================================================
# Unidade 7 - GAM em SDM
# Script 01: Estrutura de pastas
# ============================================================
pastas <- c(
  "dados/unidade07",
  "dados/unidade07/processados",
  "figuras/unidade07",
  "tabelas/unidade07",
  "resultados/unidade07"
)

for (p in pastas) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

message("Estrutura de pastas da Unidade 7 criada com sucesso.")
