source("scripts/_bootstrap.R")

# ============================================================
# Unidade 4 - Multicolinearidade em SDM
# Script 01: Estrutura de pastas
# ============================================================
pastas <- c(
  "dados/unidade04",
  "dados/unidade04/processados",
  "figuras/unidade04",
  "tabelas/unidade04",
  "resultados/unidade04"
)

for (p in pastas) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

message("Estrutura de pastas da Unidade 4 criada com sucesso.")
