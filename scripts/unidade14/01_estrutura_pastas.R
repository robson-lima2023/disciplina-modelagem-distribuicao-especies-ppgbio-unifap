source("scripts/_bootstrap.R")

# ============================================================
# Unidade 14 - Transferência, extrapolação, MESS e MOP
# Script 01: Estrutura de pastas
# ============================================================
pastas <- c(
  "dados/unidade14",
  "dados/unidade14/processados",
  "figuras/unidade14",
  "tabelas/unidade14",
  "resultados/unidade14",
  "resultados/unidade14/mess",
  "resultados/unidade14/mop",
  "resultados/unidade14/extrapolacao"
)

for (p in pastas) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

message("Estrutura de pastas da Unidade 14 criada com sucesso.")
