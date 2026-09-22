source("scripts/_bootstrap.R")

# ============================================================
# Unidade 16 - Incertezas em SDM
# Script 01: Estrutura de pastas
# ============================================================
pastas <- c(
  "dados/unidade16",
  "dados/unidade16/processados",
  "figuras/unidade16",
  "tabelas/unidade16",
  "resultados/unidade16",
  "resultados/unidade16/componentes"
)

for (p in pastas) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

message("Estrutura de pastas da Unidade 16 criada com sucesso.")

