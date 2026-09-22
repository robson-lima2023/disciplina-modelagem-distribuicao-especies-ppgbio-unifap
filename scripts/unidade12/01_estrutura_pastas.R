source("scripts/_bootstrap.R")

# ============================================================
# Unidade 12 - Avaliação de modelos em SDM
# Script 01: Estrutura de pastas
# ============================================================
pastas <- c(
  "dados/unidade12",
  "dados/unidade12/processados",
  "figuras/unidade12",
  "tabelas/unidade12",
  "resultados/unidade12"
)

for (p in pastas) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

message("Estrutura de pastas da Unidade 12 criada com sucesso.")
