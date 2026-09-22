source("scripts/_bootstrap.R")

# ============================================================
# Unidade 8 - Random Forest em SDM
# Script 01: Estrutura de pastas
# ============================================================
pastas <- c(
  "dados/unidade08",
  "dados/unidade08/processados",
  "figuras/unidade08",
  "tabelas/unidade08",
  "resultados/unidade08"
)

for (p in pastas) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

message("Estrutura de pastas da Unidade 8 criada com sucesso.")
