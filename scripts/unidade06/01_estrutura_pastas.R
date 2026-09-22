source("scripts/_bootstrap.R")

# ============================================================
# Unidade 6 - GLM aplicado à SDM
# Script 01: Estrutura de pastas
# ============================================================
pastas <- c(
  "dados/unidade06",
  "dados/unidade06/processados",
  "figuras/unidade06",
  "tabelas/unidade06",
  "resultados/unidade06"
)

for (p in pastas) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

message("Estrutura de pastas da Unidade 6 criada com sucesso.")

